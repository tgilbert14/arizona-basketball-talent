## ===========================================================================
## nightlyRefresh.R -- the unattended nightly refresh orchestrator.
##
## Runs the whole pipeline with a safety net at every step: snapshot first,
## scrape, geocode, audit + validate against the snapshot, precompute the
## default renders, then commit + push + deploy + verify -- and restores
## the snapshot the moment validation says the night went sideways.
##
##   Rscript scripts/nightlyRefresh.R                 # the full night
##   Rscript scripts/nightlyRefresh.R no-rosters      # skip a stage
##
## Skip flags (positional, any order):
##   no-classes  no-rosters  no-geocode  no-records  no-precompute
##   no-push     no-deploy   no-alert
##
## Stages:
##   S0 preflight   lock, quick_check, 247 reachability probe, git
##                  ls-remote probe (a dead credential fails here, not
##                  after a full scrape), prune
##   S1 snapshot    content hash + whole-file db copy to backups/
##   S2 ingest      refreshClassYear.R (both sports, newest cycle, PLUS an
##                  ahead-year probe of MAX(Year)+1 with --allow-empty: the
##                  night 247 opens the next cycle's pages its rows land,
##                  MAX(Year) advances, and the normal scrape owns the new
##                  cycle from then on -- rollover is automatic, no manual
##                  seed; an ahead-year failure only ever warns),
##                  scrapeRosters.R (both sports), fetchOutcomes.R (CFBD)
##   S3 enrich      backfillProfiles.R (hometowns from 247 profiles, cap 40)
##                  then geocodeMissing.R (both non-fatal)
##   S3.5 qc        qcSweep.R -- geo/dupe/completeness flags into the
##                  qc_flags ledger + placeholder-zero auto-fix; new
##                  high-severity flags surface as a manifest note
##                  (non-fatal; accepted flags never re-raise)
##   S4 validate    auditRefreshHoles.R + validateRefresh.R vs the snapshot;
##                  either failing restores the snapshot and aborts
##   S5 ledger      content hash again -> changed?; write refresh_log row
##   S6 precompute  precomputeDefaults.R (only if data changed), then
##                  updateManifest.R to re-checksum manifest.json; a failure
##                  in either degrades the night and blocks publishing (never
##                  ship a db/rds/manifest mismatch)
##   S6.5 brief     weeklyBrief.R rewrites docs/brief/ when data changed
##                  (non-fatal)
##   S7 commit+push git add db + precomputed + manifests + docs brief,
##                  commit, push
##   S8 deploy      scripts/deployApp.R (shinyapps.io)
##   S9 verify      GET both live URLs, 2 retries 30 s apart (idle-sleep)
##   S10 report     manifests, summary, gh alert, release lock
##
## Exit codes: 0 = success or no-op, 1 = failure, 2 = degraded (any stage
## failed or warned -- check the manifest stages for whether it published).
## Runs from the repo root; resolves it from its own path if launched
## elsewhere (the scheduled-task ps1 wrapper cds there anyway).
## ===========================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
  library(httr)
})

## ---------------------------------------------------------------------------
## locate the repo root and settle in
## ---------------------------------------------------------------------------
resolve_root <- function() {
  full <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", full, value = TRUE)
  if (length(fa) >= 1) {
    script <- normalizePath(sub("^--file=", "", fa[1]), winslash = "/",
                            mustWork = FALSE)
    cand <- dirname(dirname(script))          # scripts/ -> repo root
    if (file.exists(file.path(cand, "data", "recruiting.db"))) return(cand)
  }
  getwd()
}
setwd(resolve_root())
if (!file.exists(file.path("data", "recruiting.db"))) {
  stop("nightlyRefresh.R must run from the repo root ",
       "(data/recruiting.db not found from ", getwd(), ")")
}

source(file.path("scripts", "lib", "refresh_utils.R"))

## ---------------------------------------------------------------------------
## flags + shared state
## ---------------------------------------------------------------------------
args <- tolower(commandArgs(trailingOnly = TRUE))
flag <- function(x) x %in% args
no_classes    <- flag("no-classes")
no_rosters    <- flag("no-rosters")
no_geocode    <- flag("no-geocode")
no_records    <- flag("no-records")
no_precompute <- flag("no-precompute")
no_push       <- flag("no-push")
no_deploy     <- flag("no-deploy")
no_alert      <- flag("no-alert")

## Publishing is deliberately pinned to main. The scheduled wrapper runs in
## a dedicated automation worktree/branch so a developer's interactive
## checkout can never decide where the nightly data commit lands.
publish_branch <- trimws(Sys.getenv("GIRTH_PUBLISH_BRANCH", "main"))
nightly_branch <- trimws(Sys.getenv(
  "GIRTH_NIGHTLY_BRANCH", "automation/nightly-main"))
if (!nzchar(publish_branch)) publish_branch <- "main"
if (!nzchar(nightly_branch)) nightly_branch <- "automation/nightly-main"
dedicated_nightly_runner <- identical(
  trimws(Sys.getenv("GIRTH_NIGHTLY_RUNNER", "0")), "1")

rscript_bin <- file.path(R.home("bin"), "Rscript.exe")
if (!file.exists(rscript_bin)) rscript_bin <- "Rscript"

db_path    <- file.path("data", "recruiting.db")
run_id     <- format(Sys.time(), "%Y%m%d_%H%M%S")
started_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

stages         <- list()
notes          <- character(0)
failed_schools <- character(0)
push_probe_ok       <- TRUE
publish_guard_ok    <- FALSE
data_commit_created <- FALSE
publish_worktree_branch <- ""
lock_acquired       <- FALSE
log_written    <- FALSE
snap           <- NULL
s2_ran         <- FALSE
validated      <- FALSE
pre_hash       <- NULL
post_hash      <- NULL
changed        <- NA
counts_before  <- NULL

## ---------------------------------------------------------------------------
## helpers
## ---------------------------------------------------------------------------
banner <- function(x) cat("\n==== ", x, " ====\n", sep = "")

## the newest cycle the pipeline may touch: recruiting runs ONE year ahead of
## the calendar (the class of N signs Dec N-1 and enrolls fall N), so
## calendar+1 is the active ceiling. 247 opens pages -- with real early
## commits -- up to TWO cycles out, so an uncapped MAX(Year)+1 probe finds
## rows, MAX(Year) advances, and the rollover compounds a year every time
## (this is how 2028 rows landed in July 2026). The cap makes the rollover
## calendar-governed: the ahead probe only fires while MAX(Year) < calendar+1.
cycle_cap <- as.integer(format(Sys.Date(), "%Y")) + 1L

newest_year <- function(db, sport) {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  y <- dbGetQuery(conn, paste0("SELECT MAX(Year) y FROM recruit_class_", sport))$y
  min(y, cycle_cap)
}

quick_check <- function(db) {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(conn, "PRAGMA quick_check")[[1]][1]
}

## the exact browser UA the scrapers send -- read from refreshClassYear.R so
## the probe and the scrape always present the same face to the same host
read_scraper_ua <- function(path = file.path("scripts", "refreshClassYear.R")) {
  fallback <- paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
    "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")
  tryCatch({
    txt <- readLines(path, warn = FALSE)
    i <- grep("UA <- user_agent", txt)[1]
    if (is.na(i)) {
      fallback
    } else {
      block <- paste(txt[i:min(i + 3, length(txt))], collapse = " ")
      pieces <- regmatches(block, gregexpr('"[^"]*"', block))[[1]]
      ua <- paste0(gsub('^"|"$', "", pieces), collapse = "")
      if (grepl("^Mozilla", ua)) ua else fallback
    }
  }, error = function(e) fallback)
}

## run a child Rscript with the orchestrator's db connections all CLOSED
## (every db helper here is short-lived, so nothing is ever open when this
## is called). Captures output for the log + failed-school harvesting.
## harvest_failures = FALSE for the ahead-year probes: before 247 opens a
## cycle's pages EVERY school legitimately gives up, and listing all 16 as
## "fetch problems" in the summary/commit/alert would cry wolf nightly.
run_child <- function(label, script, extra_args = character(0),
                      harvest_failures = TRUE) {
  cat("\n==== ", label, " ====\n", sep = "")
  t0 <- Sys.time()
  out <- suppressWarnings(system2(
    rscript_bin, c(file.path("scripts", script), extra_args),
    stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  if (length(out) > 0) cat(out, sep = "\n")
  mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  cat("[", label, "] ",
      if (status == 0) "ok" else paste0("EXIT CODE ", status),
      " (", mins, " min)\n", sep = "")

  ## harvest per-school failures from the child's own log lines
  if (harvest_failures) {
    m <- regmatches(out, regexec(
      "^\\s*([a-z0-9-]+)\\s+(GAVE UP|DEMOTED|FAILED:|EMPTY RESPONSE)", out))
    slugs <- vapply(m[lengths(m) > 0], function(x) x[2], character(1))
    if (length(slugs) > 0) {
      failed_schools <<- unique(c(failed_schools, slugs))
    }
  }
  list(ok = status == 0, status = status)
}

find_shinyapps_url <- function() {
  dcfs <- list.files(file.path("rsconnect", "shinyapps.io"),
                     pattern = "\\.dcf$", recursive = TRUE,
                     full.names = TRUE)
  for (f in dcfs) {
    lines <- tryCatch(readLines(f, warn = FALSE),
                      error = function(e) character(0))
    hit <- grep("^url:", lines, value = TRUE)
    if (length(hit) > 0) return(trimws(sub("^url:", "", hit[1])))
  }
  "https://t-lama.shinyapps.io/Big-12-Talent-Pathways/"
}

find_connect_url <- function() {
  idx <- file.path("docs", "index.html")
  if (!file.exists(idx)) return(NA_character_)
  txt <- tryCatch(readLines(idx, warn = FALSE),
                  error = function(e) character(0))
  m <- unlist(regmatches(txt, regexpr(
    "https://[a-z0-9-]+\\.share\\.connect\\.posit\\.cloud/?", txt)))
  if (length(m) > 0) m[1] else NA_character_
}

## GET with retries -- the Connect Cloud free tier idle-sleeps, and the wake
## race means the first hit can catch the worker mid-restart
## marker: optional literal string that must appear in the 200 response body
## (e.g. the app's freshness badge "data updated Jul 12, 2026") -- proves the
## NEW bundle is being served, not just that A server answered. Only usable on
## hosts that serve the app HTML directly (shinyapps does; Connect Cloud's
## share URL serves an iframe wrapper, so it gets a plain 200 check with a
## longer cold-start budget instead).
verify_url <- function(url, attempts = 3, wait_s = 30, marker = NULL) {
  for (i in seq_len(attempts)) {
    resp <- tryCatch(GET(url, timeout(60)), error = function(e) NULL)
    code <- if (is.null(resp)) -1L else status_code(resp)
    ok <- identical(code, 200L)
    if (ok && !is.null(marker)) {
      body <- tryCatch(content(resp, as = "text", encoding = "UTF-8"),
                       error = function(e) "")
      ok <- grepl(marker, body, fixed = TRUE)
      cat("  attempt ", i, ": ", code,
          if (ok) " + freshness marker found" else
            " but freshness marker missing (old bundle still serving?)",
          "\n", sep = "")
    } else {
      cat("  attempt ", i, ": ", code, "\n", sep = "")
    }
    if (ok) return(TRUE)
    if (i < attempts) Sys.sleep(wait_s)
  }
  FALSE
}

git_run <- function(...) suppressWarnings(system2("git", c(...)))

git_capture <- function(...) {
  out <- suppressWarnings(system2(
    "git", c(...), stdout = TRUE, stderr = FALSE))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(ok = identical(status, 0L), status = status, output = out)
}

git_state_exists <- function(name) {
  state <- git_capture("rev-parse", "--git-path", name)
  if (!isTRUE(state$ok) || !length(state$output)) return(FALSE)
  lines <- trimws(as.character(state$output))
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(FALSE)
  path <- tail(lines, 1L)
  dir.exists(path) || file.exists(path)
}

## compact manifest per the shared contract
build_compact <- function(status, finished_at, counts_now = NULL) {
  if (is.null(counts_now)) {
    counts_now <- tryCatch(table_counts(db_path),
                           error = function(e) integer(0))
  }
  list(run_id         = run_id,
       started_at     = started_at,
       finished_at    = finished_at,
       status         = status,
       changed        = isTRUE(changed),
       row_counts     = as.list(counts_now),
       failed_schools = I(as.character(failed_schools)),
       stages         = stages)
}

update_log_row <- function(finished_at, status, tables_json, note_txt) {
  conn <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbExecute(conn, paste0(
    "UPDATE refresh_log SET finished_at = ?, status = ?, ",
    "tables_json = ?, notes = ? WHERE run_id = ?"),
    params = list(finished_at, status, tables_json, note_txt, run_id))
}
## Public landing-page beacon: source capture and pipeline health are different
## facts. Keep them separate so a failed validation cannot be presented as a
## new dataset. This intentionally publishes a concise controlled message, not
## raw run notes (which can contain operator-only detail).
source_capture_date <- function(db) {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  tables <- intersect(
    c("recruit_class_football", "recruit_class_basketball",
      "roster_football", "roster_basketball"),
    dbListTables(conn)
  )
  stamps <- unlist(lapply(tables, function(tbl) {
    fields <- dbListFields(conn, tbl)
    if (!"ScrapedAt" %in% fields) return(NA_character_)
    value <- dbGetQuery(conn, paste0("SELECT MAX(ScrapedAt) AS stamp FROM ",
                                    tbl))$stamp
    if (!length(value) || is.na(value[[1]])) NA_character_ else value[[1]]
  }), use.names = FALSE)
  days <- suppressWarnings(as.Date(substr(as.character(stamps), 1, 10)))
  days <- days[!is.na(days)]
  if (!length(days)) NULL else max(days)
}

public_pipeline_message <- function(status) {
  switch(tolower(trimws(as.character(status)[1])),
         failed = "Latest refresh failed validation; the previous source snapshot remains published.",
         failure = "Latest refresh failed; the previous source snapshot remains published.",
         error = "Latest refresh failed; the previous source snapshot remains published.",
         degraded = "Latest refresh completed with warnings; review the pipeline status before treating it as a new snapshot.",
         noop = "Latest refresh found no source changes.",
         ok = "Latest refresh completed.",
         "Latest pipeline status is unavailable.")
}

write_public_status <- function(status, checked_at = Sys.time()) {
  tryCatch({
    cnts <- table_counts(db_path)
    getn <- function(tbl) {
      if (tbl %in% names(cnts)) as.integer(cnts[[tbl]]) else 0L
    }
    newest <- tryCatch(
      suppressWarnings(max(newest_year(db_path, "football"),
                           newest_year(db_path, "basketball"), na.rm = TRUE)),
      error = function(e) NA_integer_
    )
    if (!is.finite(newest)) newest <- NA_integer_
    captured <- source_capture_date(db_path)
    checked <- suppressWarnings(as.Date(substr(as.character(checked_at)[1], 1, 10)))
    if (is.na(checked)) checked <- Sys.Date()
    beacon <- list(
      updated = if (is.null(captured)) NULL else format(captured),
      source_capture = if (is.null(captured)) NULL else format(captured),
      pipeline_status = tolower(trimws(as.character(status)[1])),
      pipeline_checked = format(checked),
      pipeline_message = public_pipeline_message(status),
      football_rows = getn("recruit_class_football"),
      basketball_rows = getn("recruit_class_basketball"),
      newest_class = newest,
      brief = "brief/"
    )
    status_paths <- c(file.path("docs", "status.json"),
                      file.path("www", "pipeline-status.json"))
    for (status_path in status_paths) {
      jsonlite::write_json(
        beacon, status_path,
        auto_unbox = TRUE, pretty = TRUE, null = "null",
        na = "null"
      )
    }
    TRUE
  }, error = function(e) {
    cat("[status.json] write skipped (", conditionMessage(e), ")\n", sep = "")
    FALSE
  })
}

## the one exit ramp: ledger + manifests + summary + alert + lock + quit

status_beacon_paths <- function() {
  c(file.path("docs", "status.json"),
    file.path("www", "pipeline-status.json"))
}

restore_nightly_managed <- function(paths, reason) {
  if (!isTRUE(dedicated_nightly_runner) ||
      !identical(publish_worktree_branch, nightly_branch)) {
    return(FALSE)
  }
  paths <- unique(as.character(paths))
  st <- git_run("restore", "--staged", "--worktree", "--", paths)
  if (st != 0L) {
    cat("[status publish] managed-file recovery failed after ",
        reason, "\n", sep = "")
    return(FALSE)
  }

  remaining <- git_capture(
    "status", "--porcelain", "--untracked-files=all", "--", paths)
  remaining_lines <- trimws(as.character(remaining$output))
  remaining_lines <- remaining_lines[nzchar(remaining_lines)]
  if (!isTRUE(remaining$ok) || length(remaining_lines)) {
    cat("[status publish] recovery left managed/untracked files after ",
        reason, "\n", sep = "")
    return(FALSE)
  }
  cat("[status publish] restored managed files after ", reason, "\n", sep = "")
  TRUE
}

publish_status_only <- function(status) {
  paths <- status_beacon_paths()

  if (isTRUE(no_push) || !nzchar(Sys.which("git"))) {
    cat("[status publish] skipped -- push disabled or git unavailable\n")
    return(FALSE)
  }
  if (!isTRUE(publish_guard_ok)) {
    cat("[status publish] skipped -- publish branch guard did not pass\n")
    return(FALSE)
  }

  rollback_paths <- c(
    "data/recruiting.db", "data/refresh-manifest.json",
    "precomputed", "docs/brief", "manifest.json")
  managed_paths <- unique(c(rollback_paths, paths))

  ## S7 may have staged files and then failed to commit. Normalize the index
  ## before deciding what the terminal-status commit owns; keep the worktree
  ## bytes until the branch-specific decision below.
  st <- git_run("restore", "--staged", "--", managed_paths)
  if (st != 0L) {
    cat("[status publish] could not normalize the managed-file index\n")
    return(FALSE)
  }

  if (!isTRUE(data_commit_created)) {
    ## A no-op or blocked publish must not leak a changed DB/precompute bundle.
    ## Destructive cleanup is permitted only in the dedicated automation
    ## worktree; never restore files in an interactive main checkout.
    if (!isTRUE(restore_nightly_managed(
      rollback_paths, "an unpublished/no-op data stage"))) {
      cat("[status publish] skipped -- unsafe or unable to roll back ",
          "managed files\n", sep = "")
      return(FALSE)
    }
  }

  final_data_paths <- if (isTRUE(data_commit_created)) {
    c("data/recruiting.db", "data/refresh-manifest.json")
  } else {
    character(0)
  }
  dirty_paths <- c(paths, final_data_paths)
  sidecar_untracked <- git_run("ls-files", "--error-unmatch",
                               "www/pipeline-status.json") != 0L
  dirty_final <- sidecar_untracked ||
    git_run("diff", "--quiet", "--", dirty_paths) != 0L
  if (!dirty_final) return(TRUE)

  terminal_paths <- c(paths, final_data_paths, "manifest.json")
  if (git_run("diff", "--quiet", "--", "manifest.json") != 0L) {
    cat("[status publish] skipped -- manifest.json has unrelated edits\n")
    restore_nightly_managed(terminal_paths, "a dirty terminal manifest")
    return(FALSE)
  }

  manifest_targets <- c(
    "www/pipeline-status.json",
    if (isTRUE(data_commit_created)) "data/recruiting.db")
  manifest_result <- run_child(
    "update terminal manifest", "updateManifest.R",
    extra_args = paste0("--paths=", paste(manifest_targets, collapse = ",")),
    harvest_failures = FALSE)
  if (!isTRUE(manifest_result$ok)) {
    restore_nightly_managed(terminal_paths, "terminal manifest failure")
    return(FALSE)
  }

  commit_paths <- c(paths, final_data_paths, "manifest.json")
  st <- git_run("add", "--", commit_paths)
  if (st == 0L) {
    status_msg <- paste0(
      "Finalize pipeline status ", format(Sys.Date()), " [auto]")
    st <- git_run("commit", "-m", shQuote(status_msg, type = "cmd"))
  }
  if (st != 0L) {
    restore_nightly_managed(commit_paths, "terminal status commit failure")
    cat("[status publish] terminal status commit failed\n")
    return(FALSE)
  }

  if (!isTRUE(push_probe_ok)) {
    cat("[status publish] committed locally; remote probe failed earlier\n")
    return(TRUE)
  }

  st <- git_run("pull", "--rebase", "origin", publish_branch)
  if (st != 0L) {
    try(git_run("rebase", "--abort"), silent = TRUE)
  }
  if (st == 0L) {
    st <- git_run("push", "origin", paste0("HEAD:", publish_branch))
  }
  if (st != 0L) cat("[status publish] git update did not reach origin\n")
  st == 0L
}

finalize <- function(status, exit_code, extra_note = NULL) {
  if (!is.null(extra_note)) notes <<- c(notes, extra_note)
  finished_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  counts_now <- tryCatch(table_counts(db_path), error = function(e) NULL)
  tables_json <- tryCatch(
    as.character(jsonlite::toJSON(as.list(counts_now), auto_unbox = TRUE)),
    error = function(e) "{}")
  note_txt <- paste(notes, collapse = " | ")

  ## ledger -- guarded, the db may be unreadable on a corrupt-abort
  try({
    ensure_refresh_log(db_path)
    if (isTRUE(log_written)) {
      update_log_row(finished_at, status, tables_json, note_txt)
    } else {
      write_refresh_log(db_path, run_id, started_at, finished_at, status,
                        tables_json, note_txt)
    }
  }, silent = TRUE)

  ## Keep the local beacon truthful even on early failures; the release path
  ## decides whether it is published, but it must never retain a fake success.
  status_written <- tryCatch(
    write_public_status(status, finished_at),
    error = function(e) FALSE
  )

  ## The compact manifest must exist before the terminal commit can include
  ## it. Record this final Git step as pending there; the gitignored full
  ## manifest below is rewritten with its actual outcome.
  status_publish_expected <- isTRUE(status_written) &&
    !isTRUE(no_push) && nzchar(Sys.which("git")) &&
    isTRUE(publish_guard_ok)
  stages$status_publish <- if (!isTRUE(status_written)) {
    "warn"
  } else if (status_publish_expected) {
    "pending"
  } else {
    "skipped"
  }

  compact <- build_compact(status, finished_at, counts_now)
  try(write_manifest(file.path("data", "refresh-manifest.json"), compact),
      silent = TRUE)

  status_publish_ok <- if (isTRUE(status_written)) {
    isTRUE(publish_status_only(status))
  } else {
    FALSE
  }

  if (!isTRUE(status_written)) {
    cleanup_paths <- c(
      status_beacon_paths(), "data/recruiting.db",
      "data/refresh-manifest.json", "manifest.json",
      if (!isTRUE(data_commit_created)) c("precomputed", "docs/brief"))
    cleanup_ok <- if (!isTRUE(no_push)) {
      restore_nightly_managed(cleanup_paths, "status-beacon write failure")
    } else {
      TRUE
    }
    notes <<- c(
      notes,
      "terminal pipeline status beacon could not be written",
      if (!cleanup_ok) paste0(
        "managed-file cleanup was incomplete; the dedicated worktree ",
        "requires recovery before its next run"))
    if (!identical(status, "failed")) {
      status <- "degraded"
      exit_code <- max(as.integer(exit_code), 2L)
    }
  } else if (status_publish_expected) {
    stages$status_publish <- if (status_publish_ok) "ok" else "warn"
    if (!status_publish_ok) {
      notes <<- c(notes, paste0(
        "terminal pipeline status commit/push failed; any clean local ",
        "commit will be retried automatically by the next scheduled run"))
      if (!identical(status, "failed")) {
        status <- "degraded"
        exit_code <- max(as.integer(exit_code), 2L)
      }
    }
  }

  full <- build_compact(status, finished_at, counts_now)
  full$counts_before <- as.list(counts_before)
  full$counts_after  <- as.list(counts_now)
  full$pre_hash      <- pre_hash
  full$post_hash     <- post_hash
  full$snapshot      <- snap
  full$flags         <- I(args)
  full$notes         <- I(notes)
  try(write_manifest(
    file.path("logs", paste0("refresh_manifest_", run_id, ".json")), full),
    silent = TRUE)

  ## summary
  cat("\n==== NIGHTLY REFRESH SUMMARY ====\n")
  if (!is.null(counts_before) && !is.null(counts_now)) {
    for (t in names(counts_before)) {
      a <- counts_now[t]
      a <- if (is.na(a)) NA_integer_ else as.integer(a)
      cat(sprintf("%-28s %6s -> %6s (%+d)\n", t,
                  format(counts_before[[t]], big.mark = ","),
                  format(a, big.mark = ","),
                  a - counts_before[[t]]))
    }
  }
  cat("\nStages:\n")
  for (s in names(stages)) cat(sprintf("  %-22s %s\n", s, stages[[s]]))
  if (length(failed_schools) > 0) {
    cat("Schools with fetch problems:",
        paste(failed_schools, collapse = ", "), "\n")
  }
  if (length(notes) > 0) {
    cat("Notes:\n")
    cat(paste0("  - ", notes, collapse = "\n"), "\n", sep = "")
  }
  cat("\nRun ", run_id, ": status=", status,
      " changed=", isTRUE(changed), " exit=", exit_code, "\n", sep = "")

  if (status %in% c("failed", "degraded") && !no_alert) {
    body <- paste0(
      "Run ", run_id, " finished with status **", status, "**.\n\n",
      "Stages:\n",
      paste0("- ", names(stages), ": ", unlist(stages), collapse = "\n"),
      "\n\n",
      if (length(notes) > 0) {
        paste0("Notes:\n", paste0("- ", notes, collapse = "\n"), "\n\n")
      } else "",
      if (length(failed_schools) > 0) {
        paste0("Schools with fetch problems: ",
               paste(failed_schools, collapse = ", "), "\n")
      } else "")
    posted <- gh_alert(
      paste0("Nightly refresh ", status, " -- ", format(Sys.Date())), body)
    cat("GitHub alert:", if (isTRUE(posted)) "posted" else "not posted", "\n")
  }

  if (isTRUE(lock_acquired)) release_lock()
  quit(save = "no", status = exit_code)
}

## ===========================================================================
## the night, start to finish
## ===========================================================================
tryCatch({

  ## -------------------------------------------------------------------------
  ## S0 preflight
  ## -------------------------------------------------------------------------
  banner("S0 preflight")
  dir.create("logs", showWarnings = FALSE)
  dir.create("backups", showWarnings = FALSE)

  acquire_lock()                       # stop()s if a fresh lock is held
  lock_acquired <- TRUE
  cat("Lock acquired: logs/refresh.lock\n")

  ## Clear an interrupted rebase before asking for the branch name. Linked
  ## worktrees store this state outside .git, so resolve it through Git.
  if (isTRUE(dedicated_nightly_runner) && nzchar(Sys.which("git"))) {
    Sys.setenv(GIT_TERMINAL_PROMPT = "0", GCM_INTERACTIVE = "Never")
    if (git_state_exists("rebase-merge") ||
        git_state_exists("rebase-apply")) {
      cat("Leftover rebase state found -- running git rebase --abort\n")
      st_abort <- git_run("rebase", "--abort")
      if (st_abort != 0L) {
        stages$preflight <- "failed"
        finalize("failed", 1L,
                 "preflight could not abort an interrupted Git rebase")
      }
      notes <- c(notes, "preflight: aborted a leftover rebase from a prior run")
    }
  }

  ## Refuse to publish from an arbitrary interactive feature branch. This
  ## guard runs before any scrape mutates the database. Manual no-push runs
  ## remain available from any branch for diagnostics.
  if (!no_push && nzchar(Sys.which("git"))) {
    current <- git_capture("branch", "--show-current")
    branch_lines <- trimws(as.character(current$output))
    branch_lines <- branch_lines[nzchar(branch_lines)]
    current_branch <- if (isTRUE(current$ok) && length(branch_lines)) {
      tail(branch_lines, 1L)
    } else {
      ""
    }
    publish_worktree_branch <- current_branch
    allowed_branches <- unique(c(publish_branch, nightly_branch))
    if (!nzchar(current_branch) || !(current_branch %in% allowed_branches)) {
      refusal <- paste0(
        "publish guard: refusing to run from branch '",
        if (nzchar(current_branch)) current_branch else "(detached HEAD)",
        "'; expected '", publish_branch, "' or '", nightly_branch, "'")
      cat("FATAL: ", refusal, "\n", sep = "")
      if (isTRUE(lock_acquired)) {
        try(release_lock(), silent = TRUE)
        lock_acquired <- FALSE
      }
      quit(save = "no", status = 1L)
    }
    publish_guard_ok <- TRUE
    cat("Publish guard: ", current_branch, " -> origin/",
        publish_branch, "\n", sep = "")
  }

  qc <- quick_check(db_path)
  if (!identical(qc, "ok")) {
    stages$preflight <- "failed"
    finalize("failed", 1L,
             paste0("PRAGMA quick_check failed before the run: ", qc))
  }
  cat("quick_check: ok\n")

  ## push preflight: a dead credential re-fails IDENTICALLY every night and
  ## would otherwise only surface after a full scrape. Probe the remote now
  ## so the failure is named from minute one. A miss is a WARN, not an
  ## abort -- the night still earns its keep (scrape + shinyapps deploy are
  ## git-independent) -- but S7 skips the doomed push and a second
  ## consecutive miss escalates to failed at S10.
  if (!no_push && Sys.which("git") != "") {
    Sys.setenv(GIT_TERMINAL_PROMPT = "0")  # fail fast, never prompt
    st <- suppressWarnings(system2(
      "git", c("ls-remote", "--exit-code", "origin",
               paste0("refs/heads/", publish_branch)),
      stdout = FALSE, stderr = FALSE))
    if (identical(st, 0L)) {
      cat("git ls-remote probe: ok\n")
    } else {
      push_probe_ok <- FALSE
      stages$push_probe <- "warn"
      notes <- c(notes, paste0(
        "preflight git ls-remote failed (exit ", st, ") -- expired ",
        "credential or network; the push will be skipped this run"))
      cat("git ls-remote probe FAILED (exit ", st,
          ") -- the push will be skipped this run\n", sep = "")
    }
  }

  scraping <- !(no_classes && no_rosters)
  if (scraping) {
    ua <- read_scraper_ua()
    probe_year <- newest_year(db_path, "football")
    probe_url <- paste0("https://247sports.com/college/arizona/season/",
                        probe_year, "-football/commits/")
    cat("Probing 247 reachability:", probe_url, "\n")
    code <- tryCatch(
      status_code(GET(probe_url, user_agent(ua), timeout(30))),
      error = function(e) -1L)
    if (code != 200) {
      stages$preflight <- "failed"
      finalize("failed", 1L, paste0(
        "247 probe returned ", code,
        " -- aborting before touching the db (site down or blocking)"))
    }
    cat("247 probe: 200 OK\n")
  } else {
    cat("Scraping stages skipped -- 247 probe not needed\n")
  }

  prune_old("backups", "^pre_run_.*\\.db$", 14)
  prune_old("logs", "^refresh_.*\\.(log|json)$", 30)
  stages$preflight <- "ok"

  ## -------------------------------------------------------------------------
  ## S1 snapshot
  ## -------------------------------------------------------------------------
  banner("S1 snapshot")
  counts_before <- table_counts(db_path)
  pre_hash <- db_content_hash(db_path)
  cat("Pre-run content hash:", pre_hash, "\n")
  snap <- snapshot_db(db_path, "backups")
  stages$snapshot <- "ok"

  ## -------------------------------------------------------------------------
  ## S2 ingest (child failures are non-fatal unless EVERYTHING failed --
  ## the S4 gates decide whether the night is salvageable)
  ## -------------------------------------------------------------------------
  banner("S2 ingest")
  if (no_classes) {
    stages$classes_football         <- "skipped"
    stages$classes_basketball       <- "skipped"
    stages$classes_ahead_football   <- "skipped"
    stages$classes_ahead_basketball <- "skipped"
    cat("[classes] skipped by flag\n")
  } else {
    s2_ran <- TRUE
    for (sp in c("football", "basketball")) {
      yr <- newest_year(db_path, sp)
      r <- run_child(paste("classes:", sp, yr), "refreshClassYear.R",
                     c(sp, as.character(yr)))
      stages[[paste0("classes_", sp)]] <- if (r$ok) "ok" else "failed"
      if (!r$ok) {
        notes <- c(notes, paste0("refreshClassYear.R ", sp, " ", yr,
                                 " exited ", r$status))
      }
    }
    ## probe one cycle ahead with --allow-empty, CAPPED at calendar+1: before
    ## 247 opens the pages the child exits 0 without touching the db; the
    ## night rows first land, MAX(Year) advances and the loop above owns the
    ## new cycle from the NEXT run -- that is the rollover, no manual seed.
    ## The cap stops the compounding (247 lists commits two cycles out, so an
    ## uncapped probe kept rolling: 2027 -> 2028 in July 2026). Ahead-year
    ## problems are only ever a warn: the current cycle must publish anyway.
    for (sp in c("football", "basketball")) {
      yr_ahead <- newest_year(db_path, sp) + 1L
      if (yr_ahead > cycle_cap) {
        stages[[paste0("classes_ahead_", sp)]] <- "skipped"
        cat("[classes ahead: ", sp, "] skipped -- ", yr_ahead,
            " is beyond the calendar+1 ceiling (", cycle_cap, ")\n", sep = "")
        next
      }
      r <- run_child(paste("classes ahead:", sp, yr_ahead),
                     "refreshClassYear.R",
                     c(sp, as.character(yr_ahead), "--allow-empty"),
                     harvest_failures = FALSE)
      stages[[paste0("classes_ahead_", sp)]] <- if (r$ok) "ok" else "warn"
      if (!r$ok) {
        notes <- c(notes, paste0("refreshClassYear.R ", sp, " ", yr_ahead,
                                 " (ahead-year probe) exited ", r$status,
                                 " (non-fatal; retried next night)"))
      }
    }
  }

  if (no_rosters) {
    stages$rosters_football   <- "skipped"
    stages$rosters_basketball <- "skipped"
    cat("[rosters] skipped by flag\n")
  } else {
    s2_ran <- TRUE
    for (sp in c("football", "basketball")) {
      r <- run_child(paste("rosters:", sp), "scrapeRosters.R", sp)
      stages[[paste0("rosters_", sp)]] <- if (r$ok) "ok" else "failed"
      if (!r$ok) {
        notes <- c(notes, paste0("scrapeRosters.R ", sp,
                                 " exited ", r$status))
      }
    }
  }

  if (no_records) {
    stages$records <- "skipped"
    cat("[records] skipped by flag\n")
  } else if (Sys.getenv("CFBD_API_KEY") == "") {
    stages$records <- "skipped"
    cat("[records] skipped -- no CFBD_API_KEY in this session's .Renviron\n")
  } else {
    s2_ran <- TRUE
    r <- run_child("season records", "fetchOutcomes.R")
    stages$records <- if (r$ok) "ok" else "failed"
    if (!r$ok) notes <- c(notes, paste0("fetchOutcomes.R exited ", r$status))
  }

  ## total-loss guard: both class scrapes AND both roster scrapes dead
  ## means the source is unusable tonight -- put everything back and stop
  classes_all_failed <- !no_classes &&
    identical(stages$classes_football, "failed") &&
    identical(stages$classes_basketball, "failed")
  rosters_all_failed <- !no_rosters &&
    identical(stages$rosters_football, "failed") &&
    identical(stages$rosters_basketball, "failed")
  if (classes_all_failed && rosters_all_failed) {
    restore_db(snap, db_path)
    finalize("failed", 1L, paste0(
      "ingest failed across the board (both class scrapes and both roster ",
      "scrapes) -- snapshot restored"))
  }

  ## -------------------------------------------------------------------------
  ## S3 enrich (non-fatal)
  ## -------------------------------------------------------------------------
  banner("S3 enrich")
  if (no_geocode) {
    stages$profiles <- "skipped"
    stages$geocode <- "skipped"
    cat("[enrich] skipped by flag\n")
  } else {
    s2_ran <- TRUE
    ## profile hometown backfill FIRST, so the Locations it fills get
    ## geocoded on the same night (both stages non-fatal by design)
    r <- run_child("profile hometown backfill", "backfillProfiles.R",
                   c("both", "--max-fetches", "40"))
    stages$profiles <- if (r$ok) "ok" else "warn"
    if (!r$ok) {
      notes <- c(notes, paste0("backfillProfiles.R exited ", r$status,
                               " (non-fatal; hometowns retry next run)"))
    }
    r <- run_child("geocode new players", "geocodeMissing.R")
    stages$geocode <- if (r$ok) "ok" else "warn"
    if (!r$ok) {
      notes <- c(notes, paste0("geocodeMissing.R exited ", r$status,
                               " (non-fatal; new players stay off the map)"))
    }
  }

  ## -------------------------------------------------------------------------
  ## S3.5 qc sweep -- mark (and safely fix) possible data errors every run.
  ## qcSweep.R re-checks geo sanity (wrong-state pins, centroid collapse),
  ## dupes, thin classes, and placeholder zeros (which it NULLs itself), and
  ## records suspects in the qc_flags ledger: an ACCEPTED flag (a verified
  ## legit outlier, e.g. an international recruit pinned abroad) never
  ## re-raises. Exit 2 = NEW high-severity flags -> surfaced as a manifest
  ## note; never blocks the night (the S4 gates own go/no-go).
  ## -------------------------------------------------------------------------
  r_qc <- run_child("qc sweep", "qcSweep.R")
  stages$qc <- if (r_qc$ok) "ok" else if (r_qc$status == 2) "flags" else "warn"
  if (identical(r_qc$status, 2L) || identical(r_qc$status, 2)) {
    notes <- c(notes, paste0(
      "qcSweep found NEW high-severity data flags -- review with: ",
      "SELECT * FROM qc_flags WHERE status='open' ORDER BY first_seen DESC; ",
      "mark verified-legit rows status='accepted', fix real errors"))
  } else if (!r_qc$ok) {
    notes <- c(notes, paste0("qcSweep.R exited ", r_qc$status,
                             " (non-fatal; sweep retries next run)"))
  }

  ## -------------------------------------------------------------------------
  ## S4 validate -- the gates that decide whether tonight's db survives
  ## -------------------------------------------------------------------------
  banner("S4 validate")
  r_audit <- run_child("hole audit", "auditRefreshHoles.R", snap)
  stages$audit <- if (r_audit$ok) "ok" else "failed"
  r_val <- run_child("sanity gate", "validateRefresh.R", snap)
  stages$validate <- if (r_val$ok) "ok" else "failed"

  if (!r_audit$ok || !r_val$ok) {
    restore_db(snap, db_path)
    finalize("failed", 1L, paste0(
      "validation gate failed (audit=", stages$audit,
      ", validate=", stages$validate, ") -- snapshot restored"))
  }
  validated <- TRUE

  ## -------------------------------------------------------------------------
  ## S5 ledger
  ## -------------------------------------------------------------------------
  banner("S5 ledger")
  counts_after <- table_counts(db_path)
  post_hash <- db_content_hash(db_path)
  changed <- !identical(pre_hash, post_hash)
  cat("Post-run content hash:", post_hash, "\n")
  cat("Data changed:", changed, "\n")

  any_bad <- any(unlist(stages) %in% c("failed", "warn"))
  status_s5 <- if (any_bad) "degraded" else if (!changed) "noop" else "ok"
  ensure_refresh_log(db_path)
  ## the date the app's freshness badge will show once this db deploys --
  ## captured HERE (not at S9) so a run that crosses midnight still checks
  ## for the date the ledger row actually carries
  published_date <- Sys.Date()
  write_refresh_log(
    db_path, run_id, started_at,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), status_s5,
    as.character(jsonlite::toJSON(as.list(counts_after),
                                  auto_unbox = TRUE)),
    paste(notes, collapse = " | "))
  log_written <- TRUE
  cat("refresh_log row written:", run_id, "->", status_s5,
      "(finalized again at S10 if later stages change it)\n")

  ## -------------------------------------------------------------------------
  ## S6 precompute -- the deployed defaults must match the shipped db
  ## -------------------------------------------------------------------------
  banner("S6 precompute")
  if (!changed) {
    stages$precompute <- "skipped"
    cat("[precompute] skipped -- content unchanged, the shipped rds still",
        "match\n")
  } else if (no_precompute) {
    stages$precompute <- "skipped"
    cat("[precompute] skipped by flag -- publishing is blocked this run",
        "(a shipped db/rds mismatch is worse than a stale night)\n")
  } else {
    r <- run_child("precompute defaults", "precomputeDefaults.R")
    stages$precompute <- if (r$ok) "ok" else "failed"
    if (!r$ok) {
      notes <- c(notes, paste0(
        "precomputeDefaults.R exited ", r$status,
        " -- publish skipped to avoid shipping a db/rds mismatch"))
    } else {
      ## Sync manifest.json checksums to the refreshed db + precomputed rds.
      ## Connect Cloud deploys from the committed manifest; a stale checksum
      ## makes it serve the OLD bundle. Only the files section changes --
      ## packages are preserved, so this can never break the Connect install.
      ## A failure here downgrades precompute to blocked (never publish a
      ## manifest that disagrees with the shipped files).
      rm <- run_child("update manifest", "updateManifest.R")
      if (!rm$ok) {
        stages$precompute <- "failed"
        notes <- c(notes, paste0(
          "updateManifest.R exited ", rm$status,
          " -- publish skipped to avoid shipping a stale manifest"))
      }
    }
  }

  ## -------------------------------------------------------------------------
  ## S6.5 weekly brief -- rewrite docs/brief/ so the landing site's
  ## auto-written "what changed" page rides the same push (non-fatal: a
  ## brief failure keeps the old page, never blocks publishing the data)
  ## -------------------------------------------------------------------------
  banner("S6.5 weekly brief")
  if (!changed) {
    stages$brief <- "skipped"
    cat("[brief] skipped -- content unchanged, the published brief stands\n")
  } else {
    r <- run_child("weekly brief", "weeklyBrief.R")
    stages$brief <- if (r$ok) "ok" else "warn"
    if (!r$ok) {
      notes <- c(notes, paste0("weeklyBrief.R exited ", r$status,
                               " (non-fatal; docs/brief keeps the old page)"))
    }
  }

  ## docs/status.json distinguishes source-capture freshness from the latest
  ## pipeline result. It is written before a data publish so the same commit
  ## carries both facts; finalize writes it again with the terminal status.
  beacon_status <- if (any(unlist(stages) %in% c("failed", "warn"))) {
    "degraded"
  } else {
    status_s5
  }
  st_ok <- write_public_status(beacon_status, Sys.time())
  cat("[status.json]", if (isTRUE(st_ok)) "written" else "not written", "\n")

  if (isTRUE(st_ok) && isTRUE(changed) && identical(stages$precompute, "ok")) {
    status_manifest <- run_child(
      "update status manifest", "updateManifest.R",
      extra_args = "--paths=www/pipeline-status.json", harvest_failures = FALSE)
    if (!isTRUE(status_manifest$ok)) {
      stages$precompute <- "failed"
      notes <- c(notes, paste0(
        "updateManifest.R could not checksum www/pipeline-status.json; publish skipped"))
    }
  }

  ## -------------------------------------------------------------------------
  ## S7 commit + push (Connect Cloud republishes straight off the push)
  ## -------------------------------------------------------------------------
  banner("S7 commit + push")
  if (!changed) {
    stages$push <- "skipped"
    cat("[push] skipped -- nothing changed\n")
  } else if (no_push) {
    stages$push <- "skipped"
    cat("[push] skipped by flag\n")
  } else if (!identical(stages$precompute, "ok")) {
    stages$push <- "skipped"
    cat("[push] skipped -- precompute is not ok (never publish a db/rds",
        "mismatch)\n")
  } else if (Sys.which("git") == "") {
    stages$push <- "warn"
    notes <- c(notes, "git not found on PATH -- commit/push skipped")
  } else {
    ## write the compact manifest first so the committed copy describes
    ## THIS run; push/deploy/verify have not run yet, so record them as
    ## 'pending' (the full logs/ manifest at finalize carries final values)
    prov <- if (any(unlist(stages) %in% c("failed", "warn"))) {
      "degraded"
    } else {
      "ok"
    }
    compact_s7 <- build_compact(prov, format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"))
    compact_s7$stages$push   <- "pending"
    compact_s7$stages$deploy <- "pending"
    compact_s7$stages$verify <- "pending"
    write_manifest(file.path("data", "refresh-manifest.json"), compact_s7)
    msg <- paste0("Nightly data refresh ", format(Sys.Date()), " [auto]")
    ## commit body names what actually changed (per-table deltas + any
    ## failed/demoted schools) instead of a bare stamp -- each line rides
    ## its own -m flag, which git joins into paragraphs (multi-line args
    ## through system2 on Windows are quoting quicksand)
    body_lines <- character(0)
    if (!is.null(counts_before) && !is.null(counts_after)) {
      for (t in names(counts_after)) {
        ## atomic [[ on a missing name ERRORS (it does not return NULL) --
        ## guard for a content table first created mid-run (e.g. a fresh
        ## team_seasons_football from fetchOutcomes.R on an older db)
        b <- if (t %in% names(counts_before)) counts_before[[t]] else NULL
        a <- counts_after[[t]]
        if (!is.null(b) && !identical(a, b)) {
          body_lines <- c(body_lines,
                          sprintf("%s: %d -> %d (%+d)", t, b, a, a - b))
        }
      }
      if (length(body_lines) == 0) {
        body_lines <- "row counts unchanged (in-place value updates)"
      }
    }
    if (length(failed_schools) > 0) {
      body_lines <- c(body_lines, paste("failed/demoted schools kept on",
                                        "existing rows:",
                                        paste(failed_schools, collapse = ", ")))
    }
    msg_args <- c("-m", shQuote(msg, type = "cmd"))
    for (l in body_lines) msg_args <- c(msg_args, "-m", shQuote(l, type = "cmd"))
    ## machine-written files ONLY -- docs/index.html is human-authored and
    ## must never ride an unattended commit (auto-publishing WIP landing-page
    ## edits is worse than a stale brief link). Only add paths that exist:
    ## a missing pathspec (e.g. docs/status.json before its first write, or a
    ## brief that failed to render) makes `git add` nonzero and would abort
    ## the whole data commit -- the db must ship regardless.
    add_paths <- c("data/recruiting.db", "precomputed", "manifest.json",
                   "data/refresh-manifest.json", "docs/brief",
                    "docs/status.json", "www/pipeline-status.json")
    add_paths <- add_paths[file.exists(add_paths)]
    st <- git_run("add", "--", add_paths)
    where <- "add"
    if (st == 0) {
      st <- git_run("commit", msg_args)
      where <- "commit"
      if (st == 0) data_commit_created <- TRUE
    }
    if (st == 0 && !push_probe_ok) {
      ## the S0 ls-remote probe already failed -- the local commit is the
      ## valuable part (a checkpoint that ships when a push next lands);
      ## attempting the doomed pull/push would only add noise
      stages$push <- "warn"
      cat("[push] committed locally; push SKIPPED (preflight probe failed)\n")
      notes <- c(notes, paste0(
        "push skipped: the preflight ls-remote probe failed (expired ",
        "credential or network); the commit is local and ships when a ",
        "push next lands -- Connect Cloud serves the previous data until ",
        "then"))
    } else {
      if (st == 0) {
        st <- git_run("pull", "--rebase", "--autostash",
                      "origin", publish_branch)
        where <- "pull --rebase"
        ## never leave the repo mid-rebase -- abort immediately on failure
        if (st != 0) try(git_run("rebase", "--abort"), silent = TRUE)
      }
      if (st == 0) {
        st <- git_run("push", "origin", paste0("HEAD:", publish_branch))
        where <- paste0("push origin HEAD:", publish_branch)
      }
      if (st == 0) {
        stages$push <- "ok"
        cat("[push] committed and pushed:", msg, "\n")
      } else {
        stages$push <- "warn"
        ## honesty: only a TRANSIENT failure self-heals. A binary rebase
        ## conflict (origin gained a data commit from another machine) or a
        ## dead credential re-fails identically every night -- that is what
        ## the S10 second-consecutive-miss escalation exists to catch.
        notes <- c(notes, paste0(
          "git ", where, " exited ", st,
          " -- the commit stays local; a transient network blip heals on ",
          "the next push, but a diverged origin (binary rebase conflict) ",
          "or an expired credential will NOT -- a second consecutive miss ",
          "escalates to failed"))
      }
    }
  }

  ## -------------------------------------------------------------------------
  ## S8 deploy to shinyapps.io -- the bundle deploy is git-independent, so a
  ## failed push must not block it; only the data/precompute gates apply
  ## -------------------------------------------------------------------------
  banner("S8 deploy shinyapps")
  if (no_deploy) {
    stages$deploy <- "skipped"
    cat("[deploy] skipped by flag\n")
  } else if (!changed) {
    stages$deploy <- "skipped"
    cat("[deploy] skipped -- nothing changed\n")
  } else if (!identical(stages$precompute, "ok")) {
    stages$deploy <- "skipped"
    cat("[deploy] skipped -- precompute is not ok (never publish a db/rds",
        "mismatch)\n")
  } else if (!file.exists(file.path("scripts", "deployApp.R"))) {
    stages$deploy <- "warn"
    notes <- c(notes, "scripts/deployApp.R not found -- shinyapps deploy skipped")
    cat("[deploy] scripts/deployApp.R not found\n")
  } else {
    r <- run_child("deploy shinyapps", "deployApp.R")
    stages$deploy <- if (r$ok) "ok" else "warn"
    if (!r$ok) {
      notes <- c(notes, paste0("deployApp.R exited ", r$status,
                               " -- shinyapps may be serving the old bundle"))
    }
  }

  ## -------------------------------------------------------------------------
  ## S9 verify the live sites
  ## -------------------------------------------------------------------------
  banner("S9 verify live URLs")
  ## verify whichever publish channel actually ran: the push feeds Connect
  ## Cloud, the deploy feeds shinyapps -- one failing must not skip the other.
  ## shinyapps serves the app HTML directly, so we demand the freshness badge
  ## ("source capture <today>") in the body -- a 200 from the OLD bundle is a
  ## false pass. The marker format MUST match app.R's last_refresh_label
  ## (format "%b %d, %Y" with the day's leading zero stripped). Connect
  ## Cloud's share URL serves an iframe wrapper (the app HTML is not in the
  ## GET body), so it gets a plain 200 check with a longer budget to ride out
  ## the post-push rebuild.
  marker <- paste0("source capture ",
                   sub(" 0", " ", format(published_date, "%b %d, %Y"),
                       fixed = TRUE))
  urls <- c(if (identical(stages$deploy, "ok")) c(shinyapps = find_shinyapps_url()),
            if (identical(stages$push, "ok"))   c(connect   = find_connect_url()))
  urls <- urls[!is.na(urls)]
  if (length(urls) == 0) {
    stages$verify <- "skipped"
    cat("[verify] skipped -- no publish channel completed this run\n")
  } else {
    all_ok <- TRUE
    for (nm in names(urls)) {
      cat("checking ", nm, ": ", urls[[nm]], "\n", sep = "")
      ok <- if (nm == "shinyapps") {
        cat("  (requiring freshness marker: '", marker, "')\n", sep = "")
        verify_url(urls[[nm]], attempts = 4, wait_s = 30, marker = marker)
      } else {
        verify_url(urls[[nm]], attempts = 6, wait_s = 45)
      }
      if (!ok) {
        all_ok <- FALSE
        notes <- c(notes, paste0(
          nm, " live check FAILED (", urls[[nm]], ") -- ",
          if (nm == "shinyapps") "no 200 with today's freshness badge"
          else "no 200 within the cold-start budget",
          "; the host may be serving the old bundle"))
      }
    }
    stages$verify <- if (all_ok) "ok" else "warn"
  }

  ## -------------------------------------------------------------------------
  ## S10 report
  ## -------------------------------------------------------------------------
  banner("S10 report")
  ## chronic-push escalation (ship's exam, 2026-07-11): ONE missed push is
  ## routine; a SECOND consecutive miss is not self-healing -- a diverged
  ## origin (binary rebase conflict) or a dead credential re-fails
  ## identically every night while the git-independent shinyapps deploy
  ## keeps that host fresh, so Connect Cloud silently serves aging data.
  ## Escalate so the run reads failed (exit 1) and the alert says chronic.
  push_escalated <- FALSE
  if (identical(stages$push, "warn")) {
    prev_push <- tryCatch(last_manifest_push_status("logs", run_id),
                          error = function(e) NA_character_)
    if (!is.na(prev_push) && prev_push %in% c("warn", "failed")) {
      stages$push <- "failed"
      push_escalated <- TRUE
      notes <- c(notes, paste0(
        "push has missed 2+ consecutive runs (previous run: ", prev_push,
        ") -- NOT self-healing; check for a diverged origin or an expired ",
        "credential (git ls-remote origin); Connect Cloud is serving aging ",
        "data while shinyapps stays fresh"))
      cat("[escalation] second consecutive push miss -> failed\n")
    }
  }

  any_bad <- any(unlist(stages) %in% c("failed", "warn"))
  final_status <- if (push_escalated) "failed"
                  else if (any_bad) "degraded"
                  else if (!changed) "noop" else "ok"
  exit_code <- if (final_status == "failed") 1L
               else if (final_status == "degraded") 2L else 0L
  finalize(final_status, exit_code)

}, error = function(e) {
  msg <- conditionMessage(e)
  cat("\nUNEXPECTED ERROR:", msg, "\n")
  if (!isTRUE(lock_acquired)) {
    ## most likely a fresh lock is held by another run -- nothing was
    ## touched, and the lock is not ours to release
    quit(save = "no", status = 1)
  }
  if (isTRUE(s2_ran) && !isTRUE(validated) &&
      !is.null(snap) && file.exists(snap)) {
    try(restore_db(snap, db_path), silent = TRUE)
  }
  tryCatch(
    finalize("failed", 1L, paste0("unexpected error: ", msg)),
    error = function(e2) {
      try(release_lock(), silent = TRUE)
      quit(save = "no", status = 1)
    })
})
