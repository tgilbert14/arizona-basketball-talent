## ===========================================================================
## backfillConference.R  --  PHASE 1 per-conference onboarding orchestrator
##
## Onboards ONE Power-4 conference end to end: it walks a conference's hidden
## (onboarded=FALSE) teams through the full historical data pipeline, validates
## the result against a pre-run snapshot, and only then flips those teams to
## onboarded=TRUE and commits. Nothing half-populated ever renders, because the
## app filters to onboarded teams and this script flips the flag LAST.
##
##   Rscript scripts/backfillConference.R "SEC"
##   Rscript scripts/backfillConference.R "Big Ten"
##   Rscript scripts/backfillConference.R "ACC" --resume    # (resume is default)
##
## DO NOT run this from an interactive agent session or on the nightly
## scheduler -- it is a one-time, multi-HOUR, OFF-SCHEDULER job (25-40 min of
## uncapped geocoding per conference alone). Run it by hand with the nightly
## disabled for the window.
##
## Safety + resumability:
##   * holds logs/refresh.lock for the WHOLE run (heart-beated before every
##     stage) so the nightly's acquire_lock() sees a fresh lock and aborts
##     rather than colliding mid-backfill
##   * takes ONE pre-run db snapshot and records it in the checkpoint, so a
##     resumed run always validates against the true pre-backfill baseline
##   * checkpoints every (stage, sport, year) to logs/backfill_<conf>.json;
##     a re-run skips finished stages and picks up where it stopped
##   * a failed stage or a FAILED validate aborts BEFORE onboarding -- the
##     teams stay hidden, the checkpoint survives, re-running resumes
##
## PREREQUISITE (Phase 1 contract, bullet 1): R/team_config.R must already load
## data/team_config.csv at boot so TEAM_CONFIG contains this conference's slugs
## -- the child scrapers resolve --slugs against TEAM_CONFIG. The guard below
## stops loudly if that wiring is not in place yet.
## ===========================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
  library(jsonlite)
})

source(here::here("scripts", "lib", "refresh_utils.R"))
source(here::here("R", "team_config.R"))

## ---------------------------------------------------------------------------
## args
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
args <- args[args != "--resume"]   # resume is the only mode; flag is a no-op
if (length(args) < 1 || !nzchar(args[1])) {
  cat("Usage: Rscript scripts/backfillConference.R \"<Conference>\"\n",
      "  e.g. \"SEC\", \"Big Ten\", \"ACC\"\n")
  quit(save = "no", status = 1)
}
conf <- args[1]

repo_root <- here::here()
db_path   <- file.path(repo_root, "data", "recruiting.db")
cfg_path  <- file.path(repo_root, "data", "team_config.csv")
rscript   <- file.path(R.home("bin"), "Rscript")

if (!file.exists(cfg_path)) {
  stop("data/team_config.csv not found -- run scripts/buildTeamConfig.R first")
}
if (!file.exists(db_path)) stop("data/recruiting.db not found")

## ---------------------------------------------------------------------------
## target slugs: this conference's teams that are still hidden
## ---------------------------------------------------------------------------
cfg <- read.csv(cfg_path, stringsAsFactors = FALSE)
if (!conf %in% cfg$conference) {
  stop("conference '", conf, "' not found in data/team_config.csv. Known: ",
       paste(sort(unique(cfg$conference)), collapse = ", "))
}
targets <- cfg$slug[cfg$conference == conf & !as.logical(cfg$onboarded)]
if (length(targets) == 0) {
  cat("Nothing to do: every '", conf, "' team is already onboarded.\n", sep = "")
  quit(save = "no", status = 0)
}

## PREREQUISITE guard: the child scrapers intersect --slugs against
## TEAM_CONFIG$slug. If R/team_config.R has not been wired to load
## data/team_config.csv yet, TEAM_CONFIG is still the inline 16 and every
## child would stop() with "no slugs match". Fail here, before the lock.
missing_in_config <- setdiff(targets, TEAM_CONFIG$slug)
if (length(missing_in_config) > 0) {
  stop("R/team_config.R is not loading data/team_config.csv yet -- ",
       length(missing_in_config), " target slug(s) absent from TEAM_CONFIG (",
       paste(head(missing_in_config, 5), collapse = ", "),
       "...). Wire the CSV loader (Phase 1 contract bullet 1) before backfilling.")
}

slug_csv <- paste(targets, collapse = ",")
conf_slug <- gsub("[^a-z0-9]+", "-", tolower(conf))   # for file names

cur_year   <- as.integer(format(Sys.Date(), "%Y"))
CLASS_YEARS <- 2016:cur_year          # historical class window
AHEAD_YEAR  <- cur_year + 1L          # open-cycle probe (allow-empty)
ROSTER_YEAR <- cur_year               # rosters are current-season only
PROFILE_CAP <- 100000L                # raised cap = effectively uncapped
## onboarding floor: a real 2016..current P4 backfill lands hundreds of
## recruit_class_football rows; a wrong/broken slug lands 0. This cleanly
## separates "has real data" from "scraped empty" without a fussy threshold --
## a team below it stays hidden (fails SAFE) instead of onboarding empty boards.
MIN_ONBOARD_ROWS <- 20L

## ---------------------------------------------------------------------------
## checkpoint -- logs/backfill_<conf>.json
## ---------------------------------------------------------------------------
dir.create(file.path(repo_root, "logs"),    showWarnings = FALSE)
dir.create(file.path(repo_root, "backups"), showWarnings = FALSE)
ckpt_path <- file.path(repo_root, "logs", paste0("backfill_", conf_slug, ".json"))

load_ckpt <- function() {
  if (file.exists(ckpt_path)) {
    ck <- tryCatch(jsonlite::fromJSON(ckpt_path, simplifyVector = TRUE),
                   error = function(e) NULL)
    if (!is.null(ck)) {
      if (is.null(ck$stages)) ck$stages <- list()
      ck$stages <- as.list(ck$stages)
      return(ck)
    }
  }
  list(conference = conf, targets = targets,
       started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       baseline_snapshot = NA_character_, onboarded = FALSE,
       stages = list())
}
save_ckpt <- function(ck) {
  jsonlite::write_json(ck, ckpt_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
}
ck <- load_ckpt()
stage_done <- function(key) identical(ck$stages[[key]], "ok")
mark_stage <- function(key, val) { ck$stages[[key]] <<- val; save_ckpt(ck) }

## ---------------------------------------------------------------------------
## lock -- hold it for the whole run, heart-beat before every stage so the
## nightly never mistakes a long backfill for a stale lock
## ---------------------------------------------------------------------------
lock_path <- file.path(repo_root, "logs", "refresh.lock")
touch_lock <- function() {
  writeLines(c(paste0("pid: ", Sys.getpid()),
               paste0("backfill: ", conf),
               paste0("heartbeat: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
             lock_path)
}
acquire_lock(lock_path)          # stop()s if another fresh run holds it
touch_lock()
cat("Lock acquired + held for the", conf, "backfill:", lock_path, "\n")
lock_held <- TRUE

## release the lock no matter how we exit
finish <- function(status, msg = NULL) {
  if (!is.null(msg)) cat("\n", msg, "\n", sep = "")
  if (isTRUE(lock_held)) release_lock(lock_path)
  quit(save = "no", status = status)
}

## run a child Rscript from the repo root; heart-beat the lock first. Returns
## the child's exit status (0 = ok).
run_child <- function(label, script, args = character(0)) {
  touch_lock()
  cat("\n---- ", label, " ----\n", sep = "")
  t0 <- Sys.time()
  ## shQuote every arg: absolute paths under "VGS - R" carry a space, and
  ## system2 concatenates unquoted args -- the validate stage's baseline
  ## path split into three args and failed the whole 2026-07-16 SEC run
  ## at the last gate. Quoting is a no-op for slug CSVs and flags.
  st <- system2(rscript, c(shQuote(file.path("scripts", script)), shQuote(args)))
  mins <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)
  cat("[", label, "] ", if (st == 0) "ok" else paste0("EXIT ", st),
      " (", mins, " min)\n", sep = "")
  st
}

cat("\n==== BACKFILL", conf, "====\n")
cat("targets (", length(targets), "):", slug_csv, "\n")
cat("class years:", min(CLASS_YEARS), "-", max(CLASS_YEARS),
    "(+ ahead-year", AHEAD_YEAR, "probe)\n")

## ---------------------------------------------------------------------------
## S1 pre-run snapshot (once; reused on resume so validate is honest)
## ---------------------------------------------------------------------------
touch_lock()
if (is.na(ck$baseline_snapshot) || !file.exists(ck$baseline_snapshot)) {
  ck$baseline_snapshot <- snapshot_db(db_path, file.path(repo_root, "backups"))
  save_ckpt(ck)
} else {
  cat("Reusing baseline snapshot:", ck$baseline_snapshot, "\n")
}
baseline <- ck$baseline_snapshot

## track whether any critical stage failed -- if so we never onboard
critical_fail <- character(0)

## ---------------------------------------------------------------------------
## S2 historical classes -- every year, both sports, scoped to --slugs
## ---------------------------------------------------------------------------
for (sport in c("football", "basketball")) {
  for (yr in CLASS_YEARS) {
    key <- paste0("classes:", sport, ":", yr)
    if (stage_done(key)) { cat("skip (done):", key, "\n"); next }
    st <- run_child(key, "refreshClassYear.R",
                    c(sport, yr, "--slugs", slug_csv))
    if (st == 0) mark_stage(key, "ok") else {
      mark_stage(key, paste0("exit", st))
      critical_fail <- c(critical_fail, key)
    }
  }
  ## open-cycle probe: exits 0 without touching the db until 247 opens it
  keya <- paste0("classes-ahead:", sport, ":", AHEAD_YEAR)
  if (!stage_done(keya)) {
    st <- run_child(keya, "refreshClassYear.R",
                    c(sport, AHEAD_YEAR, "--slugs", slug_csv, "--allow-empty"))
    if (st == 0) mark_stage(keya, "ok") else mark_stage(keya, paste0("exit", st))
    ## ahead-year is best-effort, never critical
  }
}

## ---------------------------------------------------------------------------
## S3 rosters -- current season, both sports, scoped to --slugs
## ---------------------------------------------------------------------------
for (sport in c("football", "basketball")) {
  key <- paste0("rosters:", sport, ":", ROSTER_YEAR)
  if (stage_done(key)) { cat("skip (done):", key, "\n"); next }
  st <- run_child(key, "scrapeRosters.R",
                  c(sport, ROSTER_YEAR, "--slugs", slug_csv))
  if (st == 0) mark_stage(key, "ok") else {
    mark_stage(key, paste0("exit", st))
    critical_fail <- c(critical_fail, key)
  }
}

## ---------------------------------------------------------------------------
## S4 season records (CFBD) -- iterates every configured team; harmless to
## re-run, fills the new teams' rows. Skipped cleanly if no CFBD key.
## ---------------------------------------------------------------------------
if (!stage_done("outcomes")) {
  if (Sys.getenv("CFBD_API_KEY") == "") {
    cat("\n[outcomes] skipped -- no CFBD_API_KEY in this session\n")
    mark_stage("outcomes", "skipped")
  } else {
    st <- run_child("outcomes", "fetchOutcomes.R")
    ## fetchOutcomes exits 1 when a year fetched nothing but keeps old rows --
    ## that is a soft warning, not a backfill blocker
    mark_stage("outcomes", if (st == 0) "ok" else paste0("exit", st))
  }
}

## ---------------------------------------------------------------------------
## S5 hometowns + geocode. backfillProfiles runs FIRST (fills 247 hometowns)
## so geocodeMissing maps them the same run -- the proven refreshAll order.
## The profile cap is raised to effectively uncapped for the one-time backfill.
## ---------------------------------------------------------------------------
if (!stage_done("profiles")) {
  st <- run_child("profile hometown backfill", "backfillProfiles.R",
                  c("both", "--max-fetches", PROFILE_CAP))
  mark_stage("profiles", if (st == 0) "ok" else paste0("exit", st))
}
if (!stage_done("geocode")) {
  st <- run_child("geocode new players", "geocodeMissing.R")
  mark_stage("geocode", if (st == 0) "ok" else paste0("exit", st))
}

## ---------------------------------------------------------------------------
## S6 validate the live db against the pre-run baseline. A FAIL aborts before
## onboarding: the teams stay hidden and the checkpoint lets a fixed re-run
## resume. (Adding a whole conference is GROWTH -- validateRefresh is
## Phase-1-aware and does not trip on new schools / new roster years.)
## ---------------------------------------------------------------------------
touch_lock()
if (length(critical_fail) > 0) {
  finish(1, paste0("Critical stage(s) failed: ",
                   paste(critical_fail, collapse = ", "),
                   "\n-> NOT onboarding. Fix and re-run (resumes from here)."))
}
st_val <- run_child("validate vs baseline", "validateRefresh.R",
                    c(baseline, db_path))
if (st_val != 0) {
  finish(1, paste0("validateRefresh FAILED against ", baseline,
                   "\n-> NOT onboarding. Inspect the [FAIL] lines above, fix, ",
                   "and re-run (resumes from validate)."))
}
mark_stage("validate", "ok")

## ---------------------------------------------------------------------------
## S7 refresh the shipped bundle (default renders + manifest checksums) so the
## committed db/config ship coherently. Best-effort -- absence is not fatal.
## ---------------------------------------------------------------------------
if (!stage_done("precompute") &&
    file.exists(file.path(repo_root, "scripts", "precomputeDefaults.R"))) {
  st <- run_child("precompute defaults", "precomputeDefaults.R")
  mark_stage("precompute", if (st == 0) "ok" else paste0("exit", st))
}
if (!stage_done("manifest") &&
    file.exists(file.path(repo_root, "scripts", "updateManifest.R"))) {
  st <- run_child("update manifest", "updateManifest.R")
  mark_stage("manifest", if (st == 0) "ok" else paste0("exit", st))
}

## ---------------------------------------------------------------------------
## S8 ONBOARD -- flip to onboarded=TRUE, but ONLY teams that actually landed
## data, then commit LOCALLY. A landing-page-valid slug can still scrape thin (a
## broken class page, a stale roster); onboarding it empty would render a half-
## populated board, which the design forbids ("a wrong slug fails SAFE -- skips
## the team"). Gate each target on a real recruiting-class footprint; teams that
## fall short stay hidden and are reported. The flip is idempotent; no push (a
## local checkpoint the nightly ships on its next push).
## ---------------------------------------------------------------------------
touch_lock()
class_rows <- function(slug) {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  if (!"recruit_class_football" %in% dbListTables(conn)) return(0L)
  tryCatch(as.integer(dbGetQuery(conn,
    "SELECT COUNT(*) AS n FROM recruit_class_football WHERE School = ?",
    params = list(slug))$n), error = function(e) 0L)
}
footprint <- vapply(targets, class_rows, integer(1))
ready     <- targets[footprint >= MIN_ONBOARD_ROWS]
not_ready <- targets[footprint <  MIN_ONBOARD_ROWS]
if (length(not_ready) > 0) {
  cat("\nHELD BACK (< ", MIN_ONBOARD_ROWS,
      " recruit_class_football rows -- staying hidden):\n  ",
      paste(sprintf("%s (%d)", not_ready, footprint[not_ready]),
            collapse = ", "), "\n", sep = "")
  cat("  To onboard these later: re-scrape just them, e.g.\n",
      "    Rscript scripts/refreshClassYear.R football <year> --slugs ",
      paste(not_ready, collapse = ","), "\n",
      "  (or delete ", ckpt_path, " to force a full re-run), then re-run this ",
      "script.\n", sep = "")
}
ck$held_back <- not_ready
if (length(ready) == 0) {
  save_ckpt(ck)
  finish(1, paste0("No ", conf, " team cleared the ", MIN_ONBOARD_ROWS,
                   "-row onboarding floor -- nothing onboarded. Check the slug ",
                   "map / scrape logs above, then re-run."))
}
cfg_now <- read.csv(cfg_path, stringsAsFactors = FALSE)
cfg_now$onboarded[cfg_now$slug %in% ready] <- TRUE
write.csv(cfg_now, cfg_path, row.names = FALSE)
cat("\nFlipped onboarded=TRUE for", length(ready), "of", length(targets),
    conf, "team(s).\n")

git_run <- function(...) suppressWarnings(system2("git", c(...)))
add_paths <- c("data/team_config.csv", "data/recruiting.db", "precomputed",
               "manifest.json", "data/refresh-manifest.json", "docs/brief")
add_paths <- add_paths[file.exists(file.path(repo_root, add_paths))]
if (Sys.which("git") != "" && length(add_paths) > 0) {
  Sys.setenv(GIT_TERMINAL_PROMPT = "0")
  st <- git_run("add", "--", add_paths)
  if (st == 0) {
    msg <- paste0("Onboard ", conf, " (", length(ready),
                  " teams) -- Phase 1 per-conference backfill")
    st <- git_run("commit", "-m", shQuote(msg, type = "cmd"))
  }
  if (st == 0) {
    cat("[commit] committed locally (no push -- the nightly ships it):\n  ",
        paste(add_paths, collapse = ", "), "\n")
  } else {
    cat("[commit] git add/commit exited", st,
        "-- config + db are updated on disk; commit them by hand.\n")
  }
} else {
  cat("[commit] git unavailable or nothing to add -- ",
      "config + db updated on disk; commit by hand.\n", sep = "")
}

ck$onboarded <- TRUE
ck$finished_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
save_ckpt(ck)

finish(0, paste0("DONE: ", conf, " onboarded (", length(ready), " of ",
                 length(targets), " team(s)",
                 if (length(not_ready) > 0)
                   paste0("; ", length(not_ready), " held back") else "",
                 "). Checkpoint: ", ckpt_path))
