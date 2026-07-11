## ===========================================================================
## refresh_utils.R -- shared helpers for the unattended refresh pipeline
## (nightlyRefresh.R, validateRefresh.R and friends). Source from repo root:
##   source(file.path("scripts", "lib", "refresh_utils.R"))
##
## LOCKING RULE: every db helper here takes a db PATH and opens a
## SHORT-LIVED connection (dbConnect ... on.exit(dbDisconnect)) per call,
## so the orchestrator never holds a handle open while a child Rscript
## runs -- SQLite writers need the database to themselves.
## ===========================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
})

## ---------------------------------------------------------------------------
## run lock -- one refresh at a time
## ---------------------------------------------------------------------------
acquire_lock <- function(lock_path = "logs/refresh.lock", stale_hours = 3) {
  dir.create(dirname(lock_path), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(lock_path)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(lock_path),
                                 units = "hours"))
    if (is.finite(age_h) && age_h < stale_hours) {
      stop("refresh lock held (", lock_path, ", ", round(age_h, 2),
           " h old) -- another run appears active", call. = FALSE)
    }
    message("Stale lock (", round(age_h, 1), " h old) -- overwriting: ",
            lock_path)
  }
  writeLines(c(paste0("pid: ", Sys.getpid()),
               paste0("started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
             lock_path)
  invisible(lock_path)
}

release_lock <- function(lock_path = "logs/refresh.lock") {
  if (file.exists(lock_path)) unlink(lock_path)
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## content tables -- the data the app actually reads. refresh_log is NOT a
## content table: a bookkeeping row must never make a run look like a data
## change (db_content_hash relies on this).
## ---------------------------------------------------------------------------
content_tables <- function() {
  c("recruit_class_football", "recruit_class_basketball",
    "roster_football", "roster_basketball", "team_seasons_football")
}

## named integer vector of row counts for content tables that exist
table_counts <- function(db = "data/recruiting.db") {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  tabs <- intersect(content_tables(), dbListTables(conn))
  vapply(tabs, function(t) {
    as.integer(dbGetQuery(conn, paste0("SELECT COUNT(*) AS n FROM ", t))$n)
  }, integer(1))
}

## deterministic digest over the content tables' full contents. Rows are
## sorted with method = "radix" (locale-independent) so the same data always
## hashes the same regardless of insertion order or session locale.
db_content_hash <- function(db = "data/recruiting.db") {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  tabs <- intersect(content_tables(), dbListTables(conn))
  payload <- lapply(tabs, function(t) {
    df <- dbGetQuery(conn, paste0("SELECT * FROM ", t))
    ## drop volatile bookkeeping columns (scrapeRosters restamps ScrapedAt
    ## every run) so an unchanged night hashes the same as the night before
    df <- df[, setdiff(names(df), "ScrapedAt"), drop = FALSE]
    if (nrow(df) > 1) {
      ord <- do.call(order, c(unname(as.list(df)), list(method = "radix")))
      df <- df[ord, , drop = FALSE]
    }
    rownames(df) <- NULL
    df
  })
  names(payload) <- tabs
  digest::digest(payload, algo = "sha256")
}

## ---------------------------------------------------------------------------
## snapshot / restore -- whole-file copies of the SQLite db
## ---------------------------------------------------------------------------
snapshot_db <- function(db = "data/recruiting.db", dir = "backups") {
  if (!file.exists(db)) stop("snapshot_db: db not found: ", db, call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(dir, paste0("pre_run_",
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".db"))
  ## defensively clear any SQLite sidecars at the destination so the
  ## snapshot is always a clean standalone restore source
  unlink(paste0(dest, c("-journal", "-wal", "-shm")))
  ok <- file.copy(db, dest, overwrite = TRUE)
  if (!ok || !file.exists(dest)) {
    stop("snapshot_db: copy failed: ", dest, call. = FALSE)
  }
  message("Snapshot: ", dest, " (",
          round(file.size(dest) / 1024 / 1024, 1), " MB)")
  dest
}

restore_db <- function(snapshot_path, db = "data/recruiting.db") {
  if (!file.exists(snapshot_path)) {
    stop("restore_db: snapshot not found: ", snapshot_path, call. = FALSE)
  }
  ## stale sidecars left beside the overwritten db would be replayed over
  ## the restored file on the next open -- remove them first
  unlink(paste0(db, c("-journal", "-wal", "-shm")))
  ok <- file.copy(snapshot_path, db, overwrite = TRUE)
  if (!ok) {
    stop("restore_db: copy failed: ", snapshot_path, " -> ", db, call. = FALSE)
  }
  message("Restored ", db, " from ", snapshot_path)
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## refresh ledger -- one row per run, lives inside the app db (but is
## excluded from db_content_hash, see content_tables above)
## ---------------------------------------------------------------------------
ensure_refresh_log <- function(db = "data/recruiting.db") {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbExecute(conn, paste0(
    "CREATE TABLE IF NOT EXISTS refresh_log (",
    "run_id TEXT, started_at TEXT, finished_at TEXT, ",
    "status TEXT, tables_json TEXT, notes TEXT)"))
  invisible(TRUE)
}

write_refresh_log <- function(db, run_id, started_at, finished_at, status,
                              tables_json, notes) {
  ensure_refresh_log(db)
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbExecute(conn, paste0(
    "INSERT INTO refresh_log ",
    "(run_id, started_at, finished_at, status, tables_json, notes) ",
    "VALUES (?, ?, ?, ?, ?, ?)"),
    params = list(run_id, started_at, finished_at, status,
                  tables_json, notes))
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## manifests
## ---------------------------------------------------------------------------
write_manifest <- function(path, manifest_list) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(manifest_list, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  invisible(path)
}

## ---------------------------------------------------------------------------
## gh_alert -- create or comment on the open issue labeled 'auto-refresh'.
## Search first (dedupe: one rolling issue, new runs comment on it).
## Returns TRUE/FALSE, never errors -- an alert failure must not take the
## pipeline down with it.
## ---------------------------------------------------------------------------
gh_alert <- function(title, body) {
  tryCatch({
    if (Sys.which("gh") == "") {
      message("gh CLI not found -- alert skipped: ", title)
      return(FALSE)
    }
    body_file <- tempfile(fileext = ".md")
    on.exit(unlink(body_file), add = TRUE)
    writeLines(c(paste0("## ", title), "", body), body_file)

    ## dedupe within this alert's own title family only (strip the trailing
    ## "-- <date>") so we never thread onto another writer's issue, e.g. the
    ## watchdog's 'Nightly refresh looks stale'
    family <- trimws(sub("\\s*--.*$", "", title))
    listed <- suppressWarnings(system2("gh", c(
      "issue", "list", "--label", "auto-refresh", "--state", "open",
      "--search", shQuote(paste0(family, " in:title"), type = "cmd"),
      "--json", "number", "--jq", shQuote(".[0].number", type = "cmd"),
      "--limit", "1"), stdout = TRUE, stderr = TRUE))
    num <- NA_integer_
    if (is.null(attr(listed, "status")) || attr(listed, "status") == 0) {
      num <- suppressWarnings(
        as.integer(trimws(paste(listed, collapse = ""))))
    }

    if (length(num) == 1 && !is.na(num)) {
      st <- suppressWarnings(system2("gh", c(
        "issue", "comment", as.character(num),
        "--body-file", shQuote(body_file, type = "cmd")),
        stdout = FALSE, stderr = FALSE))
      return(st == 0)
    }
    st <- suppressWarnings(system2("gh", c(
      "issue", "create", "--title", shQuote(title, type = "cmd"),
      "--body-file", shQuote(body_file, type = "cmd"),
      "--label", "auto-refresh"), stdout = FALSE, stderr = FALSE))
    if (st != 0) {
      ## the label may not exist yet on the repo -- retry without it
      st <- suppressWarnings(system2("gh", c(
        "issue", "create", "--title", shQuote(title, type = "cmd"),
        "--body-file", shQuote(body_file, type = "cmd")),
        stdout = FALSE, stderr = FALSE))
    }
    st == 0
  }, error = function(e) {
    message("gh_alert failed (non-fatal): ", conditionMessage(e))
    FALSE
  })
}

## ---------------------------------------------------------------------------
## prune_old -- delete matching files older than N days. The permanent
## baseline backups/recruiting_HEAD.db is NEVER deleted, whatever the
## pattern says.
## ---------------------------------------------------------------------------
prune_old <- function(dir, pattern, days) {
  if (!dir.exists(dir)) return(invisible(character(0)))
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  files <- files[basename(files) != "recruiting_HEAD.db"]
  if (length(files) == 0) return(invisible(character(0)))
  mt <- file.mtime(files)
  old <- files[!is.na(mt) & mt < (Sys.time() - days * 86400)]
  if (length(old) > 0) {
    unlink(old)
    message("Pruned ", length(old), " file(s) older than ", days,
            " days from ", dir)
  }
  invisible(old)
}
