## ===========================================================================
## validateRefreshExceptions.R -- isolated regressions for the strict
## source-reviewed decommit exception gate in validateRefresh.R.
##
## Usage: Rscript scripts/validateRefreshExceptions.R
##
## Creates only temporary SQLite fixtures. It proves that:
##   1. the exact registered active-cycle 2 -> 1 decommit passes;
##   2. an aged audit_heals record cannot bypass a 2 -> 0 wipe; and
##   3. replacing the named retained commit with a new player fails.
## ===========================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
})

script_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(repo_root)

validator <- file.path(repo_root, "scripts", "validateRefresh.R")
if (!file.exists(validator)) {
  stop("validateRefresh.R was not found at ", validator, call. = FALSE)
}

scratch <- tempfile("girth-validate-refresh-")
dir.create(scratch)
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

class_year <- as.integer(format(Sys.Date(), "%Y")) + 1L
onboarded_n <- tryCatch({
  cfg <- utils::read.csv(file.path(repo_root, "data", "team_config.csv"),
                         stringsAsFactors = FALSE)
  sum(toupper(as.character(cfg$onboarded)) == "TRUE", na.rm = TRUE)
}, error = function(e) 16L)
if (!is.finite(onboarded_n) || onboarded_n < 1L) onboarded_n <- 16L
roster_team_count <- as.integer(ceiling(0.75 * onboarded_n))
registry_path <- file.path(scratch, "refresh-validation-exceptions.csv")
registry <- data.frame(
  tbl = "recruit_class_basketball",
  school = "arkansas",
  year = class_year,
  baseline_rows = 2L,
  live_rows = 1L,
  removed_name = "Removed Commit",
  retained_name = "Retained Commit",
  expires_on = format(Sys.Date() + 30, "%Y-%m-%d"),
  verified_on = format(Sys.Date(), "%Y-%m-%d"),
  source_url = "https://example.test/arkansas",
  reason = "Synthetic regression fixture for strict validation",
  stringsAsFactors = FALSE
)
utils::write.csv(registry, registry_path, row.names = FALSE, quote = TRUE)

old_registry_path <- Sys.getenv("GIRTH_DECOMMIT_EXCEPTION_PATH", unset = "")
Sys.setenv(GIRTH_DECOMMIT_EXCEPTION_PATH = normalizePath(registry_path,
                                                         winslash = "/"))
on.exit({
  if (nzchar(old_registry_path)) {
    Sys.setenv(GIRTH_DECOMMIT_EXCEPTION_PATH = old_registry_path)
  } else {
    Sys.unsetenv("GIRTH_DECOMMIT_EXCEPTION_PATH")
  }
}, add = TRUE)

make_basketball <- function(arkansas_names) {
  arkansas <- data.frame(
    School = rep("arkansas", length(arkansas_names)),
    Year = rep(class_year, length(arkansas_names)),
    Name = arkansas_names,
    Type = rep("Commit", length(arkansas_names)),
    Weight = rep(200, length(arkansas_names)),
    Ranking = rep(90, length(arkansas_names)),
    stringsAsFactors = FALSE
  )
  filler_id <- seq_len(100L)
  fillers <- data.frame(
    School = sprintf("basketball-filler-%03d", filler_id),
    Year = rep(class_year, length(filler_id)),
    Name = sprintf("Basketball Filler %03d", filler_id),
    Type = rep("Commit", length(filler_id)),
    Weight = rep(200, length(filler_id)),
    Ranking = rep(90, length(filler_id)),
    stringsAsFactors = FALSE
  )
  rbind(arkansas, fillers)
}

make_football <- function() {
  filler_id <- seq_len(100L)
  data.frame(
    School = sprintf("football-filler-%03d", filler_id),
    Year = rep(class_year, length(filler_id)),
    Name = sprintf("Football Filler %03d", filler_id),
    Type = rep("Commit", length(filler_id)),
    Weight = rep(220, length(filler_id)),
    Ranking = rep(90, length(filler_id)),
    stringsAsFactors = FALSE
  )
}

make_roster <- function() {
  team_id <- seq_len(roster_team_count)
  data.frame(
    School = sprintf("roster-team-%03d", team_id),
    RosterYear = rep(class_year, length(team_id)),
    Weight = rep(200, length(team_id)),
    stringsAsFactors = FALSE
  )
}

make_fixture <- function(path, arkansas_names, audit_streak = NULL) {
  conn <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(conn), add = TRUE)

  dbWriteTable(conn, "recruit_class_basketball", make_basketball(arkansas_names))
  dbWriteTable(conn, "recruit_class_football", make_football())
  dbWriteTable(conn, "roster_basketball", make_roster())
  dbWriteTable(conn, "roster_football", make_roster())
  dbExecute(conn, paste(
    "CREATE TABLE team_seasons_football",
    "(slug TEXT, year INTEGER, wins INTEGER)"
  ))
  dbExecute(conn,
            "INSERT INTO team_seasons_football (slug, year, wins) VALUES (?, ?, ?)",
            params = list("arkansas", class_year, 10L))

  if (!is.null(audit_streak)) {
    dbExecute(conn, paste(
      "CREATE TABLE audit_heals",
      "(tbl TEXT, School TEXT, Year INTEGER, streak INTEGER, last_healed TEXT,",
      "PRIMARY KEY (tbl, School, Year))"
    ))
    dbExecute(conn, paste(
      "INSERT INTO audit_heals (tbl, School, Year, streak, last_healed)",
      "VALUES (?, ?, ?, ?, ?)"
    ), params = list("recruit_class_basketball", "arkansas", class_year,
                     as.integer(audit_streak), "2026-07-23 00:00:00"))
  }
  invisible(path)
}

rscript_bin <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
if (!file.exists(rscript_bin)) rscript_bin <- "Rscript"

run_validator <- function(base_db, live_db) {
  output <- suppressWarnings(system2(
    rscript_bin,
    c(shQuote(validator), shQuote(base_db), shQuote(live_db)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  list(
    status = if (is.null(status)) 0L else as.integer(status),
    output = paste(output, collapse = "\n")
  )
}

expect_status <- function(result, expected, label, required_text = NULL) {
  if (result$status != expected) {
    stop(sprintf(
      "%s: expected validator exit %d, got %d\n%s",
      label, expected, result$status, result$output
    ), call. = FALSE)
  }
  if (!is.null(required_text) &&
      !grepl(required_text, result$output, fixed = TRUE)) {
    stop(sprintf(
      "%s: expected validator output to contain %s\n%s",
      label, shQuote(required_text), result$output
    ), call. = FALSE)
  }
  cat("[PASS]", label, "\n")
}

## 1. The single, verified nonzero class change is allowed.
exact_base <- file.path(scratch, "exact-base.sqlite")
exact_live <- file.path(scratch, "exact-live.sqlite")
make_fixture(exact_base, c("Removed Commit", "Retained Commit"))
make_fixture(exact_live, "Retained Commit")
expect_status(
  run_validator(exact_base, exact_live), 0L,
  "exact registered active-cycle decommit passes",
  "exact verified decommit allowed"
)

## 2. A stale audit_heals entry must not waive a complete class wipe.
audit_base <- file.path(scratch, "audit-base.sqlite")
audit_live <- file.path(scratch, "audit-live.sqlite")
make_fixture(audit_base, c("Removed Commit", "Retained Commit"))
make_fixture(audit_live, character(), audit_streak = 4L)
expect_status(
  run_validator(audit_base, audit_live), 1L,
  "aged audit_heals record cannot bypass a zero-row loss",
  sprintf("arkansas %d (2 -> 0)", class_year)
)

## 3. A source entry cannot authorize replacing the named retained commit.
replacement_base <- file.path(scratch, "replacement-base.sqlite")
replacement_live <- file.path(scratch, "replacement-live.sqlite")
make_fixture(replacement_base, c("Removed Commit", "Baseline Other"))
make_fixture(replacement_live, "Retained Commit")
expect_status(
  run_validator(replacement_base, replacement_live), 1L,
  "wrong retained-player replacement is rejected",
  sprintf("arkansas %d (2 -> 1)", class_year)
)

cat("All strict decommit-exception regression checks passed.\n")
