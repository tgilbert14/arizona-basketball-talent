#!/usr/bin/env Rscript

## Deterministic checks for the Home dashboard source + pipeline status contract.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

source("R/dashboard_status.R")
source("R/home_fingerprint.R")

check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

conn <- dbConnect(SQLite(), ":memory:")
on.exit(dbDisconnect(conn), add = TRUE)
isolated_status_path <- tempfile(fileext = ".json")
on.exit(unlink(isolated_status_path), add = TRUE)


empty <- dashboard_refresh_meta(conn, status_path = isolated_status_path)
check(identical(empty$status, "unknown"), "empty database status failed")
check(is.null(empty$capture_date), "empty database capture should be NULL")
check(identical(dashboard_freshness_info(empty)$state, "unknown"),
      "empty database freshness failed")

dbExecute(conn, paste(
  "CREATE TABLE refresh_log (run_id TEXT, started_at TEXT, finished_at TEXT,",
  "status TEXT, tables_json TEXT, notes TEXT)"))
dbExecute(conn, paste(
  "INSERT INTO refresh_log VALUES",
  "('a','2026-07-16 01:00:00','2026-07-16 01:10:00','ok','{}','')"))

for (table in c("recruit_class_football", "recruit_class_basketball")) {
  dbExecute(conn, paste0(
    "CREATE TABLE ", table,
    " (School TEXT, ScrapedAt TEXT, Year INTEGER)"))
}
for (table in c("roster_football", "roster_basketball")) {
  dbExecute(conn, paste0(
    "CREATE TABLE ", table,
    " (School TEXT, ScrapedAt TEXT, RosterYear INTEGER)"))
}
dbExecute(conn, paste(
  "CREATE TABLE team_seasons_football",
  "(slug TEXT, year INTEGER, wins INTEGER)"))

dbExecute(conn, paste(
  "INSERT INTO recruit_class_football VALUES",
  "('arizona','2026-07-17',2027),('asu','2026-07-17',2027)"))
dbExecute(conn, paste(
  "INSERT INTO recruit_class_basketball VALUES",
  "('arizona','2026-07-18',2027)"))
dbExecute(conn, paste(
  "INSERT INTO roster_football VALUES",
  "('arizona','2026-07-18',2026),('asu','2026-07-16',2025)"))
dbExecute(conn, paste(
  "INSERT INTO roster_basketball VALUES",
  "('arizona','2026-07-18',2026)"))
dbExecute(conn, paste(
  "INSERT INTO team_seasons_football VALUES",
  "('arizona',2025,9),('asu',2025,10),('arizona',2024,8)"))

meta <- dashboard_refresh_meta(conn, status_path = isolated_status_path)
check(identical(meta$checked_date, as.Date("2026-07-16")),
      "pipeline check date failed")
check(identical(meta$capture_date, as.Date("2026-07-18")),
      "latest source capture failed")
check(meta$sources$recruiting$football$teams == 2L,
      "recruiting coverage failed")
check(meta$sources$rosters$football$year == 2026L &&
        meta$sources$rosters$football$teams == 1L,
      "active roster-year coverage failed")
check(meta$sources$outcomes$year == 2025L &&
        meta$sources$outcomes$teams == 2L,
      "outcomes coverage failed")

fresh <- dashboard_freshness_info(meta, as.Date("2026-07-18"))
check(identical(fresh$state, "fresh") &&
        grepl("source capture", fresh$detail, fixed = TRUE),
      "source freshness presentation failed")
pipeline <- dashboard_pipeline_info(meta, as.Date("2026-07-18"))
check(identical(pipeline$state, "warning") &&
        grepl("trails", pipeline$label, fixed = TRUE),
      "ledger-lag warning failed")

dbExecute(conn, "DELETE FROM refresh_log")
dbExecute(conn, paste(
  "INSERT INTO refresh_log VALUES",
  "('b','2026-07-18 01:00:00','2026-07-18 01:02:00','noop','{}','')"))
noop <- dashboard_pipeline_info(
  dashboard_refresh_meta(conn, status_path = isolated_status_path), as.Date("2026-07-18"))
check(identical(noop$state, "fresh") &&
        grepl("no source changes", noop$label, fixed = TRUE),
      "fresh noop presentation failed")

dbExecute(conn, "DELETE FROM refresh_log")
dbExecute(conn, paste(
  "INSERT INTO refresh_log VALUES",
  "('c','2026-07-18 01:00:00','2026-07-18 01:10:00','degraded','{}','partial')"))
degraded <- dashboard_pipeline_info(
  dashboard_refresh_meta(conn, status_path = isolated_status_path), as.Date("2026-07-18"))
check(identical(degraded$state, "warning"),
      "degraded refresh should warn")

dbExecute(conn, "DELETE FROM refresh_log")
dbExecute(conn, paste(
  "INSERT INTO refresh_log VALUES",
  "('d','2026-07-18 01:00:00','2026-07-18 01:10:00','failed','{}','network')"))
failed <- dashboard_pipeline_info(
  dashboard_refresh_meta(conn, status_path = isolated_status_path), as.Date("2026-07-18"))
check(identical(failed$state, "stale") &&
        grepl("failed", failed$label, fixed = TRUE),
      "failed refresh should be explicit")
check(grepl("rolled back", failed$detail, fixed = TRUE) &&
        grepl("Jul 18, 2026", failed$detail, fixed = TRUE),
      "failed refresh detail should name the retained source snapshot")

sidecar_path <- tempfile(fileext = ".json")
on.exit(unlink(sidecar_path), add = TRUE)
jsonlite::write_json(list(pipeline_status = "failed", pipeline_checked = "2026-07-22", pipeline_message = "Latest refresh failed validation; the previous source snapshot remains published."), sidecar_path, auto_unbox = TRUE)
sidecar_meta <- dashboard_refresh_meta(conn, status_path = sidecar_path)
check(identical(sidecar_meta$checked_date, as.Date("2026-07-22")) && identical(sidecar_meta$status, "failed") && identical(sidecar_meta$capture_date, as.Date("2026-07-18")), "newer sidecar should override only pipeline status")
sidecar_failed <- dashboard_pipeline_info(sidecar_meta, as.Date("2026-07-23"))
check(identical(sidecar_failed$state, "stale") && grepl("Latest refresh failed validation", sidecar_failed$detail, fixed = TRUE) && grepl("Jul 18, 2026", sidecar_failed$detail, fixed = TRUE), "sidecar failure should retain the DB-derived source snapshot")

dbExecute(conn, "UPDATE recruit_class_football SET ScrapedAt = NULL")
missing_stamp <- .dashboard_table_snapshot(conn, "recruit_class_football")
check(is.null(missing_stamp$date) && missing_stamp$teams == 2L,
      "NULL source stamp handling failed")
missing_table <- .dashboard_table_snapshot(conn, "does_not_exist")
check(is.null(missing_table$date) && missing_table$teams == 0L,
      "missing table handling failed")

old_meta <- list(capture_date = as.Date("2026-07-10"),
                 checked_date = as.Date("2026-07-10"),
                 updated_date = as.Date("2026-07-10"),
                 status = "ok")
stale <- dashboard_freshness_info(old_meta, as.Date("2026-07-18"))
check(identical(stale$state, "stale") && stale$age_days == 8L,
      "age-based staleness failed")

elite <- .hf_blue_chip_stat(c(103, 95, 90))
check(elite$n == 3L && identical(elite$value, 100),
      "101-103 elite ratings must count as blue chips")
rating_spec <- .hf_metric_specs("football", 3L, 2L)[[1]]
elite_mean <- rating_spec$stat(data.frame(Ranking = c(103, 95, 90)))
check(elite_mean$n == 3L && isTRUE(all.equal(elite_mean$value, 96)),
      "101-103 elite ratings must count in the fingerprint average")

message("Dashboard status validation passed.")
