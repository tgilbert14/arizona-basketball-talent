## ---------------------------------------------------------------------------
## refreshAll.R -- the one-command data refresh
##
##   Rscript scripts/refreshAll.R                # everything below, in order
##   Rscript scripts/refreshAll.R no-rosters     # skip the roster scrape
##
## Steps (each script keeps its own safety rails -- per-school replace,
## retries, timestamped backups, validation gates):
##   1. classes   refreshClassYear.R for the newest cycle, both sports
##                (commits + portal transfers; ratings re-rank all cycle)
##   2. rosters   scrapeRosters.R, both sports (heights/weights/classes)
##   3. geocode   geocodeMissing.R (new commits -> map, state-bbox checked)
##   4. audit     auditRefreshHoles.R (restores any school-year a failed
##                fetch wiped)
##   5. records   fetchOutcomes.R (CFBD season records + SP+; needs the
##                CFBD_API_KEY in ~/.Renviron; skip until a season ends)
##
## Skip flags: no-classes, no-rosters, no-geocode, no-audit, no-records
##
## To START A NEW CYCLE (e.g. the 2027 classes appear on 247): run once
##   Rscript scripts/refreshClassYear.R football 2027
## after that, this script picks 2027 up automatically (newest year in db).
## ---------------------------------------------------------------------------

suppressMessages({
  library(DBI)
  library(RSQLite)
})

skip <- tolower(commandArgs(trailingOnly = TRUE))
rscript <- file.path(R.home("bin"), "Rscript")
db_path <- here::here("data", "recruiting.db")

table_counts <- function() {
  conn <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(conn))
  tabs <- c("recruit_class_football", "recruit_class_basketball",
            "roster_football", "roster_basketball", "team_seasons_football")
  sapply(tabs, function(t) {
    tryCatch(dbGetQuery(conn, paste0("SELECT COUNT(*) n FROM ", t))$n,
             error = function(e) NA_integer_)
  })
}

newest_year <- function(sport) {
  conn <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(conn))
  dbGetQuery(conn, paste0("SELECT MAX(Year) y FROM recruit_class_", sport))$y
}

run_step <- function(label, script, args = character(0)) {
  cat("\n==== ", label, " ====\n", sep = "")
  t0 <- Sys.time()
  status <- system2(rscript, c(file.path("scripts", script), args))
  mins <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)
  cat("[", label, "] ",
      ifelse(status == 0, "ok", paste0("EXIT CODE ", status)),
      " (", mins, " min)\n", sep = "")
  status == 0
}

before <- table_counts()
results <- c()

if (!"no-classes" %in% skip) {
  for (sp in c("football", "basketball")) {
    yr <- newest_year(sp)
    results[paste("classes", sp, yr)] <-
      run_step(paste("classes:", sp, yr), "refreshClassYear.R", c(sp, yr))
  }
}
if (!"no-rosters" %in% skip) {
  for (sp in c("football", "basketball")) {
    results[paste("rosters", sp)] <-
      run_step(paste("rosters:", sp), "scrapeRosters.R", sp)
  }
}
if (!"no-geocode" %in% skip) {
  results["geocode"] <- run_step("geocode new players", "geocodeMissing.R")
}
if (!"no-audit" %in% skip) {
  results["audit"] <- run_step("hole audit", "auditRefreshHoles.R")
}
if (!"no-records" %in% skip) {
  if (Sys.getenv("CFBD_API_KEY") == "") {
    cat("\n[records] skipped -- no CFBD_API_KEY in this session's .Renviron\n")
  } else {
    results["records"] <- run_step("season records", "fetchOutcomes.R")
  }
}

after <- table_counts()

cat("\n==== REFRESH SUMMARY ====\n")
for (t in names(before)) {
  cat(sprintf("%-28s %6s -> %6s (%+d)\n", t,
              format(before[[t]], big.mark = ","),
              format(after[[t]], big.mark = ","),
              after[[t]] - before[[t]]))
}
if (length(results) > 0 && any(!results)) {
  cat("\nFAILED steps:", paste(names(results)[!results], collapse = ", "),
      "\n  -> re-run those individually; the hole audit protects the db.\n")
} else {
  cat("\nAll steps clean. The deployed app picks this up on the next\n",
      "rsconnect::deployApp() (the db ships inside the bundle).\n", sep = "")
}
