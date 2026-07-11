## ===========================================================================
## auditRefreshHoles.R
## Safety net for refreshClassYear.R: compare the live db against a baseline
## snapshot (backups/recruiting_HEAD.db, extracted from git) and flag any
## school-year that HAD commits before but has ZERO now -- the signature of
## a fetch failure that wiped rows under the old delete-whole-year logic.
## Heals holes by restoring the baseline rows for just those school-years.
## ===========================================================================

suppressMessages({library(dplyr); library(DBI); library(RSQLite)})

## baseline resolution: CLI arg -> backups/recruiting_HEAD.db -> regenerate
## the HEAD copy of the db straight from git
arg_base <- commandArgs(trailingOnly = TRUE)[1]
base_path <- if (!is.na(arg_base) && nzchar(arg_base) && file.exists(arg_base)) {
  cat("Baseline: CLI arg", arg_base, "\n")
  arg_base
} else {
  here::here("backups", "recruiting_HEAD.db")
}
if (!file.exists(base_path)) {
  cat("No baseline snapshot found -- regenerating from git HEAD...\n")
  base_path <- tempfile(fileext = ".db")
  status <- system2("git",
                    c("-C", shQuote(here::here()),
                      "cat-file", "blob", "HEAD:data/recruiting.db"),
                    stdout = base_path)
  if (status != 0 || !file.exists(base_path) || file.size(base_path) == 0) {
    stop("could not regenerate baseline from git ",
         "(git cat-file exit ", status, ")")
  }
  ## the redirect must be byte-exact for SQLite to accept the file
  chk_conn <- dbConnect(RSQLite::SQLite(), base_path)
  chk <- tryCatch(dbGetQuery(chk_conn, "PRAGMA quick_check")[[1]][1],
                  error = function(e) conditionMessage(e))
  dbDisconnect(chk_conn)
  if (!identical(chk, "ok")) {
    stop("git-regenerated baseline failed PRAGMA quick_check ('", chk,
         "') -- refusing to audit against a corrupt snapshot")
  }
  cat("Baseline: regenerated from git HEAD (quick_check ok)\n")
}
## optional 2nd CLI arg = the db to audit/heal (default the live db) --
## lets tests run the full heal path against scratch copies
arg_live <- commandArgs(trailingOnly = TRUE)[2]
live_path <- if (!is.na(arg_live) && nzchar(arg_live) && file.exists(arg_live)) {
  cat("Auditing db: CLI arg", arg_live, "\n")
  arg_live
} else {
  here::here("data", "recruiting.db")
}
live <- dbConnect(RSQLite::SQLite(), live_path)
base <- dbConnect(RSQLite::SQLite(), base_path)

## heal ledger: a hole healed on MANY CONSECUTIVE runs is not a fetch
## failure -- it is 247 persistently NOT listing those commits (a decommit /
## reclassification; e.g. Kayden Allen leaving Cincinnati's 2026 class made
## the audit resurrect him nightly). Heal a school-year at most
## MAX_HEAL_STREAK runs in a row; after that, stop healing and say so
## loudly -- the next snapshot then rolls forward without the rows and the
## ledger entry clears itself. Transient page glitches still get healed.
MAX_HEAL_STREAK <- 3
dbExecute(live, paste0(
  "CREATE TABLE IF NOT EXISTS audit_heals (",
  "tbl TEXT, School TEXT, Year INTEGER, streak INTEGER, last_healed TEXT, ",
  "PRIMARY KEY (tbl, School, Year))"))

total_restored <- 0
for (tbl in c("recruit_class_football", "recruit_class_basketball")) {
  ## count COMMITS on both sides -- school-years that legitimately hold
  ## only portal transfers must not read as holes (an all-rows baseline
  ## count re-appended those transfers on every audit, duplicating them)
  old_counts <- dbGetQuery(base, paste0(
    "SELECT School, Year, COUNT(*) AS n_old FROM ", tbl,
    " WHERE Type = 'Commit' GROUP BY School, Year"))
  new_counts <- dbGetQuery(live, paste0(
    "SELECT School, Year, COUNT(*) AS n_new FROM ", tbl,
    " WHERE Type = 'Commit' GROUP BY School, Year"))

  holes <- old_counts %>%
    left_join(new_counts, by = c("School", "Year")) %>%
    mutate(n_new = ifelse(is.na(n_new), 0, n_new)) %>%
    filter(n_old > 0, n_new == 0)

  ## clear ledger entries whose hole did not recur this run (commits came
  ## back, or the baseline rolled forward without them) -- streaks are
  ## CONSECUTIVE by construction
  led <- dbGetQuery(live, paste0(
    "SELECT School, Year, streak FROM audit_heals WHERE tbl = '", tbl, "'"))
  if (nrow(led) > 0) {
    gone <- anti_join(led, holes, by = c("School", "Year"))
    for (j in seq_len(nrow(gone))) {
      dbExecute(live, paste0(
        "DELETE FROM audit_heals WHERE tbl = '", tbl, "' AND School = '",
        gone$School[j], "' AND Year = ", gone$Year[j]))
      cat("  heal streak cleared:", gone$School[j], gone$Year[j],
          "(hole did not recur)\n")
    }
  }

  if (nrow(holes) == 0) {
    cat(tbl, ": no holes\n")
    next
  }
  cat(tbl, "HOLES (school-years wiped by failed fetches):\n")
  print(holes)

  for (i in seq_len(nrow(holes))) {
    prev <- dbGetQuery(live, paste0(
      "SELECT streak FROM audit_heals WHERE tbl = '", tbl,
      "' AND School = '", holes$School[i], "' AND Year = ", holes$Year[i]))
    streak <- if (nrow(prev) == 0) 1L else prev$streak[1] + 1L
    dbExecute(live, paste0(
      "INSERT INTO audit_heals (tbl, School, Year, streak, last_healed) ",
      "VALUES ('", tbl, "', '", holes$School[i], "', ", holes$Year[i], ", ",
      streak, ", '", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "') ",
      "ON CONFLICT (tbl, School, Year) DO UPDATE SET streak = ", streak,
      ", last_healed = excluded.last_healed"))

    if (streak > MAX_HEAL_STREAK) {
      cat("  NOT healed:", holes$School[i], holes$Year[i],
          "(healed on", MAX_HEAL_STREAK, "consecutive runs already --",
          "probable decommit/removal on 247; letting the source win;",
          "pre-run snapshots in backups/ keep the history)\n")
      next
    }

    rows <- dbGetQuery(base, paste0(
      "SELECT * FROM ", tbl, " WHERE School = '", holes$School[i],
      "' AND Year = ", holes$Year[i]))
    ## never duplicate players the live db still has (e.g. transfers that
    ## survived the wipe) -- restore only what is actually missing
    have <- dbGetQuery(live, paste0(
      "SELECT Name, Type FROM ", tbl, " WHERE School = '", holes$School[i],
      "' AND Year = ", holes$Year[i]))
    if (nrow(have) > 0) rows <- anti_join(rows, have, by = c("Name", "Type"))
    if (nrow(rows) == 0) next
    dbWriteTable(live, tbl, rows, append = TRUE)
    total_restored <- total_restored + nrow(rows)
    cat("  restored", nrow(rows), "rows:", holes$School[i], holes$Year[i],
        "(heal streak", streak, "of", MAX_HEAL_STREAK, ")\n")
  }
}
cat("\nTotal rows restored from baseline:", total_restored, "\n")
dbDisconnect(live); dbDisconnect(base)
