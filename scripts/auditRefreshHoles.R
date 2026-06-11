## ===========================================================================
## auditRefreshHoles.R
## Safety net for refreshClassYear.R: compare the live db against a baseline
## snapshot (backups/recruiting_HEAD.db, extracted from git) and flag any
## school-year that HAD commits before but has ZERO now -- the signature of
## a fetch failure that wiped rows under the old delete-whole-year logic.
## Heals holes by restoring the baseline rows for just those school-years.
## ===========================================================================

suppressMessages({library(dplyr); library(DBI); library(RSQLite)})

base_path <- here::here("backups", "recruiting_HEAD.db")
stopifnot(file.exists(base_path))
live <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
base <- dbConnect(RSQLite::SQLite(), base_path)

total_restored <- 0
for (tbl in c("recruit_class_football", "recruit_class_basketball")) {
  old_counts <- dbGetQuery(base, paste0(
    "SELECT School, Year, COUNT(*) AS n_old FROM ", tbl,
    " GROUP BY School, Year"))
  new_counts <- dbGetQuery(live, paste0(
    "SELECT School, Year, COUNT(*) AS n_new FROM ", tbl,
    " WHERE Type = 'Commit' GROUP BY School, Year"))

  holes <- old_counts %>%
    left_join(new_counts, by = c("School", "Year")) %>%
    mutate(n_new = ifelse(is.na(n_new), 0, n_new)) %>%
    filter(n_old > 0, n_new == 0)

  if (nrow(holes) == 0) {
    cat(tbl, ": no holes\n")
    next
  }
  cat(tbl, "HOLES (school-years wiped by failed fetches):\n")
  print(holes)

  for (i in seq_len(nrow(holes))) {
    rows <- dbGetQuery(base, paste0(
      "SELECT * FROM ", tbl, " WHERE School = '", holes$School[i],
      "' AND Year = ", holes$Year[i]))
    dbWriteTable(live, tbl, rows, append = TRUE)
    total_restored <- total_restored + nrow(rows)
    cat("  restored", nrow(rows), "rows:", holes$School[i], holes$Year[i], "\n")
  }
}
cat("\nTotal rows restored from baseline:", total_restored, "\n")
dbDisconnect(live); dbDisconnect(base)
