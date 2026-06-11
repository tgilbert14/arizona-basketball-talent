## Back-fill the portal-era class years (2021-2025) for both sports by
## invoking refreshClassYear.R once per sport-year. Each run replaces that
## year's rows with a fresh scrape INCLUDING transfers; geo columns carry
## over via the normalized-name join. ~15-20 min total (polite delays).
rs <- file.path(R.home("bin"), "Rscript.exe")
script <- here::here("scripts", "refreshClassYear.R")
for (year in 2021:2025) {
  for (sport in c("football", "basketball")) {
    cat("\n=== refreshing", sport, year, "===\n")
    status <- system2(rs, c(shQuote(script), sport, year))
    if (status != 0) cat("!!! run failed for", sport, year, "\n")
  }
}
cat("\nBACKFILL COMPLETE\n")
