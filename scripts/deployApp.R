## ---------------------------------------------------------------------------
## deployApp.R -- headless shinyapps.io deploy for the Big 12 Talent Lab
##
##   Rscript scripts/deployApp.R              # bundle, deploy, health-check
##   Rscript scripts/deployApp.R --dry-run    # print the plan, touch nothing
##
## Bundles ONLY what app.R needs at runtime (verified against app.R):
##   app.R                 the app itself (Shiny auto-sources R/)
##   R/                    plot builders, coach eras, team config, helpers
##   www/                  16 team logos + html-to-image.min.js
##   data/recruiting.db    SQLite content tables (read-only at runtime)
##   precomputed/          default-view girafe renders (readRDS at startup)
## docs/, scripts/, insights/, backups/, logs/ are never read by app.R.
## .rscignore at the repo root is the belt-and-braces guard for any manual
## rsconnect::deployApp() call that forgets to pin appFiles.
##
## Exit codes: 0 = deployed (health-check warning still exits 0), 1 = failed.
## ---------------------------------------------------------------------------

args <- tolower(commandArgs(trailingOnly = TRUE))
dry_run <- any(args %in% c("--dry-run", "dry-run"))

root <- here::here()
setwd(root)

APP_ID   <- 16698009
APP_NAME <- "Big-12-Talent-Pathways"
ACCOUNT  <- "t-lama"
SERVER   <- "shinyapps.io"
APP_URL  <- sprintf("https://%s.shinyapps.io/%s/", ACCOUNT, APP_NAME)

APP_FILES <- c("app.R", "R", "www", "data/recruiting.db", "precomputed")

## -- sanity: every bundled path must exist --------------------------------
missing <- APP_FILES[!file.exists(APP_FILES)]
if (length(missing) > 0) {
  message("FATAL: bundle paths missing from ", root, ":")
  for (m in missing) message("  - ", m)
  quit(save = "no", status = 1)
}

rds <- list.files("precomputed", pattern = "\\.rds$")
if (length(rds) == 0) {
  message("WARN: precomputed/ holds no .rds files -- the deployed app will ",
          "fall back to live first renders. Run scripts/precomputeDefaults.R.")
} else {
  db_mtime  <- file.mtime("data/recruiting.db")
  rds_mtime <- max(file.mtime(file.path("precomputed", rds)))
  if (rds_mtime < db_mtime) {
    message("WARN: precomputed/*.rds mtimes are older than data/recruiting.db. ",
            "mtime is a weak signal (OneDrive sync and git checkouts both ",
            "rewrite it) -- the nightly pipeline hash-gates precompute, so ",
            "trust its hash gate. If the live defaults actually look stale, ",
            "run scripts/precomputeDefaults.R.")
  }
}

## -- expand the bundle for display + size ----------------------------------
expand_bundle <- function(paths) {
  unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE)
    else p
  }), use.names = FALSE)
}
bundle <- expand_bundle(APP_FILES)
bundle_mb <- round(sum(file.size(bundle)) / 1024^2, 1)

## -- account guard ----------------------------------------------------------
account_ok <- tryCatch({
  accts <- rsconnect::accounts()
  !is.null(accts) && nrow(accts) > 0 &&
    any(accts$name == ACCOUNT & accts$server == SERVER)
}, error = function(e) FALSE)

setup_help <- function() {
  message("FATAL: rsconnect has no '", ACCOUNT, "' account on ", SERVER, ".")
  message("One-time setup (token page: https://www.shinyapps.io/admin/#/tokens):")
  message("  Rscript -e \"rsconnect::setAccountInfo(name='", ACCOUNT,
          "', token='<TOKEN>', secret='<SECRET>')\"")
}

## -- dry run: print the plan and stop --------------------------------------
if (dry_run) {
  cat("== deployApp.R dry run ==\n")
  cat("Would call:\n")
  cat("  rsconnect::deployApp(appDir = '.',\n")
  cat("    appFiles = c(", paste(sQuote(APP_FILES), collapse = ", "), "),\n")
  cat("    appId = ", APP_ID, ", appName = '", APP_NAME, "',\n", sep = "")
  cat("    account = '", ACCOUNT, "', server = '", SERVER, "',\n", sep = "")
  cat("    forceUpdate = TRUE, launch.browser = FALSE, logLevel = 'quiet')\n")
  cat("\nBundle:", length(bundle), "files,", bundle_mb, "MB\n")
  for (f in bundle) cat("  ", f, "\n")
  cat("\nAccount '", ACCOUNT, "' configured: ",
      if (account_ok) "yes" else "NO (deploy would fail -- see setup below)",
      "\n", sep = "")
  if (!account_ok) setup_help()
  quit(save = "no", status = 0)
}

if (!account_ok) {
  setup_help()
  quit(save = "no", status = 1)
}

## -- deploy -----------------------------------------------------------------
message("Deploying ", APP_NAME, " (appId ", APP_ID, "): ",
        length(bundle), " files, ", bundle_mb, " MB")
deploy_ok <- tryCatch({
  rsconnect::deployApp(
    appDir         = ".",
    appFiles       = APP_FILES,
    appId          = APP_ID,
    appName        = APP_NAME,
    account        = ACCOUNT,
    server         = SERVER,
    forceUpdate    = TRUE,
    launch.browser = FALSE,
    logLevel       = "quiet")
  TRUE
}, error = function(e) {
  message("DEPLOY FAILED: ", conditionMessage(e))
  FALSE
})
if (!deploy_ok) quit(save = "no", status = 1)
message("Deploy succeeded: ", APP_URL)

## -- post-deploy health check (warn-only: the deploy itself succeeded) -----
## first hit after a deploy can be slow while the worker cold-starts
resp <- tryCatch(httr::GET(APP_URL, httr::timeout(90)), error = function(e) e)
if (inherits(resp, "error")) {
  message("WARN: health check could not reach ", APP_URL, ": ",
          conditionMessage(resp))
} else if (httr::status_code(resp) != 200) {
  message("WARN: ", APP_URL, " returned HTTP ", httr::status_code(resp),
          " -- check the shinyapps.io dashboard logs.")
} else {
  message("Health check: HTTP 200 from ", APP_URL)
}

quit(save = "no", status = 0)
