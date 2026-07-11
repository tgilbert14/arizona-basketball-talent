## ===========================================================================
## updateManifest.R
## Keep manifest.json's per-file checksums in lock-step with the runtime
## files, so Posit Connect Cloud (which deploys from the committed manifest)
## ships the CURRENT data bundle, not a stale one.
##
## WHY THIS EXISTS: the nightly pipeline rewrites data/recruiting.db and
## precomputed/*.rds. manifest.json embeds an MD5 per runtime file; if those
## checksums do not track the refreshed files, Connect Cloud serves the OLD
## snapshot (the same lesson the NEON sibling apps learned -- they regenerate
## their manifest on every refresh). shinyapps.io is unaffected (it deploys by
## rsconnect appFiles, not the manifest).
##
## WHAT IT TOUCHES: only the `files` section -- it enumerates the true runtime
## set (app.R, R/, www/, data/recruiting.db, precomputed/), computes MD5 for
## each (base tools::md5sum, the same digest rsconnect writes), ADDS any file
## missing from the manifest (e.g. precomputed renders added after the manifest
## was last written), DROPS entries whose file no longer exists, and refreshes
## every checksum. The packages / version / locale / platform / metadata /
## users sections are preserved EXACTLY -- packages change only when a human
## edits code + regenerates the full manifest with rsconnect::writeManifest(),
## never on a data-only refresh. That keeps this step incapable of breaking the
## Connect Cloud package install.
##
##   Rscript scripts/updateManifest.R            # rewrite in place
##   Rscript scripts/updateManifest.R --check    # exit 1 if out of date, no write
##
## Run from the repo root, after precompute and before the deploy commit.
## ===========================================================================

suppressMessages({library(jsonlite); library(tools)})

args     <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
manifest_path <- "manifest.json"

if (!file.exists(manifest_path)) {
  stop("manifest.json not found -- run from the repo root")
}

## The runtime file set == what the app actually needs at serve time.
## Mirror deployApp.R's APP_FILES: app.R + the R/, www/, precomputed/ dirs +
## the db. A directory contributes every file under it (recursively).
runtime_roots <- c("app.R", "R", "www", "data/recruiting.db", "precomputed")

expand_files <- function(roots) {
  out <- character(0)
  for (r in roots) {
    if (!file.exists(r)) {
      warning("runtime path missing: ", r)
      next
    }
    if (dir.exists(r)) {
      out <- c(out, list.files(r, recursive = TRUE, full.names = TRUE))
    } else {
      out <- c(out, r)
    }
  }
  ## normalize to forward-slash repo-relative paths (manifest convention)
  gsub("\\\\", "/", out)
}

files <- expand_files(runtime_roots)
files <- sort(unique(files))

## tools::md5sum returns the same lowercase hex MD5 rsconnect embeds.
checks <- unname(tools::md5sum(files))
if (any(is.na(checks))) {
  stop("could not checksum: ", paste(files[is.na(checks)], collapse = ", "))
}
new_files <- setNames(lapply(checks, function(c) list(checksum = unname(c))), files)

## Read the existing manifest, swap ONLY the files section, preserve the rest.
m <- fromJSON(manifest_path, simplifyVector = FALSE)
old_files <- m$files %||% list()

## Compare (added / dropped / changed) for logging + the --check gate.
old_keys <- names(old_files)
new_keys <- names(new_files)
added   <- setdiff(new_keys, old_keys)
dropped <- setdiff(old_keys, new_keys)
changed <- intersect(old_keys, new_keys)
changed <- changed[vapply(changed, function(k)
  !identical(old_files[[k]]$checksum, new_files[[k]]$checksum), logical(1))]

up_to_date <- length(added) == 0 && length(dropped) == 0 && length(changed) == 0

cat("manifest files:", length(new_keys), "runtime files\n")
if (length(added))   cat("  + added  :", paste(added,   collapse = ", "), "\n")
if (length(dropped)) cat("  - dropped:", paste(dropped, collapse = ", "), "\n")
if (length(changed)) cat("  ~ changed:", paste(changed, collapse = ", "), "\n")

if (check_only) {
  if (up_to_date) {
    cat("manifest.json is up to date.\n")
    quit(status = 0)
  }
  cat("manifest.json is OUT OF DATE (run without --check to rewrite).\n")
  quit(status = 1)
}

if (up_to_date) {
  cat("manifest.json already current -- no write.\n")
  quit(status = 0)
}

## Preserve package count as a guard: this script must never alter packages.
pkgs_before <- length(m$packages %||% list())
m$files <- new_files
pkgs_after <- length(m$packages %||% list())
if (!identical(pkgs_before, pkgs_after)) {
  stop("internal error: package section changed (", pkgs_before, " -> ",
       pkgs_after, ") -- refusing to write")
}

## Write with rsconnect-compatible formatting: 2-space indent, unboxed scalars,
## explicit nulls (metadata$primary_rmd etc.), no trailing newline surprises.
json <- toJSON(m, auto_unbox = TRUE, pretty = 2, null = "null", na = "null")
writeLines(json, manifest_path)

cat("manifest.json rewritten:", length(added), "added,",
    length(dropped), "dropped,", length(changed), "checksums updated;",
    pkgs_after, "packages preserved.\n")
