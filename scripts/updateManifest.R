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
## set (app.R, R/, www/, data/recruiting.db, data/team_config.csv,
## precomputed/), computes MD5 for each (base tools::md5sum, the same digest
## rsconnect writes), ADDS any file missing from the manifest (e.g. renders
## added after it was last written), DROPS entries whose file no longer exists, and refreshes
## every checksum. The packages / version / locale / platform / metadata /
## users sections are preserved EXACTLY -- packages change only when a human
## edits code + regenerates the full manifest with rsconnect::writeManifest(),
## never on a data-only refresh. That keeps this step incapable of breaking the
## Connect Cloud package install.
##
##   Rscript scripts/updateManifest.R                       # rewrite in place
##   Rscript scripts/updateManifest.R --check               # exit 1 if out of date, no write
##   Rscript scripts/updateManifest.R --paths=www/pipeline-status.json
##       # update only listed runtime checksums; preserves all other entries
##
## Run from the repo root, after precompute and before the deploy commit.
## ===========================================================================

suppressMessages({library(jsonlite); library(tools)})

if (!exists("%||%", mode = "function"))
  `%||%` <- function(x, y) if (is.null(x)) y else x

args     <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
manifest_path <- "manifest.json"

target_args <- grep("^--paths=", args, value = TRUE)
if (length(target_args) > 1L) stop("pass at most one --paths= argument")
target_paths <- if (length(target_args)) {
  raw <- sub("^--paths=", "", target_args[[1]])
  raw <- strsplit(raw, ",", fixed = TRUE)[[1]]
  trimws(gsub("\\\\", "/", raw))
} else NULL
if (!is.null(target_paths) &&
    (!length(target_paths) || any(!nzchar(target_paths)))) {
  stop("--paths= needs one or more comma-separated runtime paths")
}
if (!file.exists(manifest_path)) {
  stop("manifest.json not found -- run from the repo root")
}

## The runtime file set == what the app actually needs at serve time.
## Mirror deployApp.R's APP_FILES: app.R + R/, www/, precomputed/, the db, and
## the external 67-team configuration. A directory contributes every file.
runtime_roots <- c("app.R", "R", "www", "data/recruiting.db",
                   "data/team_config.csv", "precomputed")

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
  out <- gsub("\\\\", "/", out)
  ## Never publish local patch/reject backups that happen to sit under R/.
  out[!grepl("\\.(orig|rej)$", out, ignore.case = TRUE)]
}

all_files <- expand_files(runtime_roots)
all_files <- sort(unique(all_files))
if (!is.null(target_paths)) {
  missing_targets <- setdiff(target_paths, all_files)
  if (length(missing_targets)) {
    stop("--paths= is not a present runtime file: ",
         paste(missing_targets, collapse = ", "))
  }
  files <- target_paths
} else {
  files <- all_files
}

## Git stores text as LF, while a Windows checkout can expose the same tracked
## bytes as CRLF.  Connect deploys the committed form, so normalize CRLF to LF
## for known text assets before hashing.  This keeps --check deterministic on
## Windows and Linux without ever changing a runtime file in the worktree.
text_extensions <- c("r", "css", "js", "csv", "html", "htm", "json",
                     "svg", "txt")

runtime_md5 <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% text_extensions) {
    return(unname(tools::md5sum(path)))
  }

  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  if (length(bytes) > 1L) {
    cr <- as.raw(0x0D)
    lf <- as.raw(0x0A)
    drop_cr <- c(bytes[-length(bytes)] == cr & bytes[-1L] == lf, FALSE)
    bytes <- bytes[!drop_cr]
  }

  normalized <- tempfile(fileext = paste0(".", ext))
  on.exit(unlink(normalized), add = TRUE)
  writeBin(bytes, normalized)
  unname(tools::md5sum(normalized))
}

checks <- vapply(files, runtime_md5, character(1), USE.NAMES = FALSE)
if (any(is.na(checks))) {
  stop("could not checksum: ", paste(files[is.na(checks)], collapse = ", "))
}

## Read the existing manifest, swap ONLY the files section, preserve the rest.
m <- fromJSON(manifest_path, simplifyVector = FALSE)
old_files <- m$files %||% list()

if (!is.null(target_paths)) {
  new_files <- old_files
  for (i in seq_along(files)) {
    new_files[[files[[i]]]] <- list(checksum = unname(checks[[i]]))
  }
} else {
  new_files <- setNames(lapply(checks, function(c) list(checksum = unname(c))), files)
}

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
## explicit nulls (metadata$primary_rmd etc.), and UTF-8 bytes regardless of
## the host's Windows locale.
json <- toJSON(m, auto_unbox = TRUE, pretty = 2, null = "null", na = "null")
json <- enc2utf8(as.character(json))
json <- sub("[[:space:]]+$", "", json)
manifest_con <- file(manifest_path, open = "wb")
writeBin(charToRaw(json), manifest_con)
writeBin(as.raw(0x0A), manifest_con)
close(manifest_con)

cat("manifest.json rewritten:", length(added), "added,",
    length(dropped), "dropped,", length(changed), "checksums updated;",
    pkgs_after, "packages preserved.\n")
