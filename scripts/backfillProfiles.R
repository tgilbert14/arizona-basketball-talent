## ===========================================================================
## backfillProfiles.R
## Fill missing hometowns from 247Sports player profile pages.
##
## refreshClassYear.R stores each player's profile href (ProfileUrl), but
## portal transfers (and the odd commit) arrive with no Location string, so
## they can never geocode onto the map. This script visits the profile page
## of every row that HAS a ProfileUrl but NO Location, reads the Prospect
## Info block -- 247 labels the hometown "City", never "Hometown" -- and
## writes Location back by rowid in the house format
## "High School (City, ST)" so geocodeMissing.R picks it up the same run.
##
## Resumable by construction: a failed fetch or an unparseable page is
## skipped, its Location stays NULL, and a future run retries it. The fetch
## cap keeps each night polite; the script exits 0 even when candidates
## remain.
##
## Both sports share ONE queue ordered Year DESC, so a deep football
## backlog can no longer starve basketball of the whole budget. A give-up
## ledger (backfill_attempts) parks rows that failed 5 times -- pages that
## permanently lack a City entry stop burning the cap every night; a
## success deletes the row's ledger entry.
##
## Run from the project root:
##   Rscript scripts/backfillProfiles.R                        # both, cap 40
##   Rscript scripts/backfillProfiles.R football --max-fetches 10
##   Rscript scripts/backfillProfiles.R both --db <copy.db>    # tests only
## ===========================================================================

suppressMessages({
  library(rvest)
  library(httr)
  library(DBI)
  library(RSQLite)
})

## same browser face as refreshClassYear.R (the nightly S0 probe reads its
## UA from that file; this one just mirrors it)
UA <- user_agent(paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"))

## ---------------------------------------------------------------------------
## args: [sport] [--max-fetches N] [--db path]
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sport <- "both"
max_fetches <- 40L
db_path <- here::here("data", "recruiting.db")
i <- 1L
while (i <= length(args)) {
  a <- args[i]
  if (a == "--max-fetches" && i < length(args)) {
    max_fetches <- suppressWarnings(as.integer(args[i + 1L]))
    i <- i + 2L
  } else if (a == "--db" && i < length(args)) {
    db_path <- args[i + 1L]
    i <- i + 2L
  } else {
    sport <- tolower(a)
    i <- i + 1L
  }
}
if (!sport %in% c("both", "football", "basketball")) {
  stop("sport must be football, basketball, or both (got '", sport, "')")
}
if (is.na(max_fetches) || max_fetches < 0) {
  stop("--max-fetches must be a non-negative integer")
}
if (!file.exists(db_path)) stop("db not found: ", db_path)

tables <- if (sport == "both") {
  c("recruit_class_football", "recruit_class_basketball")
} else {
  paste0("recruit_class_", sport)
}

## ---------------------------------------------------------------------------
## fetch + parse
## ---------------------------------------------------------------------------
fetch_profile <- function(url) {
  ## test affordance: a non-http ProfileUrl is read as a saved local page,
  ## so the write path can be exercised without touching 247
  if (!grepl("^https?://", url)) {
    if (file.exists(url)) {
      return(tryCatch(read_html(url), error = function(e) NULL))
    }
    return(NULL)
  }
  resp <- try(GET(url, UA, timeout(25)), silent = TRUE)
  if (inherits(resp, "try-error") || status_code(resp) != 200) return(NULL)
  read_html(content(resp, "text"))
}

## Prospect Info block: <ul class="details "> of <li><span>Label</span>
## <span>Value</span></li> pairs. Hometown is the li labeled exactly "City"
## (value "City, ST"); the "High School" li names the school. The ul's class
## attribute carries a trailing space, so only class selectors match it.
parse_profile <- function(page) {
  out <- list(city = NA_character_, hs = NA_character_)
  lis <- tryCatch(html_nodes(page, ".details li"), error = function(e) NULL)
  if (is.null(lis) || length(lis) == 0) return(out)
  for (li in lis) {
    spans <- html_nodes(li, "span")
    if (length(spans) < 2) next
    lab <- html_text(spans[[1]], trim = TRUE)
    val <- html_text(spans[[2]], trim = TRUE)
    if (identical(lab, "City") && nzchar(val)) out$city <- val
    if (identical(lab, "High School") && nzchar(val)) out$hs <- val
  }
  out
}

## db Location strings look like "School Name (City, ST)" -- geocodeMissing.R
## parses the parenthesized part, so the backfilled value must match that
## shape. Pages without a High School entry get a bare "(City, ST)", which
## still parses. Non-US hometowns (e.g. "Toronto, ON") are written as-is:
## geocodeMissing's state-bbox gate skips them safely, and writing what the
## profile says stops the row from burning the fetch cap every night.
build_location <- function(info) {
  if (is.na(info$city) || !nzchar(info$city)) return(NA_character_)
  if (!is.na(info$hs) && nzchar(info$hs)) {
    paste0(info$hs, " (", info$city, ")")
  } else {
    paste0("(", info$city, ")")
  }
}

## ---------------------------------------------------------------------------
## main loop: ONE queue across both tables (interleaved Year DESC) sharing
## the fetch budget, with a give-up ledger so permanently unfillable rows
## stop hogging the head of the queue
## ---------------------------------------------------------------------------
conn <- dbConnect(RSQLite::SQLite(), db_path)

## give-up ledger. Keyed (tbl, row_id); row_id is the recruit table's SQLite
## rowid. refreshClassYear's per-school delete+rewrite renumbers rowids, so
## a ledger entry can go stale -- worst case a parked slot re-opens or a new
## player inherits a failed attempt or two, both harmless for a retry
## throttle. attempts >= 5 parks the candidate; success deletes the entry.
invisible(dbExecute(conn, paste0(
  "CREATE TABLE IF NOT EXISTS backfill_attempts (",
  "tbl TEXT NOT NULL, row_id INTEGER NOT NULL, ",
  "attempts INTEGER NOT NULL DEFAULT 0, last_try TEXT, ",
  "PRIMARY KEY (tbl, row_id))")))

queue <- list()
for (tbl in tables) {
  cols <- dbGetQuery(conn, paste0("PRAGMA table_info(", tbl, ")"))$name
  if (!"ProfileUrl" %in% cols) {
    cat(tbl, ": no ProfileUrl column yet (refreshClassYear.R adds it on its",
        "next run) -- skipping\n")
    next
  }
  todo <- dbGetQuery(conn, paste0(
    "SELECT rowid AS rid, Name, School, Year, Type, ProfileUrl FROM ", tbl,
    " WHERE ProfileUrl IS NOT NULL AND ProfileUrl != ''",
    " AND ProfileUrl != 'NA'",
    " AND (Location IS NULL OR Location = '' OR Location = 'NA')"))
  cat(tbl, ":", nrow(todo), "rows with a ProfileUrl but no Location\n")
  if (nrow(todo) > 0) {
    todo$tbl <- tbl
    queue[[tbl]] <- todo
  }
}
queue <- do.call(rbind, queue)
if (is.null(queue)) queue <- data.frame(rid = integer(0), Year = integer(0),
                                        tbl = character(0),
                                        ProfileUrl = character(0),
                                        Name = character(0),
                                        School = character(0),
                                        stringsAsFactors = FALSE)

## park rows the ledger says have burned 5 attempts already
parked <- 0L
if (nrow(queue) > 0) {
  led <- dbGetQuery(conn,
                    "SELECT tbl, row_id, attempts FROM backfill_attempts")
  give_up <- paste(queue$tbl, queue$rid) %in%
    paste(led$tbl, led$row_id)[led$attempts >= 5L]
  parked <- sum(give_up)
  queue <- queue[!give_up, , drop = FALSE]
}
## newest classes first ACROSS both sports; order() is stable, so within a
## year football rows precede basketball (queue build order) -- the budget
## split follows the data, not the table iteration order
queue <- queue[order(-queue$Year), , drop = FALSE]
if (parked > 0) {
  cat("parked by the give-up ledger (attempts >= 5):", parked, "\n")
}

total_candidates <- nrow(queue)
attempted <- 0L
filled <- 0L
budget <- max_fetches

for (j in seq_len(nrow(queue))) {
  if (budget <= 0L) break
  budget <- budget - 1L
  attempted <- attempted + 1L
  row <- queue[j, ]
  ok <- tryCatch({
    page <- fetch_profile(row$ProfileUrl)
    if (is.null(page)) {
      FALSE
    } else {
      loc <- build_location(parse_profile(page))
      if (is.na(loc)) {
        FALSE
      } else {
        dbExecute(conn, paste0(
          "UPDATE ", row$tbl, " SET Location = ? WHERE rowid = ?"),
          params = list(loc, row$rid))
        TRUE
      }
    }
  }, error = function(e) {
    message("  ", row$Name, " (", row$School, " ", row$Year, "): ",
            conditionMessage(e))
    FALSE
  })
  if (ok) {
    filled <- filled + 1L
    dbExecute(conn,
              "DELETE FROM backfill_attempts WHERE tbl = ? AND row_id = ?",
              params = list(row$tbl, row$rid))
  } else {
    dbExecute(conn, paste0(
      "INSERT INTO backfill_attempts (tbl, row_id, attempts, last_try) ",
      "VALUES (?, ?, 1, ?) ",
      "ON CONFLICT (tbl, row_id) DO UPDATE SET ",
      "attempts = attempts + 1, last_try = excluded.last_try"),
      params = list(row$tbl, row$rid,
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  }
  if (attempted %% 10 == 0) {
    cat("  progress:", attempted, "fetched,", filled, "filled\n")
  }
  ## polite pacing for live fetches only (local test pages skip the nap)
  if (grepl("^https?://", row$ProfileUrl)) Sys.sleep(runif(1, 2, 4))
}

dbDisconnect(conn)

cat(sprintf("filled %d of %d candidates (%d remain)\n",
            filled, total_candidates, total_candidates - filled))
## candidates left over (cap reached, fetch hiccups, pages without a City
## entry) are picked up on the next run -- never a failure. Rows that fail
## 5 runs in a row are parked by the ledger and stop consuming the budget.
