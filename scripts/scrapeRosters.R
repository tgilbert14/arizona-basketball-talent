## ===========================================================================
## scrapeRosters.R
## Scrape CURRENT TEAM ROSTERS (not recruiting classes) for every Big 12
## program from 247Sports, for football or basketball.
##
## This replaces the single-team offset-math approach in scrapeData_teamGirth.R
## with a much more robust pipeline:
##   1. AUTO-DISCOVER each team's roster URL from its 247 college landing page
##      (no hardcoded team IDs like "Arizona-Wildcats-Football-146" needed)
##   2. Parse the roster with html_table() -- the page ships two aligned
##      tables (sticky Name column + stats columns) that we cbind together
##   3. Clean + standardize columns, parse height/weight
##   4. Write to data/recruiting.db (table roster_<sport>) + CSV backup
##
## Run from the project root:
##   Rscript scripts/scrapeRosters.R                  # football, current year
##   Rscript scripts/scrapeRosters.R basketball 2026  # sport + roster year
##
## Optional scope filters (default = every configured team, so a no-op unless
## passed) let a per-conference backfill target one league:
##   Rscript scripts/scrapeRosters.R football --conference "Big 12"
##   Rscript scripts/scrapeRosters.R football --slugs arizona,utah
## ===========================================================================

suppressMessages({
  library(rvest)
  library(httr)
  library(dplyr)
  library(stringr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

source(here::here("R", "team_config.R"))

## a normal browser user agent keeps the request polite + consistent
UA <- user_agent(paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"))

## fetch a page politely; returns xml document or NULL
fetch_page <- function(url) {
  resp <- try(GET(url, UA, timeout(25)), silent = TRUE)
  if (inherits(resp, "try-error") || status_code(resp) != 200) {
    return(NULL)
  }
  read_html(content(resp, "text"))
}

## ---------------------------------------------------------------------------
## find the roster URL for a team + sport from its 247 college landing page
## e.g. "arizona" + "football" ->
##   https://247sports.com/college/arizona/Team/Arizona-Wildcats-Football-146/Roster/
## ---------------------------------------------------------------------------
discover_roster_url <- function(slug, sport) {
  landing <- fetch_page(paste0("https://247sports.com/college/", slug, "/"))
  if (is.null(landing)) return(NA_character_)

  hrefs <- landing %>% html_nodes("a") %>% html_attr("href")
  pattern <- paste0("/Team/[^/]*-", sport, "-[0-9]+/Roster/")
  hit <- unique(hrefs[grepl(pattern, hrefs, ignore.case = TRUE)])
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

## ---------------------------------------------------------------------------
## scrape one team's roster for a given year
## ---------------------------------------------------------------------------
scrape_roster <- function(slug, sport, year) {
  roster_url <- discover_roster_url(slug, sport)
  if (is.na(roster_url)) {
    stop("could not discover roster URL for ", slug)
  }

  page <- fetch_page(paste0(roster_url, "?year=", year))
  if (is.null(page)) stop("could not fetch roster page for ", slug)

  tbls <- page %>% html_table()
  if (length(tbls) < 2) stop("expected 2 roster tables for ", slug,
                             ", got ", length(tbls))

  names_tbl <- as.data.frame(tbls[[1]])  # sticky column: Name
  stats_tbl <- as.data.frame(tbls[[2]])  # Jersey | POS | Height | Weight | ...
  if (nrow(names_tbl) != nrow(stats_tbl)) {
    stop("name/stat row mismatch for ", slug, " (",
         nrow(names_tbl), " vs ", nrow(stats_tbl), ")")
  }

  roster <- cbind(names_tbl, stats_tbl)
  colnames(roster) <- str_to_title(colnames(roster))   # Pos, Height, Weight...
  roster %>%
    rename(Position = Pos, Class = Yr, HighSchool = `High School`) %>%
    mutate(
      Weight    = suppressWarnings(as.numeric(Weight)),
      Age       = suppressWarnings(as.numeric(Age)),
      Rating    = suppressWarnings(as.numeric(Rating)),
      School    = slug,
      TeamName  = team_label(slug),
      Sport     = tolower(sport),
      RosterYear = year,
      ScrapedAt = format(Sys.Date())
    )
}

## ---------------------------------------------------------------------------
## main: loop all Big 12 teams
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

## optional scope filters (default: every TEAM_CONFIG slug -- a no-op unless
## passed). --conference <name> keeps that conference's teams; --slugs <csv>
## keeps an explicit comma-separated slug list. Both accept "--flag value" or
## "--flag=value". These let the Phase-1 per-conference backfill target one
## league; the per-team replace below is keyed on the scraped set, so unscoped
## teams keep their existing rows.
flag_value <- function(flag, a) {
  eq <- grep(paste0("^", flag, "="), a, value = TRUE)
  if (length(eq)) return(sub(paste0("^", flag, "="), "", eq[1]))
  i <- match(flag, a)
  if (!is.na(i) && i < length(a)) return(a[i + 1])
  NULL
}
drop_flag <- function(flag, a) {
  a <- a[!grepl(paste0("^", flag, "="), a)]
  i <- match(flag, a)
  if (!is.na(i)) a <- a[-c(i, if (i < length(a)) i + 1)]
  a
}
conf_filter  <- flag_value("--conference", args)
slugs_filter <- flag_value("--slugs", args)
args <- drop_flag("--conference", args)
args <- drop_flag("--slugs", args)

sport <- if (length(args) >= 1) tolower(args[1]) else "football"
year  <- if (length(args) >= 2) as.integer(args[2]) else
  as.integer(format(Sys.Date(), "%Y"))

target_slugs <- TEAM_CONFIG$slug
if (!is.null(conf_filter)) {
  target_slugs <- intersect(target_slugs, conf_slugs(conf_filter))
}
if (!is.null(slugs_filter)) {
  target_slugs <- intersect(target_slugs,
                            trimws(strsplit(slugs_filter, ",")[[1]]))
}
if (length(target_slugs) == 0) {
  stop("no TEAM_CONFIG slugs match the --conference/--slugs filter -- nothing ",
       "to scrape (conference='", conf_filter %||% "", "', slugs='",
       slugs_filter %||% "", "')")
}

scope_note <- if (length(target_slugs) < nrow(TEAM_CONFIG))
  paste0("[scoped to ", length(target_slugs), " of ",
         nrow(TEAM_CONFIG), " teams] ") else ""
cat("Scraping ", sport, " rosters for ", year, " ", scope_note, "...\n\n", sep = "")

## a parse that returns implausibly few rows is a stub/header-only page, not
## a roster -- counting it as success would wipe the team's existing rows
min_roster_rows <- if (sport == "basketball") 5 else 30

all_rosters <- list()
failures <- character(0)

for (slug in target_slugs) {
  result <- NULL
  for (attempt in 1:3) {   # transient fetch failures get retried
    result <- tryCatch(scrape_roster(slug, sport, year), error = function(e) {
      cat(sprintf("  %-16s attempt %d failed: %s\n", slug, attempt,
                  conditionMessage(e)))
      NULL
    })
    if (!is.null(result)) break
    Sys.sleep(5 * attempt)
  }
  if (is.null(result)) {
    cat(sprintf("  %-16s GAVE UP after 3 attempts\n", slug))
    failures <- c(failures, slug)
  } else if (nrow(result) < min_roster_rows) {
    cat(sprintf("  %-16s scraped EMPTY (%d rows < %d floor) -- existing rows kept\n",
                slug, nrow(result), min_roster_rows))
    failures <- c(failures, slug)
  } else {
    cat(sprintf("  %-16s %3d players\n", slug, nrow(result)))
    all_rosters[[slug]] <- result
  }

  ## be polite between teams (each team = 2 requests)
  Sys.sleep(runif(1, 2, 4))
}

roster_data <- bind_rows(all_rosters)
cat("\nTotal players scraped:", nrow(roster_data),
    "across", length(all_rosters), "teams\n")
if (length(failures) > 0) {
  cat("FAILED teams (re-run later):", paste(failures, collapse = ", "), "\n")
}

## minimum-success gate: a mostly-failed run must not touch the db at all.
## The floor scales with the SCRAPED set (75% of it, Power-4 ready): the full
## 16 teams need >= 12 exactly as before; a --conference/--slugs run needs 75%
## of its own target set rather than a fixed 12.
n_teams <- length(target_slugs)
floor_ok <- ceiling(0.75 * n_teams)
if (length(all_rosters) < floor_ok) {
  stop("only ", length(all_rosters), "/", n_teams, " teams scraped ",
       "successfully (need >= ", floor_ok, ") -- nothing written. Failed: ",
       paste(failures, collapse = ", "))
}

if (nrow(roster_data) > 0) {
  ## CSV backup of the fresh scrape (backups/ is gitignored)
  dir.create(here::here("backups"), showWarnings = FALSE)
  csv_path <- here::here("backups", paste0("rosters_", sport, "_", year, "_",
                                           format(Sys.Date()), ".csv"))
  write_csv(roster_data, csv_path)
  cat("CSV backup:", csv_path, "\n")

  ## write to the app database (new table; recruit tables untouched)
  tbl <- paste0("roster_", sport)
  conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
  if (tbl %in% dbListTables(conn)) {
    ## back up the OLD table before any write touches it
    old <- dbGetQuery(conn, paste0("SELECT * FROM ", tbl))
    write_csv(old, here::here("backups", paste0(
      tbl, "_pre_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")))
    cat("Old-table backup:", nrow(old), "rows\n")

    ## align fresh columns to the existing table schema exactly
    missing_cols <- setdiff(names(old), names(roster_data))
    for (mc in missing_cols) roster_data[[mc]] <- NA
    roster_data <- roster_data[, names(old)]

    ## replace rows ONLY for teams that scraped successfully -- a fetch
    ## failure must never delete a team's existing roster
    ok_slugs <- names(all_rosters)
    slug_list <- paste0("'", ok_slugs, "'", collapse = ", ")
    where <- paste0("School IN (", slug_list, ")")
    if ("RosterYear" %in% names(old)) {
      where <- paste0(where, " AND RosterYear = ", year)
    }
    ## atomic: a crash between delete and append must not lose the old rows
    dbWithTransaction(conn, {
      dbExecute(conn, paste0("DELETE FROM ", tbl, " WHERE ", where))
      dbAppendTable(conn, tbl, roster_data)
    })
    cat("Database table updated (per-team replace): ", tbl, "\n", sep = "")
  } else {
    dbWriteTable(conn, tbl, roster_data)
    cat("Database table created: ", tbl, "\n", sep = "")
  }
  dbDisconnect(conn)
}
