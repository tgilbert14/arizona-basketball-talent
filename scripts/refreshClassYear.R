## ===========================================================================
## refreshClassYear.R
## Re-scrape ONE recruiting class year for all Big 12 teams and replace those
## rows in data/recruiting.db. Fixes two staleness problems with the original
## January-2026 snapshot:
##   * ratings get re-ranked through the cycle (and were stored as rounded
##     integers -- borderline 89.x players looked like 90+ "blue chips")
##   * late/spring commits were missing entirely (e.g. 2026 basketball)
## Also captures PORTAL TRANSFERS (Type = "Transfer") from the same pages.
##
## Geocoded columns (lat/long etc.) are carried over for players already in
## the db; brand-new players get NA geo until the geocoding pipeline runs,
## so they appear in size/rating analyses immediately but not on the map.
##
## Run from the project root:
##   Rscript scripts/refreshClassYear.R football 2026
##   Rscript scripts/refreshClassYear.R basketball 2026
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

UA <- user_agent(paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"))

fetch_page <- function(url) {
  resp <- try(GET(url, UA, timeout(25)), silent = TRUE)
  if (inherits(resp, "try-error") || status_code(resp) != 200) return(NULL)
  read_html(content(resp, "text"))
}

## ratings appear either as 0-1 composites ("0.9213") or 0-100 -- normalize
norm_rating <- function(x) {
  v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
  ifelse(!is.na(v) & v <= 1.5, round(v * 100, 2), round(v, 2))
}

## ---------------------------------------------------------------------------
## scrape the commits page for one school/year/sport
## (same node selectors as the original updatingSQLdatabase.R pipeline)
## ---------------------------------------------------------------------------
scrape_class <- function(slug, sport, year) {
  url <- paste0("https://247sports.com/college/", slug, "/season/", year,
                "-", sport, "/commits/")
  page <- fetch_page(url)
  if (is.null(page)) stop("page fetch failed")

  scores <- page %>%
    html_nodes(paste0(".ri-page__star-and-score .score , .ri-page__name-link , ",
                      ".wrapper .position , .wrapper .metrics , .posrank , ",
                      ".withDate , .meta , .sttrank , .natrank")) %>%
    html_text(trim = TRUE)

  pr <- scores[scores != "Rating"]
  pr <- pr[!grepl("^Commit", pr)]

  commits <- NULL
  if (length(pr) >= 8) {
    name_vec <- pr[seq(1, length(pr), by = 8)]
    loc_vec <- pr[seq(2, length(pr), by = 8)]
    meta_vec <- pr[seq(3, length(pr), by = 8)]
    rank_vec <- pr[seq(4, length(pr), by = 8)]
    nat_vec <- pr[seq(5, length(pr), by = 8)]
    pos_rank_vec <- pr[seq(6, length(pr), by = 8)]
    st_rank_vec <- pr[seq(7, length(pr), by = 8)]
    pos_vec <- pr[seq(8, length(pr), by = 8)]

    n <- length(name_vec)
    pad <- function(v) { length(v) <- n; v }

    meta_clean <- gsub(" ", "", pad(meta_vec))
    hw <- str_split_fixed(meta_clean, "/", 2)
    loc_clean <- gsub(" ", "", pad(loc_vec))
    state <- gsub("\\)", "", str_split_fixed(loc_clean, ",", 2)[, 2])

    commits <- data.frame(
      Name = name_vec,
      Location = pad(loc_vec),
      Height = hw[, 1],
      Weight = suppressWarnings(as.numeric(hw[, 2])),
      Ranking = norm_rating(pad(rank_vec)),
      NationalRank = suppressWarnings(as.numeric(pad(nat_vec))),
      PositionRank = suppressWarnings(as.numeric(pad(pos_rank_vec))),
      StateRank = suppressWarnings(as.numeric(pad(st_rank_vec))),
      State = state,
      Position = pad(pos_vec),
      Type = "Commit",
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(Ranking))
  }

  ## portal transfers on the same page (ratings marked "(T)")
  tmeta <- page %>%
    html_nodes(".player .score , .portal-list_itm .position , .player .metrics , .player a") %>%
    html_text(trim = TRUE)
  t_idx <- which(str_detect(tmeta, "\\(T\\)"))
  transfers <- NULL
  if (length(t_idx) > 0) {
    rows <- lapply(t_idx, function(i) {
      size <- gsub(" ", "", tmeta[i - 1])
      hw <- str_split_fixed(size, "/", 2)
      data.frame(
        Name = tmeta[i - 2],
        Location = NA_character_,
        Height = hw[, 1],
        Weight = suppressWarnings(as.numeric(hw[, 2])),
        Ranking = norm_rating(gsub(" \\(.*\\)", "", tmeta[i])),
        NationalRank = NA_real_, PositionRank = NA_real_,
        StateRank = NA_real_, State = NA_character_,
        Position = tmeta[i + 2],
        Type = "Transfer",
        stringsAsFactors = FALSE)
    })
    transfers <- bind_rows(rows) %>% filter(!is.na(Ranking))
  }

  out <- bind_rows(commits, transfers)
  if (nrow(out) == 0) return(out)
  out %>%
    mutate(Year = year, School = slug) %>%
    distinct(Name, Type, .keep_all = TRUE)
}

## ---------------------------------------------------------------------------
## main
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sport <- if (length(args) >= 1) tolower(args[1]) else "football"
year <- if (length(args) >= 2) as.integer(args[2]) else 2026
tbl <- paste0("recruit_class_", sport)

cat("Refreshing", sport, year, "classes...\n\n")

fresh <- list()
ok_slugs <- character(0)   # only these schools' old rows get replaced
for (slug in TEAM_CONFIG$slug) {
  res <- NULL
  for (attempt in 1:3) {   # transient fetch failures get retried
    res <- tryCatch(scrape_class(slug, sport, year), error = function(e) {
      cat(sprintf("  %-16s attempt %d failed: %s\n", slug, attempt,
                  conditionMessage(e)))
      NULL
    })
    if (!is.null(res)) break
    Sys.sleep(5 * attempt)
  }
  if (is.null(res)) {
    cat(sprintf("  %-16s GAVE UP after 3 attempts -- existing rows kept\n",
                slug))
  } else {
    cat(sprintf("  %-16s %3d commits, %2d transfers\n", slug,
                sum(res$Type == "Commit"), sum(res$Type == "Transfer")))
    ## a successful scrape with zero players is suspicious for a past year:
    ## keep the existing rows rather than wiping them with nothing
    if (nrow(res) > 0) {
      fresh[[slug]] <- res
      ok_slugs <- c(ok_slugs, slug)
    } else {
      cat(sprintf("  %-16s scraped EMPTY -- existing rows kept\n", slug))
    }
  }
  Sys.sleep(runif(1, 2, 4))
}
fresh <- bind_rows(fresh)
cat("\nScraped", nrow(fresh), "players across", length(ok_slugs), "schools\n")
stopifnot(nrow(fresh) > 0)

conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
old <- dbGetQuery(conn, paste0("SELECT * FROM ", tbl))

## backup before replacing anything (timestamped so repeated runs on the
## same day never overwrite each other's snapshots)
dir.create(here::here("backups"), showWarnings = FALSE)
write_csv(old, here::here("backups", paste0(
  tbl, "_before_refresh_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")))

## carry geo + bookkeeping columns over from the existing rows.
## join on a NORMALIZED name key -- display names drift between scrapes
## (e.g. "RJ Mosley" vs "R.J. Mosley") and an exact join would silently
## drop their map coordinates
name_key <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))
geo_cols <- c("count", "loc_inside_parens", "location_name", "Location_Clean",
              "lat", "long", "School_Clean", "School_City",
              "college_lat", "college_long")
carry <- old %>%
  filter(Year == year) %>%
  mutate(.key = name_key(Name)) %>%
  select(.key, School, all_of(geo_cols)) %>%
  distinct(.key, School, .keep_all = TRUE)

## campus coords are constant per school -- fill for brand-new players
campus <- old %>%
  group_by(School) %>%
  summarize(c_lat = first(college_lat[!is.na(college_lat)]),
            c_long = first(college_long[!is.na(college_long)]),
            c_clean = first(School_Clean[!is.na(School_Clean)]),
            c_city = first(School_City[!is.na(School_City)]),
            .groups = "drop")

fresh_full <- fresh %>%
  mutate(sport = sport, .key = name_key(Name)) %>%
  left_join(carry, by = c(".key", "School")) %>%
  select(-.key) %>%
  left_join(campus, by = "School") %>%
  mutate(college_lat = ifelse(is.na(college_lat), c_lat, college_lat),
         college_long = ifelse(is.na(college_long), c_long, college_long),
         School_Clean = ifelse(is.na(School_Clean), c_clean, School_Clean),
         School_City = ifelse(is.na(School_City), c_city, School_City)) %>%
  select(-c_lat, -c_long, -c_clean, -c_city)

## align to the existing table schema exactly
missing_cols <- setdiff(names(old), names(fresh_full))
for (mc in missing_cols) fresh_full[[mc]] <- NA
fresh_full <- fresh_full[, names(old)]

n_new <- nrow(fresh_full)
n_geo <- sum(!is.na(fresh_full$lat))
## replace rows ONLY for schools that scraped successfully -- a fetch
## failure must never delete a school's existing data
slug_list <- paste0("'", ok_slugs, "'", collapse = ", ")
dbExecute(conn, paste0("DELETE FROM ", tbl, " WHERE Year = ", year,
                       " AND School IN (", slug_list, ")"))
dbWriteTable(conn, tbl, fresh_full, append = TRUE)
dbDisconnect(conn)

cat("Replaced", year, "rows in", tbl, ":", n_new, "players (",
    n_geo, "with carried-over map coordinates )\n")
cat("New players without coordinates appear in size/rating analyses but not",
    "on the map until geocoded.\n")
