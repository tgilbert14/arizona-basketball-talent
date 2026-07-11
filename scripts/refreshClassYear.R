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
## plausibility gate: if 247 shifts the stride-8 page layout, fields land in
## the wrong columns and parse to garbage. A row FAILS when any NON-NA field
## is implausible (NA never fails -- transfers carry NA by design). A school
## is demoted when >30% of its rows fail, or when it parsed rows but every
## Ranking is NA. Returns list(ok, reason).
## ---------------------------------------------------------------------------
validate_class <- function(df, sport) {
  bad_height <- !is.na(df$Height) &
    !grepl("^[4-7]-(0|1)?[0-9](\\.[0-9]{1,2})?$", df$Height)
  wt_max <- if (sport == "basketball") 320 else 420
  bad_weight <- !is.na(df$Weight) & (df$Weight < 130 | df$Weight > wt_max)
  bad_rank <- !is.na(df$Ranking) & (df$Ranking < 55 | df$Ranking > 110)
  row_fail <- bad_height | bad_weight | bad_rank

  if (mean(row_fail) > 0.30) {
    return(list(ok = FALSE, reason = sprintf(
      "%d/%d rows implausible (height/weight/rank out of range)",
      sum(row_fail), nrow(df))))
  }
  if (nrow(df) > 0 && all(is.na(df$Ranking))) {
    return(list(ok = FALSE, reason = "all rows have NA Ranking"))
  }
  list(ok = TRUE, reason = "")
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
failed_slugs <- character(0)
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
    failed_slugs <- c(failed_slugs, slug)
  } else {
    cat(sprintf("  %-16s %3d commits, %2d transfers\n", slug,
                sum(res$Type == "Commit"), sum(res$Type == "Transfer")))
    ## a successful scrape with zero players is suspicious for a past year:
    ## keep the existing rows rather than wiping them with nothing
    if (nrow(res) > 0) {
      check <- validate_class(res, sport)
      if (check$ok) {
        fresh[[slug]] <- res
        ok_slugs <- c(ok_slugs, slug)
      } else {
        ## layout drift, not a fetch failure: keep the existing rows
        message(sprintf("  %-16s DEMOTED (%s) -- existing rows kept",
                        slug, check$reason))
        failed_slugs <- c(failed_slugs, slug)
      }
    } else {
      cat(sprintf("  %-16s scraped EMPTY -- existing rows kept\n", slug))
      failed_slugs <- c(failed_slugs, slug)
    }
  }
  Sys.sleep(runif(1, 2, 4))
}
fresh <- bind_rows(fresh)
cat("\nScraped", nrow(fresh), "players across", length(ok_slugs), "schools\n")
if (length(failed_slugs) > 0) {
  cat("FAILED schools (rows kept, re-run later):",
      paste(failed_slugs, collapse = ", "), "\n")
}
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
## failure must never delete a school's existing data.
## Transfer guard: a page whose portal section fails to parse still
## validates on its commits alone -- if the fresh scrape holds ZERO
## transfers for a school while the db holds >= 3, keep the old Transfer
## rows and replace only that school's commits
transfers_wiped <- function(s) {
  n_fresh <- sum(fresh_full$School == s & fresh_full$Type == "Transfer",
                 na.rm = TRUE)
  n_db <- sum(old$School == s & old$Year == year & old$Type == "Transfer",
              na.rm = TRUE)
  n_fresh == 0 && n_db >= 3
}
keep_transfer_slugs <- ok_slugs[vapply(ok_slugs, transfers_wiped, logical(1))]
for (s in keep_transfer_slugs) {
  cat(sprintf("  %-16s fresh scrape has 0 transfers but db holds %d --",
              s, sum(old$School == s & old$Year == year &
                       old$Type == "Transfer", na.rm = TRUE)),
      "keeping old Transfer rows (Commit-only replace)\n")
}
full_slugs <- setdiff(ok_slugs, keep_transfer_slugs)
in_list <- function(x) paste0("'", x, "'", collapse = ", ")
## atomic: a crash between delete and append must not lose the old rows
dbWithTransaction(conn, {
  if (length(full_slugs) > 0) {
    dbExecute(conn, paste0("DELETE FROM ", tbl, " WHERE Year = ", year,
                           " AND School IN (", in_list(full_slugs), ")"))
  }
  if (length(keep_transfer_slugs) > 0) {
    dbExecute(conn, paste0("DELETE FROM ", tbl, " WHERE Year = ", year,
                           " AND Type = 'Commit' AND School IN (",
                           in_list(keep_transfer_slugs), ")"))
  }
  dbWriteTable(conn, tbl, fresh_full, append = TRUE)
})
dbDisconnect(conn)

cat("Replaced", year, "rows in", tbl, ":", n_new, "players (",
    n_geo, "with carried-over map coordinates )\n")
cat("New players without coordinates appear in size/rating analyses but not",
    "on the map until geocoded.\n")
