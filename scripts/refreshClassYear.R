## ===========================================================================
## refreshClassYear.R
## Re-scrape ONE recruiting class year for all Big 12 teams and replace those
## rows in data/recruiting.db. Fixes two staleness problems with the original
## January-2026 snapshot:
##   * ratings get re-ranked through the cycle (and were stored as rounded
##     integers -- borderline 89.x players looked like 90+ "blue chips")
##   * late/spring commits were missing entirely (e.g. 2026 basketball)
## Also captures PORTAL TRANSFERS (Type = "Transfer") from the same pages,
## and each player's 247 profile href (ProfileUrl) so backfillProfiles.R can
## later fill missing hometowns from the profile pages.
##
## Geocoded columns (lat/long etc.) are carried over for players already in
## the db; brand-new players get NA geo until the geocoding pipeline runs,
## so they appear in size/rating analyses immediately but not on the map.
##
## Run from the project root:
##   Rscript scripts/refreshClassYear.R football 2026
##   Rscript scripts/refreshClassYear.R basketball 2026
##   Rscript scripts/refreshClassYear.R football 2027 --allow-empty
##
## Optional scope filters (default = every configured team, so a no-op unless
## passed) let a per-conference backfill target one league:
##   Rscript scripts/refreshClassYear.R football 2026 --conference "Big 12"
##   Rscript scripts/refreshClassYear.R football 2026 --slugs arizona,utah
##
## --allow-empty: a totally empty scrape exits 0 without touching the db
## (no backup CSV, no delete, no write). This is how the nightly pipeline
## probes MAX(Year)+1 before 247 opens the next cycle's pages; without the
## flag an all-empty scrape is still a hard stop (a past year scraping to
## zero rows means the source or selectors broke, never "no data yet").
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

## normalized name key -- display names drift between scrapes
## ("RJ Mosley" vs "R.J. Mosley"); used for the geo carry-forward join and
## for pairing profile hrefs to parsed rows
name_key <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))

## 247 hrefs arrive in two shapes: commits' name links are PROTOCOL-RELATIVE
## ("//247sports.com/Player/<slug>-<id>/"), transfer anchors are absolute
## college-scoped URLs. Normalize both to absolute https. Never prefix the
## host onto a protocol-relative href -- the double host 404s.
## Quotes, angle brackets, and spaces never appear in a clean profile URL;
## an href carrying one is scraped-markup breakage (or an attribute-breakout
## injection attempt) and is rejected outright.
abs_247 <- function(href) {
  bad <- grepl('["\'<> ]', href)
  ifelse(is.na(href) | !nzchar(href) | bad, NA_character_,
  ifelse(grepl("^//", href), paste0("https:", href),
  ifelse(grepl("^https?://", href), href,
  ifelse(grepl("^/", href), paste0("https://247sports.com", href),
         NA_character_))))
}
## Return TRUE only when a successful 247 page exposes an explicit empty-state
## message. A selector/parser miss remains "unclassified", never a verified
## empty class, and rows are retained in both cases.
explicit_empty_commit_page <- function(page) {
  nodes <- html_nodes(page, paste(
    c(".ri-page__empty", ".ri-page__no-results", ".no-results",
      ".no-results-message", "[class*='empty']", "[class*='no-result']"),
    collapse = ", "))
  if (!length(nodes)) return(FALSE)
  text <- tolower(gsub("\\s+", " ", paste(html_text(nodes, trim = TRUE),
                                             collapse = " ")))
  grepl(
    "(there (is|are) no (commitments?|commits|prospects?|players?)|no (commitments?|commits|prospects?|players?)( yet| found| available| for this class)?)",
    text, perl = TRUE
  )
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

  ## profile hrefs: text + href pulled from the SAME name-link node list, so
  ## the name-to-url pairing can never drift. (The stride-8 text vector above
  ## is filtered before pairing -- indexing hrefs into it would misalign.)
  link_nodes <- html_nodes(page, ".ri-page__name-link")
  link_map <- data.frame(
    .pkey = name_key(html_text(link_nodes, trim = TRUE)),
    url = abs_247(html_attr(link_nodes, "href")),
    stringsAsFactors = FALSE)
  link_map <- link_map[nzchar(link_map$.pkey) & !is.na(link_map$url), ,
                       drop = FALSE]
  ## two players sharing a name key is ambiguous -- keeping the FIRST url
  ## would attach the wrong player's profile (and later the wrong hometown),
  ## so drop BOTH and let the row fall back to NA / the search URL
  link_dup <- duplicated(link_map$.pkey) |
    duplicated(link_map$.pkey, fromLast = TRUE)
  link_map <- link_map[!link_dup, , drop = FALSE]

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
      filter(!is.na(Ranking)) %>%
      mutate(ProfileUrl = link_map$url[match(name_key(Name), link_map$.pkey)])
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
    ## transfer profile hrefs live on the same portal list; the anchors
    ## interleave empty-text scholarshipdistribution links, so keep only
    ## anchors with visible text AND a /player/ href
    t_nodes <- html_nodes(page, ".portal-list_itm a")
    t_href <- html_attr(t_nodes, "href")
    t_text <- html_text(t_nodes, trim = TRUE)
    t_keep <- !is.na(t_href) & grepl("/player/", t_href, ignore.case = TRUE) &
      nzchar(t_text)
    t_map <- data.frame(.pkey = name_key(t_text[t_keep]),
                        url = abs_247(t_href[t_keep]),
                        stringsAsFactors = FALSE)
    ## same ambiguity rule as link_map: drop ALL duplicated keys, never
    ## guess which of two same-named players owns the href
    t_dup <- duplicated(t_map$.pkey) | duplicated(t_map$.pkey, fromLast = TRUE)
    t_map <- t_map[!t_dup, , drop = FALSE]
    transfers <- bind_rows(rows) %>%
      filter(!is.na(Ranking)) %>%
      mutate(ProfileUrl = t_map$url[match(name_key(Name), t_map$.pkey)])
  }

  out <- bind_rows(commits, transfers)
  if (nrow(out) == 0) {
    attr(out, "empty_state") <- if (explicit_empty_commit_page(page))
      "verified" else "unclassified"
    return(out)
  }
  out %>%
    mutate(Year = year, School = slug) %>%
    distinct(Name, Type, .keep_all = TRUE)
}

## ---------------------------------------------------------------------------
## plausibility gate: if 247 shifts the stride-8 page layout, fields land in
## the wrong columns and parse to garbage. A row FAILS when any NON-NA field
## is implausible (NA never fails -- transfers carry NA by design). A school
## is demoted when >30% of its rows fail AND at least 2 rows fail (small
## basketball classes sit at n=6, where one genuinely odd row is 16.7% --
## a single bad row must never demote a school), or when it parsed rows but
## every Ranking is NA. Returns list(ok, reason).
## ---------------------------------------------------------------------------
validate_class <- function(df, sport) {
  bad_height <- !is.na(df$Height) &
    !grepl("^[4-7]-(0|1)?[0-9](\\.[0-9]{1,2})?$", df$Height)
  wt_max <- if (sport == "basketball") 320 else 420
  bad_weight <- !is.na(df$Weight) & (df$Weight < 130 | df$Weight > wt_max)
  bad_rank <- !is.na(df$Ranking) & (df$Ranking < 55 | df$Ranking > 110)
  row_fail <- bad_height | bad_weight | bad_rank

  if (mean(row_fail) > 0.30 && sum(row_fail) >= 2) {
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
allow_empty <- "--allow-empty" %in% args
args <- args[args != "--allow-empty"]

## optional scope filters (default: every TEAM_CONFIG slug -- a no-op unless
## passed). --conference <name> keeps that conference's teams; --slugs <csv>
## keeps an explicit comma-separated slug list. Both accept "--flag value" or
## "--flag=value". These let the Phase-1 per-conference backfill target one
## league without changing the default all-teams behavior. The per-school
## replace below is keyed on the scraped set, so unscoped schools are untouched.
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
year <- if (length(args) >= 2) as.integer(args[2]) else 2026
tbl <- paste0("recruit_class_", sport)

## hard ceiling: the app tracks at most ONE cycle ahead of the calendar
## (class of N signs Dec N-1). 247 lists early commits two cycles out, so
## without this guard an uncapped caller keeps rolling the db forward
## (2028 rows landed in July 2026). Refuse rather than write.
cycle_cap <- as.integer(format(Sys.Date(), "%Y")) + 1L
if (!is.na(year) && year > cycle_cap) {
  cat("refreshClassYear: ", year, " is beyond the calendar+1 ceiling (",
      cycle_cap, ") -- refusing to scrape a cycle more than one year out\n",
      sep = "")
  quit(save = "no", status = 1)
}

## default (nightly): only ONBOARDED teams -- the nightly never scrapes a
## hidden program. A --conference/--slugs flag OVERRIDES to an explicit set that
## MAY include not-yet-onboarded teams: that is how the per-conference backfill
## (backfillConference.R) reaches a league BEFORE it is flipped onboarded. With
## a flag the base set is the FULL config; without one it is onboarded_slugs()
## (= the shipped 16 at Phase 1, so the nightly is byte-identical).
if (!is.null(conf_filter) || !is.null(slugs_filter)) {
  target_slugs <- TEAM_CONFIG$slug
  if (!is.null(conf_filter)) {
    target_slugs <- intersect(target_slugs, conf_slugs(conf_filter))
  }
  if (!is.null(slugs_filter)) {
    target_slugs <- intersect(target_slugs,
                              trimws(strsplit(slugs_filter, ",")[[1]]))
  }
} else {
  target_slugs <- onboarded_slugs()
}
if (length(target_slugs) == 0) {
  stop("no TEAM_CONFIG slugs match the --conference/--slugs filter -- nothing ",
       "to scrape (conference='", conf_filter %||% "", "', slugs='",
       slugs_filter %||% "", "')")
}

## note only when a --conference/--slugs flag scoped the run (backfill/manual);
## the unflagged nightly runs the full onboarded universe and needs no note.
scope_note <- if (!is.null(conf_filter) || !is.null(slugs_filter))
  paste0(" [scoped to ", length(target_slugs), " team(s)]") else ""
cat("Refreshing ", sport, " ", year, " classes", scope_note, "...\n\n", sep = "")

fresh <- list()
ok_slugs <- character(0)   # only these schools' old rows get replaced
failed_slugs <- character(0)
verified_empty_slugs <- character(0)
unclassified_empty_slugs <- character(0)
for (slug in target_slugs) {
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
      empty_state <- attr(res, "empty_state")
      if (identical(empty_state, "verified")) {
        cat(sprintf("  %-16s VERIFIED EMPTY (explicit 247 empty-state; existing rows kept)\n",
                    slug))
        verified_empty_slugs <- c(verified_empty_slugs, slug)
      } else {
        cat(sprintf("  %-16s EMPTY RESPONSE (no explicit 247 empty marker; existing rows kept)\n",
                    slug))
        unclassified_empty_slugs <- c(unclassified_empty_slugs, slug)
      }
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
if (length(unclassified_empty_slugs) > 0) {
  cat("UNCLASSIFIED empty pages (rows kept; re-check source/selector):",
      paste(unclassified_empty_slugs, collapse = ", "), "\n")
}
if (length(verified_empty_slugs) > 0) {
  cat("VERIFIED empty pages (explicit 247 message; rows kept):",
      paste(verified_empty_slugs, collapse = ", "), "\n")
}
## the ahead-year probe: before 247 opens a cycle's pages, every school
## scrapes to nothing -- that is expected, not a failure. Exit here, BEFORE
## the db connection opens and BEFORE the backup CSV is written (a backup of
## a run that changed nothing would only mislead a later restore).
if (allow_empty && nrow(fresh) == 0) {
  cat("no rows yet for ", sport, " ", year,
      " -- nothing to write (allow-empty)\n", sep = "")
  quit(save = "no", status = 0)
}
stopifnot(nrow(fresh) > 0)

conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))

## self-migrating: the schema-align projection below keeps ONLY columns the
## table already has, so ProfileUrl must exist in the db BEFORE names(old)
## is read -- otherwise every scraped href would be silently dropped
for (mig_tbl in c("recruit_class_football", "recruit_class_basketball")) {
  mig_cols <- dbGetQuery(conn, paste0("PRAGMA table_info(", mig_tbl, ")"))$name
  if (!"ProfileUrl" %in% mig_cols) {
    dbExecute(conn, paste0("ALTER TABLE ", mig_tbl,
                           " ADD COLUMN ProfileUrl TEXT"))
    cat("Schema migration:", mig_tbl, "gained ProfileUrl TEXT\n")
  }
  ## ScrapedAt is the recruit-table freshness stamp the rosters already carry.
  ## Same reasoning as ProfileUrl above: it must exist in the db BEFORE
  ## names(old) is read, or the schema-align projection would silently drop
  ## the stamp off every fresh row. Additive only; db_content_hash excludes
  ## ScrapedAt so restamping never makes an unchanged night look changed.
  if (!"ScrapedAt" %in% mig_cols) {
    dbExecute(conn, paste0("ALTER TABLE ", mig_tbl,
                           " ADD COLUMN ScrapedAt TEXT"))
    cat("Schema migration:", mig_tbl, "gained ScrapedAt TEXT\n")
  }
}

old <- dbGetQuery(conn, paste0("SELECT * FROM ", tbl))

## backup before replacing anything (timestamped so repeated runs on the
## same day never overwrite each other's snapshots)
dir.create(here::here("backups"), showWarnings = FALSE)
write_csv(old, here::here("backups", paste0(
  tbl, "_before_refresh_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")))

## carry geo + bookkeeping columns over from the existing rows.
## join on the NORMALIZED name key (name_key, defined up top) -- an exact
## join would silently drop map coordinates when display names drift.
## ProfileUrl rides along renamed so a page that stops serving a player's
## href (layout drift) keeps the previously captured URL
geo_cols <- c("count", "loc_inside_parens", "location_name", "Location_Clean",
              "lat", "long", "School_Clean", "School_City",
              "college_lat", "college_long")
carry <- old %>%
  filter(Year == year) %>%
  mutate(.key = name_key(Name)) %>%
  select(.key, School, all_of(geo_cols), .old_profile = ProfileUrl) %>%
  distinct(.key, School, .keep_all = TRUE)

## campus coords are constant per school -- fill for brand-new players
campus <- old %>%
  group_by(School) %>%
  summarize(c_lat = first(college_lat[!is.na(college_lat)]),
            c_long = first(college_long[!is.na(college_long)]),
            c_clean = first(School_Clean[!is.na(School_Clean)]),
            c_city = first(School_City[!is.na(School_City)]),
            .groups = "drop")

## defensive: a page whose layout hides every name link still binds cleanly
if (!"ProfileUrl" %in% names(fresh)) fresh$ProfileUrl <- NA_character_

fresh_full <- fresh %>%
  mutate(sport = sport, ScrapedAt = format(Sys.Date()), .key = name_key(Name)) %>%
  left_join(carry, by = c(".key", "School")) %>%
  mutate(ProfileUrl = ifelse(is.na(ProfileUrl), .old_profile, ProfileUrl)) %>%
  select(-.key, -.old_profile) %>%
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
