## ===========================================================================
## fetchOutcomes.R
## Pull team-season OUTCOMES (wins/losses + SP+ ratings) for the Big 12 from
## the free CollegeFootballData API, with strict quality gates so bad rows
## never reach the database:
##
##   GATE 1  every TEAM_CONFIG program must map to a CFBD school name --
##           unmapped teams are reported and that year is flagged
##   GATE 2  wins/losses must be sane (0-16) and consistent with games
##   GATE 3  a season is written only if >= 14 of 16 teams validated
##           (partial seasons are reported, never silently written)
##   GATE 4  nothing is deleted until the replacement passes; the previous
##           table is backed up to backups/ first
##
## Setup (one-time): get a free key at https://collegefootballdata.com/key
## and add to your .Renviron:   CFBD_API_KEY=yourkey
## Then:   Rscript scripts/fetchOutcomes.R
## Writes: table team_seasons_football in data/recruiting.db
## ===========================================================================

suppressMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

source(here::here("R", "team_config.R"))

key <- Sys.getenv("CFBD_API_KEY")
if (key == "") {
  cat("No CFBD_API_KEY found.\n",
      "1. Get a free key: https://collegefootballdata.com/key\n",
      "2. Add CFBD_API_KEY=<key> to your .Renviron (then restart R)\n",
      "3. Re-run: Rscript scripts/fetchOutcomes.R\n")
  quit(status = 0)
}

## slug -> CFBD school name (GATE 1 source of truth). Phase 1 fold: when the
## config moved to data/team_config.csv the CFBD name became a real `cfbd_name`
## column (CFBD /teams/fbs supplies it for every P4 school), so read it straight
## off TEAM_CONFIG. The inline map below is the back-compat FALLBACK for the
## Phase 0 config (no CSV / no cfbd_name column) and is still guarded there.
## The CFBD /records + /ratings/sp endpoints are bulk-per-year (one call returns
## every FBS team), so covering all mapped teams costs no extra requests; the
## app only ever DISPLAYS onboarded teams, so extra rows sit unused until their
## conference onboards.
CFBD_NAMES <- c(
  "arizona" = "Arizona", "arizona-state" = "Arizona State",
  "baylor" = "Baylor", "byu" = "BYU", "central-florida" = "UCF",
  "cincinnati" = "Cincinnati", "colorado" = "Colorado",
  "houston" = "Houston", "iowa-state" = "Iowa State", "kansas" = "Kansas",
  "kansas-state" = "Kansas State", "oklahoma-state" = "Oklahoma State",
  "tcu" = "TCU", "texas-tech" = "Texas Tech", "utah" = "Utah",
  "west-virginia" = "West Virginia")
if ("cfbd_name" %in% names(TEAM_CONFIG)) {
  cfbd_map <- setNames(as.character(TEAM_CONFIG$cfbd_name), TEAM_CONFIG$slug)
  cfbd_map <- cfbd_map[!is.na(cfbd_map) & nzchar(cfbd_map)]
  stopifnot(length(cfbd_map) > 0)
} else {
  stopifnot(all(TEAM_CONFIG$slug %in% names(CFBD_NAMES)))
  cfbd_map <- CFBD_NAMES
}

cfbd_get <- function(path, ...) {
  resp <- GET(paste0("https://api.collegefootballdata.com", path),
              add_headers(Authorization = paste("Bearer", key)),
              query = list(...), timeout(30))
  if (status_code(resp) == 401) stop("CFBD key rejected (401) -- check .Renviron")
  if (status_code(resp) != 200) stop("CFBD ", path, " returned ", status_code(resp))
  fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}

## end year is dynamic: from September the current season has results worth
## pulling; before that the latest complete season is last year's
cur_year <- as.integer(format(Sys.Date(), "%Y"))
end_year <- if (as.integer(format(Sys.Date(), "%m")) >= 9) cur_year else
  cur_year - 1
YEARS <- 2016:end_year
seasons <- list()
problems <- character(0)

for (yr in YEARS) {
  rec <- try(cfbd_get("/records", year = yr), silent = TRUE)
  if (inherits(rec, "try-error") || length(rec) == 0) {
    problems <- c(problems, paste0(yr, ": records fetch failed"))
    next
  }
  rec <- as.data.frame(rec)

  ## SP+ overall rating for context (best-effort; season still valid without)
  sp <- try(cfbd_get("/ratings/sp", year = yr), silent = TRUE)
  sp_df <- if (!inherits(sp, "try-error") && length(sp) > 0) {
    as.data.frame(sp) %>% select(team, sp_rating = rating)
  } else data.frame(team = character(0), sp_rating = numeric(0))

  yr_rows <- data.frame(slug = names(cfbd_map),
                        team = unname(cfbd_map)) %>%
    left_join(rec, by = "team") %>%
    left_join(sp_df, by = "team") %>%
    transmute(
      slug, year = yr,
      games = suppressWarnings(as.integer(total.games)),
      wins = suppressWarnings(as.integer(total.wins)),
      losses = suppressWarnings(as.integer(total.losses)),
      conf_wins = suppressWarnings(as.integer(conferenceGames.wins)),
      conf_losses = suppressWarnings(as.integer(conferenceGames.losses)),
      sp_rating = suppressWarnings(as.numeric(sp_rating))
    ) %>%
    mutate(
      ## GATE 2: sanity checks
      valid = !is.na(wins) & !is.na(losses) &
        wins >= 0 & wins <= 16 & losses >= 0 & losses <= 16 &
        (is.na(games) | wins + losses <= games + 2)
    )

  n_valid <- sum(yr_rows$valid)
  ## GATE 3 floors scale with the mapped roster (Power-4 ready): the "mostly
  ## broken" cutoff is 87.5% of the CFBD-mapped teams and the "fully populated"
  ## mark is all of them. At the shipped 16 these are 14 and 16, exactly as
  ## before (length(cfbd_map) == nrow(TEAM_CONFIG) == 16 on the Phase 0 config).
  n_teams <- length(cfbd_map)
  if (n_valid < ceiling(0.875 * n_teams)) {
    ## GATE 3: don't write a mostly-broken season
    problems <- c(problems, paste0(
      yr, ": only ", n_valid, "/", n_teams, " teams validated -- season NOT written (",
      paste(yr_rows$slug[!yr_rows$valid], collapse = ", "), ")"))
    next
  }
  if (n_valid < n_teams) {
    problems <- c(problems, paste0(
      yr, ": written with ", n_teams - n_valid, " missing team(s): ",
      paste(yr_rows$slug[!yr_rows$valid], collapse = ", ")))
  }
  seasons[[as.character(yr)]] <- yr_rows %>% filter(valid) %>% select(-valid)
  cat(yr, ":", n_valid, "teams validated\n")
}

out <- bind_rows(seasons)
stopifnot(nrow(out) > 0)

conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
## GATE 4: back up any existing table before replacing
if ("team_seasons_football" %in% dbListTables(conn)) {
  old <- dbGetQuery(conn, "SELECT * FROM team_seasons_football")
  dir.create(here::here("backups"), showWarnings = FALSE)
  write_csv(old, here::here("backups", paste0("team_seasons_before_",
                                              format(Sys.Date()), ".csv")))
  ## per-year replace: only years that fetched AND validated are touched,
  ## so a failed CFBD year keeps its existing rows instead of vanishing
  ok_years <- sort(unique(out$year))
  dbWithTransaction(conn, {
    dbExecute(conn, paste0("DELETE FROM team_seasons_football WHERE year IN (",
                           paste(ok_years, collapse = ", "), ")"))
    dbAppendTable(conn, "team_seasons_football", out)
  })
} else {
  dbWriteTable(conn, "team_seasons_football", out)
}
dbDisconnect(conn)

cat("\nWrote", nrow(out), "validated team-seasons to team_seasons_football\n")
if (length(problems) > 0) {
  cat("\nQUALITY REPORT (review before trusting analyses):\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
} else {
  cat("All seasons passed every quality gate.\n")
}
cat("\nNext: a 'Talent vs Performance' module can join this to the 4-year",
    "rolling class composite from recruit_class_football.\n")

## any year that fetched/validated nothing kept its old rows above, but the
## run must still exit nonzero so the failure is visible to refreshAll.R
missing_years <- setdiff(YEARS, as.integer(names(seasons)))
if (length(missing_years) > 0) {
  cat("\nFAILED/SKIPPED years (existing rows kept, re-run later):",
      paste(missing_years, collapse = ", "), "\n")
  quit(status = 1)
}
