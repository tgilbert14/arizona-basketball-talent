#!/usr/bin/env Rscript

## Fast, deterministic validation for the Talent Origins data contract.
## No Shiny server is started and no files are changed.

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(geosphere)
  library(DBI)
  library(RSQLite)
})

if (!exists("%||%", mode = "function"))
  `%||%` <- function(x, y) if (is.null(x)) y else x

source("R/team_config.R")
source("R/girth_functions.R")
source("R/talent_origins.R")

check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

board_expected <- function(x, metric) {
  switch(metric,
         commit_n = x$N,
         blue_n = x$BlueN,
         blue_share = x$BlueShare,
         median_rating = x$MedianRating)
}

trend_expected <- function(x, metric) {
  switch(metric,
         commit_n = 100 * x$N / x$PoolN,
         blue_n = 100 * x$BlueN / x$PoolBlueN,
         blue_share = x$BlueShare,
         median_rating = x$MedianRating - x$PoolMedian)
}

conn <- dbConnect(SQLite(), "data/recruiting.db")
on.exit(dbDisconnect(conn), add = TRUE)

check(all(is.finite(TEAM_CONFIG$campus_lat)) &&
        all(is.finite(TEAM_CONFIG$campus_long)),
      "one or more Power-4 destinations lack campus coordinates")
check(identical(pretty_university(TEAM_CONFIG$slug), TEAM_CONFIG$team_name),
      "configured destination labels are not canonical")

for (sport in c("football", "basketball")) {
  raw <- dbGetQuery(conn, paste0(
    "SELECT * FROM recruit_class_", sport, " WHERE Year <= 2027"))
  origins <- prep_origin_data(raw, sport, today = as.Date("2026-07-18"))
  pool <- origin_talent_pool(origins, c(2016, 2027), us_only = TRUE)

  mapped <- is.finite(origins$lat) & is.finite(origins$long) &
    origins$School %in% TEAM_CONFIG$slug
  check(all(is.finite(origins$college_lat[mapped])) &&
          all(is.finite(origins$college_long[mapped])) &&
          all(is.finite(origins$miles_away[mapped])),
        paste(sport, "Program Reach campus fallback failed"))

  check(nrow(pool) > 0, paste(sport, "pool is empty"))
  check(all(pool$Type == "Commit"), paste(sport, "pool contains transfers"))
  check(all(pool$OriginKind == "hs_prep"),
        paste(sport, "pool contains unreviewed source levels"))
  check(all(pool$StateClean %in% ORIGIN_US_CODES),
        paste(sport, "pool contains invalid state codes"))
  check(!anyDuplicated(paste(pool$AthleteKey, pool$Year)),
        paste(sport, "athlete/class dedupe failed"))

  for (metric in unname(origin_metric_choices("board"))) {
    board <- origin_state_board(pool, metric, "All", 15, "AZ")
    check(nrow(board) > 0, paste(sport, metric, "state board is empty"))
    check(all(is.finite(board$Value)),
          paste(sport, metric, "board has invalid values"))
    check(identical(attr(board, "metric"), metric),
          paste(sport, metric, "board routed to the wrong measure"))
    check(isTRUE(all.equal(as.numeric(board$Value),
                           as.numeric(board_expected(board, metric)),
                           tolerance = 1e-10)),
          paste(sport, metric, "board formula failed"))
    check("RatedN" %in% names(board),
          paste(sport, metric, "rated sample is hidden"))

    positions <- origin_position_board(pool, metric, 3)
    check(nrow(positions) > 0, paste(sport, metric, "position board is empty"))
    check(isTRUE(all.equal(as.numeric(positions$Value),
                           as.numeric(board_expected(positions, metric)),
                           tolerance = 1e-10)),
          paste(sport, metric, "position formula failed"))
    check(max(positions$Rank) <= 3,
          paste(sport, metric, "position top-n failed"))
  }

  for (metric in unname(origin_metric_choices("trend"))) {
    trend <- origin_year_board(pool, "AZ", metric, "All",
                               today = as.Date("2026-07-18"))
    check(nrow(trend) > 0, paste(sport, metric, "trend is empty"))
    check(identical(attr(trend, "metric"), metric),
          paste(sport, metric, "trend routed to the wrong measure"))
    valid <- is.finite(trend$Value)
    check(isTRUE(all.equal(as.numeric(trend$Value[valid]),
                           as.numeric(trend_expected(trend, metric)[valid]),
                           tolerance = 1e-10)),
          paste(sport, metric, "trend formula failed"))
    check(any(trend$IsOpenCycle),
          paste(sport, metric, "open cycle is not flagged"))
    check(all(trend$N <= trend$PoolN),
          paste(sport, metric, "trend denominator failed"))
  }

  board <- origin_state_board(pool, "blue_n", "All", 15, "AZ")
  check("AZ" %in% board$StateClean,
        paste(sport, "selected state was not retained"))

  html <- origin_state_table_html(board, "validation")
  check(grepl("<caption>", html, fixed = TRUE),
        paste(sport, "table caption missing"))
  check(all(vapply(board$StateName, grepl, logical(1), x = html, fixed = TRUE)),
        paste(sport, "table and chart frame diverged"))

  message(
    sprintf("%s: %s unique HS/prep athletes; %s states; leader %s",
            sport, format(nrow(pool), big.mark = ","),
            length(unique(pool$StateClean)), board$StateClean[1])
  )
}

## Edge cases: repaired state, blue-chip boundary, conservative JUCO rule,
## and a new COLLEGE name entering review instead of being guessed.
synthetic <- data.frame(
  Name = c("A", "B", "C", "D", "A"),
  Location = c("Pittsburg (Pittsburg, CA)",
               "Cambridge Rindge (Cambridge, MA)",
               "Hutchinson C.C. (Hutchinson, KS)",
               "Snow College (Ephraim, UT)",
               "Pittsburg (Pittsburg, CA)"),
  State = c("CA", "ZZ", "KS", "UT", "CA"),
  Ranking = c(90, 103, 91, 92, 90),
  Position = c("WR", "QB", "OL", "RB", "S"),
  Year = 2026L,
  School = c("arizona", "arizona", "arizona", "arizona", "california"),
  Type = "Commit",
  ProfileUrl = c("https://example.test/a", "https://example.test/b",
                 "https://example.test/c", "https://example.test/d",
                 "https://example.test/a"),
  stringsAsFactors = FALSE
)

syn <- prep_origin_data(synthetic, "football", as.Date("2026-07-18"))
check(identical(syn$StateClean, c("CA", "MA", "KS", "UT", "CA")),
      "state normalization failed")
check(identical(syn$OriginKind,
                c("hs_prep", "hs_prep", "juco", "needs_review", "hs_prep")),
      "source classification failed")
check(identical(syn$RatingClean, c(90, 103, 91, 92, 90)),
      "supported 101-103 rating contract failed")
check(all(syn$IsBlueChip), "90+ blue-chip boundary failed")
syn_pool <- origin_talent_pool(syn, c(2026, 2026), TRUE)
check(nrow(syn_pool) == 2,
      "profile-based athlete dedupe failed")
check(any(syn_pool$OriginPositionConflict),
      "conflicting duplicate position was not flagged")
check(nrow(.origin_filter_position(syn_pool, "WR")) == 0,
      "position-conflict athlete leaked into a position view")

quality_fixture <- data.frame(
  N = c(80, 80), RatedN = c(49, 50), BlueN = c(20, 25),
  BlueShare = c(100 * 20 / 49, 50), MedianRating = c(89, 90)
)
quality_result <- .origin_add_metric(quality_fixture, "blue_share", 50)
check(is.na(quality_result$Value[1]) &&
        identical(quality_result$Value[2], 50),
      "quality gate is not using RatedN")

open_fixture <- data.frame(
  Year = c(2026L, 2027L), IsOpenCycle = c(FALSE, TRUE)
)
check(grepl("50.0% of captured athletes in this pool",
            origin_open_cycle_note(open_fixture), fixed = TRUE),
      "open-cycle note misstates its denominator")

## Open rows outside the core position groups must not inflate the position
## board's disclosure denominator.
open_ath <- syn_pool[1, , drop = FALSE]
open_ath$AthleteKey <- "url:https://example.test/open-ath"
open_ath$OriginPositionConflict <- FALSE
open_ath$Position <- "ATH"
open_ath$PosGroup <- factor("ATH", levels = position_levels("football"))
open_ath$Year <- 2027L
open_ath$IsOpenCycle <- TRUE
position_note_fixture <- origin_position_board(
  dplyr::bind_rows(syn_pool, open_ath), "blue_n", 3)
check(identical(attr(position_note_fixture, "open_note"), ""),
      "non-core positions leaked into the position-board open note")

## Legacy/synthetic frames may omit Type or School. Preparation must materialize
## both columns so later pool and display helpers fail closed instead of erroring.
legacy <- synthetic[, setdiff(names(synthetic), c("Type", "School"))]
legacy_prep <- prep_origin_data(legacy, "football", as.Date("2026-07-18"))
check(all(c("Type", "School") %in% names(legacy_prep)),
      "missing-schema columns were not materialized")
check(nrow(origin_talent_pool(legacy_prep, c(2026, 2026), TRUE)) == 0,
      "missing Type should produce an empty talent pool")

message("Talent Origins validation passed.")
