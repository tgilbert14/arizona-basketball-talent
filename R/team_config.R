## ---------------------------------------------------------------------------
## team_config.R
## One table that drives everything team-related:
##   db slug -> display name, logo file, brand colors
## This replaces the 16 copy-pasted logo actionButtons / observeEvents in app.R
## and the hand-built logo_df in box_plot.R.
## ---------------------------------------------------------------------------

TEAM_CONFIG <- data.frame(
  slug = c("arizona", "arizona-state", "baylor", "byu", "central-florida",
           "cincinnati", "colorado", "houston", "iowa-state", "kansas",
           "kansas-state", "oklahoma-state", "tcu", "texas-tech", "utah",
           "west-virginia"),
  team_name = c("Arizona", "Arizona State", "Baylor", "BYU", "UCF",
                "Cincinnati", "Colorado", "Houston", "Iowa State", "Kansas",
                "Kansas State", "Oklahoma State", "TCU", "Texas Tech", "Utah",
                "West Virginia"),
  logo = c("arizona.png", "arizona-state.png", "baylor.png", "byu.png",
           "ucf.png", "cincinnati.png", "colorado.png", "houston.png",
           "iowa-state.png", "kansas.png", "kansas-state.png",
           "oklahoma-state.png", "tcu.png", "texas-tech.png", "utah.png",
           "west-virginia.png"),
  primary = c("#CC0033", "#8C1D40", "#154734", "#002E5D", "#BA9B37",
              "#E00122", "#CFB87C", "#C8102E", "#C8102E", "#0051BA",
              "#512888", "#FF7300", "#4D1979", "#CC0000", "#BE0000",
              "#002855"),
  secondary = c("#0C234B", "#FFC627", "#FFB81C", "#FFFFFF", "#000000",
                "#000000", "#000000", "#FFFFFF", "#F1BE48", "#E8000D",
                "#D1D1D1", "#000000", "#A3A9AC", "#000000", "#808080",
                "#EAAA00"),
  ## conference is a REAL per-team column (not a recycled scalar): every board,
  ## median, rank and superlative scopes to the ACTIVE team's conference before
  ## pooling, and every "Big 12" label derives from conf_label(), not a literal.
  ## At 16 Big-12-only rows this is inert (all 16 pool, label stays "Big 12");
  ## the instant a second conference's rows land, set that team's value here and
  ## the whole app scopes + relabels correctly. See CONF_CONFIG below.
  conference = c("Big 12", "Big 12", "Big 12", "Big 12", "Big 12",
                 "Big 12", "Big 12", "Big 12", "Big 12", "Big 12",
                 "Big 12", "Big 12", "Big 12", "Big 12", "Big 12",
                 "Big 12"),
  state = c("AZ", "AZ", "TX", "UT", "FL", "OH", "CO", "TX", "IA", "KS",
            "KS", "OK", "TX", "TX", "UT", "WV"),
  ## realignment-honest baseline: the class year each program actually joined
  ## its CURRENT conference. The four Pac-12 refugees (Arizona/ASU/Colorado/
  ## Utah) arrived in the Big 12 in 2024; the AAC four (BYU/UCF/Cincinnati/
  ## Houston) a cycle earlier in 2023; the eight legacy members predate the
  ## app's window, stamped 2012. Any conference-wide band drawn before a
  ## member's join is a BACKCAST -- the charts say so instead of pretending
  ## the 16-team league always existed. (Renamed from big12_since; the old
  ## accessor team_big12_since() survives as a shim below.)
  conf_since = c(2024, 2024, 2012, 2023, 2023, 2023, 2024, 2023, 2012, 2012,
                 2012, 2012, 2012, 2012, 2024, 2012),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------------
## CONF_CONFIG -- one row per conference. The seam that lets copy, ordering and
## the realignment backcast go dynamic when a second conference's rows land.
##   conf       : conference name (matches TEAM_CONFIG$conference)
##   order       : display order (Big 12 first = 1)
##   color       : Okabe-Ito aggregate hue for conf-level charts (Phase 2+)
##   conf_whole  : the class year the CURRENT membership was all in-conference
##                 (2024 realignment) -- backcast band solid at/after this year,
##                 ghosted before it. Replaces the hardcoded 2024 literals in
##                 the plot builders (B12_WHOLE, the <2024 backcast checks).
## Phase 0 has ONE row (Big 12); Phase 2 adds Big Ten / SEC / ACC.
## ---------------------------------------------------------------------------
CONF_CONFIG <- data.frame(
  conf       = "Big 12",
  order      = 1L,
  color      = "#0072B2",  ## Okabe-Ito blue
  conf_whole = 2024L,
  stringsAsFactors = FALSE
)

## the class year a program joined today's conference (for backcast honesty);
## vectorized + NA-safe (unknown/NA slug -> NA).
team_conf_since <- function(slug) {
  TEAM_CONFIG$conf_since[match(slug, TEAM_CONFIG$slug)]
}

## back-compat shim: the pre-realignment name. Kept as a thin wrapper so callers
## mid-migration keep working while the codebase moves to team_conf_since().
team_big12_since <- function(slug) team_conf_since(slug)

## conference lookups --------------------------------------------------------

## the conference a team currently plays in; vectorized + NA-safe.
team_conference <- function(slug) {
  TEAM_CONFIG$conference[match(slug, TEAM_CONFIG$slug)]
}

## the slugs that belong to a conference (or set of conferences) -- the pooling
## scope for every board / median / rank. Preserves TEAM_CONFIG order; NA-safe
## (NA or unknown conf -> character(0)). Pass team_conference(team) to scope a
## board to the active team's conference members.
conf_slugs <- function(conf) {
  TEAM_CONFIG$slug[TEAM_CONFIG$conference %in% conf]
}

## display label for a conference OR a team slug -- the single seam where the
## hardcoded "Big 12" copy becomes dynamic. A slug resolves to its conference
## first; a conference name passes through. Currently the label IS the
## conference name (a distinct display string would slot in here). Vectorized +
## NA-safe (NA in -> NA out).
conf_label <- function(slug_or_conf) {
  x <- as.character(slug_or_conf)
  is_slug <- x %in% TEAM_CONFIG$slug
  ifelse(is_slug, team_conference(x), x)
}

## the school's home state (for in-state recruiting share)
team_state <- function(slug) {
  TEAM_CONFIG$state[match(slug, TEAM_CONFIG$slug)]
}

## pick readable highlight colors for a main + compare team pair; if their
## primaries are near-identical (e.g. UA cardinal vs ASU maroon) the compare
## team falls back through: its secondary -> Okabe-Ito blue -> orange.
## A candidate must ALSO be dark enough to survive white chart panels --
## BYU/Houston secondaries are pure white and K-State's is light grey, so
## the old one-step fallback could render the compare team invisible.
highlight_colors <- function(team1, team2 = NULL) {
  col1 <- team_color(team1)
  if (is.null(team2) || is.na(team2) || team2 == "" || team2 == team1) {
    return(c(main = col1, compare = NA_character_))
  }
  usable <- function(col) {
    !is.na(col) && nzchar(col) &&
      sum(abs(col2rgb(col1) - col2rgb(col))) >= 220 &&
      mean(col2rgb(col)) <= 200
  }
  col2 <- team_color(team2)
  if (!usable(col2)) {
    for (cand in c(TEAM_CONFIG$secondary[match(team2, TEAM_CONFIG$slug)],
                   "#0072B2", "#E69F00")) {
      if (usable(cand)) { col2 <- cand; break }
    }
  }
  c(main = col1, compare = col2)
}

## quick lookups -------------------------------------------------------------

team_label <- function(slug) {
  TEAM_CONFIG$team_name[match(slug, TEAM_CONFIG$slug)]
}

team_logo <- function(slug, prefix = "www/") {
  paste0(prefix, TEAM_CONFIG$logo[match(slug, TEAM_CONFIG$slug)])
}

team_color <- function(slug) {
  col <- TEAM_CONFIG$primary[match(slug, TEAM_CONFIG$slug)]
  ifelse(is.na(col), "#0072B2", col)
}

## html <img> labels for ggtext axis logos, named by any key column
team_logo_labels <- function(key = TEAM_CONFIG$team_name, width = 32,
                             prefix = "www/") {
  setNames(
    paste0("<img src='", prefix, TEAM_CONFIG$logo, "' width='", width, "'/>"),
    key
  )
}
