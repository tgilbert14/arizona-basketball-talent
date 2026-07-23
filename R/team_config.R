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
  ## onboarded: the team is live in the app (data backfilled, visible in every
  ## picker/board). The inline 16-program fallback is all TRUE; the production
  ## CSV carries the complete 67-program universe and uses the same visibility
  ## gate, so onboarded_slugs() works identically on both load paths.
  onboarded = TRUE,
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------------
## data/team_config.csv is the 67-program Power-4 source of truth. The inline
## 16-row Big 12 frame above is a defensive fallback only. Either load path
## carries the logical `onboarded` visibility gate; all 67 production rows are
## currently live.
## ---------------------------------------------------------------------------
local({
  csv <- file.path("data", "team_config.csv")
  if (!file.exists(csv)) return(invisible(NULL))
  tc <- utils::read.csv(csv, stringsAsFactors = FALSE)
  req_cols <- c("slug", "team_name", "logo", "primary", "secondary",
                "conference", "state", "conf_since", "onboarded")
  miss <- setdiff(req_cols, names(tc))
  if (length(miss) > 0) {
    stop("data/team_config.csv is missing required column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  ## coerce the load-bearing columns explicitly: read.csv guesses, but a stray
  ## quote/blank must never silently change a column's class out from under the
  ## helpers (conf_since integer, campus coords numeric, onboarded logical).
  tc$conf_since <- suppressWarnings(as.integer(tc$conf_since))
  if ("campus_lat"  %in% names(tc))
    tc$campus_lat  <- suppressWarnings(as.numeric(tc$campus_lat))
  if ("campus_long" %in% names(tc))
    tc$campus_long <- suppressWarnings(as.numeric(tc$campus_long))
  ## onboarded -> strict logical; accept the common truthy/falsy encodings,
  ## reject anything ambiguous rather than defaulting a hidden team visible
  ob  <- trimws(tolower(as.character(tc$onboarded)))
  onb <- rep(NA, length(ob))
  onb[ob %in% c("true",  "t", "1", "yes")] <- TRUE
  onb[ob %in% c("false", "f", "0", "no")]  <- FALSE
  tc$onboarded <- onb
  if (any(is.na(tc$onboarded))) {
    stop("data/team_config.csv has unparseable onboarded value(s) at row(s): ",
         paste(which(is.na(tc$onboarded)), collapse = ", "), call. = FALSE)
  }
  if (any(is.na(tc$slug)) || any(!nzchar(tc$slug))) {
    stop("data/team_config.csv has a blank slug", call. = FALSE)
  }
  if (any(duplicated(tc$slug))) {
    stop("data/team_config.csv has duplicate slug(s): ",
         paste(unique(tc$slug[duplicated(tc$slug)]), collapse = ", "),
         call. = FALSE)
  }
  TEAM_CONFIG <<- tc
})

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
  conf       = c("Big 12",  "SEC",     "Big Ten", "ACC"),
  order      = c(1L,        2L,        3L,        4L),
  ## Okabe-Ito aggregate hues — CVD-safe, mutually distinct
  color      = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
  conf_whole = c(2024L,     2024L,     2024L,     2024L),
  stringsAsFactors = FALSE
)

## the aggregate color for a conference (Conference Lab charts); NA-safe grey.
conf_color <- function(conf) {
  i <- match(conf, CONF_CONFIG$conf)
  ifelse(is.na(i), "grey60", CONF_CONFIG$color[i])
}

## conferences in display order (Big 12 first — Arizona is the default team).
conf_order <- function() CONF_CONFIG$conf[order(CONF_CONFIG$order)]

## ---------------------------------------------------------------------------
## CONF_COMPARE_POLICY -- the metric-tier registry that makes a dishonest
## conference-vs-conference chart UNBUILDABLE. Every Conference Lab metric is
## classified; the tab's metric selector is populated FROM this table, so a RED
## metric (win%, SP+, wins-above-talent) is physically absent from the axis --
## it can't be picked, so it can't mislead. See docs/p4-expansion-design.md
## "Honesty guardrails".
##   tier    : GREEN  = head-to-head honest (a 92 is a 92 in any league)
##             YELLOW = shown only with a "reflects geography/strategy, not
##                      talent" caveat baked into the caption
##             RED    = refused as a conference leaderboard (never in the list)
##   col     : the player-level column reduced per team (NA => a custom reducer
##             handled in conf_spread_data)
##   reducer : how a team's value is computed from its players
##   fmt     : value formatter
##   caveat  : YELLOW-only caption line (NULL for GREEN)
## ---------------------------------------------------------------------------
CONF_COMPARE_POLICY <- list(
  AvgRating = list(
    label = "Average 247 rating", tier = "GREEN", reducer = "mean_rating",
    fmt = function(v) sprintf("%.1f", v), caveat = NULL),
  BlueChipShare = list(
    label = "Blue-chip share (rating ≥ 90)", tier = "GREEN",
    reducer = "blue_share", fmt = function(v) paste0(round(v), "%"),
    caveat = NULL),
  AvgWeight = list(
    label = "Average weight (lbs)", tier = "GREEN", reducer = "mean_weight",
    fmt = function(v) paste0(round(v), " lbs"), caveat = NULL),
  AvgLbsPerIn = list(
    label = "Pounds per inch of height", tier = "GREEN",
    reducer = "mean_lpi", fmt = function(v) sprintf("%.2f", v), caveat = NULL),
  InStateShare = list(
    label = "In-state share", tier = "YELLOW", reducer = "instate_share",
    fmt = function(v) paste0(round(v), "%"),
    caveat = paste("In-state share reflects a state's high-school talent",
                   "supply and a program's geography — NOT recruiting quality.",
                   "Texas and Florida programs sit in dense talent beds; a",
                   "Northwestern or a Boston College cannot.")),
  TransferShare = list(
    label = "Portal-transfer share", tier = "YELLOW", reducer = "transfer_share",
    fmt = function(v) paste0(round(v), "%"),
    caveat = paste("Transfer share reflects roster STRATEGY (portal-heavy",
                   "rebuilds vs. high-school development), not talent. A high",
                   "share is a philosophy, not a grade."))
)

## the metrics offered in the Conference Lab selector, in a sensible order,
## GREEN first then YELLOW -- RED metrics are simply never listed. Returns a
## named character vector (label -> key) for selectInput choices.
conf_metric_choices <- function() {
  keys <- names(CONF_COMPARE_POLICY)
  tier <- vapply(keys, function(k) CONF_COMPARE_POLICY[[k]]$tier, character(1))
  keys <- keys[order(match(tier, c("GREEN", "YELLOW")))]
  labs <- vapply(keys, function(k) {
    p <- CONF_COMPARE_POLICY[[k]]
    if (p$tier == "YELLOW") paste0(p$label, "  (context metric)") else p$label
  }, character(1))
  setNames(keys, labs)
}

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

## the slugs that belong to a conference (or set of conferences) -- the FULL
## membership regardless of onboarding. Preserves TEAM_CONFIG order; NA-safe
## (NA or unknown conf -> character(0)). This is the conference primitive used
## by the per-conference BACKFILL (scrapers' --conference flag) so it can reach
## a league's teams BEFORE they are flipped onboarded. For the app's display /
## pooling universe use onboarded_slugs() instead. They currently agree in
## production because all 67 configured programs are live.
conf_slugs <- function(conf) {
  TEAM_CONFIG$slug[TEAM_CONFIG$conference %in% conf]
}

## the ONBOARDED slugs -- the app's team universe: pickers, the deep-link
## validator, every board's pooling scope, and the nightly scrapers' default
## iteration all draw from this, so a not-yet-backfilled (onboarded = FALSE)
## program never appears anywhere until its conference is backfilled. Optionally
## filtered to a conference (or set of conferences); pass team_conference(team)
## to pool only the active team's onboarded conference members (so a half-
## onboarded league never renders partial). Preserves TEAM_CONFIG order; NA in
## `onboarded` counts as FALSE (fail hidden, never accidentally visible). All
## 67 production programs are currently TRUE; the guard remains for safe loads.
onboarded_slugs <- function(conf = NULL) {
  keep <- TEAM_CONFIG$onboarded %in% TRUE
  if (!is.null(conf)) keep <- keep & TEAM_CONFIG$conference %in% conf
  TEAM_CONFIG$slug[keep]
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

## The global comparison picker deliberately spans all onboarded Power-4
## programs. This helper makes that choice explicit at every rendering
## surface: a same-league team is a conference peer; an out-of-league team is
## an external reference that must never alter the active conference's ranks,
## averages, or outcome calibration.
comparison_context <- function(team_slug, compare_slug = NULL) {
  team_slug <- as.character(team_slug)[1]
  compare_slug <- if (is.null(compare_slug) || !length(compare_slug)) "" else
    as.character(compare_slug)[1]
  compare_slug <- trimws(compare_slug)

  team_ok <- !is.na(team_slug) && team_slug %in% TEAM_CONFIG$slug
  cmp_ok <- !is.na(compare_slug) && nzchar(compare_slug) &&
    compare_slug %in% TEAM_CONFIG$slug && !identical(compare_slug, team_slug)

  team_conf <- if (team_ok) team_conference(team_slug) else NA_character_
  cmp_conf <- if (cmp_ok) team_conference(compare_slug) else NA_character_
  cross_conf <- isTRUE(cmp_ok) && !is.na(team_conf) && !is.na(cmp_conf) &&
    !identical(team_conf, cmp_conf)

  list(
    active = isTRUE(cmp_ok),
    cross_conference = cross_conf,
    same_conference = isTRUE(cmp_ok) && !cross_conf,
    team_slug = if (team_ok) team_slug else "",
    compare_slug = if (cmp_ok) compare_slug else "",
    team_name = if (team_ok) team_label(team_slug) else "",
    compare_name = if (cmp_ok) team_label(compare_slug) else "",
    team_conference = team_conf,
    compare_conference = cmp_conf
  )
}

is_cross_conference_compare <- function(team_slug, compare_slug = NULL) {
  isTRUE(comparison_context(team_slug, compare_slug)$cross_conference)
}

## html <img> labels for ggtext axis logos, named by any key column
team_logo_labels <- function(key = TEAM_CONFIG$team_name, width = 32,
                             prefix = "www/") {
  setNames(
    paste0("<img src='", prefix, TEAM_CONFIG$logo, "' width='", width, "'/>"),
    key
  )
}
