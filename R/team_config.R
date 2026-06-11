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
  ## multi-conference roadmap: add rows with a different conference value and
  ## the rest of the app (filters, boards, scrapers) picks them up
  conference = "Big 12",
  state = c("AZ", "AZ", "TX", "UT", "FL", "OH", "CO", "TX", "IA", "KS",
            "KS", "OK", "TX", "TX", "UT", "WV"),
  stringsAsFactors = FALSE
)

## the school's home state (for in-state recruiting share)
team_state <- function(slug) {
  TEAM_CONFIG$state[match(slug, TEAM_CONFIG$slug)]
}

## pick readable highlight colors for a main + compare team pair; if their
## primaries are near-identical (e.g. UA cardinal vs ASU maroon) the compare
## team falls back to its secondary color
highlight_colors <- function(team1, team2 = NULL) {
  col1 <- team_color(team1)
  if (is.null(team2) || is.na(team2) || team2 == "" || team2 == team1) {
    return(c(main = col1, compare = NA_character_))
  }
  col2 <- team_color(team2)
  if (sum(abs(col2rgb(col1) - col2rgb(col2))) < 220) {
    col2 <- TEAM_CONFIG$secondary[match(team2, TEAM_CONFIG$slug)]
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
