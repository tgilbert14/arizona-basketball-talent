## ---------------------------------------------------------------------------
## girth_functions.R
## Shared helpers for the Size Lab ("girth") analysis:
##   - height parsing ("6-4.5" -> 76.5 inches)
##   - size metrics (lbs-per-inch, BMI)
##   - position grouping (single source of truth for football + basketball)
##   - data prep for size visualizations
## Sourced automatically by Shiny from the R/ folder (after functions.R).
## ---------------------------------------------------------------------------

## parse 247Sports height strings like "6-4", "5-11.75" into total inches
parse_height <- function(h) {
  h <- as.character(h)
  parts <- str_split_fixed(h, "-", 2)
  feet <- suppressWarnings(as.numeric(parts[, 1]))
  inches <- suppressWarnings(as.numeric(parts[, 2]))
  total <- feet * 12 + inches
  ## junk filter: anything outside human athlete range becomes NA
  total[total < 60 | total > 90] <- NA_real_
  total
}

## 76.5 -> 6'4.5" for labels
format_height <- function(inches) {
  ifelse(is.na(inches), NA_character_,
         paste0(floor(inches / 12), "'", round(inches %% 12 * 4) / 4, "\""))
}

## pounds carried per inch of height -- the "girth" headline number
lbs_per_inch <- function(weight, height_in) {
  round(weight / height_in, 2)
}

## standard BMI from imperial units
calc_bmi <- function(weight, height_in) {
  round(703 * weight / height_in^2, 1)
}

## one source of truth for position groups (was duplicated in box_plot.R)
position_group <- function(position, sport) {
  sport <- tolower(sport)
  if (sport == "football") {
    dplyr::case_when(
      position %in% c("QB", "PRO", "DUAL")              ~ "QB",
      position %in% c("RB", "APB", "FB")                ~ "RB",
      position %in% c("WR")                             ~ "WR",
      position %in% c("TE")                             ~ "TE",
      ## "T"/"G" appear on 247 roster pages as tackle/guard shorthand
      position %in% c("OT", "OG", "OC", "IOL", "OL",
                      "T", "G")                         ~ "OL",
      position %in% c("DT", "DL", "SDE", "WDE", "DE",
                      "Edge", "EDGE", "NT")             ~ "DL/Edge",
      position %in% c("LB", "ILB", "OLB")               ~ "LB",
      position %in% c("CB", "S", "DB")                  ~ "DB",
      position %in% c("ATH")                            ~ "ATH",
      position %in% c("K", "P", "LS", "RET")            ~ "ST",
      TRUE                                              ~ "Other"
    )
  } else {
    dplyr::case_when(
      position %in% c("PG", "SG", "CG", "G")            ~ "Guard",
      position %in% c("SF", "PF", "F")                  ~ "Forward",
      position %in% c("C")                              ~ "Center",
      TRUE                                              ~ "Other"
    )
  }
}

## display order for the groups
position_levels <- function(sport) {
  if (tolower(sport) == "football") {
    c("QB", "RB", "WR", "TE", "OL", "DL/Edge", "LB", "DB", "ATH", "ST", "Other")
  } else {
    c("Guard", "Forward", "Center", "Other")
  }
}

## big-bodied trench positions (football) -- "games are won up front"
is_trench <- function(pos_group) {
  pos_group %in% c("OL", "DL/Edge")
}

## pretty school names, centralizing the logic that lived inside app.R
pretty_university <- function(school_slug) {
  u_of <- c("arizona", "utah", "kansas", "houston",
            "colorado", "cincinnati", "central-florida")
  out <- ifelse(
    nchar(school_slug) == 3,
    toupper(school_slug),
    ifelse(school_slug %in% u_of,
           paste0("University of ", str_to_title(school_slug)),
           paste0(str_to_title(school_slug), " University"))
  )
  str_replace_all(out, "-", " ")
}

## ---------------------------------------------------------------------------
## prep one sport's recruit table for size analysis
## expects raw rows from recruit_class_<sport>; returns cleaned + metric columns
## ---------------------------------------------------------------------------
prep_size_data <- function(raw, sport) {
  ## the raw table has its own `sport` column -- keep the argument out of the
  ## data mask so it doesn't get shadowed inside mutate()
  sp_name <- tolower(sport)
  raw %>%
    mutate(
      Weight       = suppressWarnings(as.numeric(Weight)),
      Height_in    = parse_height(Height),
      Ranking      = suppressWarnings(as.numeric(Ranking)),
      Year         = as.integer(Year)
    ) %>%
    ## drop junk rows (e.g. Weight 0 / Height "0-0" placeholders)
    filter(!is.na(Height_in), !is.na(Weight), Weight >= 120) %>%
    mutate(
      LbsPerInch  = lbs_per_inch(Weight, Height_in),
      BMI         = calc_bmi(Weight, Height_in),
      PosGroup    = factor(position_group(Position, sp_name),
                           levels = position_levels(sp_name)),
      Trench      = is_trench(as.character(PosGroup)),
      HeightLabel = format_height(Height_in),
      TeamName    = team_label(School),
      University  = pretty_university(School),
      InState     = State == team_state(School),
      ## distance from high school to campus (for era recruiting-footprint)
      miles_away  = round(geosphere::distGeo(
        p1 = cbind(suppressWarnings(as.numeric(long)),
                   suppressWarnings(as.numeric(lat))),
        p2 = cbind(suppressWarnings(as.numeric(college_long)),
                   suppressWarnings(as.numeric(college_lat)))) / 1609.34, 0)
    ) %>%
    add_coach_era(sp_name)
}

## prep a CURRENT ROSTER table to the same shape the size plots expect --
## the "roster reality" view: the players actually on campus right now
## (closer to who plays than commit lists; true starters need usage data)
prep_roster_size <- function(roster_raw, sport) {
  sp_name <- tolower(sport)
  roster_raw %>%
    mutate(
      Weight = suppressWarnings(as.numeric(Weight)),
      Height_in = parse_height(Height),
      Year = as.integer(RosterYear)
    ) %>%
    filter(!is.na(Height_in), !is.na(Weight), Weight >= 120) %>%
    mutate(
      LbsPerInch = lbs_per_inch(Weight, Height_in),
      BMI = calc_bmi(Weight, Height_in),
      PosGroup = factor(position_group(Position, sp_name),
                        levels = position_levels(sp_name)),
      Trench = is_trench(as.character(PosGroup)),
      HeightLabel = format_height(Height_in),
      TeamName = team_label(School)
    )
}

## summarize a size metric per team (optionally filtered upstream)
team_size_summary <- function(size_data) {
  size_data %>%
    group_by(School, TeamName) %>%
    summarize(
      Players      = n(),
      AvgHeight    = mean(Height_in, na.rm = TRUE),
      AvgWeight    = mean(Weight, na.rm = TRUE),
      AvgLbsPerIn  = mean(LbsPerInch, na.rm = TRUE),
      AvgBMI       = mean(BMI, na.rm = TRUE),
      .groups = "drop"
    )
}

## ---------------------------------------------------------------------------
## shared girafe builder -- used by BOTH the app's girafe_wrap() and
## scripts/precomputeDefaults.R, so precomputed objects can't drift from
## live-rendered ones. `phone` mirrors the app's <700px canvas shrink.
## ---------------------------------------------------------------------------
girafe_build <- function(p, w = 11.5, h = 6.5, name = "big12-girth-index",
                         phone = FALSE) {
  if (isTRUE(phone)) {
    scale <- 7 / w
    h <- max(4, h * scale * 1.25)
    w <- 7
  }
  ggiraph::girafe(
    ggobj = p, width_svg = w, height_svg = h,
    options = list(
      ggiraph::opts_tooltip(css = paste0(
        "background-color:#0C234B;color:white;padding:8px;",
        "border-radius:6px;font-size:13px;"),
        offx = 25, offy = -20, delay_mouseout = 1200),
      ## nearest_distance makes hover touch-forgiving: a tap within ~30px
      ## snaps to the closest interactive element (verified present in the
      ## installed ggiraph 0.9.6 via formals(opts_hover))
      ggiraph::opts_hover(
        css = "stroke:#0C234B;stroke-width:2px;cursor:pointer;",
        nearest_distance = 30),
      ## spotlight: everything NOT hovered dims, so the hovered element and
      ## its teammates read instantly on busy charts
      ggiraph::opts_hover_inv(css = "opacity:0.3;transition:opacity 0.15s;"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_selection_key(type = "none"),
      ggiraph::opts_toolbar(saveaspng = TRUE, pngname = name)
    )
  )
}

## ---------------------------------------------------------------------------
## shared ggplot theme for the Size Lab
## ---------------------------------------------------------------------------
theme_girth <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      ## title.position = "plot" uses the full plot width (not just the
      ## panel), which together with wrap_title() stops long titles clipping
      ## inside narrow dashboard boxes
      plot.title = element_text(face = "bold", size = base_size * 1.3,
                                color = "#0C234B"),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = base_size * 0.9, color = "#46535E"),
      plot.caption = element_text(size = base_size * 0.7, color = "#8A949C",
                                  face = "italic"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = base_size)
    )
}

## wrap long plot titles/subtitles so they never run off narrow boxes
wrap_title <- function(x, width = 48) {
  str_wrap(x, width = width)
}

## attach/replace the alpha channel on an RGB(A) png array (logo watermarks)
abind_alpha <- function(img, alpha) {
  out <- array(0, dim = c(dim(img)[1], dim(img)[2], 4))
  out[, , 1:3] <- img[, , 1:3]
  out[, , 4] <- alpha
  out
}

## markdown-axis variant: NOT built on a complete theme. ggplot2 4.x flattens
## ggtext's element_markdown when it merges into complete themes (theme_minimal
## etc.), so logo <img> axis labels render as raw HTML. A bare theme() keeps
## the markdown element intact (same reason box_plot.R uses bare theme()).
theme_girth_md <- function(base_size = 14) {
  theme(
    plot.title = element_text(face = "bold", size = base_size * 1.3,
                              color = "#0C234B"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = base_size * 0.9, color = "#46535E"),
    plot.caption = element_text(size = base_size * 0.7, color = "#8A949C",
                                face = "italic"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey88"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = base_size * 0.85),
    axis.text.y = element_markdown(),
    axis.title = element_text(size = base_size),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = base_size)
  )
}

## palette for football position groups (Okabe-Ito derived, color-blind safe)
pos_group_palette <- function(sport) {
  if (tolower(sport) == "football") {
    c("QB" = "#E69F00", "RB" = "#56B4E9", "WR" = "#009E73",
      "TE" = "#F0E442", "OL" = "#0072B2", "DL/Edge" = "#D55E00",
      "LB" = "#CC79A7", "DB" = "#999999", "ATH" = "#882255",
      "ST" = "#44AA99", "Other" = "#DDDDDD")
  } else {
    c("Guard" = "#56B4E9", "Forward" = "#009E73",
      "Center" = "#D55E00", "Other" = "#DDDDDD")
  }
}
