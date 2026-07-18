## ---------------------------------------------------------------------------
## TALENT ORIGINS
## State-level recruiting-origin analysis built from raw recruiting rows.
##
## Important: `Type == "Commit"` is not automatically high-school-only.
## This module classifies the last listed pre-college school, quarantines
## obvious/review-needed college sources, and keeps geography independent of
## the height/weight cleaning used by the Size Lab.
## ---------------------------------------------------------------------------

ORIGIN_US_CODES <- c(state.abb, "DC")
ORIGIN_STATE_NAMES <- c(setNames(state.name, state.abb),
                        DC = "District of Columbia")

JUCO_HIGH_CONF_RE <- stringr::regex(
  "C\\.C\\.|COMMUNITY COLLEGE|JUNIOR COLLEGE|COLLEGE OF |MILITARY COLLEGE",
  ignore_case = TRUE
)

## Reviewed legitimate HS/prep names containing COLLEGE. New, unseen COLLEGE
## names intentionally enter needs_review instead of being guessed.
COLLEGE_HS_ALLOW <- toupper(c(
  "St. John's College", "Christian Brothers College",
  "Cardinal Ritter College Prep", "Brophy College Preparatory",
  "College Station", "Calvert Hall College", "College Park",
  "La Salle College", "LaSalle College", "Lincoln College Prep",
  "Boston College High", "State College Area", "Lake Charles College Prep",
  "Antonian College Prep", "The Scot's College",
  "Columbus Africentric Early College", "Chaminade College Prep",
  "St. Mary's College", "Baltimore City College",
  "St. Ignatius College Preparatory", "North College Hill",
  "Sydney Secondary College", "Legacy Early College",
  "Academy For College And Career Exploration",
  "Strake Jesuit College Prep", "Fallbrook College Preparatory Academy",
  "St. Joseph's College"
))

origin_state_name <- function(code) {
  out <- unname(ORIGIN_STATE_NAMES[as.character(code)])
  out[is.na(out)] <- as.character(code)[is.na(out)]
  out
}

origin_position_levels <- function(sport) {
  if (tolower(sport) == "football") {
    c("QB", "RB", "WR", "TE", "OL", "DL/Edge", "LB", "DB")
  } else {
    c("Guard", "Forward", "Center")
  }
}

origin_position_choices <- function(sport) {
  vals <- origin_position_levels(sport)
  setNames(c("All", vals), c("All positions", vals))
}

origin_metric_choices <- function(context = c("board", "trend")) {
  context <- match.arg(context)
  if (context == "trend") {
    return(c("Share of captured blue chips" = "blue_n",
             "Share of captured signees" = "commit_n",
             "Blue-chip share" = "blue_share",
             "Rating edge vs Power-4 pool" = "median_rating"))
  }

  c("Blue-chip signees (90+)" = "blue_n",
    "All captured signees" = "commit_n",
    "Blue-chip share" = "blue_share",
    "Median 247 rating" = "median_rating")
}

origin_metric_info <- function(metric, context = c("board", "trend")) {
  context <- match.arg(context)
  metric <- if (metric %in% unname(origin_metric_choices())) metric else "blue_n"

  if (context == "board") {
    return(switch(
      metric,
      commit_n = list(label = "Captured signees", axis = "Signees",
                      quality = FALSE,
                      format = function(x) format(round(x), big.mark = ",",
                                                  scientific = FALSE)),
      blue_n = list(label = "Blue-chip signees (90+)", axis = "Blue-chip signees",
                    quality = FALSE,
                    format = function(x) format(round(x), big.mark = ",",
                                                scientific = FALSE)),
      blue_share = list(label = "Blue-chip share", axis = "Blue-chip share",
                        quality = TRUE,
                        format = function(x) sprintf("%.1f%%", x)),
      median_rating = list(label = "Median 247 rating", axis = "Median 247 rating",
                           quality = TRUE,
                           format = function(x) sprintf("%.1f", x))
    ))
  }

  switch(
    metric,
    commit_n = list(label = "Share of captured signees",
                    axis = "% of captured signees", quality = FALSE,
                    format = function(x) sprintf("%.1f%%", x)),
    blue_n = list(label = "Share of captured blue chips",
                  axis = "% of captured blue chips", quality = FALSE,
                  format = function(x) sprintf("%.1f%%", x)),
    blue_share = list(label = "Blue-chip share", axis = "Blue-chip share",
                      quality = TRUE,
                      format = function(x) sprintf("%.1f%%", x)),
    median_rating = list(label = "Raw rating edge vs Power-4 pool",
                         axis = "Raw median-rating edge (points)", quality = TRUE,
                         format = function(x) sprintf("%+.1f", x))
  )
}

origin_quality_min_n <- function(sport, position_filtered = FALSE,
                                 yearly = FALSE) {
  sp <- tolower(sport %||% "football")
  if (isTRUE(yearly)) return(if (sp == "football") 10L else 5L)
  if (isTRUE(position_filtered)) return(if (sp == "football") 15L else 8L)
  if (sp == "football") 50L else 25L
}

## Add a missing column without requiring synthetic/test frames to carry the
## entire production schema.
.origin_col <- function(data, name, default = NA) {
  if (name %in% names(data)) data[[name]] else rep(default, nrow(data))
}

prep_origin_data <- function(raw, sport, today = Sys.Date()) {
  sp <- tolower(sport)
  d <- as.data.frame(raw, stringsAsFactors = FALSE)
  n <- nrow(d)
  school <- as.character(.origin_col(d, "School", ""))

  location <- as.character(.origin_col(d, "Location", ""))
  state_raw <- toupper(stringr::str_squish(
    as.character(.origin_col(d, "State", ""))))
  location_state <- toupper(stringr::str_match(
    location, ",\\s*([A-Za-z]{2})\\s*\\)\\s*$")[, 2])
  state_clean <- ifelse(state_raw %in% ORIGIN_US_CODES, state_raw,
                        ifelse(location_state %in% ORIGIN_US_CODES,
                               location_state, NA_character_))

  origin_school <- stringr::str_squish(stringr::str_remove(
    location, "\\s*\\([^()]*\\)\\s*$"))
  school_upper <- toupper(origin_school)
  type <- as.character(.origin_col(d, "Type", ""))

  high_conf_juco <- stringr::str_detect(origin_school, JUCO_HIGH_CONF_RE)
  college_review <- stringr::str_detect(school_upper, "COLLEGE") &
    !(school_upper %in% COLLEGE_HS_ALLOW) & !high_conf_juco

  origin_kind <- dplyr::case_when(
    type == "Transfer" ~ "transfer",
    is.na(state_clean) ~ "outside_us",
    high_conf_juco ~ "juco",
    college_review ~ "needs_review",
    TRUE ~ "hs_prep"
  )

  ## The team-page source contains a handful of legitimate historic 101-103
  ## elite grades; ingestion supports 0-110, so retain that same contract.
  rating_raw <- suppressWarnings(as.numeric(.origin_col(d, "Ranking")))
  rating_clean <- ifelse(is.finite(rating_raw) & rating_raw >= 0 &
                           rating_raw <= 110, rating_raw, NA_real_)
  year <- suppressWarnings(as.integer(.origin_col(d, "Year")))
  position <- toupper(stringr::str_squish(
    as.character(.origin_col(d, "Position", ""))))

  profile <- stringr::str_trim(as.character(.origin_col(d, "ProfileUrl", "")))
  profile[is.na(profile)] <- ""
  profile_key <- tolower(stringr::str_remove(profile, "[?#].*$"))
  fallback_key <- paste(
    tolower(stringr::str_squish(as.character(.origin_col(d, "Name", "")))),
    year, tolower(location), position, sep = "|"
  )
  athlete_key <- ifelse(nzchar(profile_key), paste0("url:", profile_key),
                        paste0("row:", fallback_key))

  height_in <- parse_height(.origin_col(d, "Height", NA_character_))
  weight <- suppressWarnings(as.numeric(.origin_col(d, "Weight")))
  lat <- suppressWarnings(as.numeric(.origin_col(d, "lat")))
  long <- suppressWarnings(as.numeric(.origin_col(d, "long")))
  row_college_lat <- suppressWarnings(as.numeric(.origin_col(d, "college_lat")))
  row_college_long <- suppressWarnings(as.numeric(.origin_col(d, "college_long")))
  team_idx <- match(school, TEAM_CONFIG$slug)
  config_lat <- suppressWarnings(as.numeric(
    .origin_col(TEAM_CONFIG, "campus_lat")))[team_idx]
  config_long <- suppressWarnings(as.numeric(
    .origin_col(TEAM_CONFIG, "campus_long")))[team_idx]
  row_campus_ok <- is.finite(row_college_lat) & is.finite(row_college_long)
  college_lat <- ifelse(row_campus_ok, row_college_lat, config_lat)
  college_long <- ifelse(row_campus_ok, row_college_long, config_long)
  miles <- rep(NA_real_, n)
  geo_ok <- is.finite(lat) & is.finite(long) & is.finite(college_lat) &
    is.finite(college_long)
  if (any(geo_ok)) {
    miles[geo_ok] <- round(geosphere::distGeo(
      p1 = cbind(long[geo_ok], lat[geo_ok]),
      p2 = cbind(college_long[geo_ok], college_lat[geo_ok])) / 1609.34, 0)
  }

  d$Year <- year
  d$Type <- type
  d$School <- school
  d$Ranking <- rating_raw
  d$RatingClean <- rating_clean
  d$RatingFlag <- dplyr::case_when(
    is.na(rating_raw) ~ "missing",
    is.na(rating_clean) ~ "outside_0_110",
    TRUE ~ "ok"
  )
  d$Position <- position
  d$PosGroup <- factor(position_group(position, sp),
                       levels = position_levels(sp))
  d$StateRaw <- state_raw
  d$StateClean <- state_clean
  d$StateName <- origin_state_name(state_clean)
  d$OriginSchool <- origin_school
  d$OriginKind <- origin_kind
  d$OriginRule <- dplyr::case_when(
    origin_kind == "transfer" ~ "transfer_record",
    origin_kind == "outside_us" ~ "outside_50_states_dc",
    high_conf_juco ~ "high_conf_juco_text",
    college_review ~ "college_name_review_queue",
    TRUE ~ "reviewed_hs_prep"
  )
  d$AthleteKey <- athlete_key
  d$IsBlueChip <- !is.na(rating_clean) & rating_clean >= 90
  d$IsOpenCycle <- !is.na(year) &
    year > as.integer(format(as.Date(today), "%Y"))
  d$Height_in <- height_in
  d$HeightLabel <- format_height(height_in)
  d$Weight <- weight
  d$lat <- lat
  d$long <- long
  d$college_lat <- college_lat
  d$college_long <- college_long
  d$miles_away <- miles
  d$sport <- sp
  d$TeamName <- ifelse(school %in% TEAM_CONFIG$slug,
                       team_label(school), school)
  d$University <- pretty_university(school)
  d
}

## High-confidence HS/prep athlete pool. This is deliberately separate from
## Program Reach, which may include transfers and commitment records.
origin_talent_pool <- function(origin_data, years = NULL, us_only = TRUE) {
  d <- origin_data %>%
    dplyr::filter(Type == "Commit", OriginKind == "hs_prep")
  if (!is.null(years) && length(years) == 2) {
    d <- d %>% dplyr::filter(Year >= min(years), Year <= max(years))
  }
  if (isTRUE(us_only)) d <- d %>% dplyr::filter(!is.na(StateClean))

  ## A profile can appear under two destinations after a decommit. State-level
  ## questions count that athlete once per class, not once per commitment row.
  ## When those records disagree on position group, retain the athlete in state
  ## totals but quarantine the arbitrary winner from position-specific views.
  d %>%
    dplyr::group_by(AthleteKey, Year) %>%
    dplyr::mutate(
      OriginPositionConflict = dplyr::n_distinct(
        stats::na.omit(as.character(PosGroup))) > 1L
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(AthleteKey, Year, School, OriginSchool) %>%
    dplyr::distinct(AthleteKey, Year, .keep_all = TRUE)
}

.origin_position_safe <- function(d) {
  if ("OriginPositionConflict" %in% names(d)) {
    d <- d %>% dplyr::filter(!OriginPositionConflict)
  }
  d
}

.origin_filter_position <- function(d, position = "All") {
  if (is.null(position) || identical(position, "All")) return(d)
  .origin_position_safe(d) %>%
    dplyr::filter(as.character(PosGroup) == position)
}

origin_open_cycle_note <- function(d) {
  if (is.null(d) || !nrow(d) || !"IsOpenCycle" %in% names(d)) return("")
  open <- d %>% dplyr::filter(IsOpenCycle)
  if (!nrow(open)) return("")
  years <- sort(unique(open$Year))
  lead <- if (length(years) == 1L) {
    paste0("The ", years, " class is open")
  } else {
    paste0("Classes ", paste(years, collapse = ", "), " are open")
  }
  paste0(lead, " and accounts for ",
         sprintf("%.1f%%", 100 * nrow(open) / nrow(d)),
         " of captured athletes in this pool; totals can change.")
}

.origin_state_summary <- function(d) {
  d %>%
    dplyr::group_by(StateClean) %>%
    dplyr::summarize(
      N = dplyr::n(),
      RatedN = sum(!is.na(RatingClean)),
      BlueN = sum(IsBlueChip, na.rm = TRUE),
      BlueShare = ifelse(RatedN > 0, 100 * BlueN / RatedN, NA_real_),
      MedianRating = ifelse(RatedN > 0,
                            stats::median(RatingClean, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::mutate(StateName = origin_state_name(StateClean))
}

.origin_add_metric <- function(summary, metric, min_n) {
  info <- origin_metric_info(metric, "board")
  raw <- switch(metric,
                commit_n = summary$N,
                blue_n = summary$BlueN,
                blue_share = summary$BlueShare,
                median_rating = summary$MedianRating,
                summary$BlueN)
  eligible <- !info$quality | summary$RatedN >= min_n
  summary$Value <- ifelse(eligible, raw, NA_real_)
  summary$Eligible <- eligible
  unavailable <- if (isTRUE(info$quality)) paste0("rated n<", min_n) else
    "not available"
  summary$ValueLabel <- ifelse(is.finite(summary$Value),
                               info$format(summary$Value),
                               unavailable)
  summary
}

origin_state_board <- function(d, metric = "blue_n", position = "All",
                               top_n = 15L, selected_state = NULL) {
  d <- .origin_filter_position(d, position)
  sp <- if (nrow(d)) unique(d$sport)[1] else "football"
  min_n <- origin_quality_min_n(sp, !identical(position, "All"))
  info <- origin_metric_info(metric, "board")
  s <- .origin_state_summary(d)
  s <- .origin_add_metric(s, metric, min_n) %>%
    dplyr::arrange(dplyr::desc(Value), dplyr::desc(N), StateName) %>%
    dplyr::mutate(FieldRank = dplyr::row_number())

  eligible <- s %>% dplyr::filter(is.finite(Value))
  out <- utils::head(eligible, top_n)
  if (!is.null(selected_state) && selected_state %in% eligible$StateClean &&
      !selected_state %in% out$StateClean) {
    out <- dplyr::bind_rows(
      out,
      eligible %>% dplyr::filter(StateClean == selected_state) %>%
        utils::head(1)
    )
  }
  if (!nrow(out)) return(out)

  out <- out %>%
    dplyr::mutate(
      role = ifelse(StateClean == selected_state, "selected", "field"),
      Tooltip = paste0(
        "<b>", StateName, "</b><br/>", info$label, ": ", ValueLabel,
        "<br/>", format(N, big.mark = ","), " captured signees",
        "<br/>", format(RatedN, big.mark = ","), " with supported ratings",
        "<br/>", format(BlueN, big.mark = ","), " blue chips (",
        sprintf("%.1f%%", BlueShare), ")",
        "<br/><em>Click to follow this state over time</em>"),
      Click = sprintf(
        "Shiny.setInputValue('origin_state_click','%s',{priority:'event'});",
        StateClean)
    )
  attr(out, "metric") <- metric
  attr(out, "metric_label") <- info$label
  attr(out, "min_n") <- min_n
  attr(out, "position") <- position
  attr(out, "pool_n") <- nrow(d)
  attr(out, "open_note") <- origin_open_cycle_note(d)
  out
}

origin_position_board <- function(d, metric = "blue_n", top_n = 3L) {
  d <- .origin_position_safe(d)
  sp <- if (nrow(d)) unique(d$sport)[1] else "football"
  groups <- origin_position_levels(sp)
  d <- d %>% dplyr::filter(as.character(PosGroup) %in% groups)
  min_n <- origin_quality_min_n(sp, position_filtered = TRUE)
  info <- origin_metric_info(metric, "board")

  s <- d %>%
    dplyr::group_by(PosGroup, StateClean) %>%
    dplyr::summarize(
      N = dplyr::n(),
      RatedN = sum(!is.na(RatingClean)),
      BlueN = sum(IsBlueChip, na.rm = TRUE),
      BlueShare = ifelse(RatedN > 0, 100 * BlueN / RatedN, NA_real_),
      MedianRating = ifelse(RatedN > 0,
                            stats::median(RatingClean, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::mutate(StateName = origin_state_name(StateClean))
  s <- .origin_add_metric(s, metric, min_n) %>%
    dplyr::filter(is.finite(Value)) %>%
    dplyr::group_by(PosGroup) %>%
    dplyr::arrange(dplyr::desc(Value), dplyr::desc(N), StateClean,
                   .by_group = TRUE) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      PosGroup = factor(as.character(PosGroup), levels = groups),
      ValueLabel = info$format(Value),
      Tooltip = paste0(
        "<b>", StateName, " - ", PosGroup, "</b><br/>",
        info$label, ": ", ValueLabel, "<br/>",
        format(N, big.mark = ","), " captured signees<br/>",
        format(RatedN, big.mark = ","), " with supported ratings<br/>",
        format(BlueN, big.mark = ","), " blue chips (",
        sprintf("%.1f%%", BlueShare), ")")
    )
  attr(s, "metric") <- metric
  attr(s, "metric_label") <- info$label
  attr(s, "min_n") <- min_n
  attr(s, "pool_n") <- nrow(d)
  attr(s, "open_note") <- origin_open_cycle_note(d)
  s
}

origin_year_board <- function(d, state, metric = "blue_n", position = "All",
                              today = Sys.Date()) {
  d <- .origin_filter_position(d, position)
  sp <- if (nrow(d)) unique(d$sport)[1] else "football"
  info <- origin_metric_info(metric, "trend")
  min_n <- origin_quality_min_n(sp, !identical(position, "All"), yearly = TRUE)

  pool <- d %>%
    dplyr::group_by(Year) %>%
    dplyr::summarize(
      PoolN = dplyr::n(),
      PoolBlueN = sum(IsBlueChip, na.rm = TRUE),
      PoolMedian = stats::median(RatingClean, na.rm = TRUE),
      .groups = "drop"
    )
  one <- d %>%
    dplyr::filter(StateClean == state) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarize(
      N = dplyr::n(),
      RatedN = sum(!is.na(RatingClean)),
      BlueN = sum(IsBlueChip, na.rm = TRUE),
      BlueShare = ifelse(RatedN > 0, 100 * BlueN / RatedN, NA_real_),
      MedianRating = ifelse(RatedN > 0,
                            stats::median(RatingClean, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  out <- pool %>%
    dplyr::left_join(one, by = "Year") %>%
    dplyr::mutate(
      N = tidyr::replace_na(N, 0L),
      RatedN = tidyr::replace_na(RatedN, 0L),
      BlueN = tidyr::replace_na(BlueN, 0L),
      IsOpenCycle = Year > as.integer(format(as.Date(today), "%Y")),
      StateName = origin_state_name(state)
    )
  raw <- switch(
    metric,
    commit_n = ifelse(out$PoolN > 0, 100 * out$N / out$PoolN, NA_real_),
    blue_n = ifelse(out$PoolBlueN > 0,
                    100 * out$BlueN / out$PoolBlueN, NA_real_),
    blue_share = out$BlueShare,
    median_rating = out$MedianRating - out$PoolMedian,
    ifelse(out$PoolBlueN > 0, 100 * out$BlueN / out$PoolBlueN, NA_real_)
  )
  eligible <- !info$quality | out$RatedN >= min_n
  out$Value <- ifelse(eligible, raw, NA_real_)
  out$ValueLabel <- ifelse(is.finite(out$Value), info$format(out$Value),
                           paste0("rated n<", min_n))
  out$Status <- ifelse(out$IsOpenCycle, "Open class", "Complete class")
  out$Tooltip <- paste0(
    "<b>", out$StateName, " - ", out$Year, "</b><br/>",
    info$label, ": ", out$ValueLabel, "<br/>",
    format(out$N, big.mark = ","), " state signees; rated n=",
    format(out$RatedN, big.mark = ","), "<br/>",
    format(out$PoolN, big.mark = ","), " Power-4 signees in class<br/>", out$Status)
  attr(out, "metric") <- metric
  attr(out, "metric_label") <- info$label
  attr(out, "axis_label") <- info$axis
  attr(out, "min_n") <- min_n
  attr(out, "position") <- position
  out
}

origin_position_signature <- function(d, state) {
  d <- .origin_position_safe(d)
  sp <- if (nrow(d)) unique(d$sport)[1] else "football"
  groups <- origin_position_levels(sp)
  pool <- d %>%
    dplyr::filter(as.character(PosGroup) %in% groups) %>%
    dplyr::count(PosGroup, name = "PoolN") %>%
    dplyr::mutate(PoolShare = 100 * PoolN / sum(PoolN))
  one <- d %>%
    dplyr::filter(StateClean == state,
                  as.character(PosGroup) %in% groups) %>%
    dplyr::count(PosGroup, name = "StateN") %>%
    dplyr::mutate(StateShare = 100 * StateN / sum(StateN))
  pool %>%
    dplyr::left_join(one, by = "PosGroup") %>%
    dplyr::mutate(StateN = tidyr::replace_na(StateN, 0L),
                  StateShare = tidyr::replace_na(StateShare, 0),
                  LiftPP = StateShare - PoolShare,
                  PosGroup = factor(as.character(PosGroup), levels = groups)) %>%
    dplyr::arrange(dplyr::desc(LiftPP))
}

origin_factory_board <- function(d, top_n = 5L) {
  d %>%
    dplyr::filter(nzchar(OriginSchool), !is.na(StateClean)) %>%
    dplyr::count(OriginSchool, StateClean, Location, sort = TRUE, name = "N") %>%
    dplyr::mutate(
      StateName = origin_state_name(StateClean),
      FactoryLabel = paste0(OriginSchool, " (", StateClean, ")")
    ) %>%
    utils::head(top_n)
}

origin_concentration <- function(d) {
  by_state <- d %>% dplyr::count(StateClean, name = "N")
  if (!nrow(by_state)) return(list(top4_share = NA_real_, effective_states = NA_real_))
  p <- by_state$N / sum(by_state$N)
  list(top4_share = 100 * sum(sort(p, decreasing = TRUE)[seq_len(min(4, length(p)))]),
       effective_states = 1 / sum(p^2))
}

plot_origin_state_board <- function(board, sport, selected_state = NULL) {
  if (is.null(board) || !nrow(board)) return(NULL)
  info <- origin_metric_info(attr(board, "metric") %||% "blue_n", "board")
  position <- attr(board, "position") %||% "All"
  d <- board
  d$StateAxis <- factor(d$StateName, levels = rev(d$StateName))
  selected_lab <- if (!is.null(selected_state)) origin_state_name(selected_state) else NULL
  selected_extra <- !is.null(selected_state) &&
    any(d$StateClean == selected_state & d$FieldRank > 15L)

  ggplot2::ggplot(d, ggplot2::aes(x = Value, y = StateAxis)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(fill = role, tooltip = Tooltip, data_id = StateClean,
                   onclick = Click), width = 0.66) +
    ggplot2::geom_text(ggplot2::aes(label = ValueLabel), hjust = -0.12,
                       size = 3.5, fontface = "bold", color = "#0C234B") +
    ggplot2::scale_fill_manual(values = c(field = "#0C234B",
                                          selected = "#AB0520"),
                               guide = "none") +
    ggplot2::scale_x_continuous(labels = info$format,
      expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(
      title = paste("State Talent Board -", info$label),
      subtitle = paste0(
        if (identical(position, "All")) "All position groups" else position,
        ". Top 15 state/prep locations",
        if (isTRUE(selected_extra)) paste0(" plus ", selected_lab) else "",
        "; tap or click a bar to follow that state over time."),
      x = info$axis, y = NULL,
      caption = paste0(
        "Last listed HS/prep location among captured Power-4 signees. ",
        "Obvious JUCO and unreviewed College sources excluded.",
        if (isTRUE(info$quality))
          paste0(" Quality shown only at rated n >= ", attr(board, "min_n"), ".") else "",
        if (nzchar(attr(board, "open_note") %||% ""))
          paste0(" ", attr(board, "open_note")) else "")
    ) +
    theme_girth(base_size = 13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(8, 34, 8, 8))
}

plot_origin_position_board <- function(board, sport, phone = FALSE) {
  if (is.null(board) || !nrow(board)) return(NULL)
  metric <- attr(board, "metric") %||% "blue_n"
  info <- origin_metric_info(metric, "board")
  d <- board %>%
    dplyr::mutate(RankAxis = factor(Rank, levels = c(3, 2, 1)))
  free_scale <- metric %in% c("commit_n", "blue_n")

  ggplot2::ggplot(d, ggplot2::aes(x = Value, y = RankAxis)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = Tooltip,
                   data_id = paste(PosGroup, StateClean, sep = "-")),
      width = 0.62, fill = "#0C234B") +
    ggplot2::geom_text(ggplot2::aes(x = 0, label = StateClean),
                       hjust = 1.18, size = 3.4, fontface = "bold",
                       color = "#0C234B") +
    ggplot2::geom_text(ggplot2::aes(label = ValueLabel), hjust = -0.12,
                       size = 3.1, color = "#0C234B") +
    ggplot2::facet_wrap(~PosGroup,
                        ncol = if (isTRUE(phone)) 2 else 4,
                        scales = if (free_scale) "free_x" else "fixed") +
    ggplot2::scale_x_continuous(labels = info$format,
      expand = ggplot2::expansion(mult = c(0.28, 0.22))) +
    ggplot2::labs(
      title = paste("Position Hotbeds -", info$label),
      subtitle = paste0(
        "Top three state/prep locations inside each position group",
        if (free_scale) "; each panel uses its own x-scale." else "."),
      x = info$axis, y = NULL,
      caption = paste0(
        "Captured Power-4 HS/prep signees; obvious JUCO and unreviewed ",
        "College sources excluded.",
        if (isTRUE(info$quality))
          paste0(" Cells require rated n >= ", attr(board, "min_n"), ".") else "",
        if (nzchar(attr(board, "open_note") %||% ""))
          paste0(" ", attr(board, "open_note")) else "")
    ) +
    theme_girth(base_size = if (isTRUE(phone)) 10.5 else 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"),
                   plot.margin = ggplot2::margin(8, 28, 8, 32))
}

plot_origin_trend <- function(year_board, state, sport) {
  if (is.null(year_board) || !nrow(year_board)) return(NULL)
  info <- origin_metric_info(attr(year_board, "metric") %||% "blue_n", "trend")
  d <- year_board
  finite <- d[is.finite(d$Value), , drop = FALSE]
  complete <- finite[!finite$IsOpenCycle, , drop = FALSE]
  open <- finite[finite$IsOpenCycle, , drop = FALSE]
  complete_line <- d
  complete_line$Value[complete_line$IsOpenCycle] <- NA_real_
  bridge <- NULL
  if (nrow(open)) {
    first_open <- open[which.min(open$Year), , drop = FALSE]
    prior <- d[!d$IsOpenCycle &
                 d$Year == first_open$Year[1] - 1L &
                 is.finite(d$Value), , drop = FALSE]
    if (nrow(prior)) bridge <- rbind(utils::tail(prior, 1), first_open[1, ])
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(x = Year, y = Value))
  if (any(is.finite(complete_line$Value))) {
    p <- p + ggplot2::geom_line(data = complete_line, color = "#0C234B",
                                linewidth = 1.15, na.rm = TRUE)
  }
  if (!is.null(bridge)) {
    p <- p + ggplot2::geom_line(data = bridge, color = "#0C234B",
                                linewidth = 1.05, linetype = "dashed")
  }
  p +
    ggiraph::geom_point_interactive(
      data = complete,
      ggplot2::aes(tooltip = Tooltip, data_id = Year),
      color = "#0C234B", fill = "#0C234B", shape = 21, size = 4.2,
      stroke = 1) +
    ggiraph::geom_point_interactive(
      data = open,
      ggplot2::aes(tooltip = Tooltip, data_id = Year),
      color = "#AB0520", fill = "white", shape = 21, size = 4.5,
      stroke = 1.3) +
    ggplot2::geom_text(data = finite,
                       ggplot2::aes(label = ValueLabel), vjust = -1.05,
                       size = 3.2, fontface = "bold", color = "#0C234B") +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$Year))) +
    ggplot2::scale_y_continuous(labels = info$format,
      expand = ggplot2::expansion(mult = c(0.12, 0.2))) +
    ggplot2::labs(
      title = paste(origin_state_name(state), "over time"),
      subtitle = paste0(info$label, " among captured Power-4 signees",
                        if (!identical(attr(year_board, "position"), "All"))
                          paste0(" - ", attr(year_board, "position")) else "."),
      x = "Signing class", y = attr(year_board, "axis_label") %||% info$axis,
      caption = paste0(
        "Hollow red point and dashed segment = open class. ",
        if (identical(attr(year_board, "metric"), "median_rating"))
          paste0("Raw rating edge compares the state median with that class's ",
                 "Power-4 median and is not adjusted for position mix. ") else "",
        if (isTRUE(info$quality))
          paste0("Yearly state values require rated n >= ", attr(year_board, "min_n"), ". ") else "",
        "Location is the last listed HS/prep school, not birthplace.")
    ) +
    theme_girth(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

.origin_table <- function(caption, caption_note, headers, rows) {
  esc <- htmltools::htmlEscape
  note <- if (is.null(caption_note) || !nzchar(caption_note)) "" else
    paste0(" <span class=\"twin-cap-note\">", esc(caption_note), "</span>")
  paste0(
    "<div class=\"twin-scroll\"><table class=\"twin-table\">",
    "<caption>", esc(caption), note, "</caption><thead><tr>",
    paste0("<th scope=\"col\">", esc(headers), "</th>", collapse = ""),
    "</tr></thead><tbody>", rows, "</tbody></table></div>"
  )
}

origin_state_table_html <- function(board, caption_note = NULL) {
  if (is.null(board) || !nrow(board))
    return("<p class=\"twin-empty\">No state data in this window.</p>")
  esc <- htmltools::htmlEscape
  rows <- paste0(
    "<tr><td class=\"twin-rank\">", board$FieldRank, "</td>",
    "<th scope=\"row\" class=\"twin-team\">", esc(board$StateName), "</th>",
    "<td class=\"twin-val\">", esc(board$ValueLabel), "</td>",
    "<td>", board$BlueN, " (", sprintf("%.1f%%", board$BlueShare), ")</td>",
    "<td><span class=\"twin-n\">n=", board$N, "</span></td>",
    "<td><span class=\"twin-n\">rated n=", board$RatedN, "</span></td></tr>",
    collapse = "")
  .origin_table(
    paste0(attr(board, "metric_label"), " by state - table view"),
    caption_note,
    c("Rank", "State / prep location", attr(board, "metric_label"),
      "Blue chips", "Total sample", "Rated sample"), rows)
}

origin_position_table_html <- function(board, caption_note = NULL) {
  if (is.null(board) || !nrow(board))
    return("<p class=\"twin-empty\">No position data in this window.</p>")
  esc <- htmltools::htmlEscape
  rows <- paste0(
    "<tr><th scope=\"row\" class=\"twin-team\">",
    esc(as.character(board$PosGroup)), "</th>",
    "<td class=\"twin-rank\">", board$Rank, "</td>",
    "<td>", esc(board$StateName), "</td>",
    "<td class=\"twin-val\">", esc(board$ValueLabel), "</td>",
    "<td><span class=\"twin-n\">n=", board$N, "</span></td>",
    "<td><span class=\"twin-n\">rated n=", board$RatedN, "</span></td></tr>",
    collapse = "")
  .origin_table(
    paste0(attr(board, "metric_label"), " by position - table view"),
    caption_note,
    c("Position", "Rank", "State / prep location",
      attr(board, "metric_label"), "Total sample", "Rated sample"), rows)
}

origin_trend_table_html <- function(year_board, caption_note = NULL) {
  if (is.null(year_board) || !nrow(year_board))
    return("<p class=\"twin-empty\">No trend data in this window.</p>")
  esc <- htmltools::htmlEscape
  rows <- paste0(
    "<tr><th scope=\"row\" class=\"twin-team\">", year_board$Year, "</th>",
    "<td class=\"twin-val\">", esc(year_board$ValueLabel), "</td>",
    "<td><span class=\"twin-n\">n=", year_board$N, "</span></td>",
    "<td><span class=\"twin-n\">rated n=", year_board$RatedN, "</span></td>",
    "<td>", esc(year_board$Status), "</td></tr>", collapse = "")
  .origin_table(
    paste0(unique(year_board$StateName), " - ",
           attr(year_board, "metric_label"), " over time"),
    caption_note,
    c("Class", attr(year_board, "metric_label"), "State sample",
      "Rated sample", "Status"),
    rows)
}
