## Focused Program Reach comparator regression checks.
## Run from the repository root:
##   Rscript scripts/validateReachComparator.R

suppressPackageStartupMessages(source("R/functions.R"))
source("R/team_config.R")
source("R/girth_functions.R")
source("R/talent_origins.R")
source("R/girth_plots.R")

stop_if <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

render_girafe <- function(p, label) {
  clean <- girafe_sanitize_plot(p)
  frames <- c(
    list(clean$data),
    lapply(clean$layers, function(layer) layer$data)
  )
  glue_left <- vapply(
    frames, function(data) {
      is.data.frame(data) &&
        any(vapply(data, inherits, logical(1), what = "glue"))
    }, logical(1))
  stop_if(!any(glue_left),
          paste(label, "still has a glue-class plot-data column"))
  widget <- ggiraph::girafe(
    ggobj = clean, width_svg = 10.5, height_svg = 4.7
  )
  stop_if(inherits(widget, "girafe"),
          paste(label, "did not render as a girafe widget"))
  invisible(clean)
}

glue_fixture <- data.frame(x = 1:2, y = c(2, 1),
                           id = as.character(1:2))
glue_fixture$tip <- glue::glue("<b>Point {glue_fixture$x}</b>")
glue_plot <- ggplot2::ggplot(glue_fixture, ggplot2::aes(x, y)) +
  ggiraph::geom_point_interactive(
    ggplot2::aes(tooltip = tip, data_id = id))
stop_if(inherits(glue_plot$data$tip, "glue"),
        "Synthetic tooltip lost glue class before the sanitizer test")
invisible(render_girafe(glue_plot, "synthetic glue tooltip"))

schools <- rep(c("arizona", "georgia", "arizona-state"), each = 6)
years <- rep(c(2024L, 2025L, 2026L), times = 6)
positions <- rep(c("QB", "WR", "OT", "DL", "LB", "CB"), times = 3)
pos_groups <- rep(c("QB", "WR/TE", "OL", "DL/Edge", "LB", "DB"), times = 3)
school_offset <- rep(c(0, 180, 90), each = 6)

fixture <- data.frame(
  School = schools,
  TeamName = vapply(schools, team_label, character(1)),
  Name = paste(vapply(schools, team_label, character(1)),
               positions, years),
  Year = years,
  Position = positions,
  PosGroup = pos_groups,
  Type = "Commit",
  Location = rep(c("Phoenix, AZ", "Tucson, AZ", "Dallas, TX",
                   "Atlanta, GA", "Los Angeles, CA", "Chicago, IL"), 3),
  StateClean = NA_character_,
  miles_away = c(115, 70, 880, 1530, 490, 1450) + school_offset,
  lat = rep(c(33.45, 32.22, 32.78, 33.75, 34.05, 41.88), 3),
  long = rep(c(-112.07, -110.97, -96.80, -84.39, -118.24, -87.63), 3),
  college_lat = ifelse(schools == "georgia", 33.95,
                       ifelse(schools == "arizona-state", 33.42, 32.23)),
  college_long = ifelse(schools == "georgia", -83.38,
                        ifelse(schools == "arizona-state", -111.93, -110.95)),
  HeightLabel = "6-2",
  Height = 74,
  Weight = rep(c(195, 205, 305, 275, 225, 190), 3),
  Ranking = rep(c(91, 89, 88, 92, 87, 90), 3),
  NationalRank = seq_len(18),
  stringsAsFactors = FALSE
)
fixture$lat[12] <- fixture$long[12] <- fixture$miles_away[12] <- NA_real_

roles_cross <- reach_program_data(fixture, "arizona", "georgia")
stop_if(setequal(unique(roles_cross$School), c("arizona", "georgia")),
        "Arizona-Georgia did not retain both programs")
stop_if(setequal(unique(roles_cross$ReachRole), c("Selected", "Comparison")),
        "Cross-conference roles were not explicit")

roles_peer <- reach_program_data(fixture, "arizona", "arizona-state")
stop_if(setequal(unique(roles_peer$School), c("arizona", "arizona-state")),
        "Arizona-Arizona State did not retain both programs")
stop_if(isTRUE(comparison_context("arizona", "arizona-state")$same_conference),
        "Arizona State fixture is not recognized as a same-conference peer")

roles_single <- reach_program_data(fixture, "arizona")
stop_if(identical(unique(roles_single$School), "arizona"),
        "Single-team Program Reach behavior changed")

coverage <- reach_program_coverage(fixture, "arizona", "georgia")
georgia_coverage <- coverage[coverage$School == "georgia", ]
stop_if(georgia_coverage$total == 6L && georgia_coverage$mapped == 5L &&
          georgia_coverage$distance == 5L,
        "Georgia coverage receipt is incorrect")
stop_if(length(unique(unname(reach_role_colors()))) == 2L,
        "Program Reach role colors are not distinct")

lab_cross <- plot_distance_lab(
  fixture, "arizona", "football", compare_slug = "georgia")
stop_if(inherits(lab_cross, "ggplot"), "Cross-conference distance lab did not build")
stop_if(all(c("arizona", "georgia") %in%
              unique(unlist(lapply(lab_cross$layers, function(layer) {
                if ("School" %in% names(layer$data)) as.character(layer$data$School)
                else character()
              })))),
        "Distance lab is missing a selected or comparison point series")
invisible(ggplot2::ggplot_build(lab_cross))

box_cross <- plot_distance_box(
  fixture, "arizona", "football", compare_slug = "georgia")
stop_if(inherits(box_cross, "ggplot"), "Cross-conference position distance did not build")
invisible(ggplot2::ggplot_build(box_cross))
invisible(render_girafe(lab_cross, "cross-conference distance lab"))
invisible(render_girafe(box_cross, "cross-conference position distance"))

lab_peer <- plot_distance_lab(
  fixture, "arizona", "football", compare_slug = "arizona-state")
box_peer <- plot_distance_box(
  fixture, "arizona", "football", compare_slug = "arizona-state")
invisible(ggplot2::ggplot_build(lab_peer))
invisible(ggplot2::ggplot_build(box_peer))

lab_single <- plot_distance_lab(fixture, "arizona", "football")
box_single <- plot_distance_box(fixture, "arizona", "football")
invisible(ggplot2::ggplot_build(lab_single))
invisible(ggplot2::ggplot_build(box_single))

missing_fixture <- fixture
missing_fixture$miles_away[missing_fixture$School == "georgia"] <- NA_real_
missing_fixture$lat[missing_fixture$School == "georgia"] <- NA_real_
missing_fixture$long[missing_fixture$School == "georgia"] <- NA_real_
missing_gap <- reach_comparison_gap(
  missing_fixture, "arizona", "georgia", metric = "distance")
stop_if(grepl("none has a usable", missing_gap, fixed = TRUE),
        "Missing-distance comparison receipt is not explicit")
missing_plot <- plot_distance_lab(
  missing_fixture, "arizona", "football", compare_slug = "georgia")
stop_if(inherits(missing_plot, "ggplot") &&
          grepl("none has a usable", missing_plot$labels$subtitle, fixed = TRUE),
        "Selected distance plot did not survive a missing comparison")

map_cross <- build_pipeline_map(
  fixture, "arizona", "football", compare_slug = "georgia")
map_methods <- vapply(map_cross$x$calls, function(call) call$method, character(1))
stop_if("addCircleMarkers" %in% map_methods,
        "Selected map circles are missing")
stop_if("addAwesomeMarkers" %in% map_methods,
        "Comparison star-pin markers are missing")

map_missing <- build_pipeline_map(
  missing_fixture, "arizona", "football", compare_slug = "georgia")
stop_if(inherits(map_missing, "leaflet"),
        "Map did not preserve selected-team behavior when comparison was unmapped")

if (file.exists(file.path("data", "recruiting.db"))) {
  actual_conn <- DBI::dbConnect(
    RSQLite::SQLite(), file.path("data", "recruiting.db"))
  actual_raw <- DBI::dbGetQuery(
    actual_conn,
    paste(
      "SELECT * FROM recruit_class_football",
      "WHERE School IN ('arizona','georgia','arizona-state')",
      "AND Year BETWEEN 2024 AND 2027"
    )
  )
  DBI::dbDisconnect(actual_conn)
  actual <- prep_origin_data(actual_raw, "football")
  actual_coverage <- reach_program_coverage(
    actual, "arizona", "georgia")
  stop_if(all(actual_coverage$total > 0L),
          "Released database lacks Arizona or Georgia rows")
  stop_if(all(actual_coverage$mapped > 0L) &&
            all(actual_coverage$distance > 0L),
          "Released database lacks mapped Arizona or Georgia distance rows")

  actual_lab <- plot_distance_lab(
    actual, "arizona", "football", compare_slug = "georgia")
  actual_box <- plot_distance_box(
    actual, "arizona", "football", compare_slug = "georgia")
  actual_peer <- plot_distance_lab(
    actual, "arizona", "football", compare_slug = "arizona-state")
  invisible(ggplot2::ggplot_build(actual_lab))
  invisible(ggplot2::ggplot_build(actual_box))
  invisible(render_girafe(actual_lab, "released DB distance lab"))
  invisible(render_girafe(actual_box, "released DB position distance"))
  invisible(ggplot2::ggplot_build(actual_peer))
  cat(
    "Released DB coverage:",
    paste(glue::glue(
      "{actual_coverage$ReachProgram} {actual_coverage$mapped}/{actual_coverage$total} mapped"),
      collapse = " | "),
    "\n"
  )
}
cat("Program Reach comparator validation passed.\n")
