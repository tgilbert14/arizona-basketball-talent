#!/usr/bin/env Rscript

## Deterministic cross-conference outcome-reference regressions.

suppressPackageStartupMessages(source("R/functions.R"))
source("R/team_config.R")
source("R/girth_functions.R")
source("R/girth_plots.R")

check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

render_outcome_girafe <- function(p, label) {
  clean <- girafe_sanitize_plot(p)
  frames <- c(list(clean$data),
              lapply(clean$layers, function(layer) layer$data))
  has_glue <- vapply(
    frames, function(data) {
      is.data.frame(data) &&
        any(vapply(data, inherits, logical(1), what = "glue"))
    }, logical(1))
  check(!any(has_glue),
        paste(label, "still has a glue-class plot-data column"))
  widget <- ggiraph::girafe(
    ggobj = clean, width_svg = 10.5, height_svg = 4.7
  )
  check(inherits(widget, "girafe"), paste(label, "did not render"))
  invisible(clean)
}

canonical_active <- function(frame, columns) {
  d <- frame[!frame$external_reference, columns, drop = FALSE]
  d <- as.data.frame(lapply(d, function(x) {
    if (inherits(x, "glue")) as.character(x) else x
  }), stringsAsFactors = FALSE)
  d <- d[order(d$slug), , drop = FALSE]
  rownames(d) <- NULL
  serialize(d, NULL, version = 3)
}

slugs <- c("arizona", "arizona-state", "baylor", "byu", "georgia")
years <- 2022:2025
win_base <- c(arizona = 7L, "arizona-state" = 6L, baylor = 8L,
              byu = 5L, georgia = 10L)
year_delta <- setNames(c(-1L, 0L, 1L, 0L), years)

team_seasons <- expand.grid(
  slug = slugs, year = years, stringsAsFactors = FALSE)
team_seasons$wins <- as.integer(
  win_base[team_seasons$slug] +
    year_delta[as.character(team_seasons$year)])
team_seasons$losses <- 12L - team_seasons$wins
team_seasons$sp_rating <- as.numeric(
  team_seasons$wins * 2 + match(team_seasons$slug, slugs))

rating_base <- c(arizona = 87, "arizona-state" = 84, baylor = 86,
                 byu = 82, georgia = 94)
size_data <- expand.grid(
  School = slugs, Year = 2019:2025, prospect = 1:6,
  stringsAsFactors = FALSE)
size_data$Ranking <- as.numeric(
  rating_base[size_data$School] +
    0.1 * (size_data$Year - min(size_data$Year)) +
    0.15 * (size_data$prospect - 3))

quadrant_columns <- c(
  "slug", "talent", "win_pct", "sp", "W", "L", "seasons_n", "best",
  "School", "TeamName", "role", "external_reference", "plot_label", "tip")
wat_columns <- c(
  "slug", "seasons_n", "W", "L", "games", "exp_wins", "talent",
  "actual", "expected", "mean_games", "wat", "School", "TeamName", "role",
  "external_reference", "value", "n", "wat_abs", "near_even", "tip", "lab")

quadrant_base <- quadrant_data(
  team_seasons, size_data, "arizona")
quadrant_external <- quadrant_data(
  team_seasons, size_data, "arizona", "georgia")
check(identical(
  canonical_active(quadrant_base, quadrant_columns),
  canonical_active(quadrant_external, quadrant_columns)),
  "Georgia changed active-conference quadrant rows")
check(identical(attr(quadrant_base, "talent_median"),
                attr(quadrant_external, "talent_median")) &&
        identical(attr(quadrant_base, "win_median"),
                  attr(quadrant_external, "win_median")),
      "Georgia changed active-conference quadrant medians")
q_ext <- quadrant_external[quadrant_external$external_reference, ]
check(nrow(q_ext) == 1 && identical(q_ext$slug, "georgia") &&
        identical(q_ext$role, "external"),
      "quadrant external row missing or mislabeled")
check(grepl("does not affect", attr(quadrant_external, "external_note"),
            fixed = TRUE),
      "quadrant external receipt missing")

quadrant_peer <- quadrant_data(
  team_seasons, size_data, "arizona", "arizona-state")
check(!isTRUE(attr(quadrant_peer, "external_reference")) &&
        identical(quadrant_peer$role[quadrant_peer$slug == "arizona-state"],
                  "compare"),
      "same-conference quadrant peer behavior regressed")

quadrant_missing <- quadrant_data(
  team_seasons, size_data[size_data$School != "georgia", ],
  "arizona", "georgia")
check(isTRUE(attr(quadrant_missing, "external_requested")) &&
        !isTRUE(attr(quadrant_missing, "external_reference")) &&
        grepl("no qualifying", attr(quadrant_missing, "external_note"),
              fixed = TRUE),
      "quadrant missing-rival receipt regressed")

quadrant_gg <- plot_talent_results(
  team_seasons, size_data, "arizona", "georgia")
quadrant_plot <- ggplot_build(quadrant_gg)
quadrant_shape <- quadrant_plot$plot$scales$get_scales("shape")
check(identical(as.integer(quadrant_shape$map(c("main", "external"))),
                c(16L, 23L)),
      "quadrant external reference lacks a distinct diamond shape")
invisible(render_outcome_girafe(quadrant_gg, "talent-results quadrant"))

wat_base <- wat_data(team_seasons, size_data, "arizona")
wat_external <- wat_data(
  team_seasons, size_data, "arizona", "georgia")
check(identical(
  canonical_active(wat_base, wat_columns),
  canonical_active(wat_external, wat_columns)),
  "Georgia changed active-conference WAT rows")
check(identical(attr(wat_base, "model_coefficients"),
                attr(wat_external, "model_coefficients")),
      "Georgia changed active-conference WAT model coefficients")

w_ext <- wat_external[wat_external$external_reference, ]
check(nrow(w_ext) == 1 && identical(w_ext$slug, "georgia") &&
        identical(w_ext$role, "external"),
      "WAT external row missing or mislabeled")
check(identical(attr(wat_external, "external_model_reference"),
                "active_conference_fit") &&
        grepl("unchanged Big 12", attr(wat_external, "external_note"),
              fixed = TRUE) &&
        grepl("unranked", attr(wat_external, "external_note"), fixed = TRUE),
      "WAT fixed-model/unranked receipt missing")

ext_comp <- talent_composites(
  size_data[size_data$School == "georgia", ], years)
ext_panel <- merge(
  team_seasons[team_seasons$slug == "georgia", ],
  ext_comp, by.x = c("slug", "year"), by.y = c("School", "year"))
beta <- attr(wat_external, "model_coefficients")
ext_panel$expected <- stats::plogis(beta[1] + beta[2] * ext_panel$composite)
manual_expected <- 100 * sum(ext_panel$expected *
                               (ext_panel$wins + ext_panel$losses)) /
  sum(ext_panel$wins + ext_panel$losses)
check(isTRUE(all.equal(w_ext$expected, manual_expected, tolerance = 1e-12)),
      "Georgia was not scored against the stored Big 12 coefficients")

wat_peer <- wat_data(
  team_seasons, size_data, "arizona", "arizona-state")
check(!isTRUE(attr(wat_peer, "external_reference")) &&
        identical(wat_peer$role[wat_peer$slug == "arizona-state"], "compare"),
      "same-conference WAT peer behavior regressed")

wat_missing <- wat_data(
  team_seasons, size_data[size_data$School != "georgia", ],
  "arizona", "georgia")
check(isTRUE(attr(wat_missing, "external_requested")) &&
        !isTRUE(attr(wat_missing, "external_reference")) &&
        grepl("no qualifying", attr(wat_missing, "external_note"), fixed = TRUE),
      "WAT missing-rival receipt regressed")

wat_gg <- plot_wat(team_seasons, size_data, "arizona", "georgia")
wat_plot <- ggplot_build(wat_gg)
wat_shape <- wat_plot$plot$scales$get_scales("shape")
wat_line <- wat_plot$plot$scales$get_scales("linetype")
check(identical(as.integer(wat_shape$map(c("main", "external"))),
                c(16L, 23L)) &&
        identical(as.character(wat_line$map("external")), "22"),
      "WAT external reference lacks shape/linetype redundancy")
invisible(render_outcome_girafe(wat_gg, "wins-above-talent ladder"))
scoreboard_gg <- plot_team_scoreboard(team_seasons, size_data, "arizona")
invisible(render_outcome_girafe(scoreboard_gg, "season scoreboard"))


app_source <- readLines("app.R", warn = FALSE)
check(any(grepl('rank_display <- rep("N/R"', app_source, fixed = TRUE)),
      "shared table twin no longer renders external rows as N/R")

cat("Outcome comparator validation passed.\n")
