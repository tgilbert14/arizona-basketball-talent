## ---------------------------------------------------------------------------
## home_fingerprint.R
## A dependency-light, pure-HTML "program fingerprint" for the Home tab.
##
## Public API:
##   program_fingerprint_data(...)  -> one row per fingerprint metric
##   home_program_fingerprint(...)  -> accessible htmltools tag tree
##
## The UI helper embeds its CSS through htmltools::singleton(), so it can be
## dropped into renderUI() without adding a stylesheet or a JavaScript binding.
## ---------------------------------------------------------------------------


## ---- small, private utilities ---------------------------------------------

.hf_scalar <- function(x, fallback = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) return(fallback)
  out <- trimws(as.character(x[[1]]))
  if (!nzchar(out)) fallback else out
}

.hf_int <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L) return(fallback)
  out <- suppressWarnings(as.integer(x[[1]]))
  if (length(out) == 0L || is.na(out) || out < 1L) fallback else out
}

.hf_col <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) data[[hit[[1]]]] else rep(NA, nrow(data))
}

.hf_numbers <- function(x, lower = -Inf, upper = Inf) {
  out <- suppressWarnings(as.numeric(as.character(x)))
  out[is.finite(out) & out >= lower & out <= upper]
}

.hf_mean_stat <- function(x, lower = -Inf, upper = Inf) {
  x <- .hf_numbers(x, lower, upper)
  list(value = if (length(x)) mean(x) else NA_real_, n = length(x))
}

.hf_blue_chip_stat <- function(x) {
  ## The source includes a handful of legitimate historic 101-103 elite grades.
  ## Zero is a missing-value placeholder and must not count as non-blue-chip.
  x <- .hf_numbers(x, lower = .Machine$double.eps, upper = 110)
  list(value = if (length(x)) mean(x >= 90) * 100 else NA_real_,
       n = length(x))
}

.hf_percentile <- function(x, field) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  field <- suppressWarnings(as.numeric(field))
  field <- sort(field[is.finite(field)])
  if (!is.finite(x) || length(field) < 2L) return(NA_real_)

  distinct <- unique(field)
  if (length(distinct) == 1L) {
    if (x < distinct) return(0)
    if (x > distinct) return(100)
    return(50)
  }

  ## Interpolating over ordered team values keeps a member team's minimum at
  ## 0 and maximum at 100, while also allowing a cross-conference comparison
  ## team to be located honestly against the selected team's conference.
  ranked_pct <- seq(0, 100, length.out = length(field))
  as.numeric(stats::approx(field, ranked_pct, xout = x, rule = 2,
                           ties = mean)$y)
}

.hf_team_name <- function(size_data, slug, fallback = "Selected team") {
  slug <- .hf_scalar(slug)
  if (!nzchar(slug)) return(fallback)

  if (is.data.frame(size_data) && nrow(size_data) &&
      all(c("School", "TeamName") %in% names(size_data))) {
    hit <- as.character(size_data$TeamName[as.character(size_data$School) == slug])
    hit <- hit[!is.na(hit) & nzchar(trimws(hit))]
    if (length(hit)) return(hit[[1]])
  }

  ## team_label() is available in the app, but keeping a fallback makes this
  ## file independently testable with a tiny synthetic data frame.
  label_fn <- get0("team_label", mode = "function", inherits = TRUE)
  if (!is.null(label_fn)) {
    hit <- tryCatch(.hf_scalar(label_fn(slug)), error = function(e) "")
    if (nzchar(hit)) return(hit)
  }

  tools::toTitleCase(gsub("[-_]", " ", slug))
}

.hf_height_label <- function(inches) {
  inches <- suppressWarnings(as.numeric(inches[[1]]))
  if (!is.finite(inches)) return("Not available")
  rounded <- round(inches * 4) / 4
  feet <- floor(rounded / 12)
  rem <- rounded - feet * 12
  rem_lab <- sub("\\.?0+$", "", sprintf("%.2f", rem))
  paste0(feet, "'", rem_lab, "\"")
}

.hf_value_label <- function(metric_key, value) {
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(value)) return("Not available")
  switch(
    metric_key,
    avg_rating = sprintf("%.1f", value),
    blue_chip_share = sprintf("%.0f%%", value),
    avg_weight = sprintf("%.0f lb", value),
    trench_weight = sprintf("%.0f lb", value),
    avg_height = .hf_height_label(value),
    frontcourt_height = .hf_height_label(value),
    sprintf("%.1f", value)
  )
}

.hf_ordinal <- function(x) {
  x <- round(pmin(pmax(suppressWarnings(as.numeric(x[[1]])), 0), 100))
  if (!is.finite(x)) return("percentile unavailable")
  suffix <- if (x %% 100 %in% 11:13) {
    "th"
  } else {
    switch(as.character(x %% 10), `1` = "st", `2` = "nd", `3` = "rd", "th")
  }
  paste0(x, suffix, " percentile")
}

.hf_metric_specs <- function(sport, min_team_n, subgroup_min_n) {
  common <- list(
    list(
      key = "avg_rating", label = "Average 247 rating",
      detail = "Talent quality",
      min_n = min_team_n,
      stat = function(d) .hf_mean_stat(.hf_col(d, c("Ranking", "Rating")),
                                       lower = .Machine$double.eps,
                                       upper = 110)
    ),
    list(
      key = "blue_chip_share", label = "Blue-chip share",
      detail = "Rated 90+",
      min_n = min_team_n,
      stat = function(d) .hf_blue_chip_stat(.hf_col(d, c("Ranking", "Rating")))
    ),
    list(
      key = "avg_weight", label = "Average weight",
      detail = "All additions",
      min_n = min_team_n,
      stat = function(d) .hf_mean_stat(.hf_col(d, c("Weight", "weight")),
                                       lower = 80, upper = 600)
    ),
    list(
      key = "avg_height", label = "Average height",
      detail = "All additions",
      min_n = min_team_n,
      stat = function(d) .hf_mean_stat(.hf_col(d, c("Height_in", "height_in")),
                                       lower = 55, upper = 96)
    )
  )

  if (identical(sport, "basketball")) {
    common[[5]] <- list(
      key = "frontcourt_height", label = "Frontcourt height",
      detail = "Forwards + centers",
      min_n = subgroup_min_n,
      stat = function(d) {
        group <- tolower(as.character(.hf_col(d, c("PosGroup", "Position"))))
        keep <- group %in% c("forward", "center", "f", "sf", "pf", "c")
        .hf_mean_stat(.hf_col(d, c("Height_in", "height_in"))[keep],
                      lower = 55, upper = 96)
      }
    )
  } else {
    common[[5]] <- list(
      key = "trench_weight", label = "Trench weight",
      detail = "OL + DL/Edge",
      min_n = subgroup_min_n,
      stat = function(d) {
        trench_raw <- .hf_col(d, "Trench")
        trench <- suppressWarnings(as.logical(as.character(trench_raw)))
        group <- tolower(as.character(.hf_col(d, c("PosGroup", "Position"))))
        inferred <- group %in% c("ol", "ot", "og", "oc", "iol", "t", "g",
                                 "dl/edge", "dl", "dt", "de", "edge", "nt",
                                 "sde", "wde")
        trench[is.na(trench)] <- inferred[is.na(trench)]
        .hf_mean_stat(.hf_col(d, c("Weight", "weight"))[trench %in% TRUE],
                      lower = 80, upper = 600)
      }
    )
  }
  common
}


## ---- data contract ---------------------------------------------------------

## Build the five-row fingerprint data. Percentiles are calculated from
## per-program aggregates, never from player rows, so a large class cannot
## dominate the conference benchmark. A metric needs at least min_team_n valid
## players (subgroup_min_n for trenches/frontcourt) to enter that benchmark.
program_fingerprint_data <- function(size_data, team_slug,
                                     compare_slug = NULL,
                                     conf_slugs = NULL,
                                     sport = "football",
                                     min_team_n = 3L,
                                     subgroup_min_n = 2L) {
  if (!is.data.frame(size_data)) size_data <- data.frame()
  size_data <- as.data.frame(size_data, stringsAsFactors = FALSE)
  if (!"School" %in% names(size_data)) size_data$School <- rep(NA_character_, nrow(size_data))
  size_data$School <- as.character(size_data$School)

  team_slug <- .hf_scalar(team_slug)
  compare_slug <- .hf_scalar(compare_slug)
  if (compare_slug %in% c("none", team_slug)) compare_slug <- ""
  sport <- tolower(.hf_scalar(sport, "football"))
  if (!sport %in% c("football", "basketball")) sport <- "football"
  min_team_n <- .hf_int(min_team_n, 3L)
  subgroup_min_n <- .hf_int(subgroup_min_n, 2L)

  conf_slugs <- unique(trimws(as.character(conf_slugs)))
  conf_slugs <- conf_slugs[!is.na(conf_slugs) & nzchar(conf_slugs)]
  if (!length(conf_slugs)) {
    conf_slugs <- unique(size_data$School[!is.na(size_data$School) &
                                           nzchar(size_data$School)])
  }

  specs <- .hf_metric_specs(sport, min_team_n, subgroup_min_n)

  stat_for <- function(spec, slug) {
    if (!nzchar(slug)) return(list(value = NA_real_, n = 0L))
    rows <- size_data[size_data$School == slug & !is.na(size_data$School), , drop = FALSE]
    out <- tryCatch(spec$stat(rows), error = function(e) list(value = NA_real_, n = 0L))
    value <- suppressWarnings(as.numeric(out$value[[1]]))
    n <- suppressWarnings(as.integer(out$n[[1]]))
    if (!length(value) || !is.finite(value)) value <- NA_real_
    if (!length(n) || is.na(n) || n < 0L) n <- 0L
    list(value = value, n = n)
  }

  rows <- lapply(specs, function(spec) {
    main <- stat_for(spec, team_slug)
    cmp <- stat_for(spec, compare_slug)

    peer_stats <- lapply(conf_slugs, function(slug) stat_for(spec, slug))
    peer_values <- if (length(peer_stats)) {
      vapply(peer_stats, function(x) {
        if (x$n >= spec$min_n && is.finite(x$value)) x$value else NA_real_
      }, numeric(1))
    } else numeric(0)
    peer_values <- peer_values[is.finite(peer_values)]

    main_ok <- main$n >= spec$min_n && is.finite(main$value)
    cmp_ok <- nzchar(compare_slug) && cmp$n >= spec$min_n && is.finite(cmp$value)
    data.frame(
      metric_key = spec$key,
      metric_label = spec$label,
      metric_detail = spec$detail,
      min_n = spec$min_n,
      selected_value = main$value,
      selected_n = main$n,
      selected_eligible = main_ok,
      selected_percentile = if (main_ok) .hf_percentile(main$value, peer_values) else NA_real_,
      compare_value = cmp$value,
      compare_n = cmp$n,
      compare_eligible = cmp_ok,
      compare_percentile = if (cmp_ok) .hf_percentile(cmp$value, peer_values) else NA_real_,
      benchmark_teams = length(peer_values),
      benchmark_low = if (length(peer_values)) min(peer_values) else NA_real_,
      benchmark_high = if (length(peer_values)) max(peer_values) else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "sport") <- sport
  attr(out, "team_slug") <- team_slug
  attr(out, "compare_slug") <- compare_slug
  attr(out, "conf_slugs") <- conf_slugs
  out
}


## ---- HTML presentation -----------------------------------------------------

program_fingerprint_styles <- function() {
  htmltools::singleton(htmltools::tags$style(htmltools::HTML("
.hf-card{--hf-navy:#0C234B;--hf-red:#AB0520;--hf-gold:#FFD200;--hf-ink:#15243a;--hf-muted:#64748b;background:linear-gradient(145deg,#fff 0%,#f7f9fc 72%,#fff8e6 100%);border:1px solid #dce3ec;border-radius:18px;box-shadow:0 14px 34px rgba(12,35,75,.11);color:var(--hf-ink);overflow:hidden;padding:22px 24px 18px;position:relative}
.hf-card:before{background:linear-gradient(90deg,var(--hf-red),var(--hf-gold) 48%,var(--hf-navy));content:'';height:4px;left:0;position:absolute;right:0;top:0}
.hf-head{align-items:flex-start;display:flex;gap:18px;justify-content:space-between;margin-bottom:15px}
.hf-eyebrow{color:var(--hf-red);font-size:11px;font-weight:800;letter-spacing:1.35px;margin-bottom:4px;text-transform:uppercase}
.hf-title{color:var(--hf-navy);font-family:'Rubik',sans-serif;font-size:24px;font-weight:800;letter-spacing:-.25px;line-height:1.15;margin:0}
.hf-subtitle{color:var(--hf-muted);font-size:12.5px;line-height:1.45;margin:6px 0 0}
.hf-badges{display:flex;flex-wrap:wrap;gap:6px;justify-content:flex-end}
.hf-badge{background:#eef3f8;border:1px solid #d8e0ea;border-radius:999px;color:var(--hf-navy);font-size:10.5px;font-weight:700;letter-spacing:.2px;padding:5px 9px;white-space:nowrap}
.hf-legend{align-items:center;border-bottom:1px solid #e5eaf0;display:flex;flex-wrap:wrap;gap:13px;margin-bottom:4px;padding:0 0 12px}
.hf-legend-item{align-items:center;color:#46535e;display:inline-flex;font-size:11.5px;font-weight:650;gap:7px}
.hf-key{display:inline-block;flex:0 0 auto;position:relative}
.hf-key-main{background:var(--hf-red);border:2px solid #fff;border-radius:50%;box-shadow:0 0 0 1px var(--hf-red);height:10px;width:10px}
.hf-key-compare{background:#fff;border:2px solid var(--hf-navy);height:9px;transform:rotate(45deg);width:9px}
.hf-higher{color:#8793a2;font-size:10.5px;margin-left:auto}
.hf-row{align-items:center;border-bottom:1px solid #e8edf2;display:grid;gap:16px;grid-template-columns:minmax(138px,1.15fr) minmax(190px,2.15fr) minmax(132px,1fr);min-height:70px;padding:10px 0}
.hf-row:last-child{border-bottom:0}
.hf-metric-label{color:var(--hf-navy);font-size:13px;font-weight:800;line-height:1.25}
.hf-metric-detail{color:var(--hf-muted);font-size:10.5px;margin-top:3px}
.hf-track-shell{height:29px;position:relative}
.hf-track{background:#e6ebf1;border:1px solid rgba(12,35,75,.10);border-radius:999px;box-shadow:inset 0 1px 2px rgba(12,35,75,.10);height:10px;left:0;overflow:visible;position:absolute;right:0;top:9px}
.hf-track:after{background:repeating-linear-gradient(90deg,transparent 0,transparent calc(25% - 1px),rgba(12,35,75,.14) calc(25% - 1px),rgba(12,35,75,.14) 25%);border-radius:999px;content:'';inset:0;position:absolute}
.hf-marker{display:block;position:absolute;transform:translateX(-50%);z-index:2}
.hf-marker-main{background:var(--hf-red);border:2px solid white;border-radius:50%;box-shadow:0 0 0 1px var(--hf-red),0 2px 5px rgba(12,35,75,.25);height:12px;top:-3px;width:12px}
.hf-marker-main:after{background:var(--hf-red);content:'';height:6px;left:4px;position:absolute;top:10px;width:2px}
.hf-marker-compare{background:white;border:2px solid var(--hf-navy);box-shadow:0 2px 4px rgba(12,35,75,.2);height:9px;top:10px;transform:translateX(-50%) rotate(45deg);width:9px}
.hf-track-empty{background:#f2f4f7;border:1px dashed #c9d1dc;border-radius:999px;color:#7c8795;font-size:10.5px;line-height:26px;text-align:center}
.hf-readout{text-align:right}
.hf-value{color:var(--hf-navy);font-size:14px;font-weight:850;line-height:1.2}
.hf-pct{color:var(--hf-red);display:block;font-size:10.5px;font-weight:750;margin-top:2px}
.hf-pct-limited{color:#7c8795}
.hf-compare-readout{color:var(--hf-navy);font-size:10.5px;font-weight:700;margin-top:3px}
.hf-sample{color:#8a949c;font-size:9.5px;margin-top:3px}
.hf-note{align-items:flex-start;color:#6d7886;display:flex;font-size:10.5px;gap:7px;line-height:1.45;margin:10px 0 0}
.hf-note-dot{background:var(--hf-gold);border-radius:50%;flex:0 0 auto;height:7px;margin-top:4px;width:7px}
@media(max-width:991px){.hf-card{border-radius:14px;padding:19px 15px 14px}.hf-head{display:block}.hf-badges{justify-content:flex-start;margin-top:10px}.hf-title{font-size:21px}.hf-row{gap:5px 10px;grid-template-columns:1fr auto;padding:12px 0}.hf-track-shell{grid-column:1/-1;grid-row:2}.hf-readout{min-width:120px}.hf-higher{flex-basis:100%;margin-left:0}.hf-legend{gap:10px}}
")))
}

.hf_row_tag <- function(row, team_name, compare_name = "") {
  metric_key <- as.character(row$metric_key[[1]])
  main_value <- .hf_value_label(metric_key, row$selected_value)
  main_has_pct <- isTRUE(row$selected_eligible[[1]]) &&
    is.finite(row$selected_percentile[[1]])
  compare_requested <- nzchar(compare_name)
  compare_has_pct <- compare_requested &&
    isTRUE(row$compare_eligible[[1]]) &&
    is.finite(row$compare_percentile[[1]])

  main_pct <- if (main_has_pct) .hf_ordinal(row$selected_percentile) else {
    if (is.finite(row$selected_value[[1]]) && row$selected_n[[1]] < row$min_n[[1]]) {
      paste0("Limited sample (need ", row$min_n[[1]], ")")
    } else if (isTRUE(row$selected_eligible[[1]]) && row$benchmark_teams[[1]] < 2L) {
      "Not enough conference teams"
    } else {
      "No qualifying sample"
    }
  }

  compare_value <- .hf_value_label(metric_key, row$compare_value)
  compare_pct <- if (compare_has_pct) .hf_ordinal(row$compare_percentile) else ""
  compare_status <- if (!compare_requested) {
    ""
  } else if (compare_has_pct) {
    compare_pct
  } else if (is.finite(row$compare_value[[1]]) &&
             row$compare_n[[1]] < row$min_n[[1]]) {
    paste0("percentile withheld; limited sample n=", row$compare_n[[1]],
           ", need ", row$min_n[[1]])
  } else if (isTRUE(row$compare_eligible[[1]]) &&
             row$benchmark_teams[[1]] < 2L) {
    "percentile withheld; not enough conference teams"
  } else {
    paste0("percentile withheld; no qualifying sample, n=",
           row$compare_n[[1]])
  }

  aria <- paste0(
    row$metric_label[[1]], ". ", team_name, ": ", main_value,
    ", sample ", row$selected_n[[1]], ", ", main_pct, ". ",
    if (compare_requested) paste0(compare_name, ": ", compare_value, ", ",
                                  compare_status, ". ") else "",
    "Benchmark: ", row$benchmark_teams[[1]], " conference programs."
  )

  track <- if (main_has_pct || compare_has_pct) {
    htmltools::tags$div(
      class = "hf-track-shell", `aria-hidden` = "true",
      htmltools::tags$div(
        class = "hf-track",
        if (main_has_pct) htmltools::tags$span(
          class = "hf-marker hf-marker-main",
          style = sprintf("left:%.1f%%", pmin(pmax(row$selected_percentile[[1]], 1), 99)),
          title = paste(team_name, main_value, .hf_ordinal(row$selected_percentile)),
          `aria-hidden` = "true"),
        if (compare_has_pct) htmltools::tags$span(
          class = "hf-marker hf-marker-compare",
          style = sprintf("left:%.1f%%", pmin(pmax(row$compare_percentile[[1]], 1), 99)),
          title = paste(compare_name, compare_value, .hf_ordinal(row$compare_percentile)),
          `aria-hidden` = "true")
      )
    )
  } else {
    htmltools::tags$div(
      class = "hf-track-shell hf-track-empty",
      `aria-hidden` = "true", "Percentile withheld"
    )
  }

  htmltools::tags$div(
    class = "hf-row", role = "group", `aria-label` = aria,
    htmltools::tags$div(
      class = "hf-metric",
      htmltools::tags$div(class = "hf-metric-label", row$metric_label[[1]]),
      htmltools::tags$div(class = "hf-metric-detail", row$metric_detail[[1]])
    ),
    track,
    htmltools::tags$div(
      class = "hf-readout",
      htmltools::tags$div(class = "hf-value", main_value),
      htmltools::tags$span(
        class = paste("hf-pct", if (!main_has_pct) "hf-pct-limited" else ""),
        main_pct),
      if (compare_requested) htmltools::tags$div(
        class = paste("hf-compare-readout",
                      if (!compare_has_pct) "hf-pct-limited" else ""),
        if (compare_has_pct) {
          paste0(compare_name, ": ", compare_value, " · ", compare_pct)
        } else {
          paste0(compare_name, ": ", compare_value,
                 " · percentile withheld · n=", row$compare_n[[1]])
        }),
      htmltools::tags$div(
        class = "hf-sample",
        paste0("n=", row$selected_n[[1]], " · ", row$benchmark_teams[[1]],
               if (row$benchmark_teams[[1]] == 1L) " team benchmark" else " team benchmark"))
    )
  )
}

## Render the Home card. `size_data` should already reflect the global year and
## player-type window. `conf_slugs` defines the percentile universe; a compare
## team outside that vector is still placed against that same universe.
home_program_fingerprint <- function(size_data, team_slug,
                                     compare_slug = NULL,
                                     conf_slugs = NULL,
                                     sport = "football",
                                     sport_label = NULL,
                                     window_label = NULL,
                                     conference_label = "conference",
                                     min_team_n = 3L,
                                     subgroup_min_n = 2L,
                                     title = "Program fingerprint") {
  frame <- program_fingerprint_data(
    size_data = size_data,
    team_slug = team_slug,
    compare_slug = compare_slug,
    conf_slugs = conf_slugs,
    sport = sport,
    min_team_n = min_team_n,
    subgroup_min_n = subgroup_min_n
  )

  sport <- attr(frame, "sport")
  team_slug <- attr(frame, "team_slug")
  compare_slug <- attr(frame, "compare_slug")
  team_name <- .hf_team_name(size_data, team_slug)
  compare_name <- if (nzchar(compare_slug)) {
    .hf_team_name(size_data, compare_slug, "Comparison team")
  } else ""
  sport_label <- .hf_scalar(sport_label, tools::toTitleCase(sport))
  window_label <- .hf_scalar(window_label)
  conference_label <- .hf_scalar(conference_label, "conference")
  title <- .hf_scalar(title, "Program fingerprint")

  row_tags <- lapply(seq_len(nrow(frame)), function(i) {
    .hf_row_tag(frame[i, , drop = FALSE], team_name, compare_name)
  })

  htmltools::tagList(
    program_fingerprint_styles(),
    htmltools::tags$section(
      class = "hf-card",
      `aria-label` = paste(title, "for", team_name),
      htmltools::tags$header(
        class = "hf-head",
        htmltools::tags$div(
          htmltools::tags$div(class = "hf-eyebrow", "Conference DNA"),
          htmltools::tags$h3(class = "hf-title", title),
          htmltools::tags$p(
            class = "hf-subtitle",
            paste0("Where ", team_name, " sits across five program traits — ",
                   "each marker uses per-team ", conference_label, " percentiles.")
          )
        ),
        htmltools::tags$div(
          class = "hf-badges", `aria-label` = "Fingerprint scope",
          htmltools::tags$span(class = "hf-badge", sport_label),
          if (nzchar(window_label)) htmltools::tags$span(class = "hf-badge", window_label)
        )
      ),
      htmltools::tags$div(
        class = "hf-legend", `aria-label` = "Marker legend",
        htmltools::tags$span(
          class = "hf-legend-item",
          htmltools::tags$span(class = "hf-key hf-key-main", `aria-hidden` = "true"),
          team_name
        ),
        if (nzchar(compare_name)) htmltools::tags$span(
          class = "hf-legend-item",
          htmltools::tags$span(class = "hf-key hf-key-compare", `aria-hidden` = "true"),
          compare_name
        ),
        htmltools::tags$span(class = "hf-higher", "Conference low  ←  percentile  →  high")
      ),
      htmltools::tags$div(class = "hf-metrics", row_tags),
      htmltools::tags$p(
        class = "hf-note",
        htmltools::tags$span(class = "hf-note-dot", `aria-hidden` = "true"),
        htmltools::tags$span(
          "This is a profile, not a grade: more height or weight is not always better. ",
          "Metrics with thin samples are shown without a percentile marker."
        )
      )
    )
  )
}
## ---- direct cross-conference matchup -------------------------------------

## Build a side-by-side frame without inventing a blended Power-4 rank. Each
## program is evaluated against its own conference, while displayed values stay
## directly comparable.
program_matchup_data <- function(size_data, team_slug, compare_slug,
                                 team_conf_slugs = NULL,
                                 compare_conf_slugs = NULL,
                                 sport = "football",
                                 min_team_n = 3L,
                                 subgroup_min_n = 2L) {
  team_slug <- .hf_scalar(team_slug)
  compare_slug <- .hf_scalar(compare_slug)
  if (!nzchar(team_slug) || !nzchar(compare_slug) || identical(team_slug, compare_slug)) {
    return(data.frame())
  }

  own <- program_fingerprint_data(
    size_data = size_data, team_slug = team_slug,
    conf_slugs = team_conf_slugs, sport = sport,
    min_team_n = min_team_n, subgroup_min_n = subgroup_min_n
  )
  peer <- program_fingerprint_data(
    size_data = size_data, team_slug = compare_slug,
    conf_slugs = compare_conf_slugs, sport = sport,
    min_team_n = min_team_n, subgroup_min_n = subgroup_min_n
  )

  keep <- c("metric_key", "metric_label", "metric_detail", "min_n",
            "selected_value", "selected_n", "selected_eligible",
            "selected_percentile", "benchmark_teams")
  own <- own[, keep, drop = FALSE]
  peer <- peer[, keep, drop = FALSE]
  names(own)[names(own) != "metric_key"] <- paste0("team_", names(own)[names(own) != "metric_key"])
  names(peer)[names(peer) != "metric_key"] <- paste0("compare_", names(peer)[names(peer) != "metric_key"])
  out <- merge(own, peer, by = "metric_key", sort = FALSE)
  out <- out[match(own$metric_key, out$metric_key), , drop = FALSE]
  out$delta <- out$compare_selected_value - out$team_selected_value
  out
}

.hf_matchup_conf_label <- function(slug, fallback = "Conference") {
  conf_fn <- get0("team_conference", mode = "function", inherits = TRUE)
  conf <- if (!is.null(conf_fn)) tryCatch(conf_fn(slug), error = function(e) NA_character_) else NA_character_
  .hf_scalar(conf, fallback)
}

.hf_matchup_logo <- function(slug, name) {
  logo_fn <- get0("team_logo", mode = "function", inherits = TRUE)
  src <- if (!is.null(logo_fn)) {
    tryCatch(.hf_scalar(logo_fn(slug, prefix = "")),
             error = function(e) tryCatch(.hf_scalar(logo_fn(slug)),
                                          error = function(e2) ""))
  } else ""
  if (nzchar(src)) htmltools::tags$img(src = src, alt = "") else htmltools::tags$span(substr(name, 1, 1))
}

.hf_matchup_delta <- function(metric_key, delta) {
  delta <- suppressWarnings(as.numeric(delta[[1]]))
  if (!is.finite(delta)) return("Insufficient data")
  sign <- if (delta > 0) "+" else if (delta < 0) "−" else "="
  mag <- abs(delta)
  switch(
    metric_key,
    avg_rating = sprintf("%s%.1f rating", sign, mag),
    blue_chip_share = sprintf("%s%.0f pp", sign, mag),
    avg_weight = sprintf("%s%.0f lb", sign, mag),
    trench_weight = sprintf("%s%.0f lb", sign, mag),
    avg_height = sprintf("%s%.1f in", sign, mag),
    frontcourt_height = sprintf("%s%.1f in", sign, mag),
    sprintf("%s%.1f", sign, mag)
  )
}

## Render a concise matchup card for Home and the dedicated Matchup route.
## It deliberately surfaces primary facts before offering detail, so comparison
## adds orientation rather than a wall of data.
home_program_matchup <- function(size_data, team_slug, compare_slug,
                                 team_conf_slugs = NULL,
                                 compare_conf_slugs = NULL,
                                 sport = "football",
                                 sport_label = NULL,
                                 window_label = NULL,
                                 min_team_n = 3L,
                                 subgroup_min_n = 2L,
                                 cta_id = NULL,
                                 cta_label = "Open full comparison") {
  frame <- program_matchup_data(
    size_data = size_data, team_slug = team_slug, compare_slug = compare_slug,
    team_conf_slugs = team_conf_slugs, compare_conf_slugs = compare_conf_slugs,
    sport = sport, min_team_n = min_team_n, subgroup_min_n = subgroup_min_n
  )
  if (!nrow(frame)) return(NULL)

  team_name <- .hf_team_name(size_data, team_slug)
  compare_name <- .hf_team_name(size_data, compare_slug, "Comparison team")
  team_conf <- .hf_matchup_conf_label(team_slug)
  compare_conf <- .hf_matchup_conf_label(compare_slug)
  sport_label <- .hf_scalar(sport_label, tools::toTitleCase(sport))

  cross_conference <- !identical(team_conf, compare_conf)
  footer_text <- if (cross_conference) {
    paste0("A cross-conference selection is an external reference: it never changes ",
           team_conf, " averages, ranks, or outcome calibration.")
  } else {
    paste0("This is an in-conference peer comparison on the shared ",
           team_conf, " reference field.")
  }

  window_label <- .hf_scalar(window_label)

  metric_tags <- lapply(seq_len(nrow(frame)), function(i) {
    row <- frame[i, , drop = FALSE]
    team_pct <- if (isTRUE(row$team_selected_eligible[[1]]) && is.finite(row$team_selected_percentile[[1]])) {
      .hf_ordinal(row$team_selected_percentile[[1]])
    } else "Percentile withheld"
    compare_pct <- if (isTRUE(row$compare_selected_eligible[[1]]) && is.finite(row$compare_selected_percentile[[1]])) {
      .hf_ordinal(row$compare_selected_percentile[[1]])
    } else "Percentile withheld"
    htmltools::tags$article(
      class = "gi-matchup-metric",
      htmltools::tags$div(class = "gi-matchup-metric__label", row$team_metric_label[[1]]),
      htmltools::tags$div(class = "gi-matchup-metric__detail", row$team_metric_detail[[1]]),
      htmltools::tags$div(
        class = "gi-matchup-metric__values",
        htmltools::tags$div(
          class = "gi-matchup-metric__team gi-matchup-metric__team--main",
          htmltools::tags$strong(.hf_value_label(row$metric_key[[1]], row$team_selected_value[[1]])),
          htmltools::tags$span(team_pct)
        ),
        htmltools::tags$div(
          class = "gi-matchup-metric__delta",
          .hf_matchup_delta(row$metric_key[[1]], row$delta[[1]])
        ),
        htmltools::tags$div(
          class = "gi-matchup-metric__team gi-matchup-metric__team--compare",
          htmltools::tags$strong(.hf_value_label(row$metric_key[[1]], row$compare_selected_value[[1]])),
          htmltools::tags$span(compare_pct)
        )
      )
    )
  })

  htmltools::tags$section(
    class = "gi-matchup",
    `aria-label` = paste("Direct comparison between", team_name, "and", compare_name),
    htmltools::tags$header(
      class = "gi-matchup__head",
      htmltools::tags$div(
        htmltools::tags$div(class = "gi-matchup__eyebrow", "Direct Power-4 comparison"),
        htmltools::tags$h2(paste(team_name, "vs", compare_name)),
        htmltools::tags$p("Direct recruiting facts side by side; conference percentiles remain calibrated to each program's own league.")
      ),
      htmltools::tags$div(
        class = "gi-matchup__badges",
        htmltools::tags$span(class = "gi-matchup__badge", sport_label),
        if (nzchar(window_label)) htmltools::tags$span(class = "gi-matchup__badge", window_label)
      )
    ),
    htmltools::tags$div(
      class = "gi-matchup__teams",
      htmltools::tags$div(
        class = "gi-matchup-team gi-matchup-team--main",
        htmltools::tags$div(class = "gi-matchup-team__logo", .hf_matchup_logo(team_slug, team_name)),
        htmltools::tags$div(htmltools::tags$strong(team_name), htmltools::tags$span(team_conf))
      ),
      htmltools::tags$div(class = "gi-matchup__versus", "VS"),
      htmltools::tags$div(
        class = "gi-matchup-team gi-matchup-team--compare",
        htmltools::tags$div(class = "gi-matchup-team__logo", .hf_matchup_logo(compare_slug, compare_name)),
        htmltools::tags$div(htmltools::tags$strong(compare_name), htmltools::tags$span(compare_conf))
      )
    ),
    htmltools::tags$div(class = "gi-matchup__grid", metric_tags),
    htmltools::tags$footer(
      class = "gi-matchup__footer",
      htmltools::tags$p(icon("circle-info"), footer_text),
      if (!is.null(cta_id) && nzchar(cta_id)) shiny::actionButton(
        cta_id, cta_label, class = "btn-primary", icon = icon("arrow-right"))
    )
  )
}
