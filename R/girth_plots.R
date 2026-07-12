## ---------------------------------------------------------------------------
## girth_plots.R
## Plot builders for the Size Lab. Used by BOTH the Shiny app and
## scripts/girth_analysis.R (static insight pack) so the visuals stay in sync.
## All builders take data prepared by prep_size_data().
## ---------------------------------------------------------------------------

## hometown display: 'From: NA' looks broken on a card -- show a dash for
## players whose location hasn't been scraped/geocoded yet
loc_dash <- function(loc) {
  ifelse(is.na(loc) | loc == "" | loc == "NA", "—", loc)
}

## 247Sports link per player. HS commits live on their class-year recruits
## page, so the season search works; TRANSFERS don't (they're filed under
## their original HS class, not the portal year), and 247's portal pages
## ignore name filters -- a site-scoped Google search reliably lands their
## profile instead. When the scraper has captured a real profile URL,
## `profile_url` short-circuits both fallbacks: pass it (NA-safe, vectorized)
## and any non-empty entry is returned directly, absolutized if relative
## (247 name-link hrefs come protocol-relative, "//247sports.com/Player/...").
p247_url <- function(name, year, sport, type = "Commit", profile_url = NULL) {
  enc <- vapply(as.character(name),
                function(n) utils::URLencode(n, reserved = TRUE),
                character(1), USE.NAMES = FALSE)
  ## recycle type up front: ifelse() sizes its result by the TEST, so a
  ## scalar default type with vector names would collapse fallback to 1
  type <- rep_len(as.character(type), length(enc))
  fallback <- ifelse(type == "Transfer",
                     paste0("https://www.google.com/search?q=site%3A247sports",
                            ".com+%22", enc, "%22"),
                     paste0("https://247sports.com/season/", year, "-",
                            tolower(sport), "/recruits/?&Player.FullName=",
                            enc))
  if (is.null(profile_url)) return(fallback)
  ## same trap on the profile side: a scalar NA profile_url with vector
  ## names must not shrink the result -- recycle first, then size the
  ## output by the fallback and overwrite only where a real URL exists
  pu <- rep_len(as.character(profile_url), length(fallback))
  pu[pu %in% c("", "NA")] <- NA_character_
  ## protocol-relative first ("//host/..."), then root-relative ("/path")
  pu <- ifelse(!is.na(pu) & startsWith(pu, "//"), paste0("https:", pu), pu)
  pu <- ifelse(!is.na(pu) & startsWith(pu, "/") & !startsWith(pu, "//"),
               paste0("https://247sports.com", pu), pu)
  out <- fallback
  keep <- !is.na(pu) & nzchar(pu)
  out[keep] <- pu[keep]
  out
}

## per-row ProfileUrl column if the frame carries one -- older dbs don't have
## the column at all, so callers guard with this instead of assuming it
## (atomic [[ on a missing name errors)
profile_col <- function(d) {
  if ("ProfileUrl" %in% names(d)) as.character(d$ProfileUrl)
  else rep(NA_character_, nrow(d))
}

## a player name that opens the holographic PLAYER CARD when its hover card
## is pinned (the app's JS listens for taps on .pc-open and asks the server
## for the full player record)
pc_link <- function(name, school) {
  paste0("<span class=\"pc-open\" data-pname=\"", name,
         "\" data-pschool=\"", school, "\">", name, " &#9656;</span>")
}

## standard player-scope line appended to chart captions so every plot says
## which player pool it shows (HS commits / + transfers / transfers only)
scope_note <- function(players_note) {
  if (is.null(players_note) || players_note == "") return("")
  paste0(" Showing: ", players_note, ".")
}

## metric metadata: db column -> axis label + value formatter
girth_metrics <- list(
  AvgWeight   = list(label = "Average Weight (lbs)",
                     fmt = function(x) paste0(round(x, 0), " lbs")),
  AvgHeight   = list(label = "Average Height",
                     fmt = function(x) format_height(x)),
  AvgLbsPerIn = list(label = "Pounds per Inch of Height",
                     fmt = function(x) sprintf("%.2f", x)),
  AvgBMI      = list(label = "Average BMI",
                     fmt = function(x) sprintf("%.1f", x))
)

## metric -> player-level column (for hover cards)
metric_player_col <- c(AvgWeight = "Weight", AvgHeight = "Height_in",
                       AvgLbsPerIn = "LbsPerInch", AvgBMI = "BMI")

## "1. Name (POS, '24) — 320 lbs" hover-card lines for any dot/bar
top_players_tip <- function(d, value_col, n = 3,
                            fmt = function(v) paste0(round(v), " lbs"),
                            desc = TRUE, header = NULL, school = NULL) {
  if (is.null(d) || nrow(d) == 0) return(header %||% "")
  pool <- d %>% filter(!is.na(.data[[value_col]]))
  d2 <- pool %>%
    arrange(if (desc) dplyr::desc(.data[[value_col]]) else .data[[value_col]]) %>%
    slice_head(n = n)
  if (nrow(d2) == 0) return(header %||% "")
  ## group_modify drops grouping cols, so School/Year may be absent here.
  ## Year especially: a missing column returns NULL, and paste0 with a
  ## zero-length vector silently EMPTIES every line -- guard both.
  ## `school` lets group_modify callers pass their group key (.y$School) so
  ## player-card lookups stay school-scoped on conference-wide boards --
  ## a name-only lookup can open the wrong school's player.
  sch <- school %||% (if ("School" %in% names(d2)) d2$School else "")
  yr_part <- if ("Year" %in% names(d2)) {
    paste0(", '", substr(d2$Year, 3, 4))
  } else ""
  lines <- paste0(seq_len(nrow(d2)), ". ",
                  pc_link(d2$Name, sch), " (", d2$Position, yr_part, ") — ",
                  vapply(d2[[value_col]], function(v) as.character(fmt(v)),
                         character(1)))
  ## receipts carry their sample size: "top 3 of 27" tells the reader how
  ## deep the pool behind this aggregate really is (and "all 2 shown"
  ## flags a thin one)
  n_note <- if (nrow(pool) > nrow(d2)) {
    paste0("<em>top ", nrow(d2), " of ", nrow(pool), "</em>")
  } else if (nrow(pool) > 0 && !is.null(header)) {
    paste0("<em>all ", nrow(pool), " shown</em>")
  }
  ## the app's pin JS appends a universal "tap a name" hint to any pinned
  ## card containing player links, so no per-tip hint needed here
  paste(c(header, lines, n_note), collapse = "<br/>")
}

## filter helper shared by leaderboard/trend: "All", "Trenches", or a group
filter_pos <- function(size_data, pos_filter) {
  if (is.null(pos_filter) || pos_filter == "All") return(size_data)
  if (pos_filter == "Trenches (OL + DL/Edge)") {
    return(dplyr::filter(size_data, Trench))
  }
  dplyr::filter(size_data, as.character(PosGroup) == pos_filter)
}

pos_filter_label <- function(pos_filter) {
  if (is.null(pos_filter) || pos_filter == "All") "All Positions" else pos_filter
}

## ---------------------------------------------------------------------------
## 1) BODY MAP -- height x weight scatter, conference cloud + team highlight
## ---------------------------------------------------------------------------
plot_body_map <- function(size_data, team_slug, sport, year_min = NULL,
                          year_max = NULL, pos_keep = NULL,
                          logo_path = NULL, players_note = NULL) {
  if (!is.null(year_min)) {
    size_data <- dplyr::filter(size_data, Year >= year_min, Year <= year_max)
  }
  ## pool only the active team's conference members (all 16 at Phase 0)
  size_data <- scope_to_conf(size_data, team_slug)
  team_data  <- dplyr::filter(size_data, School == team_slug)

  ## legend keys stay stable across filtering (colors don't shift)
  all_groups <- intersect(position_levels(sport),
                          as.character(unique(team_data$PosGroup)))

  ## position isolation applies to EVERYTHING -- the conference cloud and
  ## the median guides too, so it's a true position-vs-position comparison
  pos_note <- ""
  if (!is.null(pos_keep) && length(pos_keep) > 0) {
    size_data <- dplyr::filter(size_data,
                               as.character(PosGroup) %in% pos_keep)
    team_data <- dplyr::filter(team_data,
                               as.character(PosGroup) %in% pos_keep)
    pos_note <- glue(" — {paste(pos_keep, collapse = ', ')} only")
  }
  other_data <- dplyr::filter(size_data, School != team_slug)

  med_h <- median(size_data$Height_in, na.rm = TRUE)
  med_w <- median(size_data$Weight, na.rm = TRUE)
  t_col <- team_color(team_slug)
  t_lab <- team_label(team_slug)
  yr_rng <- paste0(min(size_data$Year), "–", max(size_data$Year))
  conf_lab <- conf_label(team_slug)
  ## realignment honesty: the grey cloud is the current conference membership,
  ## so a window reaching before it was whole shows programs that were not yet
  ## in the league. Count + seam year come from the config, not 16/2024.
  whole_yr <- conf_whole_year(team_slug)
  bc_note <- if (min(size_data$Year) < whole_yr) {
    glue(" Grey cloud = the current {n_conf_members(team_slug)} members, ",
         "backcast before {whole_yr}.")
  } else ""

  ## hover text for the interactive version (scraped profile URL when the
  ## frame carries one; search fallback otherwise)
  team_data$.p247 <- p247_url(team_data$Name, team_data$Year, sport,
                              team_data$Type, profile_col(team_data))
  team_data <- team_data %>%
    mutate(tip = glue(
      "<b>{pc_link(Name, School)}</b> ({Position}, {Year})<br/>",
      "{HeightLabel} • {Weight} lbs • {LbsPerInch} lbs/in<br/>",
      "From: {loc_dash(Location)}<br/>",
      "247 Rating: {ifelse(is.na(Ranking), 'unrated', round(Ranking, 0))}<br/>",
      "<a href=\"{.p247}\" target=\"_blank\">",
      "Open on 247Sports →</a>"
    ))

  ## corner tags relative to conference medians (fan-friendly quadrants);
  ## HEIGHT is vertical (tall = up) and WEIGHT is horizontal (wide = right).
  ## only shown for the all-positions view -- a single-position filter skews
  ## the panel and the tags can land on the wrong side of the medians
  pad_w <- diff(range(size_data$Weight)) * 0.02
  corners <- if (is.null(pos_keep) || length(pos_keep) == 0) {
    data.frame(
      x = c(min(size_data$Weight) + pad_w, max(size_data$Weight) - pad_w,
            min(size_data$Weight) + pad_w, max(size_data$Weight) - pad_w),
      y = c(max(size_data$Height_in), max(size_data$Height_in),
            min(size_data$Height_in), min(size_data$Height_in)),
      lab = c("SKYSCRAPERS", "GIANTS", "JITTERBUGS", "BOWLING BALLS"),
      hjust = c(0, 1, 0, 1)
    )
  } else data.frame(x = numeric(0), y = numeric(0),
                    lab = character(0), hjust = numeric(0))

  p <- ggplot(size_data, aes(x = Weight, y = Height_in))

  ## subtle team-logo watermark in the lower-right corner
  if (!is.null(logo_path) && file.exists(logo_path) &&
      requireNamespace("png", quietly = TRUE)) {
    img <- png::readPNG(logo_path)
    if (length(dim(img)) == 3) {
      a <- if (dim(img)[3] == 4) img[, , 4] * 0.12 else
        matrix(0.12, nrow(img), ncol(img))
      img <- abind_alpha(img, a)
      xr <- range(size_data$Weight, na.rm = TRUE)
      yr2 <- range(size_data$Height_in, na.rm = TRUE)
      p <- p + annotation_raster(
        img,
        xmin = xr[2] - diff(xr) * 0.12, xmax = xr[2] - diff(xr) * 0.01,
        ymin = yr2[1], ymax = yr2[1] + diff(yr2) * 0.2)
    }
  }

  ## memory/speed diet for the deployed 1GB worker: the grey cloud reads the
  ## same at ~1,200 points -- medians/ranges above are computed from the FULL
  ## data before this visual-only downsample
  if (nrow(other_data) > 1200) {
    set.seed(7)
    other_data <- other_data[sample.int(nrow(other_data), 1200), ]
  }

  p +
    ## the conference "body cloud" -- heights come in whole/half inches, so a
    ## touch of VERTICAL jitter turns the rows into a readable cloud
    geom_point(data = other_data, color = "grey80", alpha = 0.4, size = 1.7,
               position = position_jitter(width = 0, height = 0.28, seed = 7)) +
    ## conference medians as quadrant guides
    geom_vline(xintercept = med_w, linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = med_h, linetype = "dashed", color = "grey40") +
    geom_text(data = corners, aes(x = x, y = y, label = lab, hjust = hjust),
              inherit.aes = FALSE, size = 3.4, fontface = "bold",
              color = "grey55") +
    ## the selected team, colored by position group
    geom_point_interactive(
      data = team_data,
      aes(fill = PosGroup, tooltip = tip, data_id = Name),
      ## dark stroke keeps light fills (TE yellow, DB grey) visible on white
      shape = 21, color = "grey30", size = 4.2, stroke = 0.6, alpha = 0.95,
      position = position_jitter(width = 0, height = 0.12, seed = 7)
    ) +
    ## limits keep colors stable when positions are isolated via the filter
    scale_fill_manual(values = pos_group_palette(sport), name = NULL,
                      limits = all_groups) +
    scale_y_continuous(labels = function(x) format_height(x),
                       breaks = seq(60, 90, 2)) +
    labs(
      title = glue("{t_lab} {str_to_title(sport)} Body Map ({yr_rng}{pos_note})"),
      subtitle = glue(
        "{conf_lab} players (grey cloud) vs {t_lab} (colored by position). ",
        "Tall is up, heavy is right; dashed lines = conference medians",
        "{ifelse(pos_note == '', '', ' for this position')}.{bc_note}"),
      x = "Weight (lbs)", y = "Height",
      caption = paste0("Data: 247Sports. Tap or hover dots for player details.", scope_note(players_note))
    ) +
    theme_girth() +
    ## let the legend wrap on narrow screens (a forced single row clips)
    guides(fill = guide_legend(override.aes = list(size = 4)))
}

## ---------------------------------------------------------------------------
## 2) BEEF BOARD -- conference leaderboard for a size metric, logos on axis
## ---------------------------------------------------------------------------

## sprintf-style formats for the table twins (girth_metrics$fmt closures
## build display strings; tables want a bare numeric format)
girth_metric_sprintf <- c(AvgWeight = "%.1f", AvgHeight = "%.1f",
                          AvgLbsPerIn = "%.2f", AvgBMI = "%.1f")

## TABLE TWIN: the EXACT per-school frame plot_beef_board() draws -- the
## plot builder calls this internally so chart and table can never disagree.
## Contract columns: School, value, n (+ the chart's own extras).
## attrs: value_label (human metric label), value_fmt (sprintf format),
## value_fmt_fn (the chart's formatter closure -- AvgHeight renders 6'4.5"),
## yr_rng, conf_avg. logo_prefix/source_label/players_note are accepted for
## signature parity with the plot builder; they are cosmetic and ignored.
beef_board_data <- function(size_data, team_slug, sport,
                            metric = "AvgWeight", pos_filter = "All",
                            year_min = NULL, year_max = NULL,
                            compare_slug = NULL, logo_prefix = "www/",
                            source_label = NULL, players_note = NULL) {
  if (!is.null(year_min)) {
    size_data <- dplyr::filter(size_data, Year >= year_min, Year <= year_max)
  }
  ## pool only the active team's conference members (all 16 at Phase 0)
  size_data <- scope_to_conf(size_data, team_slug)
  size_data <- filter_pos(size_data, pos_filter)
  m <- girth_metrics[[metric]]
  ## NULL-safe compare slug (School == NULL inside case_when errors)
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug

  ## hover card per team dot: the top players driving that average,
  ## metric-aware and following the active position filter
  pcol <- metric_player_col[[metric]]
  tips <- size_data %>%
    group_by(School) %>%
    group_modify(~ data.frame(tip = top_players_tip(
      .x, pcol, n = 3, fmt = m$fmt, school = .y$School,
      header = glue("<b>{team_label(.y$School)} — top 3, ",
                    "{tolower(pos_filter_label(pos_filter))}</b>")))) %>%
    ungroup()

  board <- team_size_summary(size_data) %>%
    left_join(tips, by = "School") %>%
    mutate(Value = .data[[metric]]) %>%
    arrange(Value) %>%
    mutate(
      TeamName = factor(TeamName, levels = TeamName),
      role = case_when(School == team_slug ~ "main",
                       School == cmp_safe ~ "compare",
                       TRUE ~ "other"),
      ## understated class-size chip, same style as the sibling boards
      val_lab = paste0(m$fmt(Value), "  (n=", Players, ")"),
      value = Value,
      n = Players
    )
  attr(board, "value_label") <- m$label
  attr(board, "value_fmt") <- girth_metric_sprintf[[metric]]
  attr(board, "value_fmt_fn") <- m$fmt
  attr(board, "yr_rng") <- paste0(min(size_data$Year), "–",
                                  max(size_data$Year))
  attr(board, "conf_avg") <- mean(board$Value)
  board
}

plot_beef_board <- function(size_data, team_slug, sport,
                            metric = "AvgWeight", pos_filter = "All",
                            year_min = NULL, year_max = NULL,
                            compare_slug = NULL, logo_prefix = "www/",
                            source_label = NULL, players_note = NULL) {
  ## single source of truth: the chart draws exactly the table twin's frame
  board <- beef_board_data(size_data, team_slug, sport, metric = metric,
                           pos_filter = pos_filter, year_min = year_min,
                           year_max = year_max, compare_slug = compare_slug)
  m <- girth_metrics[[metric]]
  yr_rng <- attr(board, "yr_rng")
  context <- source_label %||%
    glue("{str_to_title(sport)} commits {yr_rng}")
  hl <- highlight_colors(team_slug, compare_slug)
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  conf_avg <- attr(board, "conf_avg")
  logos <- team_logo_labels(width = 30, prefix = logo_prefix)
  ## rows never overlap on a board, so the compare team keeps its true
  ## primary color (the similar-hue fallback is for overlapping charts)
  role_cols <- c(main = unname(hl["main"]),
                 compare = ifelse(is.null(compare_slug), "grey60",
                                  team_color(compare_slug)),
                 other = "grey60")

  ggplot(board, aes(x = Value, y = TeamName)) +
    geom_vline(xintercept = conf_avg, linetype = "dotted",
               color = "grey45", linewidth = 0.8) +
    ## sticks anchor at the conference average so length honestly encodes
    ## "lbs above/below the league" (a floating baseline exaggerates spread)
    geom_segment(aes(x = conf_avg, xend = Value, yend = TeamName,
                     color = role), linewidth = 1.4, show.legend = FALSE) +
    geom_point_interactive(aes(color = role, tooltip = tip,
                               data_id = School),
                           size = 5, show.legend = FALSE) +
    ## hjust/expansion match the sibling boards -- the n-chip makes these
    ## labels longer, so they need the wider right margin retention uses
    geom_text(aes(label = val_lab, color = role),
              hjust = -0.15, size = 3.6, fontface = "bold",
              show.legend = FALSE) +
    annotate("text", x = conf_avg, y = 0.6,
             label = glue("{conf_label(team_slug)} avg: {m$fmt(conf_avg)}"),
             size = 3.3, color = "grey35", hjust = -0.05, fontface = "italic") +
    scale_color_manual(values = role_cols) +
    scale_y_discrete(labels = logos) +
    scale_x_continuous(
      expand = expansion(mult = c(0.01, 0.28)),
      labels = if (metric == "AvgHeight") function(x) format_height(x) else waiver()
    ) +
    labs(
      title = wrap_title(
        glue("{conf_label(team_slug)} Beef Board — {pos_filter_label(pos_filter)}"), 38),
      subtitle = wrap_title(glue(
        "{m$label}, {context} ",
        "({team_label(team_slug)}",
        "{ifelse(cmp_safe == '', '', paste0(' vs ', team_label(cmp_safe)))}",
        " highlighted)"), 52),
      x = m$label, y = NULL,
      caption = if (!is.null(source_label)) {
        wrap_title(paste(
          "Roster weights are CURRENT (after college S&C) while commit-class",
          "weights are from signing day — expect the roster to run heavier",
          "(see Weight Room). The year window doesn't apply to the current",
          "roster. Tap or hover a dot for the top players."), 95)
      } else {
        paste0("Tap or hover a dot for the top players behind the number. ",
               "Data: 247Sports.", scope_note(players_note))
      }
    ) +
    ## bare theme (not theme_girth): keeps element_markdown alive on ggplot2 4.x
    theme_girth_md()
}

## ---------------------------------------------------------------------------
## 3) SIZE OVER TIME -- team trajectory vs the conference band
## ---------------------------------------------------------------------------
plot_size_trend <- function(size_data, team_slug, sport,
                            metric = "AvgWeight", pos_filter = "All",
                            compare_slug = NULL, show_eras = TRUE, players_note = NULL) {
  size_data <- filter_pos(size_data, pos_filter)
  m <- girth_metrics[[metric]]
  metric_col <- c(AvgWeight = "Weight", AvgHeight = "Height_in",
                  AvgLbsPerIn = "LbsPerInch", AvgBMI = "BMI")[[metric]]
  hl <- highlight_colors(team_slug, compare_slug)
  has_cmp <- !is.na(hl["compare"])

  ## band = the middle half of TEAM class averages, so it lives on the same
  ## scale as the team lines drawn over it. (A player-level band mixes
  ## units -- individual spread vs team means -- and makes every team look
  ## mid-pack; the era timeline already does this correctly.)
  ## band pools only the active team's conference (all 16 at Phase 0); the
  ## team + compare lines stay unscoped so a cross-conference compare still
  ## draws (design: g_compare is full-P4 while the band is conference-scoped)
  conf_band <- scope_to_conf(size_data, team_slug) %>%
    group_by(School, Year) %>%
    summarize(team_val = mean(.data[[metric_col]], na.rm = TRUE),
              .groups = "drop") %>%
    group_by(Year) %>%
    summarize(
      p25 = quantile(team_val, 0.25, na.rm = TRUE),
      p50 = median(team_val, na.rm = TRUE),
      p75 = quantile(team_val, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  team_slugs <- c(team_slug, if (has_cmp) compare_slug)
  m_fmt <- girth_metrics[[metric]]$fmt
  team_lines <- size_data %>%
    filter(School %in% team_slugs) %>%
    group_by(School, TeamName, Year) %>%
    summarize(val = mean(.data[[metric_col]], na.rm = TRUE),
              n = n(), .groups = "drop")

  ## hover card per dot: that team-year's top players on this metric
  line_tips <- size_data %>%
    filter(School %in% team_slugs) %>%
    group_by(School, Year) %>%
    group_modify(~ data.frame(tip = top_players_tip(
      .x, metric_col, n = 3, fmt = m_fmt, school = .y$School,
      header = glue("<b>{team_label(.y$School)} {.y$Year} — top 3</b>")))) %>%
    ungroup()
  team_lines <- team_lines %>% left_join(line_tips, by = c("School", "Year"))

  t_lab <- team_label(team_slug)
  line_cols <- setNames(c(unname(hl["main"]),
                          if (has_cmp) unname(hl["compare"])),
                        c(t_lab, if (has_cmp) team_label(compare_slug)))

  p <- ggplot() +
    geom_ribbon(data = conf_band, aes(x = Year, ymin = p25, ymax = p75),
                fill = "#F0E442", alpha = 0.45) +
    geom_line(data = conf_band, aes(x = Year, y = p50),
              color = "grey45", linetype = "dashed", linewidth = 0.9) +
    geom_line(data = team_lines,
              aes(x = Year, y = val, color = TeamName), linewidth = 1.5) +
    geom_point_interactive(
      data = team_lines,
      aes(x = Year, y = val, color = TeamName, size = n,
          tooltip = tip, data_id = paste(School, Year))) +
    scale_color_manual(values = line_cols, name = NULL) +
    ## pool-neutral (matches this chart's own "players added" caption): n
    ## counts the selected pool, which can include portal transfers
    scale_size_continuous(range = c(2.5, 6.5), name = "Players in class",
                          breaks = function(lims) unique(round(pretty(lims)))) +
    scale_x_continuous(breaks = seq(min(size_data$Year),
                                    max(size_data$Year), 1)) +
    scale_y_continuous(labels = if (metric == "AvgHeight") {
      function(x) format_height(x)
    } else waiver()) +
    labs(
      title = wrap_title(glue("{t_lab}: {m$label} by Recruiting Class"), 58),
      subtitle = wrap_title(glue(
        "{pos_filter_label(pos_filter)} — {str_to_title(sport)} vs the {conf_label(team_slug)} middle ",
        "(yellow band = middle half of team class averages, dashed = median team)"), 84),
      x = "Class Year", y = m$label,
      caption = paste0("Dot size = players added that class. Coach labels = era of the highlighted team. Data: 247Sports.", scope_note(players_note))
    ) +
    theme_girth()

  ## coach-era transitions for the MAIN team (vlines + names along the top)
  if (show_eras) {
    eras <- team_eras(team_slug, sport)
    if (!is.null(eras) && nrow(eras) > 1) {
      yr_min <- min(size_data$Year); yr_max <- max(size_data$Year)
      eras <- eras %>%
        mutate(first_class = pmax(first_class, yr_min),
               last_class = pmin(last_class, yr_max)) %>%
        filter(first_class <= last_class)
      y_rng <- range(c(conf_band$p25, conf_band$p75, team_lines$val),
                     na.rm = TRUE)
      eras$y_lab <- y_rng[2] + diff(y_rng) * 0.07
      cuts <- eras$first_class[-1] - 0.5
      p <- p +
        geom_vline(xintercept = cuts, color = hl["main"],
                   linetype = "dotted", alpha = 0.55) +
        geom_text(data = eras,
                  aes(x = (first_class + last_class) / 2, y = y_lab,
                      label = coach),
                  size = 3.1, color = "grey25",
                  fontface = "italic", vjust = 0)
    }
  }
  p
}

## ---------------------------------------------------------------------------
## 4) POSITION DNA -- conference violins by position group + team overlay
## ---------------------------------------------------------------------------
plot_position_dna <- function(size_data, team_slug, sport,
                              year_min = NULL, year_max = NULL,
                              compare_slug = NULL, players_note = NULL) {
  if (!is.null(year_min)) {
    size_data <- dplyr::filter(size_data, Year >= year_min, Year <= year_max)
  }
  size_data <- dplyr::filter(size_data, as.character(PosGroup) != "Other")
  ## violins pool only the active team's conference (all 16 at Phase 0); the
  ## team + compare overlays stay unscoped so a cross-conference compare draws
  conf_pool <- scope_to_conf(size_data, team_slug)
  team_data <- dplyr::filter(size_data, School == team_slug)
  hl <- highlight_colors(team_slug, compare_slug)
  has_cmp <- !is.na(hl["compare"])
  t_lab <- team_label(team_slug)
  yr_rng <- paste0(min(size_data$Year), "–", max(size_data$Year))

  team_means <- team_data %>%
    group_by(PosGroup) %>%
    summarize(Weight = mean(Weight), .groups = "drop")

  team_data$.p247 <- p247_url(team_data$Name, team_data$Year, sport,
                              team_data$Type, profile_col(team_data))
  team_data <- team_data %>%
    mutate(tip = glue(
      "<b>{pc_link(Name, School)}</b> ({Position}, {Year})<br/>",
      "{HeightLabel} • {Weight} lbs • 247 Rating: {round(Ranking, 0)}<br/>",
      '<a href="{.p247}" ',
      'target="_blank">Open on 247Sports →</a><br/>',
      "<em>Tap the dot to pin this card</em>"))

  ## crossbar draws UNDER the interactive dots so hover/tap targets near the
  ## median (the densest region) stay clickable
  p <- ggplot(conf_pool, aes(x = PosGroup, y = Weight)) +
    geom_violin(fill = "grey85", color = "grey60", alpha = 0.9, scale = "width") +
    stat_summary(fun = median, geom = "crossbar", width = 0.55,
                 color = "grey35", linewidth = 0.4) +
    geom_point_interactive(
      data = team_data,
      aes(tooltip = tip, data_id = Name),
      color = hl["main"], alpha = 0.55, size = 2.4,
      position = position_jitter(width = 0.13, seed = 7)) +
    geom_point(data = team_means, aes(y = Weight), color = hl["main"],
               shape = 18, size = 5.5)

  cmp_note <- ""
  if (has_cmp) {
    cmp_means <- size_data %>%
      filter(School == compare_slug) %>%
      group_by(PosGroup) %>%
      summarize(Weight = mean(Weight), .groups = "drop")
    p <- p + geom_point(data = cmp_means, aes(y = Weight),
                        color = hl["compare"], shape = 5, size = 5,
                        stroke = 1.2)
    cmp_note <- glue(" Open diamond = {team_label(compare_slug)} average.")
  }

  p +
    labs(
      title = wrap_title(glue("Position DNA: {t_lab} vs the {conf_label(team_slug)} ({yr_rng})"), 55),
      subtitle = wrap_title(glue(
        "Grey violins = conference weight distribution by position group. ",
        "{t_lab} commits = colored dots, filled diamond = {t_lab} average.",
        "{cmp_note} Grey bar = conference median."), 88),
      x = NULL, y = "Weight (lbs)",
      caption = paste0("Data: 247Sports recruiting profiles.", scope_note(players_note))
    ) +
    theme_girth()
}

## ---------------------------------------------------------------------------
## 5) HEAD TO HEAD -- two teams, average weight by position group (dumbbell)
## ---------------------------------------------------------------------------
plot_head_to_head <- function(size_data, team1, team2, sport,
                              year_min = NULL, year_max = NULL,
                              source_label = NULL, players_note = NULL) {
  if (!is.null(year_min)) {
    size_data <- dplyr::filter(size_data, Year >= year_min, Year <= year_max)
  }
  size_data <- dplyr::filter(size_data, as.character(PosGroup) != "Other")
  yr_rng <- paste0(min(size_data$Year), "–", max(size_data$Year))
  context <- source_label %||%
    glue("{str_to_title(sport)} classes {yr_rng}")

  h2h <- size_data %>%
    filter(School %in% c(team1, team2)) %>%
    group_by(School, TeamName, PosGroup) %>%
    summarize(AvgWeight = mean(Weight), n = n(), .groups = "drop")

  ## hover card per dot: that team's heaviest players in the group
  h2h_tips <- size_data %>%
    filter(School %in% c(team1, team2)) %>%
    group_by(School, PosGroup) %>%
    group_modify(~ data.frame(tip = top_players_tip(
      .x, "Weight", n = 3, school = .y$School,
      header = glue("<b>{team_label(.y$School)} {.y$PosGroup} — heaviest</b>")))) %>%
    ungroup()

  wide <- h2h %>%
    select(PosGroup, School, AvgWeight) %>%
    pivot_wider(names_from = School, values_from = AvgWeight) %>%
    filter(!is.na(.data[[team1]]), !is.na(.data[[team2]])) %>%
    left_join(h2h_tips %>% filter(School == team1) %>%
                select(PosGroup, t1_tip = tip), by = "PosGroup") %>%
    left_join(h2h_tips %>% filter(School == team2) %>%
                select(PosGroup, t2_tip = tip), by = "PosGroup") %>%
    mutate(diff = .data[[team1]] - .data[[team2]],
           edge_lab = case_when(
             round(abs(diff), 0) == 0 ~ "Even",
             diff > 0 ~ glue("{team_label(team1)} +{round(diff, 0)}"),
             TRUE     ~ glue("{team_label(team2)} +{round(-diff, 0)}")))

  hl <- highlight_colors(team1, team2)
  cols <- setNames(c(unname(hl["main"]), unname(hl["compare"])),
                   c(team_label(team1), team_label(team2)))

  ggplot(wide, aes(y = PosGroup)) +
    geom_segment(aes(x = .data[[team1]], xend = .data[[team2]],
                     yend = PosGroup), color = "grey70", linewidth = 1.3) +
    geom_point_interactive(
      aes(x = .data[[team1]], color = team_label(team1),
          tooltip = t1_tip, data_id = paste0("t1-", PosGroup)), size = 5.5) +
    geom_point_interactive(
      aes(x = .data[[team2]], color = team_label(team2),
          tooltip = t2_tip, data_id = paste0("t2-", PosGroup)), size = 5.5) +
    geom_text(aes(x = pmax(.data[[team1]], .data[[team2]]), label = edge_lab),
              hjust = -0.2, size = 3.4, fontface = "bold", color = "grey30") +
    scale_color_manual(values = cols, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.22))) +
    scale_y_discrete(limits = rev) +
    labs(
      title = wrap_title(glue("{team_label(team1)} vs {team_label(team2)}: ",
                              "Who Has the Bigger Bodies?"), 55),
      subtitle = wrap_title(glue("Average weight by position group, ",
                                 "{context}"), 80),
      x = "Average Weight (lbs)", y = NULL,
      caption = paste0("Label = weight edge in lbs. Data: 247Sports.", scope_note(players_note))
    ) +
    theme_girth()
}

## normalized name-join key: lowercase, alpha-only, then a trailing
## generational suffix stripped so "Troy Ford Jr." and "Troy Ford" resolve to
## the same "troyford". Mirrors scripts/weeklyBrief.R's name_key + strip_gen
## (longest suffix alternative first so "iii" never half-matches as "ii").
## Used ONLY as a join key -- display names are never touched.
norm_name_key <- function(x) {
  k <- tolower(gsub("[^a-z]", "", tolower(x)))
  sub("(iii|ii|iv|jr|sr|v)$", "", k)
}

## ---------------------------------------------------------------------------
## 6) WEIGHT ROOM EFFECT -- join commit-day weight to current roster weight
##    (matched players = a program's own HS signees still on the roster)
## ---------------------------------------------------------------------------
weight_room_data <- function(size_data, roster_data) {
  ## suffix-stripped join key on BOTH sides (see norm_name_key). Drop roster's
  ## Name from the carried columns so the join keeps size_data's display name
  ## (a "Jr." on either side no longer splits a matched player into a miss).
  roster_clean <- roster_data %>%
    mutate(RosterWeight = suppressWarnings(as.numeric(Weight)),
           RosterHeight_in = parse_height(Height),
           key = norm_name_key(Name)) %>%
    filter(!is.na(RosterWeight), RosterWeight >= 120) %>%
    distinct(key, School, .keep_all = TRUE) %>%
    select(School, key, RosterWeight, RosterHeight_in, Class, RosterYear)

  signees <- size_data %>%
    mutate(key = norm_name_key(Name)) %>%
    distinct(key, School, .keep_all = TRUE)

  ## the name match itself, BEFORE the measurement sanity filter (which drops
  ## typo weights, not match misses) -- so match_note reports true join quality
  matched <- signees %>%
    inner_join(roster_clean, by = c("School", "key"))

  out <- matched %>%
    mutate(WeightGain = RosterWeight - Weight,
           ## a 2021 signee has had ~5 S&C years, a 2025 signee months --
           ## per-year gain is the fair strength-program comparison
           YearsOn = pmax(suppressWarnings(as.numeric(RosterYear)) - Year, 1),
           GainPerYr = round(WeightGain / YearsOn, 1),
           ## listed-height change: negative = "shrunk" on the roster, i.e.
           ## the recruiting profile was optimistic
           HeightDelta = ifelse(
             abs(RosterHeight_in - Height_in) <= 4,
             RosterHeight_in - Height_in, NA_real_)) %>%
    ## drop junk joins / typo measurements
    filter(WeightGain > -40, WeightGain < 90)

  ## scope receipt (NOT a scrape-quality score): the gains average only over
  ## signees still on a current roster. A low share over a wide window is
  ## graduation / portal / NFL departures, not a broken join -- phrase it so
  ## it can never read as "the scrape missed (100-P)%".
  n_signees <- nrow(signees)
  n_matched <- nrow(matched)
  attr(out, "match_note") <- if (n_signees > 0) {
    glue("gains cover the {n_matched} of {n_signees} windowed signees ",
         "({round(100 * n_matched / n_signees)}%) still on a current roster; ",
         "the rest graduated, transferred, or turned pro")
  } else NA_character_
  out
}

## ---------------------------------------------------------------------------
## 6a-2) CLASS RETENTION -- the portal-era staff question: how much of each
##       class did we keep? HS signees name-matched to the current roster;
##       the unmatched are the attrition.
## ---------------------------------------------------------------------------
## TABLE TWIN: the EXACT per-school frame plot_class_retention() draws.
## Contract columns: School, value, n (+ the chart's own extras).
## attrs: value_label, value_fmt, conf_avg (weighted, from per-class counts),
## cls_years. logo_prefix is accepted for signature parity; it is ignored.
retention_board_data <- function(size_commits, roster_data, team_slug,
                                 compare_slug = NULL, logo_prefix = "www/") {
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## suffix-stripped key on BOTH sides so "Troy Ford Jr." is not counted as a
  ## departure from "Troy Ford" (see norm_name_key)
  nkey <- norm_name_key

  ## pool only the active team's conference members (all 16 at Phase 0)
  size_commits <- scope_to_conf(size_commits, team_slug)
  roster_data  <- scope_to_conf(roster_data, team_slug)

  roster_year <- suppressWarnings(
    max(as.numeric(roster_data$RosterYear), na.rm = TRUE))
  ## classes old enough to be enrolled but young enough to still have
  ## eligibility: the last four completed cycles before the current one.
  ## The newest class is CAPPED at the arriving class -- class of N enrolls
  ## fall N, and during an active recruiting cycle the db can hold classes
  ## a year ahead of the calendar (2027 signees in July 2026). A class that
  ## has not enrolled yet cannot be on any roster; uncapped it would count
  ## as all-departed and deflate every school's retention.
  arriving_class <- as.integer(format(Sys.Date(), "%Y"))
  newest_cls <- min(roster_year - 1, arriving_class)
  cls_years <- (newest_cls - 3):newest_cls

  ros_keys <- roster_data %>%
    transmute(School, key = nkey(Name)) %>%
    distinct()

  pool <- size_commits %>%
    filter(Type == "Commit", Year %in% cls_years) %>%
    mutate(key = nkey(Name)) %>%
    left_join(ros_keys %>% mutate(on_roster = TRUE),
              by = c("School", "key"))

  per_class <- pool %>%
    group_by(School, TeamName, Year) %>%
    summarize(kept = sum(!is.na(on_roster)), n = n(), .groups = "drop") %>%
    mutate(pct = round(100 * kept / n))

  board <- per_class %>%
    group_by(School, TeamName) %>%
    ## tip BEFORE n: summarize() evaluates sequentially, so assigning n first
    ## would make the tip's per-class denominators show the 4-year total
    summarize(tip = paste0(
                "<b>", first(TeamName), " — class retention</b><br/>",
                paste0(Year, ": ", kept, "/", n, " (", pct, "%)",
                       collapse = "<br/>"),
                "<br/><em>Tap the dot to pin this card</em>"),
              retention = 100 * sum(kept) / sum(n), n = sum(n),
              .groups = "drop") %>%
    arrange(retention) %>%
    mutate(TeamName = factor(TeamName, levels = TeamName),
           role = case_when(School == team_slug ~ "main",
                            School == cmp_safe ~ "compare",
                            TRUE ~ "other"),
           lab = glue("{round(retention)}%  (n={n})"),
           value = retention)

  attr(board, "value_label") <- "% of signees still on the roster"
  attr(board, "value_fmt") <- "%.0f"
  ## the chart's formatter: the twin renders "78%" instead of bare "78.0"
  attr(board, "value_fmt_fn") <- function(v) paste0(round(v), "%")
  attr(board, "conf_avg") <- 100 * sum(per_class$kept) / sum(per_class$n)
  attr(board, "cls_years") <- cls_years
  ## no match_note on retention: "still on the roster" IS this board's metric
  ## (the subtitle's retention rate), so a "matched P%" receipt would just
  ## restate it under a name that reads like a scrape-quality score. The
  ## suffix-normalized join key is the real fix and needs no receipt here.
  attr(board, "match_note") <- NA_character_
  board
}

plot_class_retention <- function(size_commits, roster_data, team_slug,
                                 compare_slug = NULL, logo_prefix = "www/") {
  hl <- highlight_colors(team_slug, compare_slug)
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## single source of truth: the chart draws exactly the table twin's frame
  board <- retention_board_data(size_commits, roster_data, team_slug,
                                compare_slug = compare_slug)
  cls_years <- attr(board, "cls_years")
  conf_avg <- attr(board, "conf_avg")
  logos <- team_logo_labels(width = 30, prefix = logo_prefix)
  role_cols <- c(main = unname(hl["main"]),
                 compare = ifelse(cmp_safe == "", "grey60",
                                  team_color(cmp_safe)),
                 other = "grey60")

  ggplot(board, aes(x = retention, y = TeamName)) +
    geom_vline(xintercept = conf_avg, linetype = "dotted", color = "grey45") +
    geom_segment(aes(x = conf_avg, xend = retention, yend = TeamName,
                     color = role), linewidth = 1.4, show.legend = FALSE) +
    geom_point_interactive(aes(color = role, tooltip = tip,
                               data_id = School),
                           size = 5, show.legend = FALSE) +
    geom_text(aes(label = lab, color = role), hjust = -0.15, size = 3.4,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = role_cols) +
    scale_y_discrete(labels = logos) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.2))) +
    labs(
      title = wrap_title("Class Retention: Who Keeps Their Signees?", 40),
      subtitle = wrap_title(glue(
        "Share of {min(cls_years)}–{max(cls_years)} HS signees still on the ",
        "current roster (conference avg: {round(conf_avg)}%). ",
        "{team_label(team_slug)}",
        "{ifelse(cmp_safe == '', '', paste0(' vs ', team_label(cmp_safe)))}",
        " highlighted."), 58),
      x = "% of Signees Still on the Roster", y = NULL,
      caption = wrap_title(paste0(
        "Name-matched to 247Sports roster pages; departures include the ",
        "portal, the NFL, medicals, and early graduation. Tap or hover a dot ",
        "for the class-by-class breakdown."), 95)
        ## NB: no "join quality %" line here -- on THIS board the roster match
        ## IS the metric (the retention rate the subtitle already prints), so a
        ## second "matched P%" line would restate it under a misleading name.
        ## The suffix-normalized join key (norm_name_key) is the real fix; it
        ## needs no caption.
    ) +
    theme_girth_md()
}

## ---------------------------------------------------------------------------
## 6b) MEASUREMENT REALITY CHECK -- recruiting profiles list optimistic
##     heights; compare listed height at commit vs the current roster
## ---------------------------------------------------------------------------
height_check_stats <- function(wr_data, team_slug) {
  ## pool only the active team's conference members (all 16 at Phase 0)
  wr_data <- scope_to_conf(wr_data, team_slug)
  hd <- wr_data %>% filter(!is.na(HeightDelta))
  team <- hd %>% filter(School == team_slug)
  shrunk <- function(d) if (nrow(d) == 0) NA else
    round(100 * mean(d$HeightDelta < 0), 0)
  biggest <- team %>%
    slice_min(HeightDelta, n = 1, with_ties = FALSE)
  list(
    pct_shrunk_conf = shrunk(hd),
    pct_shrunk_team = shrunk(team),
    n_team = nrow(team),
    biggest_shrinker = if (nrow(biggest) == 1 && biggest$HeightDelta < 0) {
      glue("{biggest$Name} ({biggest$Position}): listed {biggest$HeightLabel} ",
           "as a recruit, {format_height(biggest$RosterHeight_in)} on the roster ",
           "({biggest$HeightDelta}\")")
    } else NA_character_
  )
}

plot_height_check <- function(wr_data, team_slug, sport) {
  ## pool only the active team's conference members (all 16 at Phase 0)
  wr_data <- scope_to_conf(wr_data, team_slug)
  hd <- wr_data %>%
    filter(!is.na(HeightDelta)) %>%
    mutate(bin = round(HeightDelta * 2) / 2)   # half-inch bins
  hl <- highlight_colors(team_slug)
  t_lab <- team_label(team_slug)

  conf_dist <- hd %>% count(bin) %>% mutate(pct = 100 * n / sum(n))
  team_dist <- hd %>% filter(School == team_slug) %>%
    count(bin) %>% mutate(pct = 100 * n / sum(n))

  ggplot() +
    geom_col(data = conf_dist, aes(x = bin, y = pct),
             fill = "grey80", width = 0.42) +
    geom_col(data = team_dist, aes(x = bin, y = pct),
             fill = hl["main"], alpha = 0.75, width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey35") +
    annotate("text", x = -0.6, y = max(conf_dist$pct) * 1.04,
             label = "← 'shrunk' on the roster", hjust = 1,
             size = 3.4, color = "grey35", fontface = "italic") +
    annotate("text", x = 0.6, y = max(conf_dist$pct) * 1.04,
             label = "grew / measured taller →", hjust = 0,
             size = 3.4, color = "grey35", fontface = "italic") +
    scale_x_continuous(breaks = seq(-4, 4, 1),
                       labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "\"")) +
    labs(
      title = "The Measurement Reality Check",
      subtitle = glue(
        "Current roster height minus 247Sports commit-day listed height ",
        "({str_to_title(sport)}).\nGrey bars = all {conf_label(team_slug)} matched signees; ",
        "narrow colored bars = {t_lab}."),
      x = "Listed Height Change Since Commitment", y = "% of Players",
      caption = paste("Recruiting heights are often optimistic; roster heights",
                      "aren't gospel either. Treat all listed sizes as ±1 inch.")
    ) +
    theme_girth()
}

## TABLE TWIN: the EXACT per-school frame plot_weight_room_board() draws.
## Contract columns: School, value, n (+ the chart's own extras).
## attrs: value_label (direction-aware), value_fmt, conf_avg (player-level
## mean, NOT the mean of team means). logo_prefix is accepted for signature
## parity; it is ignored.
wr_board_data <- function(wr_data, team_slug, sport,
                          compare_slug = NULL, logo_prefix = "www/",
                          direction = "gain") {
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## capture the name-match receipt BEFORE scoping (dplyr::filter drops
  ## attributes), then pool only the active team's conference (all 16 at Phase 0)
  mn_note <- attr(wr_data, "match_note")
  wr_data <- scope_to_conf(wr_data, team_slug)
  gain_fmt <- function(v) paste0(ifelse(v >= 0, "+", ""), round(v, 1),
                                 " lbs/yr")
  ## loss-mode tips must come from actual slimmers, not "smallest gainers"
  tips_src <- if (direction == "gain") wr_data else
    wr_data %>% filter(WeightGain < 0)
  tips <- tips_src %>%
    group_by(School) %>%
    group_modify(~ data.frame(tip = top_players_tip(
      .x, "GainPerYr", n = 3, fmt = gain_fmt, school = .y$School,
      desc = (direction == "gain"),
      header = glue("<b>{team_label(.y$School)} — ",
                    "{ifelse(direction == 'gain', 'top gainers',
                             'biggest slim-downs')}</b>")))) %>%
    ungroup()

  ## gain mode ranks average lbs added PER YEAR on campus (raw totals just
  ## rank roster age-mix); loss mode ranks lbs trimmed among slimmers
  board <- if (direction == "gain") {
    wr_data %>%
      group_by(School, TeamName) %>%
      summarize(AvgGain = mean(GainPerYr), n = n(), .groups = "drop") %>%
      mutate(lab = glue("+{round(AvgGain, 1)} lbs/yr  (n={n})"))
  } else {
    wr_data %>%
      filter(WeightGain < 0) %>%
      group_by(School, TeamName) %>%
      summarize(AvgGain = mean(-WeightGain), n = n(), .groups = "drop") %>%
      mutate(lab = glue("−{round(AvgGain, 1)} lbs  (n={n})"))
  }
  board <- board %>%
    left_join(tips, by = "School") %>%
    arrange(AvgGain) %>%
    mutate(TeamName = factor(TeamName, levels = TeamName),
           role = case_when(School == team_slug ~ "main",
                            School == cmp_safe ~ "compare",
                            TRUE ~ "other"),
           value = AvgGain)

  attr(board, "value_label") <- if (direction == "gain") {
    "Avg lbs gained per year on campus"
  } else {
    "Avg lbs trimmed among slimmers"
  }
  attr(board, "value_fmt") <- "%.1f"
  ## the chart's formatter, direction-aware like value_label: gain mode is
  ## per-year ("+3.2 lbs/yr"), loss mode is total pounds trimmed ("13.0 lbs"
  ## -- gain_fmt's "+"/"per yr" framing would misread there)
  attr(board, "value_fmt_fn") <- if (direction == "gain") gain_fmt else {
    function(v) paste0(round(v, 1), " lbs")
  }
  attr(board, "conf_avg") <- if (direction == "gain") {
    mean(wr_data$GainPerYr)
  } else {
    mean(-wr_data$WeightGain[wr_data$WeightGain < 0])
  }
  ## carry the name-match receipt weight_room_data stamped on wr_data through
  ## to the board, so the chart caption + table twin can both surface it
  ## (captured before the conference scope filter dropped the attribute)
  attr(board, "match_note") <- mn_note
  board
}

## conference board: average lbs added per matched signee, by program
## direction flips the hover cards between top gainers and top slim-downs
plot_weight_room_board <- function(wr_data, team_slug, sport,
                                   compare_slug = NULL, logo_prefix = "www/",
                                   direction = "gain") {
  hl <- highlight_colors(team_slug, compare_slug)
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## single source of truth: the chart draws exactly the table twin's frame
  board <- wr_board_data(wr_data, team_slug, sport,
                         compare_slug = compare_slug, direction = direction)
  logos <- team_logo_labels(width = 30, prefix = logo_prefix)
  conf_avg <- attr(board, "conf_avg")
  ## boards keep the compare team's true primary color (rows don't overlap)
  role_cols <- c(main = unname(hl["main"]),
                 compare = ifelse(is.null(compare_slug), "grey60",
                                  team_color(compare_slug)),
                 other = "grey60")

  p <- ggplot(board, aes(x = AvgGain, y = TeamName))
  ## conference line only when it exists (no slimmers anywhere -> NaN)
  if (is.finite(conf_avg)) {
    p <- p + geom_vline(xintercept = conf_avg, linetype = "dotted",
                        color = "grey45")
  }
  p +
    geom_segment(aes(x = 0, xend = AvgGain, yend = TeamName, color = role),
                 linewidth = 1.4, show.legend = FALSE) +
    geom_point_interactive(aes(color = role, tooltip = tip,
                               data_id = School),
                           size = 5, show.legend = FALSE) +
    geom_text(aes(label = lab, color = role), hjust = -0.15, size = 3.4,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = role_cols) +
    scale_y_discrete(labels = logos) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.22))) +
    labs(
      title = wrap_title(ifelse(
        direction == "gain",
        "The Weight Room Effect: Pounds Added per Year on Campus",
        "The Cut Room: Pounds Trimmed per Slimmed-Down Signee"), 40),
      subtitle = wrap_title(glue(
        ifelse(direction == "gain",
               "Weight gained since commit day divided by years on campus, ",
               "Average pounds lost among signees who slimmed down, "),
        "{str_to_title(sport)}. Matched HS signees still on the roster only ",
        "(conference avg: {ifelse(direction == 'gain', '+', '−')}",
        "{round(conf_avg, 1)} lbs{ifelse(direction == 'gain', '/yr', '')}). ",
        "{team_label(team_slug)}",
        "{ifelse(cmp_safe == '', '', paste0(' vs ', team_label(cmp_safe)))}",
        " highlighted."), 58),
      x = ifelse(direction == "gain",
                 "Average Pounds Gained per Year on Campus",
                 "Average Weight Trimmed Among Slimmers (lbs)"),
      y = NULL,
      caption = wrap_title(paste0(
        "Tap or hover a dot for the players behind the number. Weights are ",
        "as reported by programs and listed by 247Sports.",
        ## honest scope, not a "join quality" score: the gains are computed
        ## only on signees still on a current roster -- the rest graduated,
        ## transferred, or turned pro (a low share over a wide window is those
        ## departures, NOT a broken scrape).
        {mn <- attr(board, "match_note")
         if (!is.null(mn) && !is.na(mn)) paste0(" ", mn, ".")
         else ""}), 95)
    ) +
    theme_girth_md()
}

## player-level: one team's biggest transformations since commit day
## direction = "gain" (bulk-ups) or "loss" (slim-downs)
plot_weight_room_players <- function(wr_data, team_slug, sport, top_n = 18,
                                     direction = "gain") {
  team_wr <- wr_data %>% filter(School == team_slug)
  team_wr <- if (direction == "gain") {
    team_wr %>% slice_max(WeightGain, n = top_n)
  } else {
    ## only actual slimmers -- never pad the list with small gainers
    team_wr %>% filter(WeightGain < 0) %>% slice_min(WeightGain, n = top_n)
  }
  team_wr$.p247 <- p247_url(team_wr$Name, team_wr$Year, sport,
                            team_wr$Type, profile_col(team_wr))
  team_wr <- team_wr %>%
    arrange(if (direction == "gain") WeightGain else dplyr::desc(WeightGain)) %>%
    mutate(player_lab = glue("{Name} ({Position}, '{substr(Year, 3, 4)})"),
           player_lab = factor(player_lab, levels = player_lab),
           gain_lab = glue("{ifelse(WeightGain >= 0, '+', '')}{WeightGain}"),
           tip = glue(
             "<b>{pc_link(Name, School)}</b> ({Position}, {Year} class)<br/>",
             "Commit day: {Weight} lbs → roster: {RosterWeight} lbs ",
             "({gain_lab} lbs)<br/>",
             '<a href="{.p247}" ',
             'target="_blank">Open on 247Sports →</a><br/>',
             "<em>Tap the dot to pin this card</em>"))

  t_col <- team_color(team_slug)
  t_lab <- team_label(team_slug)
  dir_word <- ifelse(direction == "gain", "Biggest Gainers", "Biggest Slim-Downs")

  ggplot(team_wr, aes(y = player_lab)) +
    geom_segment(aes(x = Weight, xend = RosterWeight, yend = player_lab),
                 color = "grey70", linewidth = 1.2,
                 arrow = arrow(length = unit(6, "pt"), type = "closed")) +
    geom_point(aes(x = Weight), color = "grey55", size = 3.4) +
    geom_point_interactive(
      aes(x = RosterWeight, tooltip = tip, data_id = Name),
      color = t_col, size = 4) +
    geom_text(aes(x = RosterWeight, label = gain_lab,
                  hjust = ifelse(WeightGain >= 0, -0.35, 1.35)),
              size = 3.4, fontface = "bold", color = t_col) +
    scale_x_continuous(expand = expansion(mult = c(0.07, 0.12))) +
    labs(
      title = wrap_title(glue("{t_lab} Weight Room: {dir_word}"), 40),
      subtitle = wrap_title(glue(
        "Weight as a recruit (grey dot) vs current roster weight (colored). ",
        "{str_to_title(sport)}; program's own HS signees on the current roster."), 58),
      x = "Weight (lbs)", y = NULL,
      caption = paste0("Tap or hover a dot for the player card; pin it to ",
                       "open their 247 page. Showing: HS signees only.")
    ) +
    theme_girth() +
    theme(panel.grid.major.y = element_blank())
}

## ---------------------------------------------------------------------------
## 7) ERA COMPARE -- how each head coach recruits differently
## ---------------------------------------------------------------------------

## a "blue chip" on the 0-100 247 scale: 90+ is four-star territory
BLUE_CHIP <- 90

## metrics available on the era timeline (beyond pure size)
ERA_METRICS <- list(
  AvgRating  = list(label = "Average 247 Rating",
                    fmt = function(x) sprintf("%.1f", x)),
  BlueChips  = list(label = "Blue-Chip Share (% rated 90+)",
                    fmt = function(x) paste0(round(x, 0), "%")),
  AvgWeight  = list(label = "Average Weight (lbs)",
                    fmt = function(x) paste0(round(x, 0), " lbs")),
  AvgHeight  = list(label = "Average Height",
                    fmt = function(x) format_height(x)),
  AvgMiles   = list(label = "Average Miles from Home",
                    fmt = function(x) paste0(round(x, 0), " mi")),
  PctInState = list(label = "% In-State Commits",
                    fmt = function(x) paste0(round(x, 0), "%"))
)

era_metric_value <- function(d, metric) {
  switch(metric,
    AvgRating  = mean(d$Ranking, na.rm = TRUE),
    BlueChips  = 100 * mean(d$Ranking >= BLUE_CHIP, na.rm = TRUE),
    AvgWeight  = mean(d$Weight, na.rm = TRUE),
    AvgHeight  = mean(d$Height_in, na.rm = TRUE),
    AvgMiles   = mean(d$miles_away, na.rm = TRUE),
    PctInState = 100 * mean(d$InState, na.rm = TRUE))
}

## yearly value of an era metric for arbitrary grouped data
era_metric_by_year <- function(size_data, metric) {
  size_data %>%
    group_by(Year) %>%
    group_modify(~ data.frame(val = era_metric_value(.x, metric),
                              n = nrow(.x))) %>%
    ungroup()
}

## per-era summary rows for one team (feeds the Era Compare table)
era_summary_table <- function(size_data, team_slug, sport) {
  size_data %>%
    filter(School == team_slug, !is.na(Coach)) %>%
    group_by(Coach) %>%
    summarize(
      Classes = paste0(min(Year), "–", max(Year)),
      Commits = n(),
      `Avg 247 Rating` = round(mean(Ranking, na.rm = TRUE), 1),
      `4★+ (90+)` = sum(Ranking >= BLUE_CHIP, na.rm = TRUE),
      `Top Signee` = {
        rk <- replace(Ranking, is.na(Ranking), -Inf)
        i <- which.max(rk)
        if (is.finite(rk[i])) paste0(Name[i], " (", Ranking[i], ")") else "—"
      },
      `Avg Weight` = paste0(round(mean(Weight, na.rm = TRUE), 0), " lbs"),
      `Avg Height` = format_height(mean(Height_in, na.rm = TRUE)),
      `Avg Miles from Home` = round(mean(miles_away, na.rm = TRUE), 0),
      `% In-State` = paste0(round(100 * mean(InState, na.rm = TRUE), 0), "%"),
      .first = min(Year),
      .groups = "drop"
    ) %>%
    arrange(.first) %>%
    select(-.first)
}

## the coach-story chart: metric by class year with shaded coach eras
plot_era_timeline <- function(size_data, team_slug, sport,
                              metric = "AvgRating", players_note = NULL) {
  m <- ERA_METRICS[[metric]]
  hl <- highlight_colors(team_slug)
  t_lab <- team_label(team_slug)

  team_pool <- size_data %>% filter(School == team_slug)
  team_yrs <- era_metric_by_year(team_pool, metric)

  ## split-pool honesty: when the selected pool mixes HS commits and portal
  ## transfers, overlay the HS-commits-only series (dashed) and split each
  ## class dot's headcount by type. A commits-only pool changes nothing.
  split_pool <- "Type" %in% names(team_pool) &&
    any(team_pool$Type == "Transfer", na.rm = TRUE)
  commit_yrs <- if (split_pool) {
    era_metric_by_year(team_pool %>% filter(Type == "Commit"), metric)
  } else team_yrs[0, ]

  ## hover card per class dot: the top-5 signees (lets users verify the data)
  ## + click opens that class's page on 247Sports
  top5 <- team_pool %>%
    group_by(Year) %>%
    arrange(desc(Ranking), .by_group = TRUE) %>%
    mutate(.rk = row_number()) %>%
    filter(.rk <= 5) %>%
    summarize(top_list = paste0(.rk, ". ", pc_link(Name, School),
                                " (", Position, ", ",
                                round(Ranking, 0), ")", collapse = "<br/>"),
              .groups = "drop")
  team_yrs <- team_yrs %>% left_join(top5, by = "Year")
  if (split_pool) {
    type_counts <- team_pool %>%
      group_by(Year) %>%
      summarize(n_hs = sum(Type == "Commit", na.rm = TRUE),
                n_portal = sum(Type == "Transfer", na.rm = TRUE),
                .groups = "drop")
    team_yrs <- team_yrs %>%
      left_join(type_counts, by = "Year") %>%
      mutate(n_split = paste0(" (", coalesce(n_hs, 0L), " HS + ",
                              coalesce(n_portal, 0L), " portal)"))
  } else {
    team_yrs$n_split <- ""
  }
  team_yrs <- team_yrs %>%
    mutate(
      ## clicking a dot PINS this card (app-level JS); the link inside the
      ## pinned card opens the class on 247Sports
      tip = glue(
        "<b>{Year} class — {n} players{n_split}</b><br/>{top_list}<br/>",
        '<a href="https://247sports.com/college/{team_slug}/season/',
        '{Year}-{tolower(sport)}/commits/" target="_blank">',
        "Open this class on 247Sports →</a><br/>",
        "<em>Tap the dot to pin this card</em>")
    )

  ## band pools only the active team's conference members (all 16 at Phase 0)
  conf_yrs <- scope_to_conf(size_data, team_slug) %>%
    group_by(School, Year) %>%
    group_modify(~ data.frame(val = era_metric_value(.x, metric))) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarize(p25 = quantile(val, 0.25, na.rm = TRUE),
              p50 = median(val, na.rm = TRUE),
              p75 = quantile(val, 0.75, na.rm = TRUE), .groups = "drop")

  ## realignment honesty: the pooled band is the conference's CURRENT
  ## membership, so any class year before that membership was whole
  ## (CONF_CONFIG$conf_whole -- 2024 for the Big 12, when the Pac-12 four
  ## joined) is a BACKCAST. Split the median guide's linetype at that seam --
  ## dotted while backcast, solid once whole. Both halves share the seam-year
  ## point so the line stays connected; empty halves draw nothing.
  whole_yr <- conf_whole_year(team_slug)
  backcast <- min(conf_yrs$Year) < whole_yr
  conf_pre  <- conf_yrs %>% filter(Year <= whole_yr)
  conf_post <- conf_yrs %>% filter(Year >= whole_yr)
  backcast_clause <- if (backcast) {
    glue(" Band = the current {n_conf_members(team_slug)} members, ",
         "backcast before {whole_yr} (dotted median).")
  } else ""

  y_rng <- range(c(team_yrs$val, commit_yrs$val, conf_yrs$p25, conf_yrs$p75),
                 na.rm = TRUE)

  p <- ggplot()

  ## shaded era bands (alternating intensity) + coach names
  eras <- team_eras(team_slug, sport)
  if (!is.null(eras)) {
    yr_min <- min(size_data$Year); yr_max <- max(size_data$Year)
    eras <- eras %>%
      mutate(first_class = pmax(first_class, yr_min),
             last_class = pmin(last_class, yr_max)) %>%
      filter(first_class <= last_class) %>%
      mutate(band_alpha = rep(c(0.05, 0.13), length.out = n()),
             y_lab = y_rng[2] + diff(y_rng) * 0.1)
    p <- p +
      geom_rect(data = eras,
                aes(xmin = first_class - 0.5, xmax = last_class + 0.5,
                    ymin = -Inf, ymax = Inf, alpha = band_alpha),
                fill = hl["main"], show.legend = FALSE) +
      scale_alpha_identity() +
      geom_text(data = eras,
                aes(x = (first_class + last_class) / 2, y = y_lab,
                    label = coach),
                size = 3.4, color = "grey20",
                fontface = "bold.italic", vjust = 1)
  }

  ## the HS-commits-only series draws dashed UNDER the solid all-additions
  ## line; no legend (interactive legends are broken under ggplot2 4.0) --
  ## the caption says what dashed means
  show_dash <- split_pool && nrow(commit_yrs) >= 2
  dash_note <- if (show_dash) {
    glue(" Dashed team line = HS commits only ",
         "(solid includes portal transfers).")
  } else ""

  p +
    geom_ribbon(data = conf_yrs, aes(x = Year, ymin = p25, ymax = p75),
                fill = "grey75", alpha = 0.35) +
    ## median guide: dotted across backcast years, solid once whole (2024+)
    geom_line(data = conf_pre, aes(x = Year, y = p50),
              color = "grey45", linetype = "dotted", linewidth = 0.8) +
    geom_line(data = conf_post, aes(x = Year, y = p50),
              color = "grey45", linetype = "solid", linewidth = 0.8) +
    (if (show_dash) {
      geom_line(data = commit_yrs, aes(x = Year, y = val),
                color = hl["main"], linetype = "dashed",
                linewidth = 0.9, alpha = 0.8)
    }) +
    geom_line(data = team_yrs, aes(x = Year, y = val),
              color = hl["main"], linewidth = 1.5) +
    geom_point_interactive(
      data = team_yrs,
      aes(x = Year, y = val, size = n, tooltip = tip, data_id = Year),
      color = hl["main"]) +
    ## pool-neutral: n counts the SELECTED pool (HS commits, + transfers, or
    ## transfers only), so "Commits" would over-claim on mixed pools
    scale_size_continuous(range = c(2.2, 6), name = "Players in class",
                          breaks = function(l) unique(round(pretty(l)))) +
    scale_x_continuous(breaks = seq(min(size_data$Year),
                                    max(size_data$Year), 1)) +
    scale_y_continuous(labels = if (metric == "AvgHeight") {
      function(x) format_height(x)
    } else waiver()) +
    labs(
      title = glue("{t_lab} by Coaching Era: {m$label}"),
      subtitle = glue(
        "Shaded bands = head-coach eras (recruiting-class attribution). ",
        "Grey band = {conf_label(team_slug)} team middle (25th–75th pct), grey line = median.",
        "{backcast_clause}"),
      x = "Class Year", y = m$label,
      caption = paste0("Tap or hover a class dot for its top-5 signees; pin ",
                       "it to open that class on 247Sports. Era assignment ",
                       "is by class year.", dash_note,
                       scope_note(players_note))
    ) +
    theme_girth()
}

## ---------------------------------------------------------------------------
## DISTANCE LAB -- interactive miles-from-home scatter (click any recruit)
## ---------------------------------------------------------------------------
plot_distance_lab <- function(size_data, team_slug, sport,
                              show_outliers = "show") {
  d <- size_data %>%
    filter(School == team_slug, !is.na(miles_away))
  removed_n <- 0
  if (show_outliers == "hide" && nrow(d) > 4) {
    removed_n <- nrow(get_Outliers(d))
    d <- remove_Outliers(d)
  }

  hl <- highlight_colors(team_slug)
  t_lab <- team_label(team_slug)
  yr_rng <- paste0(min(d$Year), "–", max(d$Year))

  band <- d %>%
    group_by(Year) %>%
    summarize(p25 = quantile(miles_away, 0.25, na.rm = TRUE),
              p75 = quantile(miles_away, 0.75, na.rm = TRUE),
              avg = mean(miles_away, na.rm = TRUE), .groups = "drop")
  med_all <- median(d$miles_away, na.rm = TRUE)

  d$.p247 <- p247_url(d$Name, d$Year, sport, d$Type, profile_col(d))
  d <- d %>%
    mutate(
      tip = glue(
        "<b>{pc_link(Name, School)}</b> ({Position}, {Year})<br/>",
        "{miles_away} miles from campus<br/>From: {loc_dash(Location)}<br/>",
        "{HeightLabel} • {Weight} lbs • 247 Rating: {round(Ranking, 0)}<br/>",
        '<a href="{.p247}" ',
        'target="_blank">Open on 247Sports →</a><br/>',
        "<em>Tap the dot to pin this card</em>")
    )

  ggplot() +
    geom_ribbon(data = band, aes(x = Year, ymin = p25, ymax = p75),
                fill = "#F0E442", alpha = 0.45) +
    geom_hline(yintercept = med_all, linetype = "dotted",
               color = "#009E73") +
    annotate("text", x = min(d$Year), y = med_all,
             label = glue("Median ({round(med_all, 0)} mi)"),
             hjust = 0, vjust = -0.6, color = "#009E73", size = 3.6) +
    geom_line(data = band, aes(x = Year, y = avg),
              color = "#D55E00", linewidth = 1.3, alpha = 0.5) +
    geom_point(data = band, aes(x = Year, y = avg),
               color = "#D55E00", size = 3, alpha = 0.5) +
    geom_point_interactive(
      data = d,
      aes(x = Year, y = miles_away, tooltip = tip, data_id = Name),
      color = hl["main"], alpha = 0.7, size = 3.4,
      position = position_jitter(width = 0.13, height = 0, seed = 7)) +
    scale_x_continuous(breaks = seq(min(d$Year), max(d$Year), 1)) +
    labs(
      title = glue("{t_lab} {str_to_title(sport)}: Miles from Home by Class ({yr_rng})"),
      subtitle = glue(
        "Each dot = one player's distance from hometown to campus. ",
        "Yellow band = 25th–75th percentile, orange = class average",
        "{ifelse(removed_n > 0, glue('. {removed_n} outliers hidden (1.5×IQR)'), '')}."),
      x = "Class Year", y = "Miles from Home",
      caption = "Tap or hover any dot for the recruit card; click to open their 247 page. Portal transfers appear once a hometown is known for them."
    ) +
    theme_girth()
}

## ---------------------------------------------------------------------------
## DISTANCE BOX -- miles from home by position group, historic vs newest
## class (replaces the legacy sourced scripts/box_plot.R)
## ---------------------------------------------------------------------------
plot_distance_box <- function(size_data, team_slug, sport) {
  hl <- highlight_colors(team_slug)
  t_lab <- team_label(team_slug)
  d <- size_data %>%
    filter(School == team_slug, !is.na(miles_away),
           as.character(PosGroup) != "Other")
  if (nrow(d) == 0) return(NULL)

  yr_max <- max(d$Year)
  his_rng <- if (min(d$Year) < yr_max) {
    paste0(min(d$Year), "–", yr_max - 1)
  } else as.character(yr_max)
  d <- d %>%
    mutate(
      era = ifelse(Year == yr_max, paste0(yr_max, " class"), his_rng),
      tip = glue(
        "<b>{pc_link(Name, School)}</b> ({Position}, {Year})<br/>",
        "{miles_away} miles from campus<br/>From: {loc_dash(Location)}<br/>",
        "247 Rating: {round(Ranking, 0)}<br/>",
        "<em>Tap the dot to pin this card</em>"))

  ## keep Hawaii/international from squashing the whole axis
  x_cap <- min(max(d$miles_away), 3100)
  hidden_n <- sum(d$miles_away > x_cap)

  ggplot(d, aes(x = miles_away, y = forcats::fct_rev(PosGroup))) +
    geom_boxplot(fill = hl["main"], alpha = 0.14, color = "grey45",
                 outliers = FALSE, width = 0.62) +
    geom_point_interactive(
      data = d %>% filter(era == his_rng, miles_away <= x_cap),
      aes(tooltip = tip, data_id = Name),
      shape = 1, color = "grey45", size = 2.6, stroke = 0.7,
      position = position_jitter(width = 0, height = 0.16, seed = 7)) +
    geom_point_interactive(
      data = d %>% filter(era != his_rng, miles_away <= x_cap),
      aes(tooltip = tip, data_id = Name),
      color = hl["main"], size = 3, alpha = 0.95,
      position = position_jitter(width = 0, height = 0.16, seed = 7)) +
    coord_cartesian(xlim = c(0, x_cap)) +
    labs(
      title = wrap_title(glue("{t_lab}: Miles from Home by Position Group"), 52),
      subtitle = wrap_title(glue(
        "Open circles = {his_rng} classes; filled {t_lab}-colored dots = the ",
        "{yr_max} class. Boxes = the full window's spread.",
        "{ifelse(hidden_n > 0, glue(' {hidden_n} players beyond ',
        '{x_cap} mi not shown.'), '')}"), 84),
      x = "Miles from Home", y = NULL,
      caption = paste("Tap or hover any dot for the recruit card. Distances need a",
                      "hometown, so portal transfers appear only once one is known.")
    ) +
    theme_girth()
}

## recruiting philosophy shift: position-group mix per era
plot_era_position_mix <- function(size_data, team_slug, sport, players_note = NULL) {
  t_lab <- team_label(team_slug)
  mix <- size_data %>%
    filter(School == team_slug, !is.na(Coach),
           as.character(PosGroup) != "Other") %>%
    group_by(Coach) %>%
    mutate(.first = min(Year), era_lab = glue("{Coach}\n({min(Year)}–{max(Year)})")) %>%
    ungroup()
  era_order <- mix %>% distinct(era_lab, .first) %>% arrange(.first)
  mix <- mix %>%
    mutate(era_lab = factor(era_lab, levels = era_order$era_lab)) %>%
    group_by(era_lab, PosGroup) %>%
    summarize(
      n = n(),
      tip = paste0("<b>", first(Coach), " — ", first(PosGroup), " (", n(),
                   ")</b><br/>",
                   top_players_tip(pick(Name, Position, Year, Ranking),
                                   "Ranking", n = 3, school = team_slug,
                                   fmt = function(v) glue("rating {round(v)}"))),
      .groups = "drop") %>%
    group_by(era_lab) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()

  ggplot(mix, aes(x = era_lab, y = pct, fill = PosGroup)) +
    geom_col_interactive(aes(tooltip = tip,
                             data_id = paste(era_lab, PosGroup)),
                         width = 0.7, color = "white", linewidth = 0.3) +
    ## label color follows each slice's fill luminance -- hardcoded white
    ## was unreadable on the yellow TE slice and weak on the grey DB one
    geom_text(data = mix %>% filter(pct >= 6) %>%
                mutate(.lab_col = ifelse(
                  colSums(col2rgb(pos_group_palette(sport)[
                    as.character(PosGroup)]) * c(0.299, 0.587, 0.114)) > 150,
                  "#1a2733", "white")),
              aes(label = paste0(round(pct), "%"), color = .lab_col),
              position = position_stack(vjust = 0.5),
              size = 3.1, fontface = "bold") +
    scale_color_identity(guide = "none") +
    scale_fill_manual(values = pos_group_palette(sport), name = NULL) +
    labs(
      title = wrap_title(glue("{t_lab}: What Each Coach Spends Their Classes On"), 44),
      subtitle = wrap_title(glue("Share of commits by position group per coaching era ",
                                 "({str_to_title(sport)})"), 60),
      x = NULL, y = "% of Commits",
      caption = paste0("Labels shown for slices ≥ 6%. Data: 247Sports.", scope_note(players_note))
    ) +
    theme_girth()
}

## ---------------------------------------------------------------------------
## 8) PIPELINE MAP -- leaflet map of main (+ compare) team recruiting
##    footprints, replacing the old single-team sourced map.R
## ---------------------------------------------------------------------------
build_pipeline_map <- function(size_data, team_slug, sport,
                               compare_slug = NULL, n_unmapped = 0) {
  prep_team <- function(slug) {
    d <- size_data %>%
      filter(School == slug,
             !is.na(suppressWarnings(as.numeric(lat))),
             !is.na(suppressWarnings(as.numeric(long)))) %>%
      mutate(lat = as.numeric(lat), long = as.numeric(long),
             college_lat = as.numeric(college_lat),
             college_long = as.numeric(college_long))
    if (nrow(d) == 0) return(d)
    d$.purl <- profile_col(d)
    ## spread players from the same school so dots don't fully overlap
    d %>%
      group_by(round(lat, 3), round(long, 3)) %>%
      mutate(lat = ifelse(n() > 1, jitter(lat, amount = 0.012), lat),
             long = ifelse(n() > 1, jitter(long, amount = 0.012), long)) %>%
      ungroup() %>%
      mutate(URL = p247_url(Name, Year, sport, Type, .purl),
             ## pc_link works in leaflet popups too -- the app's .pc-open
             ## listener is document-level, so map names open player cards
             popup = paste0(
               "<strong>", pc_link(Name, School), "</strong> (", Position,
               ", ", Year, ")<br/>",
               HeightLabel, " • ", Weight, " lbs • 247 Rating: ",
               ifelse(is.na(Ranking), "unrated", round(Ranking, 0)),
               "<br/>From: ", loc_dash(Location), "<br/>",
               miles_away, " miles from campus<br/>",
               "<em><a href='", URL, "' target='_blank'>View profile</a></em>"))
  }

  main <- prep_team(team_slug)
  cmp <- if (!is.null(compare_slug)) prep_team(compare_slug) else main[0, ]
  c_main <- team_color(team_slug)
  c_cmp <- if (!is.null(compare_slug)) team_color(compare_slug) else "grey"

  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron)

  ## smoothed state hulls for the MAIN team's footprint (sf/smoothr load
  ## lazily here -- see R/functions.R)
  hull <- tryCatch({
    main %>%
      sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      group_by(State) %>%
      summarise(geometry = sf::st_combine(geometry)) %>%
      sf::st_buffer(dist = 50000) %>%
      sf::st_convex_hull() %>%
      smoothr::smooth(method = "chaikin")
  }, error = function(e) NULL)
  if (!is.null(hull)) {
    map <- map %>%
      addPolygons(data = hull, weight = 1.5, color = c_main, opacity = 0.6,
                  fillColor = c_main, fillOpacity = 0.08)
  }

  ## compare team first so the main team draws on top
  if (nrow(cmp) > 0) {
    map <- map %>%
      addCircleMarkers(data = cmp, lng = ~long, lat = ~lat, radius = 5,
                       color = c_cmp, stroke = TRUE, weight = 1.5,
                       fillColor = c_cmp, fillOpacity = 0.55,
                       popup = ~popup, group = team_label(compare_slug))
  }
  map <- map %>%
    addCircleMarkers(data = main, lng = ~long, lat = ~lat, radius = 6,
                     color = "white", stroke = TRUE, weight = 1.2,
                     fillColor = c_main, fillOpacity = 0.92,
                     popup = ~popup, group = team_label(team_slug))

  ## campus marker (the team's logo) + legend + attribution
  campus <- main %>% filter(!is.na(college_lat)) %>% slice(1)
  if (nrow(campus) == 1) {
    logo_file <- TEAM_CONFIG$logo[match(team_slug, TEAM_CONFIG$slug)]
    ## preserve the logo's aspect ratio (a fixed square morphs tall logos)
    icon_wh <- tryCatch({
      dims <- dim(png::readPNG(file.path("www", logo_file)))
      c(round(38 * dims[2] / dims[1]), 38)
    }, error = function(e) c(34, 34))
    logo_icon <- makeIcon(
      iconUrl = logo_file,
      iconWidth = icon_wh[1], iconHeight = icon_wh[2])
    map <- map %>%
      addMarkers(lng = campus$college_long, lat = campus$college_lat,
                 icon = logo_icon,
                 label = paste(team_label(team_slug), "campus")) %>%
      setView(lng = campus$college_long, lat = campus$college_lat, zoom = 4.5)
  }
  legend_labels <- c(team_label(team_slug),
                     if (nrow(cmp) > 0) team_label(compare_slug))
  legend_colors <- c(c_main, if (nrow(cmp) > 0) c_cmp)
  map %>%
    addLegend(position = "topright", colors = legend_colors,
              labels = legend_labels, opacity = 0.9,
              title = "Hometowns") %>%
    addControl(html = tags$div(
      style = "background: rgba(255,255,255,.85); padding: 3px 8px;
               border-radius: 4px; font-size: 11px; max-width: 290px;",
      tags$small(
        "Data: ", tags$a(href = "https://247sports.com",
                         "247Sports", target = "_blank"),
        " — players with mapped hometowns; transfers appear once a hometown
         is known.",
        if (n_unmapped > 0) {
          tags$b(glue(" {n_unmapped} player{ifelse(n_unmapped == 1, '', 's')}
                       in this window can't be mapped yet (no hometown on
                       file, which covers most portal transfers, or awaiting
                       geocoding)."))
        })),
      position = "bottomleft")
}

## ---------------------------------------------------------------------------
## 9) ANALYST BRIEF -- the Defensive War Room (3-3-5 lens) + roster
##    construction, talent retention, class snapshot
## ---------------------------------------------------------------------------

## ---- the 3-3-5 odd-stack lens (Rocky Long tree; Arizona DC Danny Gonzales)
## The scheme "trades size for surprise": one true big body at Nose, long
## rangy tweener Ends, multi-role Stack LBs, a middle-field S/LB hybrid, and
## corner-skilled DBs. Map any defensive body onto those roles by
## position + weight (works for both roster positions and recruit positions).
ROLE_335_LEVELS <- c("Nose (285+)", "Rangy End (245-284)",
                     "Edge Tweener (<245)", "Stack LB",
                     "Hybrid S/LB (205+)", "Safety", "Corner")

## rough two-deep headcount targets per role for an odd-stack roster
ROLE_335_SPEC <- c("Nose (285+)" = "2-3", "Rangy End (245-284)" = "4-6",
                   "Edge Tweener (<245)" = "2-3", "Stack LB" = "6-9",
                   "Hybrid S/LB (205+)" = "2-3", "Safety" = "4-6",
                   "Corner" = "5-7")

role_335 <- function(position, weight) {
  P <- toupper(trimws(position))
  dl <- c("DL", "DT", "DE", "NT", "SDE", "WDE", "EDGE")
  dplyr::case_when(
    P %in% dl & weight >= 285                 ~ "Nose (285+)",
    P %in% dl & weight >= 245                 ~ "Rangy End (245-284)",
    P %in% dl                                 ~ "Edge Tweener (<245)",
    P %in% c("LB", "ILB", "OLB")              ~ "Stack LB",
    P == "S" & weight >= 205                  ~ "Hybrid S/LB (205+)",
    P %in% c("S", "FS", "SS")                 ~ "Safety",
    P == "CB"                                 ~ "Corner",
    P == "DB" & weight >= 205                 ~ "Hybrid S/LB (205+)",
    P == "DB" & weight >= 190                 ~ "Safety",
    P == "DB"                                 ~ "Corner",
    P == "ATH" & weight >= 200 & weight <= 235 ~ "Hybrid S/LB (205+)",
    TRUE                                      ~ NA_character_
  )
}

## the centerpiece: one team's defensive roster mapped onto 3-3-5 roles,
## stacked by class standing, with the scheme's two-deep targets annotated.
## `incoming` = the newest cycle's additions (commits + transfers) not yet on
## the 247 roster page -- they stack on in gold so the staff sees what's
## arriving at each role
## incoming bodies are classified at PROJECTED weights: arrival weight plus
## the conference's median development gain for that position group (a
## 240-lb HS end is a 255-lb Rangy End to every staff, not an Edge Tweener)
project_incoming <- function(incoming, proj_gain = NULL) {
  incoming <- incoming %>%
    mutate(Weight = suppressWarnings(as.numeric(Weight)))
  if (is.null(proj_gain)) return(mutate(incoming, ProjWeight = Weight))
  ## as.numeric strips tapply's 1D-array class, which would poison the
  ## case_when inside role_335 downstream
  incoming %>%
    mutate(gain = as.numeric(proj_gain[as.character(PosGroup)]),
           gain = ifelse(is.na(gain) | gain < 0, 0, gain),
           ProjWeight = Weight + round(gain))
}

plot_roster_335 <- function(roster_data, team_slug, incoming = NULL,
                            incoming_label = "ADDS", proj_gain = NULL) {
  t_lab <- team_label(team_slug)
  rc <- roster_data %>%
    filter(School == team_slug) %>%
    mutate(Class = toupper(trimws(Class)),
           Weight = suppressWarnings(as.numeric(Weight)),
           Role = role_335(Position, Weight)) %>%
    filter(!is.na(Role), Class %in% c("FR", "SO", "JR", "SR")) %>%
    select(Name, Weight, Role, Class)

  if (!is.null(incoming) && nrow(incoming) > 0) {
    inc <- project_incoming(incoming, proj_gain) %>%
      mutate(Role = role_335(Position, ProjWeight),
             Class = incoming_label,
             Weight = ProjWeight) %>%
      filter(!is.na(Role)) %>%
      select(Name, Weight, Role, Class)
    rc <- bind_rows(rc, inc)
  }

  cls_levels <- c("FR", "SO", "JR", "SR", incoming_label)
  rc <- rc %>%
    mutate(Role = factor(Role, levels = ROLE_335_LEVELS),
           Class = factor(Class, levels = cls_levels))

  counts <- rc %>%
    group_by(Role, Class) %>%
    summarize(
      n = n(),
      tip = paste0("<b>", first(Role), " — ", first(Class), " (", n(),
                   ")</b><br/>",
                   paste(paste0(pc_link(Name, team_slug), " (",
                                round(Weight), ")"),
                         collapse = "<br/>")),
      .groups = "drop")

  spec_df <- rc %>%
    group_by(Role) %>%
    summarize(n_now = sum(Class != incoming_label),
              n_add = sum(Class == incoming_label),
              upper = sum(Class %in% c("JR", "SR")),
              .groups = "drop") %>%
    mutate(n = n_now + n_add,
           lab = glue(
             "{n_now}{ifelse(n_add > 0, paste0('+', n_add), '')} / target ",
             "{ROLE_335_SPEC[as.character(Role)]} · ",
             "{round(100 * upper / pmax(n_now, 1))}% JR/SR"))

  ramp <- c(colorRampPalette(c("#D7DEE8", team_color(team_slug)))(4),
            "#FFD200")
  names(ramp) <- cls_levels

  ggplot(counts, aes(x = n, y = forcats::fct_rev(Role), fill = Class)) +
    geom_col_interactive(aes(tooltip = tip, data_id = paste(Role, Class)),
                         width = 0.72, color = "white", linewidth = 0.3) +
    geom_text(data = spec_df,
              aes(x = n, y = forcats::fct_rev(Role), label = lab),
              inherit.aes = FALSE, hjust = -0.06, size = 3.1,
              fontface = "bold", color = "grey25") +
    scale_fill_manual(values = ramp, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.42))) +
    labs(
      title = wrap_title(glue("{t_lab} 3-3-5 Fit Board"), 44),
      subtitle = wrap_title(paste(
        "Defensive roster by odd-stack role and class standing;",
        "gold = incoming additions not yet on the roster page."), 60),
      x = "Defensive Bodies", y = NULL,
      caption = wrap_title(paste(
        "Role spec follows the Rocky Long / Gonzales odd-stack. Incoming",
        "adds are slotted at projected weights (arrival + the league's median",
        "development gain for the position). Sizes as reported by",
        "programs/247Sports. Tap or hover a segment for names + weights."), 95)
    ) +
    theme_girth() +
    theme(panel.grid.major.y = element_blank())
}

## defensive bodies vs the league, role by role (current rosters);
## `incoming` additions show as gold diamonds against the league violins
plot_def_size_profile <- function(roster_size_data, team_slug,
                                  incoming = NULL, proj_gain = NULL) {
  t_lab <- team_label(team_slug)
  hl <- highlight_colors(team_slug)
  ## pool only the active team's conference members (all 16 at Phase 0)
  roster_size_data <- scope_to_conf(roster_size_data, team_slug)
  rd <- roster_size_data %>%
    mutate(Role = role_335(Position, Weight)) %>%
    filter(!is.na(Role)) %>%
    mutate(Role = factor(Role, levels = ROLE_335_LEVELS))
  team_rd <- rd %>%
    filter(School == team_slug) %>%
    mutate(tip = glue(
      "<b>{pc_link(Name, School)}</b> ({Position}, {Class})<br/>",
      "{HeightLabel} • {Weight} lbs<br/>",
      "<em>Tap to pin this card</em>"))

  ## horizontal so the long role labels read cleanly (and it matches the
  ## Fit Board next to it); crossbar under the dots keeps them tappable
  p <- ggplot(rd, aes(x = Weight, y = forcats::fct_rev(Role))) +
    geom_violin(fill = "grey85", color = "grey60", alpha = 0.9,
                scale = "width") +
    stat_summary(fun = median, geom = "crossbar", width = 0.55,
                 color = "grey35", linewidth = 0.4) +
    geom_point_interactive(
      data = team_rd,
      aes(tooltip = tip, data_id = Name),
      color = hl["main"], alpha = 0.6, size = 2.4,
      position = position_jitter(height = 0.13, seed = 7))

  inc_note <- ""
  if (!is.null(incoming) && nrow(incoming) > 0) {
    inc <- project_incoming(incoming, proj_gain) %>%
      mutate(Role = role_335(Position, ProjWeight)) %>%
      filter(!is.na(Role), !is.na(Weight)) %>%
      mutate(Role = factor(Role, levels = ROLE_335_LEVELS),
             tip = glue(
               "<b>{pc_link(Name, School)}</b> — INCOMING ({Position}, {Year})<br/>",
               "{Weight} lbs at arrival → ~{ProjWeight} projected<br/>",
               "247 rating {round(Ranking, 0)}<br/>",
               "<em>Tap to pin this card</em>"),
             Weight = ProjWeight)
    if (nrow(inc) > 0) {
      p <- p + geom_point_interactive(
        data = inc,
        aes(tooltip = tip, data_id = paste0("inc-", Name)),
        color = "#0C234B", fill = "#FFD200", shape = 23, size = 3.4,
        stroke = 0.8,
        position = position_jitter(height = 0.1, seed = 11))
      inc_note <- " Gold diamonds = incoming adds at projected weights."
    }
  }

  p +
    labs(
      title = wrap_title(glue("Defensive Bodies vs the League — 3-3-5 Lens"), 46),
      subtitle = wrap_title(glue(
        "Grey violins = every {conf_label(team_slug)} defensive roster body by odd-stack ",
        "role; colored dots = {t_lab}'s current defense.{inc_note}"), 60),
      x = "Current Weight (lbs)", y = NULL,
      caption = "Current rosters, both lines + back end. Data: 247Sports."
    ) +
    theme_girth()
}

## defensive war-room bullets (football + roster only)
defense_notes <- function(roster_data, size_data, team_slug,
                          incoming = NULL, proj_gain = NULL) {
  t_lab <- team_label(team_slug)
  rc <- roster_data %>%
    filter(School == team_slug) %>%
    mutate(Weight = suppressWarnings(as.numeric(Weight)),
           Role = role_335(Position, Weight)) %>%
    filter(!is.na(Role)) %>%
    mutate(Role = factor(Role, levels = ROLE_335_LEVELS))
  if (nrow(rc) == 0) return(character(0))

  counts <- rc %>% count(Role)
  spec_low <- c("Nose (285+)" = 2, "Rangy End (245-284)" = 4,
                "Edge Tweener (<245)" = 2, "Stack LB" = 6,
                "Hybrid S/LB (205+)" = 2, "Safety" = 4, "Corner" = 5)
  gaps <- counts %>%
    mutate(target = spec_low[as.character(Role)],
           short = target - n) %>%
    filter(short > 0) %>%
    arrange(desc(short))

  notes <- character(0)
  notes <- c(notes, glue(
    "3-3-5 fit: {t_lab}'s defense maps to ",
    "{paste0(counts$n, ' ', counts$Role, collapse = ', ')}."))
  if (nrow(gaps) > 0) {
    notes <- c(notes, glue(
      "Thinnest odd-stack role{ifelse(nrow(gaps) > 1, 's', '')}: ",
      "{paste0(gaps$Role, ' (', gaps$n, ' vs two-deep floor of ', gaps$target,
      ')', collapse = '; ')} — the next class's defensive priority."))
  } else {
    notes <- c(notes,
               "Every odd-stack role currently meets its two-deep floor.")
  }

  ## the incoming cycle: which roles the new bodies fill
  if (!is.null(incoming) && nrow(incoming) > 0) {
    inc <- project_incoming(incoming, proj_gain) %>%
      mutate(Role = role_335(Position, ProjWeight)) %>%
      filter(!is.na(Role)) %>%
      mutate(Role = factor(Role, levels = ROLE_335_LEVELS))
    if (nrow(inc) > 0) {
      by_role <- inc %>%
        arrange(Role) %>%
        group_by(Role) %>%
        summarize(txt = glue(
          "{dplyr::n()} {first(Role)} ({paste(Name, collapse = ', ')})"),
          .groups = "drop")
      notes <- c(notes, glue(
        "Incoming defensive additions not yet on the roster page: ",
        "{paste(by_role$txt, collapse = '; ')}."))
    }
  }

  ## the scheme thesis: tweener share
  tweeners <- rc %>%
    filter(Role %in% c("Rangy End (245-284)", "Edge Tweener (<245)",
                       "Stack LB", "Hybrid S/LB (205+)"))
  notes <- c(notes, glue(
    "{round(100 * nrow(tweeners) / nrow(rc))}% of the defensive roster is ",
    "tweener-profile bodies (rangy ends, stack LBs, hybrids) — the type the ",
    "odd-stack hunts because 'there are a lot more smaller fast guys than ",
    "big strong guys who run fast' (Rocky Long)."))
  notes
}

## roster construction: who graduates next, by position group
plot_roster_construction <- function(roster_data, team_slug, sport) {
  t_lab <- team_label(team_slug)
  rc <- roster_data %>%
    filter(School == team_slug) %>%
    mutate(Class = toupper(trimws(Class)),
           PosGroup = factor(position_group(Position, sport),
                             levels = position_levels(sport))) %>%
    filter(Class %in% c("FR", "SO", "JR", "SR"),
           as.character(PosGroup) != "Other") %>%
    mutate(Class = factor(Class, levels = c("FR", "SO", "JR", "SR")))

  risk <- rc %>%
    group_by(PosGroup) %>%
    summarize(n = n(), upper = sum(Class %in% c("JR", "SR")),
              pct = round(100 * upper / n), .groups = "drop")

  counts <- rc %>%
    group_by(PosGroup, Class) %>%
    summarize(
      n = n(),
      tip = paste0("<b>", first(PosGroup), " — ", first(Class), " (", n(),
                   ")</b><br/>",
                   paste(head(pc_link(Name, team_slug), 8),
                         collapse = "<br/>"),
                   ifelse(n() > 8, paste0("<br/>+", n() - 8, " more"), "")),
      .groups = "drop")
  ramp <- colorRampPalette(c("#D7DEE8", team_color(team_slug)))(4)

  ggplot(counts, aes(x = n, y = forcats::fct_rev(PosGroup), fill = Class)) +
    geom_col_interactive(aes(tooltip = tip, data_id = paste(PosGroup, Class)),
                         width = 0.72, color = "white", linewidth = 0.3) +
    geom_text(data = risk,
              aes(x = n, y = forcats::fct_rev(PosGroup),
                  label = glue("{pct}% JR/SR")),
              inherit.aes = FALSE, hjust = -0.12, size = 3.3,
              fontface = "bold", color = "grey25") +
    scale_fill_manual(values = ramp, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.18))) +
    labs(
      title = wrap_title(glue("{t_lab} Roster Construction: Who Graduates Next?"), 44),
      subtitle = wrap_title(glue(
        "Current roster by class standing. High JR/SR share = positions the ",
        "next classes must restock."), 58),
      x = "Players on Current Roster", y = NULL,
      caption = "Data: 247Sports current roster. Redshirts counted at listed class."
    ) +
    theme_girth() +
    theme(panel.grid.major.y = element_blank())
}

## position groups with the highest upcoming attrition (for analyst notes)
roster_needs <- function(roster_data, team_slug, sport, top_n = 2) {
  roster_data %>%
    filter(School == team_slug) %>%
    mutate(Class = toupper(trimws(Class)),
           PosGroup = position_group(Position, sport)) %>%
    filter(Class %in% c("FR", "SO", "JR", "SR"), PosGroup != "Other") %>%
    group_by(PosGroup) %>%
    summarize(n = n(), upper = sum(Class %in% c("JR", "SR")),
              pct = round(100 * upper / n), .groups = "drop") %>%
    filter(n >= 5) %>%
    slice_max(pct, n = top_n)
}

## who signs the team's home-state HS talent within the conference
plot_state_retention <- function(size_data, team_slug, sport,
                                 compare_slug = NULL, logo_prefix = "www/", players_note = NULL) {
  st <- team_state(team_slug)
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## pool only the active team's conference members (all 16 at Phase 0)
  size_data <- scope_to_conf(size_data, team_slug)
  yr_rng <- paste0(min(size_data$Year), "–", max(size_data$Year))

  ## hover card per bar: that school's top-rated signees from this state
  tips <- size_data %>%
    filter(State == st) %>%
    group_by(School) %>%
    group_modify(~ data.frame(tip = top_players_tip(
      .x, "Ranking", n = 3, fmt = function(v) glue("rating {round(v)}"),
      school = .y$School,
      header = glue("<b>{team_label(.y$School)} — top {st} signees</b>")))) %>%
    ungroup()

  pool <- size_data %>%
    filter(State == st) %>%
    group_by(School, TeamName) %>%
    summarize(n = n(), blue = sum(Ranking >= BLUE_CHIP, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(tips, by = "School") %>%
    arrange(n) %>%
    mutate(TeamName = factor(TeamName, levels = TeamName),
           role = case_when(School == team_slug ~ "main",
                            School == cmp_safe ~ "compare",
                            TRUE ~ "other"),
           lab = ifelse(blue > 0, glue("{n}  ({blue} blue-chip)"), as.character(n)))

  role_cols <- c(main = team_color(team_slug),
                 compare = ifelse(cmp_safe == "", "grey60", team_color(cmp_safe)),
                 other = "grey60")
  logos <- team_logo_labels(width = 28, prefix = logo_prefix)

  ggplot(pool, aes(x = n, y = TeamName)) +
    geom_col_interactive(aes(fill = role, tooltip = tip, data_id = School),
                         width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = lab, color = role), hjust = -0.12, size = 3.4,
              fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = role_cols) +
    scale_color_manual(values = role_cols) +
    scale_y_discrete(labels = logos) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.28))) +
    labs(
      title = wrap_title(glue("Who Signs {st} High-School Talent in the {conf_label(team_slug)}?"), 44),
      subtitle = wrap_title(glue(
        "{str_to_title(sport)} commits from {st} high schools by signing ",
        "program, classes {yr_rng}. Blue-chip = 247 rating 90+."), 58),
      x = glue("Commits from {st} High Schools"), y = NULL,
      caption = paste0(conf_label(team_slug), " destinations only (players leaving for other conferences are not tracked).", scope_note(players_note))
    ) +
    theme_girth_md()
}

## headline numbers for one class (Home card + Analyst Brief)
class_snapshot <- function(size_data, team_slug, snap_year = NULL) {
  team <- size_data %>% filter(School == team_slug)
  if (nrow(team) == 0) return(NULL)
  if (is.null(snap_year)) snap_year <- max(team$Year)
  cls <- team %>% filter(Year == snap_year)
  if (nrow(cls) == 0) return(NULL)
  prior <- team %>% filter(Year %in% (snap_year - 3):(snap_year - 1))

  delta <- function(now, then, digits = 1) {
    if (!is.finite(then) || is.na(then)) return(NA_real_)
    round(now - then, digits)
  }
  top <- cls %>% slice_max(Ranking, n = 1, with_ties = FALSE)

  list(
    year = snap_year,
    n = nrow(cls),
    avg_rating = round(mean(cls$Ranking, na.rm = TRUE), 1),
    d_rating = delta(mean(cls$Ranking, na.rm = TRUE),
                     mean(prior$Ranking, na.rm = TRUE)),
    blue = sum(cls$Ranking >= BLUE_CHIP, na.rm = TRUE),
    avg_weight = round(mean(cls$Weight, na.rm = TRUE), 0),
    d_weight = delta(mean(cls$Weight, na.rm = TRUE),
                     mean(prior$Weight, na.rm = TRUE), 0),
    avg_height = format_height(mean(cls$Height_in, na.rm = TRUE)),
    pct_instate = round(100 * mean(cls$InState, na.rm = TRUE)),
    top_name = top$Name, top_pos = top$Position, top_rating = top$Ranking
  )
}

## auto-written bullets for the Analyst Brief
analyst_notes <- function(size_data, roster_data, team_slug, sport,
                          compare_slug = NULL) {
  t_lab <- team_label(team_slug)
  st <- team_state(team_slug)
  ## pool only the active team's conference members (all 16 at Phase 0);
  ## team-specific reads below (class snapshot, roster needs) are unaffected
  size_data <- scope_to_conf(size_data, team_slug)
  notes <- character(0)

  snap <- class_snapshot(size_data, team_slug)
  if (!is.null(snap)) {
    ## "additions" not "signees/commits" -- under the default player pool
    ## this count includes portal transfers, and the copy must stay honest
    ## for every pool the global radio can select
    notes <- c(notes, glue(
      "The {snap$year} class: {snap$n} additions at a {snap$avg_rating} average ",
      "247 rating ({ifelse(is.na(snap$d_rating), 'n/a',
        paste0(ifelse(snap$d_rating >= 0, '+', ''), snap$d_rating))} vs the ",
      "prior three classes) with {snap$blue} blue-chip addition",
      "{ifelse(snap$blue == 1, '', 's')} (90+). Headliner: {snap$top_name} ",
      "({snap$top_pos}, {snap$top_rating})."))
  }

  if (!is.null(roster_data)) {
    needs <- roster_needs(roster_data, team_slug, sport)
    if (nrow(needs) > 0) {
      latest <- size_data %>%
        filter(School == team_slug, Year == max(Year),
               as.character(PosGroup) %in% needs$PosGroup) %>% nrow()
      notes <- c(notes, glue(
        "Roster attrition watch: {paste0(needs$PosGroup, ' (', needs$pct,
        '% JR/SR)', collapse = ' and ')} turn over soonest — the latest class ",
        "added {latest} commit{ifelse(latest == 1, '', 's')} at those spots."))
    }
  }

  ## home-state retention over the window
  pool <- size_data %>% filter(State == st)
  if (nrow(pool) > 0) {
    own <- sum(pool$School == team_slug)
    total <- nrow(pool)
    leader <- pool %>% count(TeamName, sort = TRUE) %>% slice(1)
    notes <- c(notes, glue(
      "{st} high-school talent: {own} of {total} {st} commits to {conf_label(team_slug)} ",
      "programs signed with {t_lab} ({round(100 * own / total)}%). ",
      "Top in-state recruiter: {leader$TeamName} ({leader$n})."))
  }

  ## trench positioning (football only)
  if (tolower(sport) == "football") {
    tr <- size_data %>% filter(Trench) %>% team_size_summary() %>%
      arrange(desc(AvgWeight))
    trk <- which(tr$School == team_slug)
    if (length(trk) == 1) {
      notes <- c(notes, glue(
        "Trench profile: #{trk} of {nrow(tr)} in average OL + DL/Edge weight ",
        "({round(tr$AvgWeight[trk])} lbs)."))
    }
  }
  notes
}

## ---------------------------------------------------------------------------
## 10) TALENT vs RESULTS -- joins CFBD season records (team_seasons_football)
##     to a rolling talent composite from the recruiting data
## ---------------------------------------------------------------------------

## talent entering season Y = mean rating of all additions (HS commits +
## portal transfers) in the classes (Y-3 .. Y); early seasons use whatever
## classes exist from 2016 on
talent_composites <- function(size_data, seasons) {
  grid <- expand.grid(School = unique(size_data$School), year = seasons,
                      stringsAsFactors = FALSE)
  ## base subsetting on purpose: dplyr's data mask would make
  ## School == .data$School compare the column to itself.
  ## TOP-20 mean, not a raw mean: a plain average rewards tiny classes
  ## (12 signees at 88 beats 25 at 87 with five blue chips); averaging the
  ## window's top 20 ratings rewards accumulating real talent.
  grid$composite <- mapply(function(s, y) {
    r <- size_data$Ranking[size_data$School == s &
                             size_data$Year >= y - 3 &
                             size_data$Year <= y]
    r <- sort(r[!is.na(r)], decreasing = TRUE)
    if (length(r) == 0) NA_real_ else mean(head(r, 20))
  }, grid$School, grid$year)
  grid
}

## TABLE TWIN: the EXACT per-program frame plot_talent_results() draws.
## Contract columns: School, talent, win_pct, seasons_n (+ the chart's own
## extras -- slug rides along for data_id parity). attrs: value_label,
## value_fmt (for the talent axis), yr_rng.
quadrant_data <- function(team_seasons, size_data, team_slug,
                          compare_slug = NULL) {
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## pool only the active team's conference members (all 16 at Phase 0): the
  ## quadrant medians + talent panel stay within-conference (team_seasons keys
  ## on slug, recruits on School)
  team_seasons <- scope_to_conf(team_seasons, team_slug, "slug")
  size_data <- scope_to_conf(size_data, team_slug)
  ## the season window shows up in the hover cards too, so a pinned card
  ## still says which seasons it summarizes
  yr_rng2 <- if (min(team_seasons$year) == max(team_seasons$year)) {
    as.character(max(team_seasons$year))
  } else paste0(min(team_seasons$year), "–", max(team_seasons$year))
  comp <- talent_composites(size_data, sort(unique(team_seasons$year)))

  agg <- team_seasons %>%
    left_join(comp, by = c("slug" = "School", "year")) %>%
    group_by(slug) %>%
    summarize(
      talent = mean(composite, na.rm = TRUE),
      win_pct = 100 * sum(wins) / sum(wins + losses),
      sp = mean(sp_rating, na.rm = TRUE),
      W = sum(wins), L = sum(losses),
      seasons_n = dplyr::n(),
      best = paste0(year[which.max(wins)], " (", max(wins), " wins)"),
      .groups = "drop") %>%
    mutate(
      School = slug,
      TeamName = team_label(slug),
      role = case_when(slug == team_slug ~ "main",
                       slug == cmp_safe ~ "compare",
                       TRUE ~ "other"),
      tip = glue(
        "<b>{TeamName} ({yr_rng2})</b><br/>",
        "Talent composite: {round(talent, 1)}<br/>",
        "Record {W}–{L} ({round(win_pct)}% wins)<br/>",
        "Avg SP+: {round(sp, 1)} • Best season: {best}<br/>",
        "<em>Tap the dot to pin this card</em>"))

  attr(agg, "value_label") <- "Talent composite (247 rating points)"
  attr(agg, "value_fmt") <- "%.1f"
  ## the chart's formatter closure (matches the hover card's rounding)
  attr(agg, "value_fmt_fn") <- function(v) sprintf("%.1f", v)
  attr(agg, "yr_rng") <- yr_rng2
  agg
}

## the over/under-achiever quadrant: 10-season averages per program
plot_talent_results <- function(team_seasons, size_data, team_slug,
                                compare_slug = NULL) {
  hl <- highlight_colors(team_slug, compare_slug)
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## single source of truth: the chart draws exactly the table twin's frame
  agg <- quadrant_data(team_seasons, size_data, team_slug,
                       compare_slug = compare_slug)

  med_t <- median(agg$talent, na.rm = TRUE)
  med_w <- median(agg$win_pct, na.rm = TRUE)
  role_cols <- c(main = unname(hl["main"]),
                 compare = ifelse(cmp_safe == "", "grey55",
                                  team_color(cmp_safe)),
                 other = "grey55")
  yr_rng <- paste0(min(team_seasons$year), "–", max(team_seasons$year))
  ## realignment honesty: the median lines pool the conference's CURRENT
  ## membership, so a window reaching before it was whole backcasts programs
  ## onto seasons predating their membership (count + seam year from config)
  whole_yr <- conf_whole_year(team_slug)
  bc_note <- if (min(team_seasons$year) < whole_yr) {
    glue(" Conference = the current {n_conf_members(team_slug)} members, ",
         "backcast before {whole_yr}.")
  } else ""

  ggplot(agg, aes(x = talent, y = win_pct)) +
    geom_vline(xintercept = med_t, linetype = "dashed", color = "grey55") +
    geom_hline(yintercept = med_w, linetype = "dashed", color = "grey55") +
    ## Okabe-Ito blue/vermillion: color-blind-safe, and neither collides
    ## with a team's highlight color (the old red matched Arizona's)
    annotate("text", x = min(agg$talent), y = max(agg$win_pct),
             label = "OVERACHIEVERS", hjust = 0, vjust = 0, size = 3.6,
             fontface = "bold", color = "#0072B2") +
    annotate("text", x = max(agg$talent), y = min(agg$win_pct),
             label = "UNDERACHIEVERS", hjust = 1, vjust = 1, size = 3.6,
             fontface = "bold", color = "#D55E00") +
    geom_point_interactive(aes(color = role, tooltip = tip, data_id = slug),
                           size = 5, show.legend = FALSE) +
    geom_text_repel(aes(label = TeamName, color = role), size = 3.4,
                    fontface = "bold", show.legend = FALSE, seed = 7,
                    box.padding = 0.35) +
    scale_color_manual(values = role_cols) +
    labs(
      title = wrap_title(glue(
        "Talent vs Results, {yr_rng}: Who Outplays Their Recruiting?"), 52),
      subtitle = wrap_title(glue(
        "Each dot = one program, seasons {yr_rng}. X = rolling 4-class talent ",
        "composite (mean of the window's top-20 HS + portal ratings); ",
        "Y = win percentage. Dashed lines = conference medians.{bc_note}"), 84),
      x = "Average Talent Composite (247 rating points)",
      y = "Win Percentage",
      caption = "Records: CollegeFootballData.com. Talent: 247Sports classes 2016-2026. Tap or hover dots for the receipts."
    ) +
    theme_girth()
}

## ---------------------------------------------------------------------------
## 10a) WINS ABOVE TALENT -- fit the league's talent-to-wins curve, then rank
##      each program by wins per season above/below what its talent predicts
## ---------------------------------------------------------------------------

## TABLE TWIN: the EXACT per-program frame plot_wat() draws.
## Contract columns: School, value (= WAT, wins/season above expected),
## n (= seasons_n), plus actual/expected win pct and the chart's extras.
## attrs: value_label, value_fmt, value_fmt_fn, yr_rng, model_note.
wat_data <- function(team_seasons, size_data, team_slug, compare_slug = NULL) {
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## pool only the active team's conference members (all 16 at Phase 0): the
  ## WAT fit stays a within-conference residual (team_seasons keys on slug,
  ## recruits on School)
  team_seasons <- scope_to_conf(team_seasons, team_slug, "slug")
  size_data <- scope_to_conf(size_data, team_slug)
  yr_rng2 <- if (min(team_seasons$year) == max(team_seasons$year)) {
    as.character(max(team_seasons$year))
  } else paste0(min(team_seasons$year), "-", max(team_seasons$year))

  ## SAME panel construction plot_talent_results uses: the rolling top-20
  ## talent composite joined to each program-season's record. One row per
  ## program-season (~160 across the conference's members x the season window).
  comp <- talent_composites(size_data, sort(unique(team_seasons$year)))
  panel <- team_seasons %>%
    left_join(comp, by = c("slug" = "School", "year")) %>%
    mutate(games = wins + losses) %>%
    filter(!is.na(composite), games > 0)

  ## the league talent-to-wins curve: a quasibinomial fit of season win rate
  ## on the talent composite, prior-weighted by games (base stats, no new
  ## deps). Quasibinomial lets seasons run over/under-dispersed vs a strict
  ## binomial without moving the fitted curve. The intercept's estimating
  ## equation makes league expected wins equal league actual wins, so WAT
  ## nets to ~zero across the conference (a fair over/under-achiever split).
  fit <- stats::glm(cbind(wins, losses) ~ composite,
                    family = stats::quasibinomial(), data = panel)
  panel$exp_p <- as.numeric(stats::predict(fit, type = "response"))
  panel$exp_wins <- panel$exp_p * panel$games

  board <- panel %>%
    group_by(slug) %>%
    summarize(
      seasons_n = dplyr::n(),
      W = sum(wins), L = sum(losses),
      games = sum(games),
      exp_wins = sum(exp_wins),
      talent = mean(composite, na.rm = TRUE),
      .groups = "drop") %>%
    mutate(
      actual = 100 * W / games,
      expected = 100 * exp_wins / games,
      mean_games = games / seasons_n,
      ## WAT in WINS PER SEASON = the win-rate gap x the mean season length
      wat = (actual - expected) / 100 * mean_games,
      School = slug,
      TeamName = team_label(slug),
      role = case_when(slug == team_slug ~ "main",
                       slug == cmp_safe ~ "compare",
                       TRUE ~ "other"),
      value = wat,
      n = seasons_n,
      wat_abs = round(abs(wat), 1),
      ## near-zero collapses to a neutral read: |WAT| < 0.05 rounds to 0.0,
      ## and "-0.0 W/yr" / "won about 0 games more" looks like a rounding bug
      near_even = wat_abs < 0.05,
      tip = glue(
        "<b>{TeamName} ({yr_rng2})</b><br/>",
        "Actual {round(actual)}% wins  vs  expected {round(expected)}% ",
        "given talent<br/>",
        "{ifelse(near_even,
                 paste0('About as many wins as expected given its talent.'),
                 paste0('Given its talent, ', TeamName, ' won about ', wat_abs,
                        ' ', ifelse(wat_abs == 1, 'game', 'games'),
                        ' per season ', ifelse(wat >= 0, 'more', 'fewer'),
                        ' than expected.'))}<br/>",
        "<em>{W}-{L} over {seasons_n} seasons</em>"),
      lab = ifelse(near_even, "~even",
                   paste0(ifelse(wat >= 0, "+", "-"), wat_abs, " W/yr"))) %>%
    arrange(wat) %>%
    mutate(TeamName = factor(TeamName, levels = TeamName))

  attr(board, "value_label") <- "Wins above talent (per season)"
  attr(board, "value_fmt") <- "%+.1f"
  ## chart's formatter: "+2.1" / "-1.4", matching the row label sans unit
  attr(board, "value_fmt_fn") <- function(v) {
    paste0(ifelse(v >= 0, "+", "-"), round(abs(v), 1))
  }
  attr(board, "yr_rng") <- yr_rng2
  attr(board, "model_note") <- glue("{nrow(panel)} program-seasons")
  board
}

## the ladder: expected win % (grey) vs actual (role color) per program,
## ranked by wins above/below what talent predicts
plot_wat <- function(team_seasons, size_data, team_slug, compare_slug = NULL) {
  hl <- highlight_colors(team_slug, compare_slug)
  cmp_safe <- if (is.null(compare_slug)) "" else compare_slug
  ## single source of truth: the ladder draws exactly the table twin's frame
  board <- wat_data(team_seasons, size_data, team_slug,
                    compare_slug = compare_slug)
  yr_rng <- attr(board, "yr_rng")
  model_note <- attr(board, "model_note")
  n_seasons <- length(unique(team_seasons$year))
  logos <- team_logo_labels(width = 30, prefix = "www/")
  ## boards keep the compare team's true primary color (rows don't overlap)
  role_cols <- c(main = unname(hl["main"]),
                 compare = ifelse(cmp_safe == "", "grey55",
                                  team_color(cmp_safe)),
                 other = "grey60")
  ## the WAT label rides the far end of each dumbbell
  board$lab_x <- pmax(board$actual, board$expected)

  ggplot(board, aes(y = TeamName)) +
    ## the gap from expected (grey) to actual (role color) IS the story;
    ## color the connector by role so main/compare pop
    geom_segment(aes(x = expected, xend = actual, yend = TeamName,
                     color = role), linewidth = 1.4, show.legend = FALSE) +
    geom_point(aes(x = expected), color = "grey65", size = 3.6) +
    geom_point_interactive(aes(x = actual, color = role, tooltip = tip,
                               data_id = School),
                           size = 5, show.legend = FALSE) +
    geom_text(aes(x = lab_x, label = lab, color = role), hjust = -0.2,
              size = 3.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = role_cols) +
    scale_y_discrete(labels = logos) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0.2)),
                       labels = function(x) paste0(round(x), "%")) +
    labs(
      title = wrap_title("Wins Above Talent: Who Beats Their Recruiting?", 44),
      subtitle = wrap_title(glue(
        "Seasons {yr_rng}. Grey dot = expected win % from the league ",
        "talent-to-wins fit; colored dot = actual. Row label = wins per ",
        "season above (+) or below (-) that expectation. ",
        "{team_label(team_slug)}",
        "{ifelse(cmp_safe == '', '', paste0(' vs ', team_label(cmp_safe)))}",
        " highlighted."), 60),
      x = "Win Percentage", y = NULL,
      caption = wrap_title(glue(
        "Expected = a quasibinomial fit of season wins on the rolling ",
        "4-class talent composite ({model_note}), over {n_seasons} seasons ",
        "in the window. Records: CollegeFootballData.com; talent: 247Sports. ",
        "The season-by-season Scoreboard chart is unchanged."), 95)
    ) +
    theme_girth_md()
}

## one team's season-by-season scoreboard: wins bars + talent line
plot_team_scoreboard <- function(team_seasons, size_data, team_slug) {
  hl <- highlight_colors(team_slug)
  t_lab <- team_label(team_slug)
  comp <- talent_composites(
    size_data %>% filter(School == team_slug),
    sort(unique(team_seasons$year)))

  d <- team_seasons %>%
    filter(slug == team_slug) %>%
    left_join(comp, by = c("slug" = "School", "year")) %>%
    arrange(year) %>%
    mutate(tip = glue(
      "<b>{year} season</b><br/>",
      "Record {wins}–{losses} ({coalesce(conf_wins, 0)}–",
      "{coalesce(conf_losses, 0)} conf)<br/>",
      "SP+: {ifelse(is.na(sp_rating), 'n/a', round(sp_rating, 1))} • ",
      "Talent composite: {round(composite, 1)}"))
  if (nrow(d) == 0) return(NULL)

  ## scale the composite onto the wins axis for the overlay line
  c_rng <- range(d$composite, na.rm = TRUE)
  w_max <- max(d$wins, na.rm = TRUE)
  d <- d %>%
    mutate(comp_scaled = (composite - c_rng[1]) /
             max(diff(c_rng), 0.001) * w_max * 0.9 + w_max * 0.05)

  ggplot(d, aes(x = year)) +
    geom_col_interactive(aes(y = wins, tooltip = tip, data_id = year),
                         fill = hl["main"], alpha = 0.85, width = 0.7) +
    geom_line(aes(y = comp_scaled), color = "#0C234B", linewidth = 1.2,
              linetype = "longdash") +
    geom_point(aes(y = comp_scaled), color = "#0C234B", size = 2.6) +
    geom_text(aes(y = wins, label = wins), vjust = -0.5, size = 3.4,
              fontface = "bold", color = "grey25") +
    scale_x_continuous(breaks = d$year) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ (. - w_max * 0.05) / (w_max * 0.9) * max(diff(c_rng), 0.001) +
          c_rng[1],
        name = "Talent Composite (dashed line)")) +
    labs(
      title = wrap_title(glue("{t_lab} Scoreboard: Wins vs Talent on Hand"), 52),
      subtitle = wrap_title(paste(
        "Bars = wins per season; dashed navy line = the rolling 4-class",
        "talent composite entering that season."), 84),
      x = "Season", y = "Wins",
      caption = "Tap or hover a bar for the season card. Records: CollegeFootballData.com."
    ) +
    theme_girth()
}

## ---------------------------------------------------------------------------
## 11) TALKING POINTS -- auto-generated podcast / message-board fodder
## ---------------------------------------------------------------------------
make_talking_points <- function(size_data, team_slug, sport,
                                year_min = NULL, year_max = NULL) {
  if (!is.null(year_min)) {
    size_data <- dplyr::filter(size_data, Year >= year_min, Year <= year_max)
  }
  ## pool only the active team's conference members (all 16 at Phase 0)
  size_data <- scope_to_conf(size_data, team_slug)
  t_lab <- team_label(team_slug)
  yr_rng <- paste0(min(size_data$Year), "–", max(size_data$Year))
  pts <- character(0)
  ## wording adapts to what's in the pool (HS commits vs commits + portal)
  word <- if (any(size_data$Type == "Transfer", na.rm = TRUE)) {
    "additions (HS + portal)"
  } else "commits"

  ## overall beef rank
  board <- team_size_summary(size_data) %>% arrange(desc(AvgWeight))
  rk <- which(board$School == team_slug)
  if (length(rk) == 1) {
    pts <- c(pts, glue(
      "{t_lab} {word} average {round(board$AvgWeight[rk], 0)} lbs at ",
      "{format_height(board$AvgHeight[rk])} — the #{rk} heaviest haul ",
      "of the {nrow(board)} {conf_label(team_slug)} programs ({yr_rng})."))
  }

  ## trench rank (football only)
  if (tolower(sport) == "football") {
    tr <- size_data %>% filter(Trench) %>% team_size_summary() %>%
      arrange(desc(AvgWeight))
    trk <- which(tr$School == team_slug)
    if (length(trk) == 1) {
      pts <- c(pts, glue(
        "In the trenches (OL + DL/Edge), {t_lab} signs an average of ",
        "{round(tr$AvgWeight[trk], 0)} lbs — #{trk} in the conference. ",
        "{ifelse(trk <= 4, 'Games are won up front, and the staff knows it.',
                 ifelse(trk >= 13, 'The board will not love that number.',
                        'Solidly mid-pack up front.'))}"))
    }
  }

  ## biggest + tallest signees
  team_rows <- size_data %>% filter(School == team_slug)
  if (nrow(team_rows) > 0) {
    big <- team_rows %>% slice_max(Weight, n = 1, with_ties = FALSE)
    tall <- team_rows %>% slice_max(Height_in, n = 1, with_ties = FALSE)
    dense <- team_rows %>% slice_max(LbsPerInch, n = 1, with_ties = FALSE)
    pts <- c(pts,
      glue("Biggest body {t_lab} signed: {big$Name} ({big$Position}, ",
           "{big$Year}) at {big$HeightLabel}, {big$Weight} lbs."),
      glue("Tallest: {tall$Name} ({tall$Position}, {tall$Year}) at ",
           "{tall$HeightLabel}. Most pounds-per-inch: {dense$Name} ",
           "({dense$Position}, {dense$Year}) carrying {dense$LbsPerInch} ",
           "lbs per inch."))
  }

  ## conference superlatives
  if (nrow(board) > 1) {
    pts <- c(pts, glue(
      "Conference-wide: {board$TeamName[1]} signs the heaviest classes ",
      "({round(board$AvgWeight[1], 0)} lbs avg) while ",
      "{board$TeamName[nrow(board)]} signs the lightest ",
      "({round(board$AvgWeight[nrow(board)], 0)} lbs avg)."))
  }

  ## trend: latest full class vs the three before it
  yrs <- sort(unique(team_rows$Year))
  if (length(yrs) >= 4) {
    latest <- max(yrs)
    recent <- team_rows %>% filter(Year == latest) %>%
      summarize(w = mean(Weight)) %>% pull(w)
    prior <- team_rows %>% filter(Year %in% (latest - 3):(latest - 1)) %>%
      summarize(w = mean(Weight)) %>% pull(w)
    delta <- round(recent - prior, 0)
    if (is.finite(delta) && delta != 0) {
      pts <- c(pts, glue(
        "The {latest} {t_lab} class runs {abs(delta)} lbs ",
        "{ifelse(delta > 0, 'HEAVIER', 'lighter')} per player than the ",
        "previous three classes — ",
        "{ifelse(delta > 0, 'they are getting bigger.', 'a leaner profile.')}"))
    }
  }

  pts
}

## ---------------------------------------------------------------------------
## 12) RANKED INSIGHTS -- the Home talking points, SCORED so the most
##     notable rise to the top. Each candidate carries the size of the pool
##     that backs it (n) and a notability score = magnitude x recency; only
##     pools with at least MIN_INSIGHT_N players qualify (a two-signee
##     "insight" isn't one). Built from the SAME summaries
##     make_talking_points()/class_snapshot() use, so nothing here can
##     contradict that list -- it just orders and n-gates the same facts.
## ---------------------------------------------------------------------------

## pool floor: an insight below this many players is dropped, not ranked
MIN_INSIGHT_N <- 8L

## returns data.frame(sentence, score, n) sorted by score desc (n-gated).
## `sport` gates the football-only trench line; `year_min/max` optionally
## re-window before scoring (the app passes an already-windowed pool).
ranked_insights <- function(size_data, team_slug, sport,
                            year_min = NULL, year_max = NULL) {
  empty <- data.frame(sentence = character(0), score = numeric(0),
                      n = integer(0), stringsAsFactors = FALSE)
  if (!is.null(year_min)) {
    size_data <- dplyr::filter(size_data, Year >= year_min, Year <= year_max)
  }
  ## pool only the active team's conference members (all 16 at Phase 0)
  size_data <- scope_to_conf(size_data, team_slug)
  if (nrow(size_data) == 0) return(empty)

  t_lab  <- team_label(team_slug)
  yr_rng <- paste0(min(size_data$Year), "-", max(size_data$Year))

  ## candidate accumulator: add() applies the n-gate + drops NA scores, so
  ## every caller below can stay declarative
  cand <- list()
  add <- function(sentence, score, n) {
    if (length(score) != 1 || is.na(score) || length(n) != 1 || is.na(n) ||
        n < MIN_INSIGHT_N) {
      return(invisible())
    }
    cand[[length(cand) + 1L]] <<- list(sentence = as.character(sentence),
                                       score = as.numeric(score),
                                       n = as.integer(n))
  }
  ## rank extremity: 0 dead-center of the pack, 1 at either edge -- the
  ## magnitude term for a "#k of N" placement
  extremity <- function(rk, nb) {
    if (nb <= 1) return(0)
    abs((nb + 1) / 2 - rk) / ((nb - 1) / 2)
  }

  team_rows <- dplyr::filter(size_data, School == team_slug)
  n_team <- nrow(team_rows)

  ## (a) overall beef rank -- window-wide, so a mid recency weight (0.70)
  board <- team_size_summary(size_data) %>% dplyr::arrange(dplyr::desc(AvgWeight))
  nb <- nrow(board)
  rk <- which(board$School == team_slug)
  if (length(rk) == 1 && nb > 1) {
    add(glue("{t_lab} averages {round(board$AvgWeight[rk])} lbs at ",
             "{format_height(board$AvgHeight[rk])} - the #{rk} heaviest ",
             "haul of {nb} {conf_label(team_slug)} programs ({yr_rng})."),
        score = extremity(rk, nb) * 0.70, n = n_team)
  }

  ## (b) trenches (football only) -- games are won up front, slight boost
  if (tolower(sport) == "football") {
    tr <- dplyr::filter(size_data, Trench)
    tr_team <- sum(tr$School == team_slug)
    trb <- tr %>% team_size_summary() %>% dplyr::arrange(dplyr::desc(AvgWeight))
    ntb <- nrow(trb)
    trk <- which(trb$School == team_slug)
    if (length(trk) == 1 && ntb > 1) {
      add(glue("In the trenches (OL + DL/Edge), {t_lab} signs ",
               "{round(trb$AvgWeight[trk])} lbs on average - #{trk} of ",
               "{ntb} in the {conf_label(team_slug)}."),
          score = extremity(trk, ntb) * 0.72, n = tr_team)
    }
  }

  ## newest class vs the three before it: high recency (1.0) -- this is the
  ## freshest signal on the board. Cap at the ARRIVING class (class of N
  ## enrolls fall N): a seeded, still-filling future cycle (2027 today) is a
  ## thin, volatile pool and must not headline the Home page as if it were a
  ## settled recruiting result -- same cap the boards use.
  snap_cap <- min(max(size_data$Year), as.integer(format(Sys.Date(), "%Y")))
  snap <- class_snapshot(size_data, team_slug, snap_year = snap_cap)
  if (!is.null(snap)) {
    ## (c) rating move -- normalized by 3 rating points (a big class swing)
    if (!is.na(snap$d_rating) && snap$d_rating != 0) {
      add(glue("The {snap$year} class grades {sprintf('%+.1f', snap$d_rating)} ",
               "in average 247 rating vs the prior three - {snap$avg_rating} ",
               "avg, {snap$blue} blue-chip",
               "{ifelse(snap$blue == 1, '', 's')} (90+)."),
          score = min(abs(snap$d_rating) / 3, 1) * 1.0, n = snap$n)
    }
    ## (d) weight move -- normalized by 20 lbs (a program-shifting jump)
    if (!is.na(snap$d_weight) && snap$d_weight != 0) {
      add(glue("The {snap$year} {t_lab} class runs {abs(snap$d_weight)} lbs ",
               "{ifelse(snap$d_weight > 0, 'heavier', 'lighter')} per player ",
               "than the previous three classes."),
          score = min(abs(snap$d_weight) / 20, 1) * 1.0, n = snap$n)
    }
  }

  ## (e) home-state hold -- window-wide (0.68); magnitude = distance from a
  ## 50/50 split of the state's conference-bound talent
  st <- team_state(team_slug)
  pool <- dplyr::filter(size_data, State == st)
  if (nrow(pool) > 0) {
    own <- sum(pool$School == team_slug)
    total <- nrow(pool)
    share <- own / total
    add(glue("{t_lab} holds {own} of {total} {conf_label(team_slug)}-bound {st} recruits ",
             "({round(100 * share)}%) over {yr_rng}."),
        score = min(abs(share - 0.5) * 2, 1) * 0.68, n = total)
  }

  if (length(cand) == 0) return(empty)
  out <- data.frame(
    sentence = vapply(cand, function(x) x$sentence, character(1)),
    score    = vapply(cand, function(x) x$score, numeric(1)),
    n        = vapply(cand, function(x) x$n, integer(1)),
    stringsAsFactors = FALSE)
  out[order(-out$score), , drop = FALSE]
}
