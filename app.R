## ===========================================================================
## Power-4 Girth Index — v9
## Recruiting size, talent, geography, development, and results for all 67
## SEC, Big Ten, ACC, and Big 12 programs in football and basketball.
## v9 adds the editorial Home, Program Fingerprint, direct task routes,
## responsive global controls, page orientation, and exact-view sharing.
## Core analytics remain conference-aware and realignment-honest.
## Plot builders: R/girth_plots.R | Home: R/home_fingerprint.R
## Team/conference metadata: R/team_config.R
## ===========================================================================

#rsconnect::deployApp()

# connect to .db (works locally and in the hosted Shiny deployment) -->
db_path <- file.path("data", "recruiting.db")
conn <- dbConnect(RSQLite::SQLite(), db_path)

## the display ceiling: the app tracks at most ONE cycle ahead of the
## calendar (the class of N signs Dec N-1 and enrolls fall N). The nightly
## pipeline enforces the same cap at scrape time; this WHERE is defense in
## depth so a stray future-year row can never stretch the year slider, the
## default window, or the class previews (in July 2026 a few real class-of-
## 2028 commits leaked in via an uncapped ahead-year probe and did exactly
## that).
CYCLE_CAP <- as.integer(format(Sys.Date(), "%Y")) + 1L

## Preload both sports once. Talent Origins must start from RAW recruiting
## rows: prep_size_data() intentionally drops missing/implausible body fields,
## which is correct for body charts but would bias geography. We derive a
## dedicated origin frame before freeing each temporary raw table.
raw_football <- safe_query(conn, paste0(
  "SELECT * FROM recruit_class_football WHERE Year <= ", CYCLE_CAP))
origin_football <- prep_origin_data(raw_football, "football")
size_football <- prep_size_data(raw_football, "football")
rm(raw_football)

raw_basketball <- safe_query(conn, paste0(
  "SELECT * FROM recruit_class_basketball WHERE Year <= ", CYCLE_CAP))
origin_basketball <- prep_origin_data(raw_basketball, "basketball")
size_basketball <- prep_size_data(raw_basketball, "basketball")
rm(raw_basketball)

## current rosters (from scripts/scrapeRosters.R); NULL if not scraped yet
load_roster <- function(tbl) {
  if (tbl %in% dbListTables(conn)) dbGetQuery(conn, paste0("SELECT * FROM ", tbl))
  else NULL
}
roster_football <- load_roster("roster_football")
roster_basketball <- load_roster("roster_basketball")

## CFBD season outcomes (from scripts/fetchOutcomes.R); NULL until fetched
team_seasons <- load_roster("team_seasons_football")

## Dashboard status distinguishes the newest source capture from the nightly
## pipeline ledger. Backfills can legitimately make the bundled rows newer
## than refresh_log, so source capture is the truthful snapshot date.
refresh_meta <- dashboard_refresh_meta(conn)
last_refresh_date <- refresh_meta$capture_date
if (is.null(last_refresh_date)) last_refresh_date <- refresh_meta$updated_date

## display form, e.g. 'Jul 10, 2026' (leading zero on the day stripped)
last_refresh_label <- if (is.null(last_refresh_date)) NULL else
  sub(" 0", " ", format(last_refresh_date, "%b %d, %Y"), fixed = TRUE)

## Namespace render caches to the bundled source snapshot. A refreshed database
## can never reuse charts produced from an earlier set of rows.
DATA_REVISION <- paste(
  if (is.null(last_refresh_date)) "unknown" else format(last_refresh_date, "%Y%m%d"),
  refresh_meta$sources$recruiting$football$rows,
  refresh_meta$sources$recruiting$basketball$rows,
  refresh_meta$sources$rosters$football$rows,
  refresh_meta$sources$rosters$basketball$rows,
  sep = "-"
)

## attribution line appended to COPIED brief text only (never the on-screen
## brief) -- so a pasted talking-point list carries its provenance. Rides on
## each copy button as data-footer; the copy script tacks it on last.
copy_footer <- if (is.null(last_refresh_label))
  "girthindex.desertdatalab.com" else
  paste0("source data captured ", last_refresh_label,
         " - girthindex.desertdatalab.com")

## free what startup allocated -- the deployed worker has a hard 1GB ceiling
invisible(gc())

SIZE_YEARS <- range(c(size_football$Year, size_basketball$Year))

## Four recent classes describe the active roster pipeline while keeping first
## renders fast and comparisons timely.
DEFAULT_YEARS <- c(SIZE_YEARS[2] - 3, SIZE_YEARS[2])

## recruiting runs a year ahead of arrival: the class of year N signs in
## Dec N-1 / Feb N and ENROLLS fall N, and the next cycle's 247 pages go
## live (with commits) the summer before. So once the db carries that next
## cycle, the newest class is NOT the one arriving this season -- the War
## Room pins its "incoming adds" to the class that enrolls this fall
## (capped at the newest cycle actually in the db).
arriving_class <- as.integer(format(Sys.Date(), "%Y"))
war_room_class <- min(SIZE_YEARS[2], arriving_class)

## the DISPLAY universe: onboarded teams only. Every team picker (g_team /
## g_compare choices, the first-visit logo grid, the "who's your team" modal)
## and deep-link validator uses it. All 67 production rows are live; the gate
## still prevents a partially backfilled program from appearing accidentally.
DISPLAY_CONFIG <- TEAM_CONFIG[TEAM_CONFIG$onboarded %in% TRUE, , drop = FALSE]

## named choices for team pickers (slug values, pretty labels)
team_choices <- setNames(DISPLAY_CONFIG$slug, DISPLAY_CONFIG$team_name)

## GROUPED team choices (selectize optgroups): a flat list of 67 teams is a
## wall to scan, so the picker groups by conference -- Big 12 first (the default
## team's league), then SEC / Big Ten / ACC -- alphabetized within each. Paired
## with per-option logos + selectize's type-to-search, picking a team is fast.
conf_grouped_choices <- function() {
  out <- list()
  for (cf in conf_order()) {
    d <- DISPLAY_CONFIG[DISPLAY_CONFIG$conference == cf, , drop = FALSE]
    if (!nrow(d)) next
    d <- d[order(d$team_name), , drop = FALSE]
    out[[cf]] <- setNames(d$slug, d$team_name)
  }
  out
}
team_choices_grouped <- conf_grouped_choices()

## the standing "n per conference" line for the Conference Lab honesty modal,
## built from the data so it can't drift from the onboarded set
CONF_COUNT_LINE <- paste(
  vapply(conf_order(), function(cf)
    paste0(cf, " ", length(onboarded_slugs(cf))), character(1)),
  collapse = " · ")

## slug -> logo URL map for the selectize render (JS looks logos up by option
## value). Logos live in www/ and serve from the app root, so the bare filename
## is a valid <img src>.
gi_logo_map_js <- paste0(
  "window.GI_LOGOS = ",
  jsonlite::toJSON(setNames(as.list(DISPLAY_CONFIG$logo), DISPLAY_CONFIG$slug),
                  auto_unbox = TRUE), ";")

## shared selectize render for both team pickers: a small logo + the team name,
## in BOTH the dropdown option and the selected item. Missing logo (or the
## compare "none" option) degrades to text only.
gi_picker_render <- I("{
  option: function(item, escape) {
    var lg = (window.GI_LOGOS || {})[item.value];
    var img = lg ? '<img class=\"gi-opt-logo\" src=\"' + lg + '\" alt=\"\"/>'
                 : '<span class=\"gi-opt-logo gi-opt-blank\"></span>';
    return '<div class=\"gi-opt\">' + img + '<span>' + escape(item.label) +
           '</span></div>';
  },
  item: function(item, escape) {
    var lg = (window.GI_LOGOS || {})[item.value];
    var img = lg ? '<img class=\"gi-opt-logo\" src=\"' + lg + '\" alt=\"\"/>' : '';
    return '<div class=\"gi-item\" title=\"' + escape(item.label) + '\">' +
           img + '<span>' + escape(item.label) + '</span></div>';
  }
}")

## ---- URL DEEP LINKS: the query-string <-> global-state contract ----------
## Every global control (+ the active tab) serializes to the query string so
## a view is a shareable link; on load the link rehydrates the app. The
## parser is STRICT and PURE (no reactive reads) so it can be unit-tested:
## every param is whitelisted against the real config, and anything forged,
## out of range, or malformed is dropped so the app silently falls back to
## its defaults. tabName values must stay in sync with the sidebarMenu below.
VALID_TABS <- c("home", "sizelab", "beef", "conflab", "weightroom", "eras",
                "brief", "results", "origins", "summary", "compare", "notes")

parse_url_state <- function(query) {
  out <- list()
  if (!is.list(query)) return(out)
  ## a single, non-NA scalar string for key k, or NULL
  g <- function(k) {
    v <- query[[k]]
    if (is.null(v) || length(v) != 1 || is.na(v)) return(NULL)
    as.character(v)
  }
  ## Shared links resolve only to live programs; all 67 production programs are
  ## onboarded while the gate still rejects any hidden partial-backfill row.
  slugs <- onboarded_slugs()

  team <- g("team")
  if (!is.null(team) && team %in% slugs) out$team <- team

  ## compare: an explicit "none" (or empty) clears it; a valid slug sets it
  cmp <- g("cmp")
  if (!is.null(cmp)) {
    if (cmp %in% c("none", "")) out$compare <- ""
    else if (cmp %in% slugs) out$compare <- cmp
  }

  sport <- g("sport")
  if (!is.null(sport) && sport %in% c("football", "basketball")) {
    out$sport <- sport
  }

  ## years as "y1-y2": both 4-digit, ordered, inside the slider's domain
  yv <- g("years")
  if (!is.null(yv)) {
    m <- regmatches(yv, regexec("^([0-9]{4})-([0-9]{4})$", yv))[[1]]
    if (length(m) == 3) {
      y1 <- suppressWarnings(as.integer(m[2]))
      y2 <- suppressWarnings(as.integer(m[3]))
      if (!is.na(y1) && !is.na(y2) && y1 <= y2 &&
          y1 >= SIZE_YEARS[1] && y2 <= SIZE_YEARS[2]) {
        out$years <- c(y1, y2)
      }
    }
  }

  ## player pool: the real g_type radio values
  typ <- g("type")
  if (!is.null(typ) && typ %in% c("commit", "both", "transfer")) {
    out$type <- typ
  }

  ## Talent Origins keeps a small local story state. These values are strict
  ## enums/codes so a copied link restores the exact chart without letting a
  ## forged query create arbitrary inputs.
  ov <- g("ov")
  if (!is.null(ov) && ov %in% c("board", "positions", "trend"))
    out$origin_view <- ov

  om <- g("om")
  if (!is.null(om) && om %in% unname(origin_metric_choices()))
    out$origin_metric <- om

  op <- g("op")
  valid_origin_pos <- unique(c("All", origin_position_levels("football"),
                               origin_position_levels("basketball")))
  if (!is.null(op) && op %in% valid_origin_pos) out$origin_pos <- op

  os <- toupper(g("os") %||% "")
  if (nzchar(os) && os %in% ORIGIN_US_CODES) out$origin_state <- os

  tab <- g("tab")
  if (!is.null(tab) && tab %in% VALID_TABS) {
    ## v8 Distance Lab links now land on the consolidated Program Reach page.
    out$tab <- tab
  }

  out
}

## position-group filter choices per sport
pos_choices <- function(sport) {
  if (tolower(sport) == "football") {
    c("All", "Trenches (OL + DL/Edge)",
      setdiff(position_levels("football"), "Other"))
  } else {
    c("All", setdiff(position_levels("basketball"), "Other"))
  }
}

## girth metric choices (Conference Beef)
metric_choices <- c("Average Weight" = "AvgWeight",
                    "Average Height" = "AvgHeight",
                    "Pounds per Inch" = "AvgLbsPerIn",
                    "Average BMI" = "AvgBMI")

## small info-circle link for box titles -> opens a sources/methods modal
info_btn <- function(id) {
  label <- paste("About", gsub("_", " ", sub("^info_", "", id)), "methods")
  actionLink(id, label = NULL, icon = icon("circle-info"),
             `aria-label` = label, title = label,
             style = "color:inherit; opacity:0.75; margin-left:8px;")
}

## spinner that keeps the previous chart visible (dimmed) instead of
## collapsing the box on every control change
spin <- function(out, color = "#0C234B") {
  shinycssloaders::withSpinner(out, color = color, hide.ui = FALSE)
}

## a context note: a methodology caveat shown under a chart (on by default;
## the control-bar 'Context notes' checkbox hides them all for clean exports)
ctx_note <- function(...) {
  conditionalPanel(
    condition = "input.show_context",
    div(class = "ctx-note", icon("circle-info"), tags$span(...)))
}

## ---- TABLE TWINS -----------------------------------------------------------
## Each of the four boards ships with an accessible <table> sibling built
## from the SAME *_data() frame the chart draws (single source of truth in
## R/girth_plots.R), so chart and table can never disagree. A header link
## swaps the views with a client-side class flip: the girafe is never
## re-rendered and never display:none'd (the 0-width first-paint trap), and
## the table renders lazily via input$twin_<chart_id>.

## the understated header toggle: "view the numbers" <-> "view the chart".
## JS (see the twin-toggle script) flips the box class, the visible label,
## and aria-pressed, and reports the state as input$twin_<chart_id>.
## a11y contract: role="button" because aria-pressed promises button
## semantics on what is markup-wise a link, and a CONSTANT aria-label
## ("table view") so the accessible name never swaps with the state --
## aria-pressed alone carries on/off, while sighted users still get the
## swapping visible label.
twin_toggle <- function(chart_id) {
  actionLink(
    inputId = paste0("twin_link_", chart_id),
    label = "view the numbers",
    class = "twin-toggle",
    `data-chart` = chart_id,
    role = "button",
    `aria-label` = "table view",
    `aria-pressed` = "false",
    title = "Swap between the chart and its table of numbers")
}

## percentile-bar fill along a low -> high ramp; hex computed here so the
## table ships as plain inline-styled HTML. Default = the shared blue (low)
## -> grey (mid) -> red (high) board ramp; the quadrant twin passes a
## neutral navy ramp instead (its chart already spends Okabe-Ito blue and
## vermillion on the over/underachiever vocabulary)
twin_bar_color <- function(pct, ramp = c("#0072B2", "#98A4B3", "#D55E00")) {
  m <- grDevices::colorRamp(ramp)(pmin(pmax(pct, 0), 100) / 100)
  grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
}

## build the semantic table twin of a board frame (a *_data() result).
## The frame must carry School/TeamName/role plus the named value + n
## columns; attrs value_label + value_fmt drive the value column
## (value_fmt_fn preferred when present -- AvgHeight renders 6'4.5" instead
## of raw inches). `extras` = named list of functions of the SORTED frame,
## each returning a pre-formatted character vector (extra columns between
## the value and the n chip). `caption_note` = the chart's scope line
## (source + window + pool), rendered dim after the caption so the table
## never drops the context its chart's subtitle carried. `bar_ramp` = the
## percentile-bar color stops (see twin_bar_color). Returns one HTML string.
twin_table_html <- function(frame, caption, value_col = "value",
                            n_col = "n",
                            n_chip = function(n) paste0("n=", n),
                            extras = NULL, caption_note = NULL,
                            bar_ramp = c("#0072B2", "#98A4B3", "#D55E00")) {
  esc <- htmltools::htmlEscape
  d <- as.data.frame(frame)
  ## Some conference boards append an outside-league reference. It stays
  ## visible, but sorts after the ranked field and never changes its ranks or
  ## percentile bars. Normal boards simply have no external_reference column.
  external <- if ("external_reference" %in% names(d)) {
    as.logical(d$external_reference)
  } else {
    rep(FALSE, nrow(d))
  }
  external[is.na(external)] <- FALSE
  ord <- order(ifelse(external, 1L, 0L), -d[[value_col]], na.last = TRUE)
  d <- d[ord, , drop = FALSE]
  external <- external[ord]
  n_row <- nrow(d)
  vals <- d[[value_col]]
  n_ranked <- sum(!external)

  vlab <- attr(frame, "value_label") %||% "Value"
  fmt_fn <- attr(frame, "value_fmt_fn")
  if (is.null(fmt_fn)) {
    fspec <- attr(frame, "value_fmt") %||% "%.1f"
    fmt_fn <- function(v) sprintf(fspec, v)
  }

  ## Percentile bars describe the ranked conference field. An outside reference
  ## is placed against that unchanged range, then clipped to its endpoints.
  fin <- is.finite(vals)
  ranked_fin <- fin & !external
  pct <- rep(0, n_row)
  if (sum(ranked_fin) > 1) {
    pct[ranked_fin] <- 100 * (rank(vals[ranked_fin], ties.method = "average") - 1) /
      (sum(ranked_fin) - 1)
  } else if (sum(ranked_fin) == 1) {
    pct[ranked_fin] <- 100
  }
  if (any(external & fin) && any(ranked_fin)) {
    field <- vals[ranked_fin]
    lo <- min(field)
    hi <- max(field)
    if (is.finite(lo) && is.finite(hi) && hi > lo) {
      pct[external & fin] <- pmin(pmax(100 * (vals[external & fin] - lo) / (hi - lo), 0), 100)
    } else {
      pct[external & fin] <- 50
    }
  }

  team_display <- as.character(d$TeamName)
  if (any(external)) {
    team_display[external] <- paste0(team_display[external], " (external reference)")
  }
  team <- esc(team_display)
  logo <- TEAM_CONFIG$logo[match(d$School, TEAM_CONFIG$slug)]
  logo_img <- ifelse(is.na(logo), "",
                     paste0("<img src=\"", logo,
                            "\" alt=\"\" width=\"22\"/>"))
  val_txt <- rep("n/a", n_row)
  if (any(fin)) {
    val_txt[fin] <- vapply(vals[fin],
                           function(v) as.character(fmt_fn(v)), character(1))
  }
  chip_txt <- vapply(d[[n_col]], function(n) as.character(n_chip(n)),
                     character(1))

  ex_heads <- names(extras) %||% character(0)
  ex_cells <- lapply(extras, function(f) as.character(f(d)))

  role <- as.character(d$role)
  row_cls <- ifelse(external, " class=\"twin-external\"",
                    ifelse(role == "main", " class=\"twin-main\"",
                           ifelse(role == "compare", " class=\"twin-compare\"", "")))
  rank_display <- rep("N/R", n_row)
  rank_display[!external] <- as.character(seq_len(n_ranked))
  rank_note <- ifelse(external,
                      "external Power-4 reference, not ranked in this conference",
                      paste0("rank ", rank_display, " of ", n_ranked))

  ## One concise announcement per row for keyboard / screen-reader users.
  extra_aria <- rep("", n_row)
  for (i in seq_along(ex_heads)) {
    extra_aria <- paste0(extra_aria, ", ", ex_heads[i], " ", ex_cells[[i]])
  }
  aria <- esc(paste0(team_display, ", ", val_txt, " ",
                     tolower(vlab), ", ", rank_note,
                     extra_aria, ", ", chip_txt), attribute = TRUE)

  extra_th <- if (length(ex_heads)) {
    paste0("<th scope=\"col\">", esc(ex_heads), "</th>", collapse = "")
  } else ""
  extra_td <- rep("", n_row)
  for (i in seq_along(ex_heads)) {
    extra_td <- paste0(extra_td, "<td class=\"twin-extra\">",
                       esc(ex_cells[[i]]), "</td>")
  }

  rows <- paste0(
    "<tr tabindex=\"0\"", row_cls, " aria-label=\"", aria, "\">",
    "<td class=\"twin-rank\">", rank_display, "</td>",
    "<td class=\"twin-team\">", logo_img, team, "</td>",
    "<td class=\"twin-bar-cell\"><div class=\"twin-bar\" aria-hidden=\"true\"",
    " style=\"width:", sprintf("%.1f", pct), "%;background:",
    twin_bar_color(pct, bar_ramp), ";\"></div></td>",
    "<td class=\"twin-val\">", esc(val_txt), "</td>",
    extra_td,
    "<td><span class=\"twin-n\">", esc(chip_txt), "</span></td>",
    "</tr>", collapse = "")

  cap_note <- if (is.null(caption_note) || !nzchar(caption_note)) "" else
    paste0(" <span class=\"twin-cap-note\">", esc(caption_note), "</span>")

  paste0(
    "<div class=\"twin-scroll\"><table class=\"twin-table\">",
    "<caption>", esc(caption), cap_note, "</caption>",
    "<thead><tr><th scope=\"col\">#</th><th scope=\"col\">Team</th>",
    "<th scope=\"col\">vs the field</th>",
    "<th scope=\"col\">", esc(vlab), "</th>",
    extra_th,
    "<th scope=\"col\">n</th></tr></thead>",
    "<tbody>", rows, "</tbody></table></div>")
}
## the Conference Lab table twin -- conference-keyed (4 rows), a different
## shape from the team boards, so it gets its own small renderer. Reuses the
## twin-table CSS. `tbl` is a conf_spread_table() frame.
conf_twin_html <- function(tbl, caption, caption_note = NULL) {
  esc <- htmltools::htmlEscape
  if (is.null(tbl) || nrow(tbl) == 0)
    return("<p class=\"twin-empty\">No conference data in this window.</p>")
  mlab <- attr(tbl, "metric_label") %||% "Value"
  cols <- conf_color(tbl$Conference)
  aria <- esc(paste0(tbl$Conference, ", mean ", tbl$Mean, ", median ",
                     tbl$Median, ", range ", tbl$Range, ", top team ", tbl$Top,
                     ", bottom team ", tbl$Bottom, ", ", tbl$Teams, " teams, ",
                     tbl$Players, " players"), attribute = TRUE)
  chip <- paste0("<span style=\"display:inline-block;width:11px;height:11px;",
                 "border-radius:2px;vertical-align:middle;margin-right:7px;",
                 "background:", cols, ";\"></span>")
  rows <- paste0(
    "<tr tabindex=\"0\" aria-label=\"", aria, "\">",
    "<td class=\"twin-rank\">", seq_len(nrow(tbl)), "</td>",
    "<td class=\"twin-team\">", chip, esc(tbl$Conference), "</td>",
    "<td class=\"twin-val\">", esc(tbl$Mean), "</td>",
    "<td>", esc(tbl$Median), "</td>",
    "<td>", esc(tbl$Range), "</td>",
    "<td>", esc(tbl$Top), "</td>",
    "<td>", esc(tbl$Bottom), "</td>",
    "<td><span class=\"twin-n\">", tbl$Teams, " tm · ", tbl$Players,
    "</span></td></tr>", collapse = "")
  cap_note <- if (is.null(caption_note) || !nzchar(caption_note)) "" else
    paste0(" <span class=\"twin-cap-note\">", esc(caption_note), "</span>")
  paste0(
    "<div class=\"twin-scroll\"><table class=\"twin-table\">",
    "<caption>", esc(caption), cap_note, "</caption>",
    "<thead><tr><th scope=\"col\">#</th><th scope=\"col\">Conference</th>",
    "<th scope=\"col\">Mean ", esc(mlab), "</th>",
    "<th scope=\"col\">Median</th><th scope=\"col\">Range</th>",
    "<th scope=\"col\">Top</th><th scope=\"col\">Bottom</th>",
    "<th scope=\"col\">n</th></tr></thead>",
    "<tbody>", rows, "</tbody></table></div>")
}

## public app: visitors see a generic error message, never raw R errors
## (full errors still reach the server logs for debugging)
options(shiny.sanitize.errors = TRUE)

## Render cache: flipping back to settings you've already viewed is instant.
## The data revision in the path prevents cross-snapshot chart reuse.
## disk (not memory) so it survives across sessions within a worker -- on
## hosted tiers the worker restarts after sleep, which wipes a memory cache
## before most visitors ever benefit from it
shinyOptions(cache = cachem::cache_disk(
  file.path(dirname(tempdir()), paste0("girth-cache-", DATA_REVISION)),
  max_size = 120 * 1024^2))

## the sources & methods copy behind each info button (kept out of the UI
## so the boxes stay clean -- the user opens these only when curious)
INFO_MODALS <- list(
  ## bodies that name the DATA universe take the active conference (label +
  ## member count) so the copy stays honest once a second conference lands;
  ## at Phase 0 conf_lab = "Big 12" and conf_n = 16, so the rendered text is
  ## byte-identical to the hardcoded copy it replaces.
  info_size = list(
    title = "Size Lab — sources & methods",
    body = function(conf_lab, conf_n) paste0("
      <p><strong>Source:</strong> 247Sports team commit pages, classes
      2016–", SIZE_YEARS[2], ", all ", conf_n, " ", conf_lab, " programs.
      Historical classes were backfilled during conference onboarding; the
      active class is kept current by the nightly refresh",
      if (!is.null(last_refresh_label))
        paste0(" — source capture ", last_refresh_label),
      ".</p>
      <p><strong>What counts:</strong> high-school commits by default; the
      'Players' control in the top bar can add portal transfers or isolate
      them (transfers exist for 2021 onward).</p>
      <p><strong>Caveat:</strong> heights/weights are as listed at commit
      time. Recruiting heights run optimistic — about a quarter of ", conf_lab, "
      signees are listed shorter on the roster later (see Weight Room →
      Reality Check). Treat any listed height as ±1 inch.</p>
      <p><strong>Girth index:</strong> pounds per inch of height = weight ÷
      height. BMI = 703 × weight ÷ height².</p>")),
  info_conflab = list(
    title = "Conference Lab — how it compares leagues honestly",
    body = paste0("
      <p><strong>Distribution-first, always.</strong> A conference IS its
      members, and their ranges overlap — a top-15 Big 12 class beats the SEC
      floor. So every league is drawn as a spread: each dot is one team's
      average, the shaded box is the middle 50%, the bar is the median, and the
      hollow diamond is the mean (with the top and bottom team named). The mean
      is never a lone bar that hides the overlap.</p>
      <p><strong>What you can and can't compare.</strong> The metric menu only
      offers what's honest to rank across leagues:</p>
      <ul>
      <li><strong>Green (head-to-head OK):</strong> 247 rating, blue-chip share
      (≥90), weight, pounds-per-inch — a 92 is a 92 in any league.</li>
      <li><strong>Amber (context only):</strong> in-state share and portal
      share read <em>geography and strategy</em>, not talent, and carry that
      caveat in the caption.</li>
      <li><strong>Red — deliberately absent:</strong> win %, SP+, and
      wins-above-talent are <em>not</em> in the menu. A conference plays itself,
      so its win% averages ~.500 by construction; SP+ uses recruiting as a
      prior (circular). These can't be picked, so they can't mislead.</li>
      </ul>
      <p><strong>Realignment-honest.</strong> Every aggregate is over
      <em>today's</em> membership. A team contributes only the class years it
      was actually in its current league — Texas's pre-2024 classes count for
      the Big 12 it was in then, never the SEC it joined in 2024. Widen the year
      window past 2024 and the caption tells you how many rows were excluded as
      backcast.</p>
      <p><strong>n:</strong> ", CONF_COUNT_LINE, ". The
      Pac-12 collapsed in 2024; its former members are split across all four
      leagues, so it has no column here.</p>")),
  info_beef = list(
    title = "Conference Beef — sources & methods",
    body = "
      <p><strong>Two player pools:</strong> <em>Commit classes</em> = players
      added in your year window (247Sports commit lists), at their
      <em>signing-day</em> listed sizes. <em>Current roster</em> = everyone on
      the team's 247 roster page right now, at <em>current</em> sizes.</p>
      <p><strong>Why the roster runs heavier:</strong> the same players gain
      10–25 lbs in a college program (see the Weight Room tab), so
      current-roster weights sit above commit-day weights even when the roster
      is built from those very classes. That's development, not a data
      error. The year window applies to commit classes only — a roster is a
      single current snapshot.</p>
      <p><strong>Trenches preset</strong> = OL + DL/Edge position groups.</p>
      <p><strong>Limitation:</strong> neither pool knows who STARTS. Filtering
      to starting lineups needs participation data (snap counts) — on the
      roadmap via the CollegeFootballData API.</p>"),
  info_wr = list(
    title = "Weight Room — sources & methods",
    body = "
      <p><strong>Method:</strong> each program's HS signees are matched by
      name to its current 247 roster; gain = current roster weight minus
      commit-day listed weight, divided by years on campus (so a 2021 signee
      with five S&C years isn't compared raw to a 2025 freshman). Players who
      left (transfer/NFL/graduated) can't be matched, so this measures
      development of retained signees.</p>
      <p><strong>Reality Check:</strong> same match, applied to listed
      heights — players don't usually grow after 18, so 'shrinkage' mostly
      means the recruiting profile was optimistic.</p>
      <p><strong>Caveat:</strong> all weights are self-reported by programs
      and listed by 247Sports — they're directional, not lab-measured.</p>"),
  info_retention = list(
    title = "Class Retention — sources & methods",
    body = "
      <p><strong>Method:</strong> HS signees from the last four completed
      signing classes are name-matched to the current 247 roster. The
      retention number = the share still on the roster; the unmatched are
      the attrition (portal exits, NFL departures, medicals, graduation —
      the data can't distinguish which).</p>
      <p><strong>Why it matters:</strong> in the portal era, keeping a class
      is as hard as signing one. A program that signs at 87 and keeps 70%
      often fields more accumulated talent than one that signs at 89 and
      keeps 45%.</p>
      <p><strong>Caveat:</strong> name matching misses players whose listed
      names differ between the commit page and the roster (suffixes,
      nicknames), so treat single-point differences between teams as
      noise.</p>"),
  info_eras = list(
    title = "Coach Eras — sources & methods",
    body = "
      <p><strong>Era assignment:</strong> a class belongs to the staff that
      ran its main signing window — the December early period for 2021+
      classes (when most players sign), February NSD before that. So
      Arizona's 2024 class is Fisch's (signed Dec 2023, retained by Brennan).
      Edit <code>R/coach_eras.R</code> to adjust any call.</p>
      <p><strong>Blue chips (90+):</strong> counted from the rating shown on
      247's team pages. 247 also publishes a lower <em>Composite</em> rating —
      counting by Composite or stars can differ by 1–2 players per class.</p>
      <p><strong>Verify anything:</strong> Tap or hover a class dot for its top-5
      signees; click it to open that class on 247Sports.</p>"),
  info_brief = list(
    title = "Defensive War Room — sources & methods",
    body = function(conf_lab, conf_n) paste0("
      <p><strong>The scheme:</strong> Arizona DC Danny Gonzales (promoted
      Jan 2025; 2025 was a top-25 national turnaround with the #1 pass-
      efficiency defense and #1 turnover margin) runs the 3-3-5 odd stack he
      learned in the Rocky Long tree. Its personnel doctrine: one stout Nose
      who commands double teams; long, rangy Ends who don't need 5-tech size;
      Stack LBs who blitz like DTs, edge like 3-4 OLBs, and cover like DBs;
      a hybrid S/LB in the middle of the field; and corner-skilled DBs.
      Long's thesis: 'there are a lot more smaller fast guys in this world
      than there are big strong guys who run fast.'</p>
      <p><strong>The Fit Board:</strong> every defensive body on the current
      roster is mapped to an odd-stack role by listed position + weight
      (Nose 285+, Rangy End 245–284, Edge Tweener &lt;245, Stack LB, Hybrid
      S/LB 205+, Safety, Corner), with two-deep headcount targets per role.
      Weight cutoffs are editable in <code>R/girth_plots.R</code>
      (<code>role_335</code>).</p>
      <p><strong>Roster construction / retention:</strong> current 247 roster
      by class standing; in-state HS commits by listed school state and ",
      conf_lab, " signing school (Power-4 destinations are included; moves
      outside the tracked 67-program
      universe remain outside this view).</p>
      <p><strong>The brief:</strong> auto-written, defense first — nothing is
      hand-curated.</p>")),
  info_results = list(
    title = "Talent vs Results — sources & methods",
    body = "
      <p><strong>Records & SP+:</strong> CollegeFootballData.com (free API),
      seasons 2016–2025. SP+ is an opponent-adjusted efficiency rating —
      higher is better; roughly, +10 is a top-25 team and 0 is average.
      Refresh after each season with <code>scripts/fetchOutcomes.R</code>.</p>
      <p><strong>Talent composite:</strong> for season Y, the mean of the
      TOP 20 ratings among all additions — HS commits AND portal transfers —
      in classes Y-3 through Y. Top-20 (rather than a plain average) rewards
      accumulating real talent instead of signing tiny classes. Early seasons
      (2016–2018) use partial windows since the data starts at the 2016
      class; transfer ratings exist from 2021 on.</p>
      <p><strong>Reading it:</strong> the quadrant compares each program's
      decade of talent to its decade of wins — up-and-left of the medians
      means outplaying recruiting. The scoreboard shows whether a season's
      wins tracked the talent on hand; gaps between the bars and the dashed
      line are coaching, development, health, and luck.</p>"),
  info_wat = list(
    title = "Wins Above Talent - sources & methods",
    body = function(conf_lab, conf_n) paste0("
      <p><strong>The idea:</strong> some staffs win more than their
      recruiting says they should, and some win less. Wins Above Talent
      (WAT) puts a number on that gap: wins per season above (or below)
      what a program's talent predicts.</p>
      <p><strong>How the expectation is built:</strong> across every ", conf_lab, "
      program-season in the window, we fit the league's own talent-to-wins
      curve -- a <em>quasibinomial</em> regression of season win rate on the
      rolling 4-class talent composite (the same top-20 HS + portal rating
      used in the quadrant). The curve says: given this much talent, a
      typical program wins about this share of its games. Quasibinomial just
      lets seasons scatter more (or less) than a coin-flip model would
      without bending the curve.</p>
      <p><strong>Reading the ladder:</strong> the grey dot is a program's
      <em>expected</em> win % from that curve; the colored dot is its
      <em>actual</em> win %. The row label is the gap converted to wins per
      season: <code>+2.1 W/yr</code> means about two extra wins a year
      beyond what the talent predicted. Overachievers sit on top.</p>
      <p><strong>Honest caveats:</strong> the ladder covers only the
      completed seasons inside your selected year window -- and the default
      window is just the last few class years, so most programs rest on two
      or three seasons (fewer for the 2024 realignment arrivals). With a
      window that short, one lucky or injury-wrecked season moves a program
      a lot; widen the window (the <em>All years</em> preset spans about a
      decade) for a steadier read. The fit is built to net to about zero
      across the league, so it measures each program <em>relative to the
      conference</em>, not against football at large. And talent isn't
      destiny -- the gap is coaching, development, health, scheme, and luck,
      bundled together. The season-by-season Scoreboard below shows the same
      story one year at a time.</p>")),
  info_origins = list(
    title = "Talent Origins - sources & methods",
    body = "
      <p><strong>What this measures:</strong> the last listed high-school or
      prep-school location for unique athletes captured at the 67 Power-4
      destinations. It is not birthplace, hometown, or a census of every
      national prospect.</p>
      <p><strong>HS/prep guardrail:</strong> portal transfers are excluded.
      High-confidence junior-college text is excluded, and any new school
      name containing <code>College</code> enters a review queue unless it is
      on the reviewed HS/prep allowlist. International and non-state origins
      remain in coverage counts but not the 50-state + DC board.</p>
      <p><strong>Quality:</strong> the 247 team-page rating has near-complete
      coverage; 90+ defines a blue chip. Legitimate historical source grades
      through 110 are retained, while values outside the supported 0–110
      range are withheld. Quality rankings use the rated sample and require a
      visible minimum. Open-class contribution is stated on aggregate views
      rather than treated as settled.</p>
      <p><strong>Counting:</strong> a profile listed under two destinations
      after a decommit counts once per class in this page. Program Reach keeps
      the underlying commitment-level view. Conflicting decommit position
      groups remain in state totals but are excluded from position views.</p>"),
  info_map = list(
    title = "Program Reach — sources & methods",
    body = "
      <p><strong>Locations:</strong> the mapped location listed on each
      247Sports recruiting profile. For non-portal recruits this is usually
      the last listed high school or prep program, not birthplace. Shaded
      shapes are smoothed convex hulls of the selected program's footprint.</p>
      <p><strong>Gaps:</strong> transfers join only when a listed origin is
      present. Earlier transfer classes are often unmapped; new records reach
      the map after the nightly geocoding pass.</p>"),
  info_distance = list(
    title = "Program Reach distance — sources & methods",
    body = "
      <p><strong>Distance:</strong> geodesic (great-circle) miles from the
      recruit's listed origin to campus, from geocoded 247 locations.</p>
      <p><strong>Outliers:</strong> the toggle hides points beyond 1.5×IQR
      from the middle 50% (standard boxplot rule) and recomputes the bands.</p>
      <p><strong>Verify anything:</strong> hover a dot for the recruit card;
      click to open their 247 page.</p>")
)

## era metric choices (Coach Eras)
era_metric_choices <- c("Average 247 Rating" = "AvgRating",
                        "Blue-Chip Share (% rated 90+)" = "BlueChips",
                        "Average Weight" = "AvgWeight",
                        "Average Height" = "AvgHeight",
                        "Average Miles from Listed Origin" = "AvgMiles",
                        "% In-State Commits" = "PctInState")

## UI ========================================================================
ui <- dashboardPage(

  dashboardHeader(title = "Power-4 Girth Index"),
  skin = "blue",

  ## ---- sidebar: navigation only (global controls live in the top bar) -----
  dashboardSidebar(
    width = 230,
    collapsed = FALSE,

    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("house")),
                tags$li(class = "header", "EXPLORE"),
                menuItem("Size Lab", tabName = "sizelab", icon = icon("ruler-combined")),
                menuItem("Conference Beef", tabName = "beef", icon = icon("dumbbell")),
                menuItem("Conference Lab", tabName = "conflab", icon = icon("layer-group")),
                tags$li(class = "header", "PROGRAM"),
                menuItem("Matchup", tabName = "compare", icon = icon("scale-balanced")),
                menuItem("Weight Room", tabName = "weightroom", icon = icon("weight-hanging")),
                menuItem("Coach Eras", tabName = "eras", icon = icon("user-tie")),
                menuItem("War Room (3-3-5)", tabName = "brief", icon = icon("shield-halved")),
                menuItem("Talent vs Results", tabName = "results", icon = icon("trophy")),
                tags$li(class = "header", "GEOGRAPHY"),
                menuItem("Talent Origins", tabName = "origins", icon = icon("location-dot")),
                menuItem("Program Reach", tabName = "summary", icon = icon("route")),
                tags$li(class = "header", "REFERENCE"),
                menuItem("Data & Notes", tabName = "notes", icon = icon("circle-info"))
    )
  ),

  ## ---- body ----------------------------------------------------------------
  dashboardBody(
    useShinyjs(),

    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
                crossorigin = "anonymous"),
      tags$link(rel = "preconnect", href = "https://cdn.jsdelivr.net"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Rubik:wght@400;600;800&display=swap"),
      ## slug -> logo map for the selectize team-picker render (logos in the
      ## dropdown + on the selected item)
      tags$script(HTML(gi_logo_map_js)),
      ## per-conference color cue on the picker optgroup headers AND the Home
      ## grid section headers, driven from CONF_CONFIG so it matches the
      ## Conference Lab's color language (SEC orange, Big Ten green, ...)
      tags$style(HTML(paste(vapply(seq_len(nrow(CONF_CONFIG)), function(i)
        sprintf(paste0(".selectize-dropdown .optgroup[data-group=\"%s\"] ",
                       ".optgroup-header, .gi-pick-conf[data-conf=\"%s\"] ",
                       "{ border-left-color: %s; }"),
                CONF_CONFIG$conf[i], CONF_CONFIG$conf[i], CONF_CONFIG$color[i]),
        character(1)), collapse = "\n"))),
      tags$style(HTML("
      body, .content-wrapper { font-family: 'Rubik', 'Helvetica Neue', sans-serif; }
      .main-header .logo { font-family: 'Rubik', sans-serif; font-weight: 800;
        letter-spacing: 0.5px; }
      /* ---- team picker: logos + conference optgroups (67-team scan aid) ---- */
      .gi-opt, .gi-item { display: flex; align-items: center; gap: 8px;
        line-height: 1.2; }
      .gi-opt-logo { width: 20px; height: 20px; object-fit: contain;
        background: #fff; border-radius: 3px; flex: 0 0 auto;
        box-shadow: 0 0 0 1px rgba(0,0,0,0.06); }
      .gi-opt-blank { box-shadow: none; background: transparent; }
      /* conference section headers in the dropdown */
      .selectize-dropdown .optgroup-header {
        font-weight: 700; color: #0C234B; font-size: 10.5px;
        text-transform: uppercase; letter-spacing: 0.05em;
        background: #eef2f8; padding: 5px 8px; position: sticky; top: 0; }
      .selectize-dropdown .optgroup:not(:first-child) .optgroup-header {
        border-top: 1px solid #dbe3ee; }
      .selectize-dropdown .option { padding: 5px 8px; }
      .selectize-dropdown .option.active { background: #e8eef7; }
      /* keep the selected item's logo from inflating the control height */
      .selectize-input .gi-item .gi-opt-logo { width: 18px; height: 18px; }
      /* Home 'Pick your team' conference section headers */
      .gi-pick-conf { font-weight: 700; color: #0C234B; text-transform: uppercase;
        font-size: 12px; letter-spacing: .05em; margin: 14px 0 6px;
        padding: 3px 0 3px 10px; border-left: 4px solid #0C234B;
        text-align: left; }
      /* amber context-metric caveat banner (Conference Lab, YELLOW tier) */
      .gi-caveat { background: #FFF8E6; border-left: 4px solid #e6b800;
        border-radius: 4px; padding: 9px 12px; margin: 12px 2px 2px;
        font-size: 13px; color: #6b5800; display: flex; gap: 9px;
        align-items: flex-start; line-height: 1.45; }
      .gi-caveat .fa-triangle-exclamation { color: #c9a300; margin-top: 2px;
        flex: 0 0 auto; }
      .gi-caveat strong { color: #5a4a00; }

      /* boxes: rounded, soft shadow, sporty headers */
      .box { border-radius: 10px; box-shadow: 0 2px 8px rgba(12,35,75,0.08);
        border-top-width: 3px; }
      /* leave room for the collapse/tools buttons -- a full-width centered
         title overlaps them and steals their clicks */
      .box-header .box-title { display: block; width: calc(100% - 84px);
        margin: 0 42px; text-align: center;
        font-family: 'Rubik', sans-serif; font-weight: 600; }
      .box-header .box-tools { z-index: 5; }
      /* rebrand AdminLTE's stock blue -- the most visible chrome in the
         app should carry the navy, not the framework default */
      .box.box-primary { border-top-color: #0C234B; }
      .box.box-solid.box-primary > .box-header { background: #0C234B; }
      .box.box-solid.box-primary { border-color: #0C234B; }

      /* scoreboard-style value boxes */
      .small-box { border-radius: 10px; }
      .small-box h3 { font-size: 26px; font-family: 'Rubik', sans-serif;
        font-weight: 800; }
      .talking-points li { margin-bottom: 9px; font-size: 15px; }
      /* the pool-size chip on each ranked Home insight (n-gated at 8) */
      .talking-points .insight-n { color: #8A949C; font-size: 12px;
        font-weight: 500; white-space: nowrap; }

      /* sidebar = navigation only */
      .sidebar-menu > li.active > a {
        border-left: 4px solid #AB0520; font-weight: 600; }

      /* THE CONTROL BAR — the global settings, impossible to miss */
      /* AdminLTE sets overflow:hidden on .wrapper, which kills position:
         sticky in all descendants — relax it so the bar can stick */
      .wrapper { overflow: visible !important; }
      .control-bar {
        position: sticky; top: 50px; z-index: 900;
        background: #ffffff;
        border-left: 6px solid #AB0520;
        border-radius: 10px;
        padding: 0 16px;
        margin-bottom: 16px;
        box-shadow: 0 4px 14px rgba(12, 35, 75, 0.16); }

      /* the always-visible summary strip; tap to expand/collapse */
      .cb-head { display: flex; align-items: center; gap: 10px;
        padding: 8px 0; cursor: pointer; user-select: none; }
      .cb-head img { height: 24px; vertical-align: middle; }
      .cb-summary-text { font-size: 13px; color: #0C234B; font-weight: 600;
        display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
      .cb-summary-text .cb-dim { color: #64748b; font-weight: 500; }
      .cb-summary-text .cb-updated { font-variant: small-caps;
        letter-spacing: 0.4px; }
      .cb-chevron { margin-left: auto; color: #AB0520;
        transition: transform 0.2s; }
      .control-bar.collapsed .cb-chevron { transform: rotate(180deg); }
      .cb-body { padding-top: 6px; }
      .control-bar.collapsed .cb-body { display: none; }
      .control-bar .control-label, .control-bar > div label {
        font-size: 11px; text-transform: uppercase; letter-spacing: 0.7px;
        color: #AB0520; font-weight: 800; margin-bottom: 3px; }
      .control-bar .radio label {
        text-transform: none; letter-spacing: 0; color: #2c3e50;
        font-weight: 500; font-size: 13px; }
      .control-bar .form-group { margin-bottom: 8px; }
      .control-bar .radio { margin-top: 2px; margin-bottom: 2px; }
      .control-bar .irs--shiny .irs-bar { background: #AB0520;
        border-color: #AB0520; }
      .control-bar .irs--shiny .irs-from, .control-bar .irs--shiny .irs-to {
        background-color: #0C234B; }
      .year-presets { display: flex; gap: 6px; margin: -6px 0 8px 0; }
      .year-presets .btn { flex: 1; background: #f2f5f9; color: #0C234B;
        border: 1px solid #d8e0ea; font-size: 10px; font-weight: 600;
        padding: 4px 5px; /* finger-sized (>=24px tall) preset targets */ }
      .year-presets .btn:hover { background: #AB0520; color: white; }

      /* home hero */
      .hero {
        background: linear-gradient(135deg, #0C234B 0%, #17456e 60%, #AB0520 140%);
        color: white; padding: 26px 30px; border-radius: 12px;
        margin-bottom: 18px; box-shadow: 0 4px 14px rgba(12,35,75,0.25); }
      .hero h1 { margin: 0 0 6px 0; font-weight: 800; font-size: 32px;
        font-family: 'Rubik', sans-serif; }
      .hero p { margin: 0; font-size: 16px; color: #dbe6f1; }
      .hero .btn { margin-top: 14px; margin-right: 8px; font-weight: 600;
        border-radius: 6px; }

      /* pinned hover cards (tap any chart dot to pin; drag to reposition) */
      .pinned-card {
        position: absolute; z-index: 1500; max-width: 320px;
        background: #0C234B; color: white; padding: 12px 14px;
        border-radius: 8px; font-size: 13px; line-height: 1.45;
        box-shadow: 0 6px 18px rgba(0,0,0,0.35);
        border-left: 4px solid #FFD200; cursor: grab; touch-action: none; }
      .pinned-card:active { cursor: grabbing; }
      /* re-tapping an already-pinned dot pulses its card */
      @keyframes pinPulse {
        0%, 100% { box-shadow: 0 6px 18px rgba(0,0,0,0.35); }
        50% { box-shadow: 0 0 0 4px rgba(255,210,0,0.65),
                          0 6px 18px rgba(0,0,0,0.35); } }
      .pinned-card.pulse { animation: pinPulse 0.45s ease 2; }
      .pinned-card a { color: #FFD200; font-weight: 600; }
      .pinned-card .pin-close {
        float: right; background: transparent; border: none; color: #9fb0c1;
        font-size: 18px; line-height: 1; cursor: pointer;
        /* padded to a finger-sized target without growing visually */
        padding: 8px 10px; margin: -8px -10px 0 8px; }
      .pinned-card .pin-close:hover { color: white; }
      /* gold leader lines tying each card to its data point -- one svg
         overlay per chart box, so downloads + fullscreen include them */
      svg.pin-lines { position: absolute; top: 0; left: 0;
        width: 100%; height: 100%; z-index: 1400;
        pointer-events: none; overflow: visible; }
      /* pins are children of the chart box; the box anchors them.
         box-body anchors the interactive badge (footer-safe). */
      .box { position: relative; }
      .box .box-body { position: relative; }
      /* a fullscreened box becomes a workbench: pin, drag, resize, download */
      .box:fullscreen { overflow: auto; background: white; padding: 26px; }
      /* iPhone Safari has no Fullscreen API -- the same workbench as a
         fixed overlay (kept under the player card's z-index 3000) */
      .box.gi-fauxfs { position: fixed !important; top: 0; left: 0;
        right: 0; bottom: 0; z-index: 2500; margin: 0; overflow: auto;
        background: white; padding: 26px; border-radius: 0; }
      body.gi-fauxfs-open { overflow: hidden; }

      /* home value boxes navigate -- make them feel like buttons */
      .vb-link { cursor: pointer; }
      .vb-link .small-box { transition: transform 0.15s ease,
        box-shadow 0.15s ease; }
      .vb-link:hover .small-box { transform: translateY(-3px);
        box-shadow: 0 10px 22px rgba(12,35,75,0.22); }

      /* the 'make your own graphic' how-to strip on Home */
      .howto-strip { display: flex; flex-wrap: wrap; gap: 8px;
        align-items: center; background: #fff;
        border: 1.5px dashed #FFD200; border-radius: 10px;
        padding: 9px 14px; margin-bottom: 16px; }
      .howto-title { font-weight: 800; color: #0C234B; font-size: 13px;
        text-transform: uppercase; letter-spacing: 0.6px; }
      .howto-step { background: #f2f5f9; border-radius: 14px;
        padding: 3px 11px; font-size: 12.5px; color: #41546a; }
      .howto-step b { color: #AB0520; margin-right: 3px; }

      /* understated 'interactive' tag, bottom-left of each chart box
         (JS lifts it above the footer when one exists) */
      .tap-badge { position: absolute; left: 10px; bottom: 8px; z-index: 900;
        color: #5d6f80; border-left: 2px solid #c9d4df;
        font-size: 10px; font-weight: 600; letter-spacing: 0.8px;
        text-transform: uppercase; padding: 1px 0 1px 7px;
        pointer-events: none; }

      /* context notes: methodology caveats users can hide for clean
         exports via the control-bar checkbox */
      .ctx-note { display: flex; gap: 8px; align-items: flex-start;
        background: #FFF8E6; border-left: 3px solid #e6b800;
        border-radius: 6px; padding: 7px 11px; margin-top: 10px;
        font-size: 12.5px; color: #6b5d33; line-height: 1.45; }
      .ctx-note .fa-circle-info { margin-top: 2px; color: #c9a300; }

      /* toast for slow renders (image capture etc.) */
      .gi-toast { position: fixed; left: 50%; bottom: 22px;
        transform: translateX(-50%) translateY(20px); z-index: 4000;
        background: #0C234B; color: white; border: 1.5px solid #FFD200;
        border-radius: 10px; padding: 10px 18px; font-size: 13.5px;
        font-weight: 600; opacity: 0; pointer-events: none;
        transition: opacity 0.25s ease, transform 0.25s ease;
        box-shadow: 0 8px 26px rgba(0,0,0,0.35); }
      .gi-toast.show { opacity: 1; transform: translateX(-50%); }

      /* camera button in the control-bar strip */
      .snap-btn { background: #f2f5f9; border: 1px solid #d8e0ea;
        border-radius: 6px; color: #0C234B; padding: 3px 9px;
        font-size: 13px; cursor: pointer; margin-left: 8px; }
      .snap-btn:hover { background: #FFD200; }
      .copy-btn { background: transparent; border: 1px solid rgba(255,255,255,0.5);
        border-radius: 5px; color: inherit; font-size: 11px; padding: 1px 8px;
        margin-left: 10px; cursor: pointer; vertical-align: middle; }
      .copy-btn:hover { background: rgba(255,255,255,0.15); }

      /* slim site footer with the studio contact */
      .ddl-footer { text-align: center; color: #64748b; font-size: 12.5px;
        padding: 18px 10px 10px 10px; }
      .ddl-footer a { color: #AB0520; font-weight: 600; }

      /* ---- THE PLAYER CARD: spins in like a foil pull ---- */
      .pc-backdrop { position: fixed; inset: 0; z-index: 3000;
        background: rgba(8, 18, 38, 0.72);
        display: flex; align-items: center; justify-content: center;
        perspective: 1200px; animation: pcFade 0.3s ease; }
      @keyframes pcFade { from { opacity: 0; } to { opacity: 1; } }
      .pc-card { position: relative; width: 320px; max-width: 88vw;
        border-radius: 18px; padding: 18px 20px 16px 20px;
        color: white; text-align: center; overflow: hidden;
        transform-style: preserve-3d; will-change: transform;
        background:
          radial-gradient(120% 90% at 80% 0%, rgba(255,255,255,0.16), transparent 50%),
          linear-gradient(158deg, var(--c2) 0%, #142d52 55%, var(--c1) 130%);
        border: 3px solid rgba(255, 210, 0, 0.85);
        box-shadow: 0 24px 70px rgba(0,0,0,0.55),
                    0 0 40px rgba(255, 210, 0, 0.18);
        animation: pcSpin 1.05s cubic-bezier(0.18, 0.8, 0.25, 1);
        transition: transform 0.15s ease; }
      @keyframes pcSpin {
        0%   { transform: rotateY(900deg) scale(0.2); opacity: 0; }
        45%  { opacity: 1; }
        100% { transform: rotateY(0deg) scale(1); } }
      /* the holographic sheen sweeping across the foil */
      .pc-holo { position: absolute; inset: 0; pointer-events: none;
        background: linear-gradient(112deg,
          transparent 32%,
          rgba(255, 255, 255, 0.28) 44%,
          rgba(140, 255, 220, 0.22) 50%,
          rgba(255, 180, 240, 0.22) 56%,
          transparent 68%);
        background-size: 280% 280%;
        mix-blend-mode: screen;
        animation: pcSheen 3.2s ease-in-out infinite; }
      @keyframes pcSheen {
        0%   { background-position: 120% 120%; }
        55%  { background-position: -20% -20%; }
        100% { background-position: 120% 120%; } }
      .pc-close { position: absolute; top: 8px; right: 12px; z-index: 2;
        background: transparent; border: none; color: rgba(255,255,255,0.75);
        font-size: 24px; cursor: pointer; }
      .pc-close:hover { color: #FFD200; }
      .pc-head { display: flex; align-items: center; justify-content: center;
        gap: 8px; font-weight: 700; letter-spacing: 1.5px;
        text-transform: uppercase; font-size: 12px; opacity: 0.92; }
      .pc-head img { height: 30px; filter: drop-shadow(0 2px 4px rgba(0,0,0,0.4)); }
      .pc-avatar { width: 92px; height: 92px; margin: 14px auto 8px auto;
        border-radius: 50%; display: flex; align-items: center;
        justify-content: center; font-size: 34px; font-weight: 800;
        font-family: 'Rubik', sans-serif;
        background: radial-gradient(circle at 30% 25%,
          rgba(255,255,255,0.32), rgba(255,255,255,0.08));
        border: 2.5px solid rgba(255, 210, 0, 0.9);
        text-shadow: 0 2px 8px rgba(0,0,0,0.45); }
      .pc-name { font-size: 22px; font-weight: 800;
        font-family: 'Rubik', sans-serif; line-height: 1.15;
        text-shadow: 0 2px 10px rgba(0,0,0,0.4); }
      .pc-sub { font-size: 12.5px; opacity: 0.85; margin: 3px 0 12px 0; }
      .pc-stats { display: flex; justify-content: center; gap: 8px;
        margin-bottom: 12px; }
      .pc-stats > div { flex: 1; background: rgba(255,255,255,0.10);
        border-radius: 10px; padding: 8px 4px;
        border: 1px solid rgba(255,255,255,0.14); }
      .pc-stats b { display: block; font-size: 18px;
        font-family: 'Rubik', sans-serif; }
      .pc-stats span { font-size: 9.5px; text-transform: uppercase;
        letter-spacing: 1px; opacity: 0.7; }
      .pc-from { font-size: 12.5px; opacity: 0.85; }
      .pc-coach { font-size: 12px; opacity: 0.75; margin-top: 3px; }
      .pc-247 { display: inline-block; margin-top: 10px; color: #FFD200;
        font-weight: 700; font-size: 13px; }
      .pc-src { margin-top: 10px; font-size: 10px; opacity: 0.55;
        border-top: 1px solid rgba(255,255,255,0.18); padding-top: 7px; }
      /* names inside pinned cards that open the player card -- styled as
         tappable chips so nobody has to guess */
      .pinned-card .pc-open { color: #7FD8FF; cursor: pointer;
        font-weight: 600; background: rgba(127, 216, 255, 0.14);
        padding: 0 5px; border-radius: 5px;
        border-bottom: 1px dotted #7FD8FF; }
      .pinned-card .pc-open:hover { color: #0C234B; background: #FFD200;
        border-bottom-color: #FFD200; }
      .pinned-card .pin-hint { margin-top: 8px; padding-top: 6px;
        border-top: 1px solid rgba(255,255,255,0.2); color: #FFD200;
        font-size: 11px; font-style: italic; }
      /* cards scale from their top-left so they stay where you put them */
      .pinned-card { transform-origin: top left; }
      .pinned-card .pin-resize { position: absolute; right: 2px; bottom: 2px;
        width: 26px; height: 26px; cursor: nwse-resize; touch-action: none;
        opacity: 0.6;
        background:
          linear-gradient(135deg, transparent 52%, #FFD200 54%, #FFD200 60%,
            transparent 62%, transparent 70%, #FFD200 72%, #FFD200 78%,
            transparent 80%);
        border-bottom-right-radius: 8px; }
      .pinned-card .pin-resize:hover { opacity: 1; }

      /* ---- showcase polish: depth, motion, calm ---- */
      .content-wrapper { background:
        linear-gradient(180deg, #eef2f8 0%, #e7edf5 100%); }
      .box { transition: box-shadow 0.2s ease, transform 0.2s ease; }
      .box:hover { box-shadow: 0 6px 20px rgba(12,35,75,0.13); }
      .tab-pane.active { animation: tabIn 0.28s ease; }
      @keyframes tabIn { from { opacity: 0; transform: translateY(6px); }
        to { opacity: 1; transform: none; } }
      .main-header .navbar, .main-header .logo {
        background: linear-gradient(90deg, #0C234B 0%, #16386e 100%) !important; }
      .skin-blue .main-sidebar { background-color: #0e1d36; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background: #15294d; border-left-color: #FFD200; }

      /* the raised-hand custom-build CTA (War Room) */
      .custom-cta { display: inline-flex; align-items: center; gap: 8px;
        background: #FFF8E1; border: 1.5px solid #FFD200; color: #0C234B;
        font-weight: 700; font-size: 13px; padding: 6px 12px;
        border-radius: 8px; margin-top: 8px; }
      .custom-cta:hover { background: #FFD200; color: #0C234B;
        text-decoration: none; }
      .custom-cta .fa-hand { font-size: 16px; color: #AB0520; }

      /* class snapshot card */
      .snap-stat { text-align: center; padding: 6px 2px; }
      .snap-stat .num { font-size: 26px; font-weight: 800;
        font-family: 'Rubik', sans-serif; color: #0C234B; }
      .snap-stat .lbl { font-size: 11px; color: #6b7a89;
        text-transform: uppercase; letter-spacing: 0.5px; }
      .snap-delta-up { color: #1a7f37; font-weight: 700; }
      .snap-delta-down { color: #AB0520; font-weight: 700; }

      /* respect reduced-motion: the foil spin, sheen, pulses, and lifts
         all become instant states (WCAG 2.3.3) */
      @media (prefers-reduced-motion: reduce) {
        .pc-card, .pc-holo, .pc-backdrop, .pinned-card.pulse,
        .tab-pane.active { animation: none !important; }
        .box, .vb-link .small-box, .cb-chevron { transition: none !important; }
      }

      /* ---- table twins: the numbers view behind each board ---- */
      .twin-toggle { display: inline-flex; align-items: center; min-height: 28px;
        margin-left: 10px;
        padding: 1px 9px; color: inherit; opacity: 0.85; font-size: 10.5px;
        font-weight: 600; letter-spacing: 0.8px; text-transform: uppercase;
        vertical-align: middle; border: 1px solid rgba(255,255,255,0.45);
        border-radius: 10px; cursor: pointer; text-decoration: none; }
      .twin-toggle:hover, .twin-toggle:focus { background: #FFD200;
        border-color: #FFD200; color: #0C234B; opacity: 1;
        text-decoration: none; }
      @media (max-width: 767px) {
        .twin-toggle { min-height: 44px; padding: 8px 12px;
          font-size: 11px; }
      }
      .gi-tablewrap { display: none; }
      .box.gi-table-mode .gi-tablewrap { display: block; }
      /* the chart collapses but KEEPS its width -- display:none here would
         blank a girafe that re-renders while hidden (the 0-width trap) */
      .box.gi-table-mode .gi-chartwrap { visibility: hidden; height: 0;
        overflow: hidden; }
      /* table mode shelves the pin workbench; toggling back restores it */
      .box.gi-table-mode .pinned-card, .box.gi-table-mode svg.pin-lines,
      .box.gi-table-mode .tap-badge { display: none !important; }
      .twin-scroll { overflow-x: auto; }
      .twin-table { width: 100%; border-collapse: collapse; font-size: 13px; }
      .twin-table caption { caption-side: top; text-align: left;
        font-weight: 700; color: #0C234B; font-family: 'Rubik', sans-serif;
        padding: 2px 4px 8px 4px; }
      /* the chart's scope (source + window + pool), dimmed after the title */
      .twin-table caption .twin-cap-note { font-weight: 400;
        font-size: 11.5px; color: #8a94a3; }
      .twin-table th { font-size: 10.5px; text-transform: uppercase;
        letter-spacing: 0.6px; color: #64748b; text-align: left;
        padding: 4px 8px; border-bottom: 2px solid #d8e0ea; }
      .twin-table td { padding: 5px 8px; border-bottom: 1px solid #eef2f6;
        vertical-align: middle; }
      .twin-table tr:focus { outline: 2px solid #FFD200;
        outline-offset: -2px; }
      .twin-rank { color: #64748b; font-weight: 600; width: 34px; }
      .twin-team { white-space: nowrap; }
      .twin-team img { height: 20px; width: auto; margin-right: 7px;
        vertical-align: middle; }
      .twin-bar-cell { width: 28%; min-width: 110px; }
      .twin-bar { height: 12px; border-radius: 3px; min-width: 2px; }
      .twin-val { font-weight: 700; color: #0C234B; white-space: nowrap; }
      .twin-n { display: inline-block; background: #f2f5f9;
        border: 1px solid #d8e0ea; border-radius: 10px; padding: 0 7px;
        font-size: 11px; color: #64748b; white-space: nowrap; }
      tr.twin-main { background: #FFF8E6; }
      tr.twin-main td:first-child { border-left: 3px solid #AB0520; }
      tr.twin-main td { font-weight: 600; }
      tr.twin-compare { background: #f2f5f9; }
      tr.twin-compare td:first-child { border-left: 3px solid #0C234B; }
      .twin-empty { color: #64748b; font-size: 13px; padding: 14px 6px; }

      /* ---- branded disconnect overlay ---- */
      #shiny-disconnected-overlay { background: #0C234B !important;
        opacity: 0.55 !important; }
      .gi-reconnect { position: fixed; top: 0; right: 0; bottom: 0; left: 0;
        z-index: 99999; display: flex; align-items: center;
        justify-content: center; padding: 20px; }
      .gi-reconnect-card { background: #ffffff; border-radius: 12px;
        border-top: 4px solid #AB0520; padding: 24px 26px; max-width: 340px;
        width: 100%; text-align: center;
        box-shadow: 0 18px 50px rgba(0,0,0,0.45); }
      .gi-reconnect-card h3 { margin: 0 0 8px 0;
        font-family: 'Rubik', sans-serif; font-weight: 800; color: #0C234B;
        font-size: 20px; }
      .gi-reconnect-card p { margin: 0 0 16px 0; color: #64748b;
        font-size: 13.5px; }
      .gi-reconnect-btn { background: #0C234B; color: #ffffff; border: none;
        border-radius: 8px; padding: 9px 26px; font-weight: 700;
        font-size: 14px; cursor: pointer; min-height: 44px; }
      .gi-reconnect-btn:hover, .gi-reconnect-btn:focus { background: #FFD200;
        color: #0C234B; }

      /* ---- 'since your last visit' strip (Home) ---- */
      .lastvisit-strip { display: flex; flex-wrap: wrap; gap: 6px;
        align-items: baseline; background: #ffffff;
        border-left: 4px solid #0C234B; border-radius: 8px;
        padding: 8px 14px; margin-bottom: 16px; font-size: 13px;
        color: #41546a; box-shadow: 0 2px 8px rgba(12,35,75,0.08); }
      .lastvisit-strip .lv-lead { font-variant: small-caps;
        letter-spacing: 0.5px; font-weight: 700; color: #AB0520; }
      .lastvisit-strip b { color: #0C234B; }
      .lastvisit-strip .lv-pool { color: #8A949C; font-size: 12px; }

      /* ---- mobile polish ---- */
      @media (max-width: 767px) {
        .hero { padding: 14px 16px; border-radius: 8px; }
        .hero h1 { font-size: 19px; }
        .hero p { font-size: 13px; }
        .hero .btn { margin-top: 8px; margin-right: 4px; padding: 4px 8px;
          font-size: 12px; }
        .small-box h3 { font-size: 20px; }
        .small-box p { font-size: 12px; }
        .control-bar { padding: 0 10px; top: 50px; }
        .cb-summary-text { font-size: 12px; }
        .box-header .box-title { font-size: 15px; }
        .talking-points li { font-size: 13px; }
        .snap-stat .num { font-size: 19px; }
        .content { padding: 8px; }
        .pinned-card { max-width: 86vw; }
        .twin-table { font-size: 12px; }
        .twin-bar-cell { min-width: 70px; }
        .gi-reconnect-card { max-width: 88vw; }
      }
    ")),
      ## report the window width so charts can render larger text on phones;
      ## also restore the device's saved team (or trigger the first-visit ask)
      tags$script(HTML("
        function reportClientW() {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('client_w', window.innerWidth);
          }
        }
        window.addEventListener('resize', function() {
          clearTimeout(window.__cwT);
          window.__cwT = setTimeout(reportClientW, 350);
        });
        /* shiny:connected is jQuery-triggered -- native listeners miss it */
        $(document).on('shiny:connected', function() {
          reportClientW();
          var saved = 'none';
          try { saved = localStorage.getItem('gi_team') || 'none'; } catch (e) {}
          Shiny.setInputValue('stored_team', saved);
          Shiny.addCustomMessageHandler('saveTeam', function(slug) {
            try { localStorage.setItem('gi_team', slug); } catch (e) {}
          });
          /* WHAT CHANGED: the device's prior-visit snapshot goes up ONCE
             (read BEFORE the server can overwrite it below); the server
             recomputes today's numbers and only compares against these. */
          var snap = 'none';
          try {
            var rawSnap = localStorage.getItem('gi_snapshot');
            if (rawSnap) snap = JSON.parse(rawSnap);
          } catch (e) {}
          Shiny.setInputValue('stored_snapshot', snap);
          Shiny.addCustomMessageHandler('saveSnapshot', function(s) {
            try { localStorage.setItem('gi_snapshot', JSON.stringify(s)); }
            catch (e) {}
          });
          /* PLAYER CARD: server sends the record; we spin up the card */
          Shiny.addCustomMessageHandler('playerCard', function(p) {
            document.querySelectorAll('.pc-backdrop').forEach(function(b) { b.remove(); });
            var initials = p.name.split(/\\s+/).map(function(w) {
              return w[0] || ''; }).slice(0, 2).join('').toUpperCase();
            var bd = document.createElement('div');
            bd.className = 'pc-backdrop';
            bd.innerHTML =
              '<div class=\"pc-card\" style=\"--c1:' + p.c1 + ';--c2:' + p.c2 + '\">' +
              '  <div class=\"pc-holo\"></div>' +
              '  <button class=\"pc-close\">&times;</button>' +
              '  <div class=\"pc-head\"><img src=\"' + p.logo + '\"/>' +
              '    <span>' + p.team + '</span></div>' +
              '  <div class=\"pc-avatar\">' + initials + '</div>' +
              '  <div class=\"pc-name\">' + p.name + '</div>' +
              '  <div class=\"pc-sub\">' + p.pos + ' · ' + p.yr + ' · ' + p.type + '</div>' +
              '  <div class=\"pc-stats\">' +
              '    <div><b>' + p.ht + '</b><span>height</span></div>' +
              '    <div><b>' + p.wt + '</b><span>weight</span></div>' +
              '    <div><b>' + p.rating + '</b><span>247 rating</span></div>' +
              '  </div>' +
              '  <div class=\"pc-from\">' + p.from +
                   (p.miles ? ' · ' + p.miles : '') + '</div>' +
              (p.coach ? '<div class=\"pc-coach\">Recruited under ' + p.coach + '</div>' : '') +
              (p.url ? '<a class=\"pc-247\" target=\"_blank\" href=\"' + p.url +
                   '\">' + (p.urlLabel || 'Full 247Sports profile &rarr;') + '</a>' : '') +
              '  <div class=\"pc-src\">' + p.src + '</div>' +
              '</div>';
            /* append inside a fullscreened box if one is active, so the
               card shows in fullscreen mode too */
            (document.fullscreenElement || document.body).appendChild(bd);
            var card = bd.querySelector('.pc-card');
            /* minimal dialog semantics: name it, focus the close button so
               keyboard + screen-reader users aren't stranded behind it */
            card.setAttribute('role', 'dialog');
            card.setAttribute('aria-modal', 'true');
            card.setAttribute('aria-label', p.name + ' player card');
            var cb = bd.querySelector('.pc-close');
            if (cb) { cb.setAttribute('aria-label', 'Close player card'); cb.focus(); }
            bd.addEventListener('click', function(ev) {
              if (ev.target === bd || ev.target.closest('.pc-close')) bd.remove();
            });
            /* gentle tilt-follow once the spin settles */
            card.addEventListener('pointermove', function(ev) {
              var r = card.getBoundingClientRect();
              var rx = ((ev.clientY - r.top) / r.height - 0.5) * -10;
              var ry = ((ev.clientX - r.left) / r.width - 0.5) * 14;
              card.style.transform =
                'rotateX(' + rx + 'deg) rotateY(' + ry + 'deg)';
            });
            card.addEventListener('pointerleave', function() {
              card.style.transform = '';
            });
          });
        });
        /* taps on player names inside pinned cards ask for the card */
        document.addEventListener('click', function(e) {
          var el = e.target.closest('.pc-open');
          if (!el || !window.Shiny) return;
          Shiny.setInputValue('pc_request', {
            name: el.getAttribute('data-pname'),
            school: el.getAttribute('data-pschool')
          }, {priority: 'event'});
        });
        /* Escape closes the player card like any dialog */
        document.addEventListener('keydown', function(e) {
          if (e.key !== 'Escape') return;
          var bd = document.querySelector('.pc-backdrop');
          if (bd) bd.remove();
        });
      ")),
      ## TABLE TWINS: the header link swaps a board between its chart and
      ## its table with a class flip -- the girafe is never re-rendered,
      ## never display:none'd, and the table renders lazily through
      ## input$twin_<chart>. The branded disconnect card lives here too.
      tags$script(HTML("
        window.giTwinFlip = function(t) {
          var box = t.closest('.box');
          if (!box) return;
          var on = !box.classList.contains('gi-table-mode');
          box.classList.toggle('gi-table-mode', on);
          t.setAttribute('aria-pressed', on ? 'true' : 'false');
          /* only the VISIBLE label swaps -- the accessible name is the
             constant aria-label ('table view') set in twin_toggle(), so
             aria-pressed alone announces the state */
          t.textContent = on ? 'view the chart' : 'view the numbers';
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('twin_' + t.getAttribute('data-chart'), on);
          }
          /* nudge the girafe to re-measure when it comes back into view */
          window.dispatchEvent(new Event('resize'));
        };
        document.addEventListener('click', function(e) {
          var t = e.target.closest('.twin-toggle');
          if (!t) return;
          e.preventDefault();
          window.giTwinFlip(t);
        });
        /* the toggle is a link styled as a control: Space must work like
           Enter does (aria-pressed promises button semantics) */
        document.addEventListener('keydown', function(e) {
          if (e.key !== ' ' && e.key !== 'Spacebar') return;
          var t = e.target && e.target.closest ?
            e.target.closest('.twin-toggle') : null;
          if (!t) return;
          e.preventDefault();
          window.giTwinFlip(t);
        });
        /* a lost connection gets a calm, branded card instead of the stock
           grey curtain (shiny:disconnected is jQuery-triggered -- native
           listeners never see it) */
        $(document).on('shiny:disconnected', function() {
          if (document.querySelector('.gi-reconnect')) return;
          var d = document.createElement('div');
          d.className = 'gi-reconnect';
          d.innerHTML =
            '<div class=\"gi-reconnect-card\" role=\"alertdialog\"' +
            ' aria-label=\"Session paused\">' +
            '<h3>Session paused</h3>' +
            '<p>The connection was lost - the server may have idled to ' +
            'save resources.</p>' +
            '<button type=\"button\" class=\"gi-reconnect-btn\">' +
            'Reconnect</button></div>';
          document.body.appendChild(d);
          var b = d.querySelector('.gi-reconnect-btn');
          b.addEventListener('click', function() { location.reload(); });
          b.focus();
        });
      ")),
      ## on phones the sidebar is a full overlay -- close it after a nav
      ## tap so the selected tab is actually visible
      tags$script(HTML("
        document.addEventListener('click', function(e) {
          var link = e.target.closest('.sidebar-menu a[href^=\"#shiny-tab-\"]');
          if (!link) return;
          if (window.innerWidth < 768) {
            document.body.classList.remove('sidebar-open');
          }
        });
      ")),
      ## the control bar collapses to its summary strip on tap; phones start
      ## collapsed so the bar doesn't eat the screen. The camera button
      ## inside the strip must not trigger the collapse.
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          var bar = document.querySelector('.control-bar');
          if (!bar) return;
          if (window.innerWidth < 768) bar.classList.add('collapsed');
          bar.querySelector('.cb-head').addEventListener('click', function(e) {
            if (e.target.closest('#snap_view')) return;
            bar.classList.toggle('collapsed');
            window.dispatchEvent(new Event('resize'));
          });
        });
      ")),
      ## html-to-image powers all snapshots (handles the resized cards' CSS
      ## transforms correctly -- html2canvas smeared them -- and exports at
      ## 2x resolution for editing quality)
      ## vendored locally -- school/corporate networks that block CDNs used
      ## to lose every capture silently; the CDN is now only the fallback
      tags$script(src = "html-to-image.min.js",
                  onerror = paste0(
                    "var s=document.createElement('script');",
                    "s.src='https://cdn.jsdelivr.net/npm/html-to-image@1.11.13/dist/html-to-image.min.js';",
                    "document.head.appendChild(s);")),
      ## PIN CARDS v3: pins + leader lines live INSIDE the chart's box, so
      ## chart downloads, fullscreen mode, and tab layout all see them.
      ## Tap a chart element to pin; drag to move; grip to resize; pins
      ## clear on tab switch. Toasts announce slow renders.
      tags$script(HTML("
        (function() {
          var svgNS = 'http://www.w3.org/2000/svg';
          function host() { return document.fullscreenElement || document.body; }
          window.giToast = function(msg) {
            var t = document.querySelector('.gi-toast');
            if (!t) {
              t = document.createElement('div');
              t.className = 'gi-toast';
            }
            host().appendChild(t);
            t.textContent = msg;
            t.classList.add('show');
            return {
              done: function(m) {
                t.textContent = m;
                setTimeout(function() { t.classList.remove('show'); }, 2000);
              }
            };
          };
          function linesLayer(box) {
            var s = box.querySelector(':scope > svg.pin-lines');
            if (!s) {
              s = document.createElementNS(svgNS, 'svg');
              s.setAttribute('class', 'pin-lines');
              box.appendChild(s);
            }
            return s;
          }
          function updateLine(pin) {
            if (!pin.__line) return;
            var r = pin.getBoundingClientRect();
            var b = pin.__box.getBoundingClientRect();
            pin.__line.setAttribute('x2', r.left - b.left + r.width / 2);
            pin.__line.setAttribute('y2', r.top - b.top + r.height / 2);
          }
          window.clearPins = function() {
            document.querySelectorAll('.pinned-card').forEach(function(p) { p.remove(); });
            document.querySelectorAll('svg.pin-lines').forEach(function(s) { s.innerHTML = ''; });
          };
          function clearBoxPins(box) {
            document.querySelectorAll('.pinned-card').forEach(function(p) {
              if (p.__box === box) p.remove();
            });
            var s = box.querySelector(':scope > svg.pin-lines');
            if (s) s.innerHTML = '';
          }
          /* a re-rendered chart invalidates its pins -- a surviving card
             would keep asserting facts about marks that are gone (team or
             window changed under it). Collapsing a box likewise. */
          $(document).on('shiny:value', function(ev) {
            if (!ev.target || !ev.target.closest) return;
            /* table twins render inside the chart boxes, but a table render
               doesn't invalidate the chart's pins -- when the DATA changes
               the girafe's own shiny:value still clears them */
            if (ev.target.closest('.gi-tablewrap')) return;
            var box = ev.target.closest('.box');
            if (box && box.querySelector('.pinned-card')) clearBoxPins(box);
          });
          document.addEventListener('click', function(e) {
            var w = e.target.closest('[data-widget=\"collapse\"]');
            if (!w) return;
            var box = w.closest('.box');
            if (box) clearBoxPins(box);
          });
          /* every interactive chart gets a small 'tap to pin' badge so the
             feature is discoverable (excluded from image exports) */
          function badgeCharts() {
            document.querySelectorAll('.girafe').forEach(function(g) {
              var box = g.closest('.box');
              if (!box || box.querySelector('.tap-badge') ||
                  g.closest('.gi-no-pin')) return;
              var b = document.createElement('div');
              b.className = 'tap-badge';
              b.textContent = 'interactive \\u00b7 tap to pin';
              /* anchor inside the box BODY -- the footer lives outside it,
                 so overlap is structurally impossible */
              (box.querySelector('.box-body') || box).appendChild(b);
            });
          }
          /* head scripts run before the document body exists (NEVER write
             a literal body tag here -- the shinyapps proxy injects its
             scripts after the first one it finds, even inside a comment,
             and shatters this script block) -- defer the observer */
          document.addEventListener('DOMContentLoaded', function() {
            badgeCharts();
            new MutationObserver(function() {
              clearTimeout(window.__bT);
              window.__bT = setTimeout(badgeCharts, 400);
            }).observe(document.body, { childList: true, subtree: true });
          });
          document.addEventListener('click', function(e) {
            if (e.target.closest('.sidebar-menu a[href^=\"#shiny-tab-\"]')) {
              window.clearPins();
            }
          });
          document.addEventListener('click', function(e) {
            var el = e.target.closest('svg [data-id]');
            if (!el || !el.closest('.girafe') ||
                el.closest('.gi-no-pin')) return;
            var t = el.getAttribute('title');
            if (!t) return;
            var box = el.closest('.box') || document.body;
            var bR = box.getBoundingClientRect();
            /* one card per element per chart: re-tapping a dot pulses the
               existing card instead of stacking duplicates */
            var pid = ((el.closest('.girafe') || {}).id || '') + '::' +
              (el.getAttribute('data-id') || '');
            var dup = Array.prototype.find.call(
              document.querySelectorAll('.pinned-card'),
              function(p) { return p.__pid === pid; });
            if (dup) {
              dup.classList.remove('pulse');
              void dup.offsetWidth;
              dup.classList.add('pulse');
              return;
            }
            var ta = document.createElement('textarea');
            ta.innerHTML = t;
            var pin = document.createElement('div');
            pin.className = 'pinned-card';
            pin.__box = box;
            pin.__pid = pid;
            pin.innerHTML = \"<button class='pin-close' title='Close'>&times;</button>\" + ta.value;
            /* the 'tap to pin' hint has done its job once pinned -- strip
               it (and its line break) so cards stay compact for exports */
            pin.querySelectorAll('em').forEach(function(em) {
              if (/pin (this card|it)/i.test(em.textContent)) {
                var prev = em.previousSibling;
                if (prev && prev.nodeName === 'BR') prev.remove();
                em.remove();
              }
            });
            if (pin.querySelector('.pc-open')) {
              var hint = document.createElement('div');
              hint.className = 'pin-hint';
              hint.innerHTML = '&#9656; tap a highlighted name to open the player card';
              pin.appendChild(hint);
            }
            var grip = document.createElement('div');
            grip.className = 'pin-resize';
            grip.title = 'Drag to resize · double-tap to reset';
            pin.appendChild(grip);
            grip.addEventListener('pointerdown', function(ev) {
              ev.preventDefault();
              ev.stopPropagation();
              try { grip.setPointerCapture(ev.pointerId); } catch (err) {}
              var startX = ev.pageX;
              var startScale = pin.__scale || 1;
              var baseW = pin.getBoundingClientRect().width / startScale;
              function mv(em) {
                var s = Math.min(2.4, Math.max(0.4,
                  startScale + (em.pageX - startX) / baseW));
                pin.__scale = s;
                pin.style.transform = 'scale(' + s + ')';
                updateLine(pin);
              }
              function up() {
                grip.removeEventListener('pointermove', mv);
                grip.removeEventListener('pointerup', up);
                grip.removeEventListener('pointercancel', up);
                grip.removeEventListener('lostpointercapture', up);
              }
              grip.addEventListener('pointermove', mv);
              grip.addEventListener('pointerup', up);
              /* a cancelled pointer (palm reject, OS gesture) must end the
                 resize too, or hovering keeps resizing forever */
              grip.addEventListener('pointercancel', up);
              grip.addEventListener('lostpointercapture', up);
            });
            grip.addEventListener('dblclick', function() {
              pin.__scale = 1;
              pin.style.transform = '';
              updateLine(pin);
            });
            /* anchor + card position in BOX coordinates */
            var ax = e.clientX - bR.left, ay = e.clientY - bR.top;
            pin.style.left = Math.max(Math.min(ax + 30, bR.width - 340), 4) + 'px';
            /* clamp inside the box -- captures crop to the box node, so a
               card spawned past its edge would be cut out of the export */
            pin.style.top = Math.max(4, Math.min(ay + 18, bR.height - 70)) + 'px';
            box.appendChild(pin);
            var layer = linesLayer(box);
            var ln = document.createElementNS(svgNS, 'line');
            ln.setAttribute('x1', ax); ln.setAttribute('y1', ay);
            ln.setAttribute('stroke', '#FFD200');
            ln.setAttribute('stroke-width', '2.5');
            ln.setAttribute('stroke-linecap', 'round');
            layer.appendChild(ln);
            var dot = document.createElementNS(svgNS, 'circle');
            dot.setAttribute('cx', ax); dot.setAttribute('cy', ay);
            dot.setAttribute('r', '4.5');
            dot.setAttribute('fill', '#FFD200');
            dot.setAttribute('stroke', '#0C234B');
            dot.setAttribute('stroke-width', '1.5');
            layer.appendChild(dot);
            pin.__line = ln;
            updateLine(pin);
            pin.querySelector('.pin-close').addEventListener('click', function() {
              ln.remove(); dot.remove(); pin.remove();
            });
            pin.addEventListener('pointerdown', function(ev) {
              /* chips + links keep their clicks -- a cancelled pointerdown
                 suppresses the click event entirely */
              if (ev.target.closest('a, .pin-close, .pc-open, .pin-resize')) return;
              ev.preventDefault();
              try { pin.setPointerCapture(ev.pointerId); } catch (err) {}
              var sx = ev.clientX - pin.offsetLeft, sy = ev.clientY - pin.offsetTop;
              function mv(em) {
                /* keep the card inside the box: captures crop to the box
                   node, so anything dragged past an edge exports cropped */
                var r = pin.getBoundingClientRect();
                var maxL = Math.max(4, box.clientWidth - r.width - 4);
                var maxT = Math.max(2, box.clientHeight - r.height - 2);
                pin.style.left = Math.max(4, Math.min(em.clientX - sx, maxL)) + 'px';
                pin.style.top = Math.max(2, Math.min(em.clientY - sy, maxT)) + 'px';
                updateLine(pin);
              }
              function up() {
                pin.removeEventListener('pointermove', mv);
                pin.removeEventListener('pointerup', up);
                pin.removeEventListener('pointercancel', up);
                pin.removeEventListener('lostpointercapture', up);
              }
              pin.addEventListener('pointermove', mv);
              pin.addEventListener('pointerup', up);
              pin.addEventListener('pointercancel', up);
              pin.addEventListener('lostpointercapture', up);
            });
          });
          function downloadPng(node, name, bg, toastMsg) {
            var tt = window.giToast(toastMsg || '\\ud83d\\udcf8 Rendering image\\u2026 a few seconds');
            htmlToImage.toPng(node, {
              pixelRatio: 2, backgroundColor: bg || '#ffffff',
              filter: function(n) {
                /* UI chrome never ships in an export: toasts, badges, the
                   pinned cards' close/grip/hint controls, the chart/table
                   toggles, and the personal last-visit strip */
                return !(n.classList && (n.classList.contains('gi-toast') ||
                                         n.classList.contains('tap-badge') ||
                                         n.classList.contains('pin-close') ||
                                         n.classList.contains('pin-resize') ||
                                         n.classList.contains('pin-hint') ||
                                         n.classList.contains('twin-toggle') ||
                                         n.classList.contains('lastvisit-strip')));
              }
            }).then(function(dataUrl) {
              var a = document.createElement('a');
              a.download = name;
              window.__lastChartSnap = name;
              a.href = dataUrl;
              a.click();
              tt.done('\\u2705 Saved ' + name);
            }).catch(function() {
              tt.done('\\u26a0\\ufe0f Image render failed \\u2014 try again');
            });
          }
          /* chart download button: with pins in this chart's box, capture
             the whole box (cards + lines, 2x res); otherwise native export */
          document.addEventListener('click', function(e) {
            var icon = e.target.closest('.ggiraph-toolbar-icon');
            if (!icon) return;
            var t = (icon.getAttribute('title') || '').toLowerCase();
            var box = icon.closest('.box') || icon.closest('.girafe');
            /* fullscreen icon -> fullscreen the BOX so pins keep working.
               iPhone Safari has no Fullscreen API -- emulate with a fixed
               overlay class instead of dying silently */
            if (t.indexOf('full') !== -1) {
              e.preventDefault();
              e.stopImmediatePropagation();
              var faux = document.querySelector('.box.gi-fauxfs');
              if (document.fullscreenElement) { document.exitFullscreen(); }
              else if (faux) {
                faux.classList.remove('gi-fauxfs');
                document.body.classList.remove('gi-fauxfs-open');
              }
              else if (box && box.requestFullscreen) { box.requestFullscreen(); }
              else if (box) {
                box.classList.add('gi-fauxfs');
                document.body.classList.add('gi-fauxfs-open');
              }
              return;
            }
            if (t.indexOf('png') === -1 && t.indexOf('download') === -1) return;
            if (!box || !box.querySelector('.pinned-card')) {
              window.giToast('\\ud83d\\udcf8 Preparing download\\u2026').done(
                '\\u2705 Check your downloads');
              return; /* no pins -> let girafe's native export run */
            }
            if (typeof htmlToImage === 'undefined') {
              /* capture lib unavailable: fall through to the native export,
                 but SAY so -- the native PNG won't include the pins */
              window.giToast('Capture helper unavailable \\u2014 downloading the chart without pinned cards')
                .done('Chart saved (pins not included)');
              return;
            }
            e.preventDefault();
            e.stopImmediatePropagation();
            downloadPng(box,
              window.__snapName().replace('-view.png', '-pinned.png'),
              '#ffffff');
          }, true);
          window.__snapName = function() {
            var slug = function(s) {
              return (s || '').toLowerCase().trim()
                .replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
            };
            var team = document.getElementById('g_team');
            team = team ? team.value : 'big12';
            var sport = document.querySelector(\"input[name='g_sport']:checked\");
            sport = sport ? sport.value : '';
            var page = document.querySelector('.sidebar-menu li.active a');
            page = slug(page ? page.innerText : 'view');
            var yrs = '';
            try {
              var s = $('#g_years').data('ionRangeSlider').result;
              yrs = s.from + '-' + s.to;
            } catch (err) {}
            return [team, sport, page, yrs].filter(Boolean).join('-') +
              '-view.png';
          };
          /* camera button: the whole current page (all charts + pins) */
          document.addEventListener('click', function(e) {
            if (!e.target.closest('#snap_view')) return;
            if (typeof htmlToImage === 'undefined') {
              window.giToast('Capture library failed to load')
                .done('Reload the page and try again');
              return;
            }
            var pane = document.querySelector('.gi-page-stage') || document.body;
            downloadPng(pane, window.__snapName(), '#ecf0f5',
              '\\ud83d\\udcf8 Rendering the full page\\u2026 a few seconds');
          });
          /* one-tap copy for the talking-point / brief boxes */
          document.addEventListener('click', function(e) {
            var btn = e.target.closest('.copy-btn');
            if (!btn) return;
            var src = document.getElementById(btn.getAttribute('data-copy'));
            if (!src) return;
            var txt = Array.from(src.querySelectorAll('li'))
              .map(function(li) { return '• ' + li.innerText.trim(); })
              .join('\\n') || src.innerText.trim();
            /* the attribution line rides on the button (data-footer) and is
               appended to the COPIED text only -- the on-screen brief never
               shows it */
            var footer = btn.getAttribute('data-footer');
            if (footer) txt = txt + '\\n\\n' + footer;
            function flash(label) {
              var old = btn.innerHTML;
              btn.innerHTML = label;
              setTimeout(function() { btn.innerHTML = old; }, 1400);
            }
            if (navigator.clipboard && navigator.clipboard.writeText) {
              navigator.clipboard.writeText(txt)
                .then(function() { flash('✓ copied'); })
                .catch(function() { flash('✗ blocked'); });
            } else {
              /* http / older webviews: textarea + execCommand fallback */
              var f = document.createElement('textarea');
              f.value = txt;
              document.body.appendChild(f);
              f.select();
              try {
                document.execCommand('copy');
                flash('✓ copied');
              } catch (err) { flash('✗ blocked'); }
              f.remove();
            }
          });
        })();
      ")),
      tags$link(rel = "stylesheet", href = "girth-v9.css"),
      tags$script(src = "girth-v9.js")
    ),

    ## ---- THE CONTROL BAR: global settings, visible on every tab ------------
    ## a slim summary strip is always shown; tapping it expands/collapses the
    ## full controls (collapsed by default on phones)
    div(class = "control-bar",
        div(class = "cb-head-row",
            tags$button(type = "button", class = "cb-head",
                        `aria-expanded` = "true",
                        `aria-controls` = "global_controls",
                        `aria-label` = "Show or hide global filters",
                        `aria-describedby` = "cb_summary",
                        uiOutput("cb_summary", inline = TRUE),
                        tags$span(class = "cb-chevron", icon("chevron-up"))),
            div(class = "cb-actions",
                tags$button(id = "copy_view_link", class = "share-btn", type = "button",
                            `aria-label` = "Copy a link to this view",
                            title = "Copy a link with team, filters, and tab",
                            icon("link")),
                tags$button(id = "snap_view", class = "snap-btn", type = "button",
                            `aria-label` = "Save this view as a PNG",
                            title = paste("Save a PNG of this view —",
                                          "pinned cards included"),
                            icon("camera")))),
        div(id = "global_controls", class = "cb-body",
            fluidRow(
              ## no inline logos here -- the summary strip carries them, and
              ## floating images broke the layout at mid widths
              ## both pickers are grouped by conference (optgroups) + logos +
              ## search over all 67 onboarded Power-4 teams (team_choices_grouped).
              ## FUTURE: a global "Conference" SCOPE selector could still slot in
              ## here to narrow g_team's list to one league while g_compare stays
              ## full-P4 (Arizona-vs-Georgia works) — see docs/p4-expansion-design.md.
              tagAppendAttributes(
                conditionalPanel(
                  condition = "input.tabs !== 'origins'",
                  selectizeInput("g_team", "Your team",
                                 choices = team_choices_grouped,
                                 selected = "arizona", width = "100%",
                                 options = list(render = gi_picker_render,
                                                maxOptions = 100))
                ),
                class = "col-sm-2"
              ),
              tagAppendAttributes(
                conditionalPanel(
                  condition = "input.tabs !== 'origins'",
                  selectizeInput("g_compare", "Compare to",
                                 choices = c(list("— none —" = ""),
                                             team_choices_grouped),
                                 selected = "", width = "100%",
                                 options = list(render = gi_picker_render,
                                                allowEmptyOption = TRUE,
                                                maxOptions = 100))
                ),
                class = "col-sm-2"
              ),
              tagAppendAttributes(
                conditionalPanel(
                  condition = "input.tabs === 'origins'",
                  div(class = "gi-fixed-control gi-origin-scope-control",
                      shiny::span("Analysis scope"),
                      strong("All 67 destinations"),
                      tags$small("Team and comparison do not filter this story"))
                ),
                class = "col-sm-4"
              ),
              column(width = 2,
                     radioButtons("g_sport", "Sport",
                                  choices = c("Football" = "football",
                                              "Basketball" = "basketball"),
                                  selected = "football", inline = TRUE)),
              column(width = 3,
                     ## default = the 4-class "roster window": in the portal
                     ## era those classes supply ~92% of current rosters (the
                     ## 5th year back contributes ~4%)
                     sliderInput("g_years", "Class years",
                                 min = SIZE_YEARS[1], max = SIZE_YEARS[2],
                                 value = c(SIZE_YEARS[2] - 3, SIZE_YEARS[2]),
                                 step = 1, sep = "", width = "100%"),
                     div(class = "year-presets",
                         actionButton("preset_all", "All years", class = "btn-xs"),
                         actionButton("preset_recent", "Last 4", class = "btn-xs"),
                         actionButton("preset_now",
                                      paste0("'", SIZE_YEARS[2] %% 100, " class"),
                                      class = "btn-xs"))),
              column(width = 3,
                     conditionalPanel(
                       condition = "input.tabs !== 'origins'",
                       radioButtons("g_type", "Players",
                                    choices = c("HS commits" = "commit",
                                                "Commits + transfers" = "both",
                                                "Transfers only" = "transfer"),
                                    selected = "both")
                     ),
                     conditionalPanel(
                       condition = "input.tabs === 'origins'",
                       div(class = "gi-fixed-control",
                           shiny::span("Player pool"),
                           strong("HS/prep only"),
                           tags$small("Portal, JUCO, and review queue excluded"))
                     ),
                     checkboxInput("show_context", "Context notes",
                                   value = TRUE))
            ))),

    div(class = "gi-page-stage",
        uiOutput("page_intro"),

    tabItems(

      ## HOME ------------------------------------------------------------------
      tabItem(tabName = "home",
              tags$section(
                class = "gi-cover",
                div(
                  class = "gi-cover__copy",
                  div(class = "gi-eyebrow", "THE POWER-4 RECRUITING BODY LAB"),
                  h1("See how a roster is built — before game day."),
                  uiOutput("hero_tagline"),
                  div(
                    class = "gi-cover__actions",
                    actionButton(
                      "cover_sizelab",
                      tagList(icon("ruler-combined"), "Explore the Size Lab"),
                      class = "btn-warning"),
                    actionButton(
                      "cover_conflab",
                      tagList(icon("layer-group"), "Compare all four conferences"),
                      class = "btn-default")
                  )
                ),
                div(class = "gi-cover__lens", uiOutput("hero_team"))
              ),
              uiOutput("home_data_pulse"),
              div(
                class = "gi-section-head",
                div(
                  h2("Current program pulse"),
                  p("The selected program first; deeper analysis follows.")
                )
              ),
              fluidRow(
                ## Each status number is a native keyboard-operable door into
                ## the analysis behind it.
                column(width = 4, tags$a(
                  href = "#shiny-tab-beef", class = "vb-link",
                  title = "Open Conference Beef",
                  onclick = "document.querySelector('a[href=\"#shiny-tab-beef\"]').click(); return false;",
                  shiny::span(class = "sr-only", "Open Conference Beef. "),
                  valueBoxOutput("vb_home_rank", width = NULL))),
                column(width = 4, tags$a(
                  href = "#shiny-tab-sizelab", class = "vb-link",
                  title = "Open the Size Lab",
                  onclick = "document.querySelector('a[href=\"#shiny-tab-sizelab\"]').click(); return false;",
                  shiny::span(class = "sr-only", "Open the Size Lab. "),
                  valueBoxOutput("vb_home_class", width = NULL))),
                column(width = 4, tags$a(
                  href = "#shiny-tab-weightroom", class = "vb-link",
                  title = "Open the Weight Room",
                  onclick = "document.querySelector('a[href=\"#shiny-tab-weightroom\"]').click(); return false;",
                  shiny::span(class = "sr-only", "Open the Weight Room. "),
                  valueBoxOutput("vb_home_dev", width = NULL)))
              ),
              fluidRow(
                column(width = 12, uiOutput("home_matchup"))
              ),
              fluidRow(
                ## Renders only when this device has a meaningful prior visit.
                column(width = 12, uiOutput("last_visit_strip"))
              ),
              fluidRow(
                box(
                  title = textOutput("class_snap_title"),
                  status = "danger", solidHeader = TRUE, width = 5,
                  htmlOutput("class_snap"),
                  uiOutput("class_snap_note"),
                  footer = HTML("<em style='color:#888;'>Arriving classes lead
                    the dashboard; the next cycle is tracked separately while
                    it remains open.</em>")
                ),
                box(
                  title = textOutput("home_points_title"),
                  status = "primary", solidHeader = TRUE, width = 7,
                  htmlOutput("home_points"),
                  footer = HTML("<em style='color:#888;'>Generated from the
                    current settings — change team, sport, or years in the
                    top bar and these update.</em>")
                )
              ),
              fluidRow(
                column(width = 12, uiOutput("home_fingerprint"))
              ),
              div(
                class = "gi-section-head",
                div(
                  h2("Go deeper"),
                  p("Choose a focused question when you are ready for more detail.")
                )
              ),
              uiOutput("home_paths"),
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "gi-team-browser",
                    box(
                      title = "Browse all 67 programs",
                      status = "primary", solidHeader = TRUE, width = 12,
                      collapsible = TRUE, collapsed = TRUE,
                      tags$input(
                        id = "home_team_search", type = "search",
                        class = "gi-team-search",
                        placeholder = "Search programs",
                        `aria-label` = "Search all Power-4 programs",
                        autocomplete = "off"),
                      lapply(conf_order(), function(cf) {
                        d <- DISPLAY_CONFIG[DISPLAY_CONFIG$conference == cf, ,
                                            drop = FALSE]
                        d <- d[order(d$team_name), , drop = FALSE]
                        if (!nrow(d)) return(NULL)
                        tagList(
                          div(cf, class = "gi-pick-conf"),
                          div(
                            class = "gi-team-grid",
                            lapply(seq_len(nrow(d)), function(i) {
                              actionButton(
                                inputId = paste0("select_",
                                                 gsub("-", "_", d$slug[i])),
                                label = tagList(
                                  img(src = d$logo[i], alt = ""),
                                  shiny::span(d$team_name[i])),
                                class = "gi-home-team",
                                title = paste("Select", d$team_name[i])
                              )
                            })
                          )
                        )
                      }),
                      footer = HTML("<em style='color:#888;'>Selecting a program
                        updates the Home lens, fingerprint, and every global
                        control without taking you away from this page.</em>")
                    )
                  )
                )
              )
      ),

      ## MATCHUP ----------------------------------------------------------------
      ## Direct program comparison lives on its own route so a cross-conference
      ## pick adds clarity rather than quietly changing conference boards.
      tabItem(tabName = "compare",
              uiOutput("matchup_empty"),
              conditionalPanel(
                condition = "input.g_compare && input.g_compare !== '' && input.g_compare !== input.g_team",
                uiOutput("matchup_context"),
                fluidRow(
                  column(width = 12, uiOutput("matchup_scorecard"))
                ),
                fluidRow(
                  box(
                    title = "Head-to-head: position-group weigh-in",
                    status = "primary", solidHeader = TRUE, width = 8,
                    spin(girafeOutput("matchup_h2h_plot", height = "470px"),
                         color = "#0C234B"),
                    footer = HTML("<em style='color:#777;'>Each point compares the two programs directly within one position group. This is the fastest way to see where a raw size edge actually lives.</em>")
                  ),
                  box(
                    title = "Read this matchup",
                    status = "warning", solidHeader = TRUE, width = 4,
                    tags$div(
                      class = "gi-matchup-guide",
                      tags$p(tags$strong("Raw values answer the direct question."),
                             " The scorecard shows the actual recruiting profile on both sides."),
                      tags$p(tags$strong("Percentiles keep the context honest."),
                             " Arizona is evaluated in the Big 12 and the comparison school in its own conference."),
                      tags$p(tags$strong("Conference boards remain conference boards."),
                             " External references never alter a league rank, average, or outcome model.")
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Continue the investigation",
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(
                      class = "gi-matchup-actions",
                      actionButton("go_compare_sizelab",
                                   tagList(icon("ruler-combined"), "Map every body"),
                                   class = "btn-default"),
                      actionButton("go_compare_reach",
                                   tagList(icon("route"), "Compare recruiting reach"),
                                   class = "btn-default")
                    ),
                    footer = HTML("<em style='color:#777;'>Use Size Lab for player-level body distributions and Program Reach for the geographic footprint behind each class.</em>")
                  )
                )
              )
      ),
      ## SIZE LAB ---------------------------------------------------------------
      tabItem(tabName = "sizelab",
              fluidRow(
                valueBoxOutput("vb_height", width = 3),
                valueBoxOutput("vb_weight", width = 3),
                valueBoxOutput("vb_lbsin", width = 3),
                valueBoxOutput("vb_rank", width = 3)
              ),
              fluidRow(
                box(
                  title = tagList("The Body Map: every player, height x weight (tap the dots!)",
                                  info_btn("info_size")),
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  div(style = "text-align:center;",
                      radioButtons("body_pos", NULL,
                                   choices = c("All positions" = "All"),
                                   selected = "All", inline = TRUE)),
                  spin(girafeOutput("body_map", height = "560px"),
                              color = "#0C234B"),
                  footer = HTML("<em style='color:#888;'>Pick a position above
                    to compare just that group — cloud, medians and all.</em>")
                )
              ),
              fluidRow(
                box(
                  title = "Position DNA vs the Conference",
                  status = "primary", solidHeader = TRUE,
                  width = 7, collapsible = TRUE,
                  spin(girafeOutput("dna_plot", height = "430px"),
                              color = "#0C234B")
                ),
                box(
                  title = tagList("Insights",
                                  tags$button(class = "copy-btn", type = "button",
                                              `data-copy` = "talking_points",
                                              `data-footer` = copy_footer,
                                              icon("copy"), " copy")),
                  status = "warning", solidHeader = TRUE,
                  width = 5, collapsible = TRUE,
                  htmlOutput("talking_points")
                )
              )
      ),

      ## CONFERENCE BEEF ----------------------------------------------------------
      tabItem(tabName = "beef",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tagList("Conference Beef: who has the biggest bodies?",
                                  info_btn("info_beef")),
                  fluidRow(
                    column(width = 3,
                           selectInput("size_metric", "Metric",
                                       choices = metric_choices,
                                       selected = "AvgWeight", width = "100%")),
                    column(width = 3,
                           selectInput("size_pos", "Position group",
                                       choices = pos_choices("football"),
                                       selected = "All", width = "100%")),
                    column(width = 3,
                           radioButtons("size_source", "Players",
                                        choices = c("Commit classes" = "commits",
                                                    "Current roster" = "roster"),
                                        selected = "commits")),
                    column(width = 3,
                           div(style = "padding-top:25px; color:#777;",
                               HTML("<em>'Current roster' = who's on campus now,
                                    at CURRENT weights — players add 10–25 lbs
                                    after arriving, so the roster runs heavier
                                    than the same players' commit weights. The
                                    year window applies to commit classes only.</em>")))
                  )
                )
              ),
              fluidRow(
                column(width = 5,
                       box(
                         ## "<conf> Beef Board": the conference name is a DATA
                         ## label (this IS the active conference's leaderboard),
                         ## so it tracks conf_label -- Phase 0 shows "Big 12".
                         title = tagList(textOutput("beef_board_conf_title",
                                                    inline = TRUE),
                                         twin_toggle("beef_board")),
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         div(class = "gi-chartwrap",
                             spin(girafeOutput("beef_board", height = "640px"),
                                  color = "#0C234B")),
                         div(class = "gi-tablewrap",
                             uiOutput("beef_twin")),
                         ## the backcast-honesty note: names the conference, its
                         ## member count, and the class year the full membership
                         ## was finally in-conference (conf_whole). See
                         ## output$beef_ctx_note.
                         uiOutput("beef_ctx_note")
                       )),
                column(width = 7,
                       box(
                         title = "Size Over Time vs the Conference",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("size_trend", height = "300px"),
                                     color = "#0C234B"),
                         ctx_note("Portal transfers exist in the data from 2021",
                                  " on (a handful of earlier grad transfers",
                                  " appear for some programs) — with 'Commits",
                                  " + transfers' selected, earlier years are",
                                  " mostly HS classes.")
                       ),
                       box(
                         title = "Head to Head: Position-Group Weigh-In",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("h2h_plot", height = "300px"),
                                     color = "#0C234B")
                       ))
              )
      ),

      ## CONFERENCE LAB ------------------------------------------------------------
      tabItem(tabName = "conflab",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tagList("Conference Lab: how do the leagues stack up?",
                                  info_btn("info_conflab")),
                  fluidRow(
                    column(width = 5,
                           selectInput("conf_metric", "Compare leagues by",
                                       choices = conf_metric_choices(),
                                       selected = "AvgRating", width = "100%")),
                    column(width = 7,
                           div(style = "padding-top:25px; color:#777;",
                               HTML("<em>All four Power-4 leagues, side by side —
                                    each dot is one team's average, so you see the
                                    spread, not just the headline. Uses the global
                                    Sport, Players, and year-window controls. Win%
                                    and SP+ are deliberately absent (a league plays
                                    itself). See the info icon at the top-right of
                                    this box for the honesty rules.</em>")))
                  ),
                  ## amber caveat: only when a YELLOW context metric is picked,
                  ## so the "reads geography/strategy, not talent" warning lands
                  ## in the eye's path, not just inside the plot caption
                  uiOutput("conf_caveat")
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = tagList("The Power-4 talent spread",
                                         twin_toggle("conf_spread")),
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         div(class = "gi-chartwrap",
                             spin(girafeOutput("conf_spread", height = "560px"),
                                  color = "#0C234B")),
                         div(class = "gi-tablewrap",
                             uiOutput("conf_twin")),
                         ctx_note("Every league aggregate is over TODAY's ",
                                  "membership; a team counts only the class years ",
                                  "it was actually in its current conference. ",
                                  "Ranges overlap by design — the chart shows it.")
                       ))
              )
      ),

      ## WEIGHT ROOM ---------------------------------------------------------------
      tabItem(tabName = "weightroom",
              fluidRow(
                valueBoxOutput("vb_wr_gain", width = 3),
                valueBoxOutput("vb_wr_rank", width = 3),
                valueBoxOutput("vb_wr_gainer", width = 3),
                valueBoxOutput("vb_wr_shrink", width = 3)
              ),
              fluidRow(
                box(width = 12, status = "primary",
                    div(style = "text-align:center;",
                        radioButtons("wr_direction", NULL,
                                     choices = c("Biggest gainers (bulk-ups)" = "gain",
                                                 "Biggest slim-downs (weight cut)" = "loss"),
                                     selected = "gain", inline = TRUE)))
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = tagList(textOutput("wr_board_box_title",
                                                    inline = TRUE),
                                         info_btn("info_wr"),
                                         twin_toggle("wr_board")),
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         div(class = "gi-chartwrap",
                             spin(girafeOutput("wr_board", height = "560px"),
                                  color = "#0C234B")),
                         div(class = "gi-tablewrap",
                             uiOutput("wr_twin")),
                         footer = htmlOutput("wr_footer")
                       )),
                column(width = 6,
                       box(
                         title = textOutput("wr_players_box_title",
                                            inline = TRUE),
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("wr_players", height = "560px"),
                                     color = "#0C234B")
                       ))
              ),
              fluidRow(
                box(
                  title = tagList("Class Retention: who keeps their signees?",
                                  info_btn("info_retention"),
                                  twin_toggle("class_retention")),
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  div(class = "gi-chartwrap",
                      spin(girafeOutput("class_retention", height = "560px"),
                           color = "#0C234B")),
                  div(class = "gi-tablewrap",
                      uiOutput("retention_twin")),
                  footer = HTML("<em style='color:#888;'>The portal-era
                    scoreboard: of the last four signing classes, how many
                    players are still in the building?</em>")
                )
              ),
              fluidRow(
                box(
                  title = "The Measurement Reality Check (the 'listed height' problem)",
                  status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  spin(plotOutput("height_check", height = "360px"),
                              color = "#0C234B"),
                  footer = htmlOutput("height_check_text")
                )
              )
      ),

      ## COACH ERAS -----------------------------------------------------------------
      tabItem(tabName = "eras",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tagList("Coach Eras: how each staff recruits differently",
                                  info_btn("info_eras")),
                  fluidRow(
                    column(width = 4,
                           selectInput("era_metric", "Timeline metric",
                                       choices = era_metric_choices,
                                       selected = "AvgRating", width = "100%")),
                    column(width = 8,
                           div(style = "padding-top:25px; color:#777;",
                               HTML(paste0("<em>Always shows the full 2016–", SIZE_YEARS[2], "
                                    history. Tap or hover a class dot for its top-5
                                    signees; click to open the class on
                                    247Sports.</em>"))))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "The Coach Timeline",
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  spin(girafeOutput("era_timeline", height = "440px"),
                              color = "#0C234B"),
                  ctx_note("Classes belong to the staff that ran their December",
                           " signing window, so after a January change (Fisch →",
                           " Brennan, Jan 2024) the outgoing staff's final class",
                           " largely signed before the new coach arrived. The",
                           " newest cycle is still open — ratings re-rank and",
                           " players keep committing through signing day.")
                )
              ),
              fluidRow(
                box(
                  title = "What Each Coach Spends Their Classes On",
                  status = "primary", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  spin(girafeOutput("era_mix", height = "430px"),
                              color = "#0C234B")
                ),
                box(
                  title = "Era Report Card",
                  status = "primary", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  DTOutput("era_table")
                )
              )
      ),

      ## ANALYST BRIEF — THE DEFENSIVE WAR ROOM --------------------------------------
      tabItem(tabName = "brief",
              fluidRow(
                box(
                  width = 12, status = "danger", solidHeader = TRUE,
                  title = tagList("The Defensive War Room — built for the 3-3-5",
                                  info_btn("info_brief")),
                  HTML("<p style='font-size:14px; color:#555; margin:0;'>
                    Arizona's defense under DC <strong>Danny Gonzales</strong>
                    runs the attacking <strong>3-3-5 odd stack</strong> — a
                    scheme that trades size for surprise: one true big body at
                    Nose, long rangy Ends, multi-role Stack LBs, and a hybrid
                    S/LB patrolling the middle. This room maps the selected
                    team's bodies onto those roles, finds the thin rooms, and
                    writes the brief — all from the control-bar settings.</p>
                    <p style='font-size:12.5px; color:#999; margin:8px 0 0 0;'>
                    <em>Note: the role bands here are specialized for
                    Arizona's 3-3-5 (Rocky Long / Gonzales tree) — they're a
                    lens, not a universal depth chart. Other teams' bodies are
                    shown through the same lens for comparison.</em></p>
                    <a class='custom-cta'
                       href='mailto:desertdatalabs@gmail.com?subject=Custom%20war%20room%20for%20my%20team'>
                      <i class='fa fa-hand'></i>
                      Run a different scheme? We'll build a war room tuned to
                      YOUR team — get in touch
                    </a>")
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "The 3-3-5 Fit Board: Bodies by Odd-Stack Role",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("roster_335", height = "430px"),
                                     color = "#0C234B"),
                         footer = HTML("<em style='color:#888;'>Hover a segment
                           for names + weights; the annotation shows headcount
                           vs the scheme's two-deep target and how much of the
                           room is JR/SR.</em>")
                       )),
                column(width = 6,
                       box(
                         title = "Defensive Bodies vs the League (3-3-5 Lens)",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("def_profile", height = "430px"),
                                     color = "#0C234B")
                       ))
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "Roster Construction: Who Graduates Next?",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("roster_constr", height = "430px"),
                                     color = "#0C234B"),
                         footer = HTML("<em style='color:#888;'>High JR/SR share =
                           rooms the next one or two classes must restock.</em>")
                       )),
                column(width = 6,
                       box(
                         title = "In-State HS Talent: Keep the Fence Up",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("state_retention", height = "430px"),
                                     color = "#0C234B")
                       ))
              ),
              fluidRow(
                box(
                  title = tagList("The Brief",
                                  tags$button(class = "copy-btn", type = "button",
                                              `data-copy` = "analyst_notes_out",
                                              `data-footer` = copy_footer,
                                              icon("copy"), " copy")),
                  status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  htmlOutput("analyst_notes_out"),
                  footer = HTML("<em style='color:#888;'>Auto-written from the
                    recruiting + roster data, defense first. Ratings are
                    commit-time inputs; pairing them with outcome data (snaps,
                    all-conference, draft) is the natural next step.</em>")
                )
              )
      ),

      ## TALENT vs RESULTS ------------------------------------------------------------
      tabItem(tabName = "results",
              fluidRow(
                box(
                  title = tagList("Talent vs Results: does recruiting become winning?",
                                  info_btn("info_results")),
                  width = 12, status = "danger", solidHeader = TRUE,
                  HTML("<p style='font-size:14px; color:#555; margin:0;'>
                    Season records and SP+ from CollegeFootballData joined to
                    each program's rolling 4-class talent composite (HS +
                    portal ratings). Above the diagonal of expectations =
                    coaching and development adding wins; below it = talent
                    leaking value.</p>")
                )
              ),
              fluidRow(
                box(
                  title = tagList("The Over/Underachiever Quadrant",
                                  twin_toggle("talent_quadrant")),
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  div(class = "gi-chartwrap",
                      spin(girafeOutput("talent_quadrant", height = "520px"),
                           color = "#0C234B")),
                  div(class = "gi-tablewrap",
                      uiOutput("quadrant_twin")),
                  ctx_note("2020 was the COVID season — most programs played
                    5–10 games, so windows that include 2020 mix shortened
                    seasons into the win percentages."),
                  footer = HTML("<em style='color:#888;'>Follows the class-year
                    window in the control bar (completed seasons only).</em>")
                )
              ),
              fluidRow(
                box(
                  title = tagList("Wins Above Talent: Who Beats Their Recruiting?",
                                  info_btn("info_wat"),
                                  twin_toggle("wat_ladder")),
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  div(class = "gi-chartwrap",
                      spin(girafeOutput("wat_ladder", height = "560px"),
                           color = "#0C234B")),
                  div(class = "gi-tablewrap",
                      uiOutput("wat_twin")),
                  ctx_note("2020 was the COVID season - most programs played
                    5-10 games, so windows that include 2020 fold shortened
                    seasons into the win rates the model fits."),
                  footer = HTML("<em style='color:#888;'>Grey dot = expected
                    win % from the league's talent-to-wins fit; colored dot =
                    actual. Follows the class-year window (completed seasons
                    only).</em>")
                )
              ),
              fluidRow(
                box(
                  title = "Season Scoreboard: Wins vs Talent on Hand",
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  spin(girafeOutput("team_scoreboard", height = "380px"),
                              color = "#0C234B"),
                  ctx_note("2020 win totals are not comparable to other
                    seasons — the COVID year cut most schedules to 5–10
                    games (Arizona played just 5)."),
                  footer = HTML("<em style='color:#888;'>When the bars beat the
                    dashed line's trajectory, the staff is outcoaching its
                    talent — Arizona 2025 under the new defense is the case
                    study.</em>")
                )
              )
      ),

      ## TALENT ORIGINS -----------------------------------------------------------
      tabItem(
        tabName = "origins",
        div(
          class = "gi-origin-toolbar",
          div(
            class = "gi-origin-view-switch",
            radioButtons(
              "origin_view", "Story",
              choices = c("State Board" = "board",
                          "Position Hotbeds" = "positions",
                          "Change Over Time" = "trend"),
              selected = "board", inline = TRUE
            )
          ),
          div(
            class = "gi-origin-filter",
            selectInput(
              "origin_metric", "Measure",
              choices = origin_metric_choices(),
              selected = "blue_n", selectize = FALSE
            )
          ),
          tagAppendAttributes(
            conditionalPanel(
              condition = "input.origin_view !== 'positions'",
              selectInput(
                "origin_pos", "Position group",
                choices = origin_position_choices("football"),
                selected = "All", selectize = FALSE
              )
            ),
            class = "gi-origin-filter gi-origin-position"
          ),
          tagAppendAttributes(
            conditionalPanel(
              condition = "input.origin_view !== 'positions'",
              selectInput(
                "origin_state", "Focus state",
                choices = setNames(ORIGIN_US_CODES,
                                   origin_state_name(ORIGIN_US_CODES)),
                selected = "AZ", selectize = FALSE
              )
            ),
            class = "gi-origin-filter gi-origin-state"
          )
        ),
        uiOutput("origin_story"),
        conditionalPanel(
          condition = "input.origin_view === 'board'",
          fluidRow(
            box(
              title = tagList(
                "State Talent Board",
                info_btn("info_origins"),
                twin_toggle("origin_board")
              ),
              status = "primary", solidHeader = TRUE, width = 12,
              div(
                class = "gi-chartwrap gi-no-pin",
                spin(girafeOutput("origin_board", height = "650px"),
                     color = "#0C234B")
              ),
              div(class = "gi-tablewrap", uiOutput("origin_board_twin"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.origin_view === 'positions'",
          fluidRow(
            box(
              title = tagList(
                "Position Hotbeds",
                info_btn("info_origins"),
                twin_toggle("origin_positions")
              ),
              status = "primary", solidHeader = TRUE, width = 12,
              div(
                class = "gi-chartwrap",
                spin(girafeOutput("origin_positions", height = "680px"),
                     color = "#0C234B")
              ),
              div(class = "gi-tablewrap", uiOutput("origin_positions_twin"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.origin_view === 'trend'",
          fluidRow(
            box(
              title = tagList(
                "How the pipeline is changing",
                info_btn("info_origins"),
                twin_toggle("origin_trend")
              ),
              status = "primary", solidHeader = TRUE, width = 12,
              div(
                class = "gi-chartwrap",
                spin(girafeOutput("origin_trend", height = "470px"),
                     color = "#0C234B")
              ),
              div(class = "gi-tablewrap", uiOutput("origin_trend_twin"))
            )
          )
        ),
        tags$details(
          class = "gi-origin-deeper",
          tags$summary(
            tagList(icon("compass"), "Explore deeper signals")
          ),
          uiOutput("origin_deeper")
        ),
        ctx_note(
          "State means the last listed HS/prep school location, not birthplace. ",
          "Obvious JUCO and new unreviewed College sources are excluded; ",
          "aggregate views state the open class's partial contribution, and ",
          "the trend marks it directly."
        )
      ),


      ## PROGRAM REACH ---------------------------------------------------------------
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  title = tagList("Program Reach: mapped recruiting pipelines",
                                  info_btn("info_map")),
                  footer = HTML("<span style='color:#888;'>
                    <em>Dots show mapped listed origins. Shaded shapes trace
                    your selected program; the comparison pipeline keeps its
                    own team color.</em></span>"),
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = FALSE,
                  spin(leafletOutput("gridPlot", height = "420px"),
                              color = "#0C234B")),
                box(
                  title = "Player-level reach: farthest listed origins first",
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  DTOutput("summary_stats", height = "230px")
                )
              ),
              fluidRow(
                box(
                  fluidRow(
                    column(width = 4,
                           selectInput("show_outliers", label = "Outliers",
                                       selectize = FALSE, multiple = FALSE,
                                       choices = c("Show Outliers" = "show",
                                                   "Hide Outliers" = "hide"),
                                       selected = "show", width = "100%"))
                  ),
                  title = tagList("Travel by signing class",
                                  info_btn("info_distance")),
                  footer = HTML("<span style='color:#888;'>
                    Tap or hover any dot for the recruit card; pin it to open their
                    247Sports page.</span>"),
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  spin(
                    girafeOutput("distance_plot", height = "470px"),
                    color = "#0C234B")),
                box(
                  title = "Miles from listed origin by position",
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  footer = HTML("<span style='color:#888;'>
                    Distances need a mapped listed origin. Transfers from recent
                    cycles (2023 on) join as the nightly refresh captures
                    theirs from their 247Sports profiles; earlier transfer
                    classes stay unmapped.</span>"),
                  spin(girafeOutput("box_plot", height = "340px"),
                              color = "#0C234B"))
              )
      ),

      ## DATA & NOTES -------------------------------------------------------------------
      tabItem(tabName = "notes",
              fluidRow(
                box(
                  title = "About the data", status = "primary",
                  solidHeader = TRUE, width = 6,
                  ## body rendered server-side (output$about_data) so the
                  ## DATA-universe scope -- the conference label + member count --
                  ## tracks the active conference and stays honest across the
                  ## onboarding phases; Phase 0 renders the original copy verbatim.
                  uiOutput("about_data")
                ),
                box(
                  title = "Feedback & custom builds", status = "danger",
                  solidHeader = TRUE, width = 6,
                  HTML("
                    <p style='font-size:14px; line-height:1.7;'>
                    This app is built and maintained by
                    <strong>Desert Data Labs</strong>. Spotted a data error,
                    have a feature idea, or want a tool like this built for
                    your program, conference, or business?</p>
                    <p style='font-size:15px;'>
                    <a href='mailto:desertdatalabs@gmail.com?subject=Power-4%20Girth%20Index'>
                    <strong>desertdatalabs@gmail.com</strong></a></p>
                    <p style='font-size:13px; color:#888;'>Custom dashboards,
                    scrapers, and analytics for sports and beyond.</p>")
                ),
                box(
                  title = "What's next", status = "warning",
                  solidHeader = TRUE, width = 6,
                  HTML("
                    <p style='font-size:14px; line-height:1.7;'>
                    The full Power-4 foundation is live. The next product layer
                    moves from richer comparison to verified player outcomes:</p>
                    <ol style='font-size:14px; line-height:1.7;'>
                      <li><strong>Player outcomes</strong> — snap counts,
                        all-conference honors, starts, and draft results joined
                        back to each signee, with coverage shown before any
                        development grade.</li>
                      <li><strong>Portal lifecycle</strong> — arrivals,
                        departures, returners, and retained production in one
                        cohort view instead of treating portal share as a
                        stand-alone score.</li>
                      <li><strong>Conference eras</strong> — time-aware league
                        distributions that distinguish historical membership
                        from the current Power-4 map through realignment.</li>
                      <li><strong>Brief builder</strong> — save a set of views,
                        pinned players, and caveats into one shareable scouting
                        report without losing the active filters.</li>
                    </ol>
                    <p style='font-size:13px; color:#667085;'>
                    Have a better next question? Send it through the feedback
                    link above — the best additions start with a real decision
                    somebody is trying to make.</p>")
                )
              )
      )

    ), ## end tabItems
    ), ## end page stage

    ## site footer: who built this + how to reach us
    div(class = "ddl-footer",
        HTML(paste0(
          "Built by <strong>Desert Data Labs</strong> · feedback, bug reports, ",
          "or want something like this built for your program? ",
          "<a href='mailto:desertdatalabs@gmail.com?subject=Power-4%20Girth%20Index'>",
          "desertdatalabs@gmail.com</a>")))
  ) ## end body
) ## end UI

## SERVER =====================================================================
server <- function(input, output, session) {

  ## shared girafe wrapper -- delegates to girafe_build() (R/girth_functions)
  ## so live renders and deploy-time precomputed objects are identical.
  ## On phones (client width < 700px) the SVG canvas shrinks so text and tap
  ## targets render ~60% larger after the browser scales it to the screen.
  is_phone <- function() isTRUE((input$client_w %||% 1200) < 700)
  girafe_wrap <- function(p, w = 11.5, h = 6.5, name = "power4-girth-index") {
    girafe_build(p, w = w, h = h, name = name, phone = is_phone())
  }

  ## deploy-time precomputed renders of the DEFAULT view (see
  ## scripts/precomputeDefaults.R) -- the first Size Lab paint costs a
  ## readRDS instead of a 3-second ggplot/SVG build.
  ## CRITICAL: the rds was serialized on the dev machine, and htmlwidget
  ## objects embed dependency file paths from THAT machine's R library --
  ## they must be rebuilt against this machine's library or the widget
  ## silently fails to render on the server. Any unloadable file is
  ## dropped (the app just renders live instead).
  PRE <- local({
    files <- list.files("precomputed", pattern = "\\.rds$", full.names = TRUE)
    objs <- lapply(files, function(f) {
      tryCatch({
        g <- readRDS(f)
        if (identical(basename(f), "meta.rds")) return(g)
        g$dependencies <- htmlwidgets::getDependency("girafe", "ggiraph")
        g
      }, error = function(e) {
        message("precomputed ", basename(f), " unusable: ",
                conditionMessage(e))
        NULL
      })
    })
    objs <- setNames(objs, tools::file_path_sans_ext(basename(files)))
    objs[!vapply(objs, is.null, logical(1))]
  })

  ## transient render failures (cold-start font/memory hiccups on hosted
  ## containers) must not surface as scary sanitized errors -- show a calm
  ## retry chart instead, and log the real condition for the server logs.
  ## Changing any control makes a new cache key, so the retry works.
  ##
  ## AUTO-RETRY-ONCE: the first failure of a given output also schedules one
  ## silent re-render (invalidateLater) so a one-off cold-start hiccup heals
  ## itself without the visitor touching a control. A session-scoped ledger
  ## (keyed by the output's `what` label) caps this at ONE auto-retry per
  ## output per session, so a genuinely broken view still settles on the
  ## calm chart -- no retry loop. The ledger read is isolate()d so the write
  ## never makes THIS render a reader of the value it just set.
  ## bindCache note: see contract_notes -- the four bindCache'd outputs
  ## (body_map, dna_plot, beef_board, era_timeline) cache both values AND
  ## thrown errors, so their 4s re-run is a cache hit and the auto-retry is
  ## a no-op for them (unchanged from today); the retry heals the ~14
  ## uncached renders, whose failure path is genuinely not cached.
  retry_ledger <- reactiveValues()
  retry_key <- function(what) gsub("[^a-z0-9]+", "_", tolower(what))
  girafe_try <- function(expr, what = "chart") {
    tryCatch(expr, error = function(e) {
      message("render failed (", what, "): ", conditionMessage(e))
      k <- retry_key(what)
      n <- isolate(retry_ledger[[k]] %||% 0L)
      if (n < 1L) {
        retry_ledger[[k]] <- n + 1L
        invalidateLater(4000, session)
      }
  ## A metadata receipt must also match the bundled database revision; older
  ## or unreceipted RDS files fall back to live rendering.
      girafe_build(
        ggplot2::ggplot() +
          ggplot2::annotate(
            "text", x = 0, y = 0, size = 5.2, color = "#46535E",
            label = paste0("This view hit a temporary rendering snag.\n",
                           "Nudge any control (year, metric, team) to",
                           " reload it.")) +
          ggplot2::theme_void(),
        w = 8, h = 4, name = "render-retry")
    })
  }

  ## TRUE only when every control sits at its startup default.
  ## Reads the SAME debounced years value the renders and cache keys use --
  ## reading raw input$g_years here let a mid-drag flush disagree with the
  ## cache key. Charts that draw the compare team pass cmp_sensitive = TRUE:
  ## the precomputed dna has no comparison, and serving it for another
  ## compare pick would store the wrong chart under that bindCache key.
  at_defaults <- function(pos_input = NULL, cmp_sensitive = FALSE) {
    meta <- PRE[["meta"]]
    meta_ok <- is.list(meta) &&
      identical(meta$data_revision %||% "", DATA_REVISION) &&
      identical(meta$team %||% "", "arizona") &&
      identical(meta$sport %||% "", "football") &&
      identical(as.integer(meta$years), as.integer(DEFAULT_YEARS))
    compare_ok <- !cmp_sensitive ||
      identical(meta$compare_slug %||% "", input$g_compare %||% "")
    meta_ok && compare_ok &&
      identical(input$g_team, "arizona") &&
      g_sport() == "football" &&
      identical(as.integer(g_years_d()), as.integer(DEFAULT_YEARS)) &&
      identical(input$g_type %||% "both", "both") &&
      (is.null(pos_input) || pos_input == "All")
  }

  ## chart export filename: team + sport + chart + window, e.g.
  ## "arizona-football-beef-board-2022-2026" (debounced years -- must match
  ## what the chart actually shows)
  png_name <- function(chart) {
    yrs <- g_years_d()
    glue("{input$g_team}-{g_sport()}-{chart}-{yrs[1]}-{yrs[2]}")
  }

  ## ---- URL DEEP LINKS: hydrate once at startup, then keep in sync --------
  ## Hydrate the global controls + active tab from the query string BEFORE
  ## any output reads a control. High priority so this observer runs first in
  ## the opening flush, and freezeReactiveValue on each restored input so the
  ## charts wait for the real value instead of doing a throwaway default
  ## render. parse_url_state already dropped anything forged/out-of-range, so
  ## every value applied here is safe. url_init lets the team-memory logic
  ## below defer to the URL. The at_defaults precompute gate is unaffected --
  ## it reads the SAME resolved inputs, so a link that equals the defaults
  ## still serves the precomputed renders.
  ## the controls' STARTUP defaults (must match the UI + at_defaults). A URL
  ## param that equals the default is applied by NOT touching the input:
  ## freezing then updating an input to the value it already holds would
  ## leave it frozen forever (the client never echoes an unchanged value back
  ## to thaw it), blanking every downstream chart. So freeze+update fires
  ## only when the restored value actually differs from the default.
  URL_DEFAULTS <- list(team = "arizona", compare = "",
                       sport = "football", years = as.integer(DEFAULT_YEARS),
                       type = "both", origin_view = "board",
                       origin_metric = "blue_n", origin_pos = "All",
                       origin_state = "AZ")
  url_init <- reactiveValues(done = FALSE, has_team = FALSE)
  observeEvent(session$clientData$url_search, once = TRUE, priority = 1000, {
    st <- parse_url_state(parseQueryString(session$clientData$url_search))
    ## a team param present at all suppresses the first-visit picker + beats
    ## localStorage, even when it equals the default (no update needed then)
    url_init$has_team <- !is.null(st$team)
    if (!is.null(st$team) && !identical(st$team, URL_DEFAULTS$team)) {
      freezeReactiveValue(input, "g_team")
      updateSelectInput(session, "g_team", selected = st$team)
    }
    if (!is.null(st$compare) && !identical(st$compare, URL_DEFAULTS$compare)) {
      freezeReactiveValue(input, "g_compare")
      updateSelectInput(session, "g_compare", selected = st$compare)
    }
    if (!is.null(st$sport) && !identical(st$sport, URL_DEFAULTS$sport)) {
      freezeReactiveValue(input, "g_sport")
      updateRadioButtons(session, "g_sport", selected = st$sport)
    }
    if (!is.null(st$years) &&
        !identical(as.integer(st$years), URL_DEFAULTS$years)) {
      freezeReactiveValue(input, "g_years")
      updateSliderInput(session, "g_years", value = st$years)
    }
    if (!is.null(st$type) && !identical(st$type, URL_DEFAULTS$type)) {
      freezeReactiveValue(input, "g_type")
      updateRadioButtons(session, "g_type", selected = st$type)
    }
    if (!is.null(st$origin_view) &&
        !identical(st$origin_view, URL_DEFAULTS$origin_view)) {
      updateRadioButtons(session, "origin_view", selected = st$origin_view)
    }
    if (!is.null(st$origin_metric) &&
        !identical(st$origin_metric, URL_DEFAULTS$origin_metric)) {
      updateSelectInput(session, "origin_metric", selected = st$origin_metric)
    }
    if (!is.null(st$origin_pos)) {
      url_init$origin_pos <- st$origin_pos
      link_sport <- st$sport %||% URL_DEFAULTS$sport
      updateSelectInput(session, "origin_pos",
                        choices = origin_position_choices(link_sport),
                        selected = st$origin_pos)
    }
    if (!is.null(st$origin_state) &&
        !identical(st$origin_state, URL_DEFAULTS$origin_state)) {
      updateSelectInput(session, "origin_state", selected = st$origin_state)
    }
    ## tab restore carries no freeze (no downstream reactive gate), so a
    ## no-op update to the already-active tab is harmless
    if (!is.null(st$tab) && !identical(st$tab, "home")) {
      updateTabItems(session, "tabs", st$tab)
    }
    url_init$done <- TRUE
  })

  ## write the current view back to the query string as a shareable deep
  ## link. Debounced so slider drags don't spam; mode='replace' so it never
  ## fills the back-button history. Gated on url_init$done so it can't clobber
  ## the incoming link before hydration lands.
  url_state_out <- debounce(reactive({
    req(url_init$done)
    yrs <- g_years_d()
    req(input$g_team, input$g_sport, yrs, input$tabs,
        input$origin_view, input$origin_metric, input$origin_pos,
        input$origin_state)
    cmp_raw <- input$g_compare %||% ""
    list(team = input$g_team,
         cmp = if (nzchar(cmp_raw)) cmp_raw else "none",
         sport = g_sport(),
         years = paste0(yrs[1], "-", yrs[2]),
         type = input$g_type %||% "both",
         tab = input$tabs,
         ov = input$origin_view,
         om = input$origin_metric,
         op = input$origin_pos,
         os = input$origin_state)
  }), 500)
  observe({
    s <- url_state_out()
    ## Every value is whitelisted; URL-encode because DL/Edge contains a slash.
    q <- c(team = s$team, cmp = s$cmp, sport = s$sport,
           years = s$years, type = s$type, tab = s$tab,
           ov = s$ov, om = s$om, op = s$op, os = s$os)
    encoded <- vapply(q, function(x) URLencode(as.character(x), reserved = TRUE),
                      character(1))
    updateQueryString(
      paste0("?", paste(names(encoded), encoded, sep = "=", collapse = "&")),
      mode = "replace")
  })

  ## ---- PLAYER CARD: tap a name in any pinned card -> holographic card ----
  observeEvent(input$pc_request, tryCatch({
    req(input$pc_request$name)
    nm <- input$pc_request$name
    sch <- input$pc_request$school %||% ""
    src <- if (g_sport() == "football") size_football else size_basketball

    hit <- src %>%
      filter(Name == nm, sch == "" | School == sch) %>%
      arrange(desc(Year)) %>%
      slice_head(n = 1)

    ## roster fallback for players who only exist on a roster page
    if (nrow(hit) == 0 && !is.null(roster_now())) {
      r <- roster_now() %>%
        filter(Name == nm, sch == "" | School == sch) %>%
        slice_head(n = 1)
      if (nrow(r) == 0) return(invisible(NULL))
      slug <- r$School[1]
      session$sendCustomMessage("playerCard", list(
        name = r$Name[1], team = team_label(slug),
        logo = TEAM_CONFIG$logo[match(slug, TEAM_CONFIG$slug)],
        c1 = team_color(slug), c2 = TEAM_CONFIG$secondary[
          match(slug, TEAM_CONFIG$slug)] %||% "#0C234B",
        pos = r$Position[1], yr = paste("Roster", r$RosterYear[1]),
        type = paste0("Class: ", r$Class[1]),
        ht = r$Height[1], wt = paste0(r$Weight[1], " lbs"),
        rating = "—", from = "—", miles = "",
        coach = "", url = "",
        src = "Current 247Sports roster listing"))
      return(invisible(NULL))
    }
    req(nrow(hit) == 1)
    slug <- hit$School[1]
    ## captured 247 profile deep-link when the scraper has one (works for
    ## commits AND transfers); p247_url falls back to the search URLs
    purl <- if ("ProfileUrl" %in% names(hit)) hit$ProfileUrl[1] else NA
    has_purl <- !is.na(purl) && !purl %in% c("", "NA")
    session$sendCustomMessage("playerCard", list(
      name = hit$Name[1], team = team_label(slug),
      logo = TEAM_CONFIG$logo[match(slug, TEAM_CONFIG$slug)],
      c1 = team_color(slug), c2 = TEAM_CONFIG$secondary[
        match(slug, TEAM_CONFIG$slug)] %||% "#0C234B",
      pos = hit$Position[1], yr = paste("Class of", hit$Year[1]),
      type = ifelse(identical(hit$Type[1], "Transfer"),
                    "Portal transfer", "HS commit"),
      ht = hit$HeightLabel[1], wt = paste0(hit$Weight[1], " lbs"),
      rating = ifelse(is.na(hit$Ranking[1]), "NR",
                      format(round(hit$Ranking[1], 1))),
      from = ifelse(is.na(hit$Location[1]) | hit$Location[1] == "",
                    "—", hit$Location[1]),
      miles = ifelse(is.na(hit$miles_away[1]), "",
                     paste0(format(hit$miles_away[1], big.mark = ","),
                            " mi from campus")),
      coach = ifelse(is.na(hit$Coach[1]), "", hit$Coach[1]),
      url = p247_url(hit$Name[1], hit$Year[1], g_sport(), hit$Type[1],
                     profile_url = purl),
      urlLabel = ifelse(identical(hit$Type[1], "Transfer") && !has_purl,
                        "Find their 247Sports profile →",
                        "Full 247Sports profile →"),
      src = "Size + rating as listed by 247Sports at commitment"))
  }, error = function(e) {
    ## a malformed request must never take the session down
    message("player card lookup failed: ", conditionMessage(e))
  }))

  ## ---- TEAM MEMORY: restore the saved team, or ask once ------------------
  ## A URL ?team= WINS: it overrides this device's localStorage team AND
  ## suppresses the first-visit picker. A plain observe (not observeEvent)
  ## so it settles once BOTH the URL has resolved (url_init$done) and the
  ## stored team has arrived from JS, in either order; team_memory_resolved
  ## makes the body run exactly once.
  team_memory_resolved <- reactiveVal(FALSE)
  observe({
    req(url_init$done)
    req(!is.null(input$stored_team))
    if (isolate(team_memory_resolved())) return()
    isolate({
      team_memory_resolved(TRUE)
      ## the URL already set the team -> don't touch it, don't ask
      if (isTRUE(url_init$has_team)) return()
      st <- input$stored_team
      if (!is.null(st) && st %in% onboarded_slugs()) {
        updateSelectInput(session, "g_team", selected = st)
      } else if (identical(st, "none")) {
        showModal(modalDialog(
          title = "Choose your program",
          easyClose = TRUE, footer = NULL, size = "l",
          div(
            style = "text-align:center;",
            p("This becomes your active lens and is saved on this device. You can change it any time.",
              style = "color:#667085; margin-bottom:6px;"),
            tags$input(
              id = "team_modal_search", type = "search",
              class = "gi-team-search",
              placeholder = "Search all 67 programs",
              `aria-label` = "Search all 67 Power-4 programs",
              autocomplete = "off"),
            div(
              class = "gi-modal-scroll",
              lapply(conf_order(), function(cf) {
                d <- DISPLAY_CONFIG[DISPLAY_CONFIG$conference == cf, ,
                                    drop = FALSE]
                d <- d[order(d$team_name), , drop = FALSE]
                if (!nrow(d)) return(NULL)
                div(
                  class = "gi-modal-section",
                  div(
                    cf, class = "gi-modal-conf",
                    style = paste0("--conf-color:", conf_color(cf))),
                  div(
                    class = "gi-modal-grid",
                    lapply(seq_len(nrow(d)), function(i) {
                      actionButton(
                        paste0("pick_", gsub("-", "_", d$slug[i])),
                        label = tagList(
                          img(src = d$logo[i], alt = ""),
                          div(d$team_name[i],
                              style = "font-size:11px; font-weight:650;")),
                        class = "gi-modal-team",
                        `data-team` = tolower(d$team_name[i]),
                        title = paste("Choose", d$team_name[i]))
                    })
                  )
                )
              }),
              div(
                class = "gi-team-empty", role = "status",
                `aria-live` = "polite")
            ),
            actionLink(
              "skip_team_pick", "Skip — I'll choose from the global filters",
              style = "display:inline-block; margin-top:14px;
                       color:#667085; font-size:13px;")
          )
        ))
      }
    })
  })
  observeEvent(input$skip_team_pick, removeModal())
  ## pick_ handlers pair 1:1 with the modal grid above -- iterate the same
  ## onboarded set so no observer is wired to a button that never renders.
  lapply(seq_len(nrow(DISPLAY_CONFIG)), function(i) {
    slug <- DISPLAY_CONFIG$slug[i]
    observeEvent(input[[paste0("pick_", gsub("-", "_", slug))]], {
      updateSelectInput(session, "g_team", selected = slug)
      removeModal()
    })
  })
  ## persist every team change to the device
  observeEvent(input$g_team, {
    session$sendCustomMessage("saveTeam", input$g_team)
  })

  ## info buttons -> sources & methods modals. A body may be a plain string or
  ## a function(conf_lab, conf_n) -- the latter names the DATA universe, so it
  ## is resolved against the ACTIVE team's conference at open time (Phase 0:
  ## "Big 12" / 16, so the text is unchanged).
  lapply(names(INFO_MODALS), function(id) {
    observeEvent(input[[id]], {
      body <- INFO_MODALS[[id]]$body
      if (is.function(body)) body <- body(active_conf_lab(), active_conf_n())
      showModal(modalDialog(
        title = INFO_MODALS[[id]]$title,
        HTML(body),
        easyClose = TRUE, footer = modalButton("Got it")
      ))
    })
  })

  ## "About the data" body: the scraped universe = the active conference's
  ## members. Its label + count route through conf_label()/conf_slugs() so the
  ## page can never claim a stale team count or the wrong league. Phase 0 renders
  ## "all 16 Big 12 programs" / "quarter of Big 12 signees", unchanged.
  output$about_data <- renderUI({
    HTML(paste0("
                    <p style='font-size:13px; color:#777; border-left: 3px solid
                      #FFD200; padding-left: 10px;'>
                      Everything here depends on what programs report and what
                      247Sports lists — heights, weights, ratings, and rosters
                      are best-available numbers, not certified measurements.
                      Treat small differences between teams accordingly.</p>
                    <ul style='font-size:14px; line-height:1.7;'>
                      <li><strong>Recruiting classes</strong>: 247Sports commit
                        lists, classes from 2016 on, all ", active_conf_n(), " ", active_conf_lab(), " programs,
                        football and basketball. Portal transfers are included
                        from 2021 on; the 'Players' control switches between
                        HS commits, commits + transfers, or transfers only.
                        Every chart caption states which pool it shows.</li>
                      <li><strong>Current rosters</strong>: 247Sports team
                        roster pages, refreshed nightly",
                        if (!is.null(last_refresh_label))
                          paste0(" (latest source capture ", last_refresh_label, ")"),
                        ".</li>
                      <li><strong>Season records and SP+</strong>:
                        CollegeFootballData.com, seasons 2016–2025 (football).</li>
                      <li><strong>Sizes</strong>: heights and weights are as
                        listed. Recruiting heights run optimistic — about a
                        quarter of ", active_conf_lab(), " signees are listed shorter on the
                        roster than they were as recruits (Weight Room →
                        Reality Check). Treat any listed height as ±1 inch.</li>
                      <li><strong>Listed origins</strong>: the location attached
                        to each recruiting record is geocoded and checked against
                        its listed state before it appears on the map. It is a
                        recruiting origin, not necessarily a birthplace or
                        hometown. Recent transfers join distance views when a
                        profile exposes a usable origin; otherwise they remain
                        visibly unmapped.</li>
                      <li><strong>Blue chips (90+)</strong>: counted from the
                        rating on 247's team pages. 247's Composite runs about
                        a point lower for borderline players, so counts can
                        differ by 1–2 per class depending on which you use.</li>
                      <li><strong>Coach eras</strong>: a class belongs to the
                        staff that ran its signing window. Mid-cycle changes
                        are judgment calls; edit <code>R/coach_eras.R</code>
                        to disagree.</li>
                      <li><strong>Refreshing</strong>: ratings re-rank and late
                        commits land all cycle. Re-run
                        <code>scripts/refreshClassYear.R</code> for the active
                        class, <code>scripts/scrapeRosters.R</code> for rosters,
                        and <code>scripts/fetchOutcomes.R</code> after each
                        season.</li>
                    </ul>"))
  })
  outputOptions(output, "about_data", suspendWhenHidden = FALSE)

  ## The collapsed bar's one-line summary of every global setting.
  ## Logos are decorative because the adjacent text carries the team name.
  output$cb_summary <- renderUI({
    req(input$g_team, input$g_years)
    if (identical(input$tabs, "origins")) {
      open_scope <- if (input$g_years[2] > as.integer(format(Sys.Date(), "%Y")))
        glue(" ({input$g_years[2]} open)") else ""
      return(tags$span(
        class = "cb-summary-text cb-summary-origin",
        shiny::span(class = "cb-origin-mark", icon("location-dot")),
        strong("Power-4 talent origins"),
        tags$span(
          class = "cb-dim",
          glue("\u00b7 {str_to_title(g_sport())} \u00b7 ",
               "{input$g_years[1]}-{input$g_years[2]}{open_scope} \u00b7 ",
               "{nrow(DISPLAY_CONFIG)} destinations \u00b7 HS/prep only")
        ),
        if (!is.null(last_refresh_label))
          tags$span(class = "cb-dim cb-updated",
                    glue("\u00b7 source capture {last_refresh_label}"))
      ))
    }
    logo <- function(slug) {
      img(src = TEAM_CONFIG$logo[match(slug, TEAM_CONFIG$slug)], alt = "")
    }
    type_word <- switch(input$g_type %||% "both",
                        commit = "HS commits",
                        transfer = "transfers only",
                        "commits + transfers")
    cmp_ctx <- comparison_context(input$g_team, g_cmp())
    tags$span(
      class = "cb-summary-text",
      logo(input$g_team), team_label(input$g_team),
      if (!is.null(g_cmp())) tags$span(class = "cb-dim", "vs"),
      if (!is.null(g_cmp())) logo(g_cmp()),
      if (!is.null(g_cmp())) team_label(g_cmp()),
      if (isTRUE(cmp_ctx$cross_conference)) tags$span(
        class = "cb-dim cb-compare-context",
        glue("· {cmp_ctx$compare_conference} reference")),
      tags$span(
        class = "cb-dim",
        glue("· {str_to_title(g_sport())} · ",
             "{input$g_years[1]}–{input$g_years[2]} · {type_word} · ",
             "{active_conf_lab()} scope ({active_conf_n()})")),
      ## Freshness badge: a startup constant; absent when no stamp exists.
      if (!is.null(last_refresh_label))
        tags$span(class = "cb-dim cb-updated",
                  glue("· source capture {last_refresh_label}"))
    )
  })

  ## ---- global reactives ------------------------------------------------------
  g_sport <- reactive(tolower(input$g_sport))

  ## compare slug or NULL (none / same as main team)
  g_cmp <- reactive({
    if (is.null(input$g_compare) || input$g_compare == "" ||
        input$g_compare == input$g_team) NULL else input$g_compare
  })

  ## ---- CONFERENCE SCOPE ----------------------------------------------------
  ## Program-specific boards, ranks, and medians use the active team's current
  ## conference. Conference Lab owns the explicit cross-league comparison.
  active_conf     <- reactive(team_conference(input$g_team %||% "arizona"))
  active_conf_lab <- reactive(conf_label(active_conf()))
  ## Pool live members only: complete conference fields today, safe partial
  ## loads if the configuration grows again later.
  conf_pool_slugs <- reactive(onboarded_slugs(active_conf()))
  active_conf_n   <- reactive(length(conf_pool_slugs()))

  ## A compact editorial orientation layer for every analytical destination.
  ## Home owns the product H1, so it intentionally renders no duplicate intro.
  output$page_intro <- renderUI({
    tab <- input$tabs %||% "home"
    if (identical(tab, "home")) return(NULL)
    req(input$g_team, input$g_years, input$g_sport)

    meta <- switch(
      tab,
      sizelab = list(
        eyebrow = "BODY PROFILE", title = "Size Lab",
        body = "Map every addition by height and weight, then compare the selected program with its conference peers."),
      compare = list(
        eyebrow = "DIRECT POWER-4 COMPARISON", title = "Matchup",
        body = "Put two programs side by side with raw recruiting facts, then read each one in its own conference context."),
      beef = list(
        eyebrow = "CONFERENCE BOARD", title = "Conference Beef",
        body = "Rank team body profiles by position, source, and an explicitly labeled recruiting window."),
      conflab = list(
        eyebrow = "POWER-4 DISTRIBUTIONS", title = "Conference Lab",
        body = "Compare the four leagues on recruiting inputs only — distributions first, with geography and strategy caveats intact."),
      weightroom = list(
        eyebrow = "PLAYER DEVELOPMENT", title = "Weight Room",
        body = "Trace listed recruit bodies to current rosters and separate real development signals from measurement noise."),
      eras = list(
        eyebrow = "STAFF IDENTITY", title = "Coach Eras",
        body = "See how each staff changes rating, size, geography, and position strategy across signing cycles."),
      brief = list(
        eyebrow = "ROSTER CONSTRUCTION", title = "Defensive War Room",
        body = "Audit 3-3-5 personnel fit, incoming bodies, retention, and in-state HS recruiting. Football only."),
      results = list(
        eyebrow = "OUTCOMES", title = "Talent vs Results",
        body = "Compare accumulated recruiting talent with wins, SP+, and wins above the conference talent curve. Football only."),
      origins = list(
        eyebrow = "HIGH SCHOOL PIPELINES", title = "Talent Origins",
        body = "See which last-listed HS/prep locations supply captured Power-4 signees, where position hotbeds form, and how those pipelines change."),
      summary = list(
        eyebrow = "PROGRAM GEOGRAPHY", title = "Program Reach",
        body = "Follow a program's mapped recruiting footprint, travel distance, and position-by-position reach without mixing it into the national origin story."),
      notes = list(
        eyebrow = "TRANSPARENCY", title = "Data & Notes",
        body = "Review sources, refresh timing, metric definitions, known limits, and the product roadmap."),
      list(
        eyebrow = "POWER-4 GIRTH INDEX", title = "Explore",
        body = "Recruiting body profiles, development, geography, and results across the Power 4.")
    )

    scope <- if (identical(tab, "conflab")) {
      glue("{nrow(DISPLAY_CONFIG)} programs · four conferences · ",
           "{input$g_years[1]}–{input$g_years[2]} · {players_lab()}")
    } else if (identical(tab, "origins")) {
      open_scope <- if (input$g_years[2] > as.integer(format(Sys.Date(), "%Y")))
        glue(" ({input$g_years[2]} open)") else ""
      glue("{nrow(DISPLAY_CONFIG)} destinations \u00b7 {str_to_title(g_sport())} \u00b7 ",
           "{input$g_years[1]}-{input$g_years[2]}{open_scope} \u00b7 HS/prep only")
    } else if (identical(tab, "summary")) {
      cmp_scope <- if (!is.null(g_cmp()))
        glue(" vs {team_label(g_cmp())}") else ""
      glue("{team_label(input$g_team)}{cmp_scope} · {str_to_title(g_sport())} · ",
           "{input$g_years[1]}–{input$g_years[2]} · {players_lab()}")
    } else if (identical(tab, "notes")) {
    } else if (identical(tab, "compare")) {
      cmp <- g_cmp()
      if (is.null(cmp)) {
        glue("{team_label(input$g_team)} · choose any Power-4 program · ",
             "{str_to_title(g_sport())} · {input$g_years[1]}–{input$g_years[2]}")
      } else {
        ctx <- comparison_context(input$g_team, cmp)
        glue("{ctx$team_name} · {ctx$team_conference} vs ",
             "{ctx$compare_name} · {ctx$compare_conference} · ",
             "{str_to_title(g_sport())} · {input$g_years[1]}–{input$g_years[2]}")
      }
      paste0("Power-4 coverage",
             if (!is.null(last_refresh_label))
               paste0(" · updated ", last_refresh_label) else "")
    } else {
      glue("{team_label(input$g_team)} · {active_conf_lab()} scope ",
           "({active_conf_n()}) · {str_to_title(g_sport())} · ",
           "{input$g_years[1]}–{input$g_years[2]}")
    }

    div(
      class = "gi-page-intro",
      div(
        div(class = "gi-page-intro__eyebrow", meta$eyebrow),
        h1(meta$title),
        p(meta$body)
      ),
      div(class = "gi-page-intro__scope",
          `aria-label` = paste("Current analysis scope:", scope), scope)
    )
  })
  outputOptions(output, "page_intro", suspendWhenHidden = FALSE)

  ## full prepped table for the current sport, filtered by the player-type
  ## radio (portal transfers exist for refreshed years: 2021+ after back-fill)
  size_all <- reactive({
    d <- if (g_sport() == "football") size_football else size_basketball
    ## NULL-fallback must match the radio's startup default ("both") -- a
    ## "commit" fallback here would silently disagree with every caption
    switch(input$g_type %||% "both",
           commit = dplyr::filter(d, Type == "Commit"),
           transfer = dplyr::filter(d, Type == "Transfer"),
           d)
  })

  ## windowed by the global year range
  ## debounced year window: dragging the slider fires once it settles
  ## instead of queueing a full re-render per notch on the 1-core worker
  g_years_d <- debounce(reactive({
    req(input$g_years)
    input$g_years
  }), 450)

  size_window <- reactive({
    yrs <- g_years_d()
    size_all() %>%
      filter(Year >= yrs[1], Year <= yrs[2])
  })

  ## Raw-origin universe for geography. Talent Origins applies its own
  ## high-confidence HS/prep classifier + athlete dedupe; Program Reach keeps
  ## the selected commitment/transfer record pool.
  origin_all <- reactive({
    if (g_sport() == "football") origin_football else origin_basketball
  })

  origin_window <- reactive({
    yrs <- g_years_d()
    origin_all() %>% dplyr::filter(Year >= yrs[1], Year <= yrs[2])
  })

  origin_pool_r <- reactive({
    origin_talent_pool(origin_all(), years = g_years_d(), us_only = TRUE)
  })

  reach_window <- reactive({
    d <- origin_window()
    switch(input$g_type %||% "both",
           commit = dplyr::filter(d, Type == "Commit"),
           transfer = dplyr::filter(d, Type == "Transfer"),
           d)
  })

  ## Stable Home snapshot: lead with the class arriving this season. If the
  ## selected window contains only a future cycle, preserve that selection but
  ## label it as open. Talent/count fields come from raw recruiting records;
  ## body fields retain the measurement-qualified Size Lab contract.
  home_snapshot <- reactive({
    req(input$g_team)
    raw <- reach_window() %>%
      dplyr::filter(School == input$g_team) %>%
      dplyr::distinct(School, AthleteKey, Year, Type, .keep_all = TRUE)
    years <- raw$Year[is.finite(raw$Year)]
    if (!nrow(raw) || !length(years)) return(NULL)
    eligible <- years[years <= arriving_class]
    snap_year <- if (length(eligible)) max(eligible) else max(years)
    cls <- raw %>% dplyr::filter(Year == snap_year)
    prior <- raw %>% dplyr::filter(Year %in% (snap_year - 3L):(snap_year - 1L))
    body <- class_snapshot(size_window(), input$g_team, snap_year)

    rating <- cls$RatingClean[is.finite(cls$RatingClean)]
    prior_rating <- prior$RatingClean[is.finite(prior$RatingClean)]
    avg_rating <- if (length(rating)) round(mean(rating), 1) else NA_real_
    d_rating <- if (length(rating) && length(prior_rating)) {
      round(mean(rating) - mean(prior_rating), 1)
    } else {
      NA_real_
    }
    top <- if (length(rating)) {
      cls[which.max(ifelse(is.finite(cls$RatingClean),
                           cls$RatingClean, -Inf)), , drop = FALSE]
    } else {
      cls[0, , drop = FALSE]
    }
    state_ok <- !is.na(cls$StateClean) & nzchar(cls$StateClean)
    pct_instate <- if (any(state_ok)) {
      round(100 * mean(cls$StateClean[state_ok] == team_state(input$g_team)))
    } else {
      NA_real_
    }

    list(
      year = as.integer(snap_year),
      is_open = snap_year > arriving_class,
      n = nrow(cls),
      body_n = if (is.null(body)) 0L else body$n,
      rated_n = length(rating),
      origin_n = sum(state_ok),
      avg_rating = avg_rating,
      d_rating = d_rating,
      blue = sum(rating >= BLUE_CHIP),
      pct_instate = pct_instate,
      avg_weight = if (is.null(body)) NA_real_ else body$avg_weight,
      d_weight = if (is.null(body)) NA_real_ else body$d_weight,
      avg_height = if (is.null(body)) NA_character_ else body$avg_height,
      top_name = if (nrow(top)) as.character(top$Name[[1]]) else "—",
      top_pos = if (nrow(top)) as.character(top$Position[[1]]) else "—",
      top_rating = if (nrow(top)) top$RatingClean[[1]] else NA_real_
    )
  })

  ## current-roster view prepped to the same shape (Conference Beef source)
  roster_size <- reactive({
    req(!is.null(roster_now()))
    prep_roster_size(roster_now(), g_sport())
  })

  ## current team rows in the window
  team_rows <- reactive({
    req(input$g_team)
    size_window() %>% filter(School == input$g_team)
  })

  ## roster + weight-room join for the current sport
  roster_now <- reactive({
    d <- if (g_sport() == "football") roster_football else roster_basketball
    if (is.null(d) || !nrow(d) || !"RosterYear" %in% names(d)) return(d)
    years <- suppressWarnings(as.integer(d$RosterYear))
    active_year <- if (any(is.finite(years))) max(years, na.rm = TRUE) else NA_integer_
    if (is.finite(active_year)) {
      d <- d[is.finite(years) & years == active_year, , drop = FALSE]
    }
    d
  })
  ## Weight Room is defined as HS-signee development, so commits only
  wr_data_r <- reactive({
    req(!is.null(roster_now()))
    weight_room_data(size_window() %>% dplyr::filter(Type == "Commit"),
                     roster_now())
  })

  ## one-line label of the current player pool, stamped onto chart captions
  players_lab <- reactive({
    switch(input$g_type %||% "both",
           commit = "HS commits only",
           transfer = "portal transfers only",
           "HS commits + portal transfers")
  })

  ## conference-wide median development gain per position group (football),
  ## used to slot incoming bodies at PROJECTED weights in the War Room
  proj_gain_r <- reactive({
    if (g_sport() != "football" || is.null(roster_now())) return(NULL)
    wr_all <- weight_room_data(
      size_football %>% dplyr::filter(Type == "Commit"), roster_now())
    if (nrow(wr_all) == 0) return(NULL)
    tapply(wr_all$WeightGain, as.character(wr_all$PosGroup),
           median, na.rm = TRUE)
  })

  ## keep position-group filter in sync with the sport
  observeEvent(input$g_sport, {
    updateSelectInput(session, "size_pos",
                      choices = as.list(pos_choices(input$g_sport)),
                      selected = "All")
    origin_choices <- origin_position_choices(input$g_sport)
    origin_selected <- url_init$origin_pos %||% isolate(input$origin_pos) %||% "All"
    if (!origin_selected %in% unname(origin_choices)) origin_selected <- "All"
    updateSelectInput(session, "origin_pos", choices = origin_choices,
                      selected = origin_selected)
    url_init$origin_pos <- NULL
  })

  ## The same metric IDs persist in shared links, while the visible labels
  ## state the actual board-vs-trend denominator.
  observeEvent(input$origin_view, {
    context <- if (identical(input$origin_view, "trend")) "trend" else "board"
    choices <- origin_metric_choices(context)
    selected <- isolate(input$origin_metric) %||% "blue_n"
    if (!selected %in% unname(choices)) selected <- "blue_n"
    updateSelectInput(session, "origin_metric", choices = choices,
                      selected = selected)
  })

  ## ---- navigation -------------------------------------------------------------
  ## The persistent Home browser changes the active lens in place. This lets a
  ## visitor compare several programs without being ejected into a chart tab.
  lapply(seq_len(nrow(DISPLAY_CONFIG)), function(i) {
    slug <- DISPLAY_CONFIG$slug[i]
    btn_id <- paste0("select_", gsub("-", "_", slug))
    observeEvent(input[[btn_id]], {
      updateSelectInput(session, "g_team", selected = slug)
    })
  })

  ## Cover and task-path buttons.
  observeEvent(input$cover_sizelab, updateTabItems(session, "tabs", "sizelab"))
  observeEvent(input$cover_conflab, updateTabItems(session, "tabs", "conflab"))
  observeEvent(input$go_compare, updateTabItems(session, "tabs", "compare"))
  observeEvent(input$go_compare_sizelab, updateTabItems(session, "tabs", "sizelab"))
  observeEvent(input$go_compare_reach, updateTabItems(session, "tabs", "summary"))
  observeEvent(input$go_sizelab, updateTabItems(session, "tabs", "sizelab"))
  observeEvent(input$go_beef, updateTabItems(session, "tabs", "beef"))
  observeEvent(input$go_conflab, updateTabItems(session, "tabs", "conflab"))
  observeEvent(input$go_wr, updateTabItems(session, "tabs", "weightroom"))
  observeEvent(input$go_eras, updateTabItems(session, "tabs", "eras"))
  observeEvent(input$go_warroom, updateTabItems(session, "tabs", "brief"))
  observeEvent(input$go_results, updateTabItems(session, "tabs", "results"))
  observeEvent(input$go_origins, updateTabItems(session, "tabs", "origins"))

  ## year-window quick presets
  observeEvent(input$preset_all, {
    updateSliderInput(session, "g_years", value = SIZE_YEARS)
  })
  observeEvent(input$preset_recent, {
    ## the 4-class roster window (the default)
    updateSliderInput(session, "g_years", value = DEFAULT_YEARS)
  })
  observeEvent(input$preset_now, {
    updateSliderInput(session, "g_years",
                      value = c(SIZE_YEARS[2], SIZE_YEARS[2]))
  })

  ## ---- HOME ----------------------------------------------------------------------
  ## The selected program is the active lens, not a second product title.
  output$hero_team <- renderUI({
    req(input$g_team, input$g_years)
    div(
      class = "gi-team-lens",
      div(
        class = "gi-team-lens__logo",
        img(src = TEAM_CONFIG$logo[match(input$g_team, TEAM_CONFIG$slug)],
            alt = "")
      ),
      div(
        div(class = "gi-team-lens__conference", active_conf_lab()),
        h2(team_label(input$g_team)),
        p(glue("{str_to_title(g_sport())} · ",
               "{input$g_years[1]}–{input$g_years[2]} · {players_lab()}"))
      )
    )
  })

  output$hero_tagline <- renderUI({
    p(
      class = "gi-cover__lede",
      glue("Recruiting size, talent, geography, development, and results for ",
           "all {nrow(DISPLAY_CONFIG)} Power-4 programs — viewed through ",
           "{team_label(input$g_team)} and benchmarked on an honest ",
           "{active_conf_lab()} reference field.")
    )
  })

  output$home_data_pulse <- renderUI({
    fresh <- dashboard_freshness_info(refresh_meta)
    pipeline <- dashboard_pipeline_info(refresh_meta)
    sources <- refresh_meta$sources
    sp <- g_sport()
    recruit <- sources$recruiting[[sp]]
    roster <- sources$rosters[[sp]]
    total_programs <- nrow(DISPLAY_CONFIG)
    open <- origin_all() %>%
      dplyr::filter(Year == CYCLE_CAP, Type == "Commit") %>%
      dplyr::distinct(School, AthleteKey, .keep_all = TRUE)
    open_programs <- dplyr::n_distinct(open$School[!is.na(open$School)])
    date_lab <- function(x) {
      if (is.null(x) || !length(x) || is.na(x[[1]])) return("Unknown")
      sub(" 0", " ", format(as.Date(x[[1]]), "%b %d"), fixed = TRUE)
    }
    pulse_item <- function(label, value, detail, meta, color, state = "") {
      div(
        class = paste("gi-coverage__item gi-pulse__item",
                      if (nzchar(state)) paste0("gi-pulse__item--", state)),
        role = "listitem", style = paste0("--conf-color:", color),
        shiny::span(class = "gi-pulse__label", label),
        strong(value),
        shiny::span(class = "gi-pulse__detail", detail),
        tags$small(class = "gi-pulse__meta", meta)
      )
    }
    outcome_year <- sources$outcomes$year
    outcome_meta <- if (is.finite(outcome_year)) {
      paste("Results through", outcome_year)
    } else {
      "Results status unavailable"
    }

    tagList(
      div(
        class = "gi-coverage gi-data-pulse", role = "list",
        "aria-label" = fresh$aria,
        pulse_item("Dataset snapshot", fresh$value, fresh$detail,
                   outcome_meta, fresh$color, fresh$state),
        pulse_item("Power-4 coverage", total_programs,
                   "programs across four conferences",
                   paste(length(conf_order()), "conference lenses"),
                   "#0C234B"),
        pulse_item(
          paste(str_to_title(sp), "sources"),
          paste0(recruit$teams, "/", total_programs),
          "recruiting programs captured",
          paste0("Captured ", date_lab(recruit$date), " · rosters ",
                 roster$teams, "/", total_programs, " (",
                 ifelse(is.finite(roster$year), roster$year, "year n/a"),
                 ", ", date_lab(roster$date), ")"),
          "#0072B2"),
        pulse_item(
          paste("Class of", CYCLE_CAP),
          paste0(open_programs, "/", total_programs),
          "programs reporting · open cycle",
          paste(format(nrow(open), big.mark = ","), "HS commitments captured"),
          if (open_programs == total_programs) "#1F7A4D" else "#D97706",
          if (open_programs == total_programs) "fresh" else "warning")
      ),
      div(
        class = paste("gi-data-note",
                      paste0("gi-data-note--", pipeline$state)),
        role = "status", "aria-live" = "polite",
        icon("circle-info"),
        shiny::span(strong(pipeline$label), paste0(". ", pipeline$detail))
      )
    )
  })

  ## Sport-aware task cards keep football-only analysis out of the basketball
  ## journey while preserving the same six-question information architecture.
  output$home_paths <- renderUI({
    path_button <- function(id, icon_name, title, detail) {
      actionButton(
        id,
        label = tagList(
          shiny::span(class = "gi-path__icon", icon(icon_name)),
          shiny::span(
            class = "gi-path__copy",
            strong(title),
            shiny::span(detail)
          ),
          shiny::span(class = "gi-path__arrow", icon("arrow-right"))
        ),
        class = "gi-path"
      )
    }

    fifth <- if (identical(g_sport(), "football")) {
      path_button(
        "go_results", "trophy", "Test talent against results",
        "Expected wins, actual wins, SP+, and wins above talent."
      )
    } else {
      path_button(
        "go_eras", "user-tie", "See the coach imprint",
        "How each staff changes size, talent, and recruiting geography."
      )
    }

    div(
      class = "gi-paths",
      path_button(
        "go_sizelab", "ruler-combined", "Map every body",
        "Height, weight, position DNA, and every recruit behind the averages."
      ),
      path_button(
        "go_conflab", "layer-group", "Compare the Power 4",
        "Distribution-first league comparisons with honest guardrails."
      ),
      path_button(
        "go_beef", "dumbbell", "Rank program size",
        "Conference boards by body metric, position group, and source."
      ),
      path_button(
        "go_wr", "weight-hanging", "Measure development",
        "From listed recruiting body to the current roster."
      ),
      fifth,
      path_button(
        "go_origins", "map-location-dot", "Find where talent starts",
        "State hotbeds, position pipelines, and how sources change over time."
      )
    )
  })

  output$home_fingerprint <- renderUI({
    req(input$g_team, input$g_years)
    yrs <- g_years_d()
    open_scope <- if (yrs[2] > arriving_class) {
      paste0(" · ", yrs[2], " open")
    } else {
      ""
    }
    home_program_fingerprint(
      size_data = size_window(),
      team_slug = input$g_team,
      compare_slug = g_cmp(),
      conf_slugs = conf_pool_slugs(),
      sport = g_sport(),
      sport_label = str_to_title(g_sport()),
      window_label = glue("{yrs[1]}–{yrs[2]}{open_scope} · {players_lab()}"),
      conference_label = active_conf_lab(),
      min_team_n = if (g_sport() == "football") 8L else 5L,
      subgroup_min_n = if (g_sport() == "football") 5L else 3L
    )
  })

  ## A comparison has a dedicated, small-screen-friendly surface. Raw values
  ## can be compared across Power-4 programs; percentiles intentionally use
  ## each program's own conference instead of manufacturing a blended ranking.
  matchup_context_r <- reactive({
    req(input$g_team)
    compare_slug <- g_cmp()
    if (is.null(compare_slug)) return(NULL)
    comparison_context(input$g_team, compare_slug)
  })

  matchup_card <- function(cta_id = NULL, cta_label = "Open full comparison") {
    ctx <- matchup_context_r()
    if (is.null(ctx)) return(NULL)
    yrs <- g_years_d()
    open_scope <- if (yrs[2] > arriving_class) paste0(" · ", yrs[2], " open") else ""
    home_program_matchup(
      size_data = size_window(),
      team_slug = ctx$team_slug,
      compare_slug = ctx$compare_slug,
      team_conf_slugs = conf_pool_slugs(),
      compare_conf_slugs = onboarded_slugs(ctx$compare_conference),
      sport = g_sport(),
      sport_label = str_to_title(g_sport()),
      window_label = glue("{yrs[1]}–{yrs[2]}{open_scope} · {players_lab()}"),
      min_team_n = if (g_sport() == "football") 8L else 5L,
      subgroup_min_n = if (g_sport() == "football") 5L else 3L,
      cta_id = cta_id,
      cta_label = cta_label
    )
  }

  output$home_matchup <- renderUI({
    matchup_card(cta_id = "go_compare")
  })

  output$matchup_empty <- renderUI({
    req(input$g_team)
    if (!is.null(g_cmp())) return(NULL)
    div(
      class = "gi-matchup-empty",
      div(class = "gi-matchup__eyebrow", "Direct Power-4 comparison"),
      h2("Put another program beside ", team_label(input$g_team)),
      p("Use the Compare to picker in the top bar to open a focused matchup. You can choose any onboarded Power-4 program — conference rival or not."),
      tags$span(icon("arrow-up-right-from-square"), " Try Arizona vs Alabama to see an honest Big 12–SEC comparison.")
    )
  })

  output$matchup_context <- renderUI({
    ctx <- matchup_context_r()
    if (is.null(ctx)) return(NULL)
    relationship <- if (isTRUE(ctx$cross_conference)) {
      paste0(ctx$compare_name, " is an ", ctx$compare_conference,
             " reference. It is never mixed into ", ctx$team_conference,
             " ranks, averages, or outcome calibration.")
    } else {
      paste0("Both programs are ", ctx$team_conference,
             " peers, so the board and percentile context share the same conference pool.")
    }
    div(
      class = "gi-comparison-context",
      icon("scale-balanced"),
      div(
        tags$strong(paste0(ctx$team_name, " · ", ctx$team_conference,
                           "  vs  ", ctx$compare_name, " · ", ctx$compare_conference)),
        tags$span(relationship)
      )
    )
  })

  output$matchup_scorecard <- renderUI({
    matchup_card()
  })
  ## current-status boxes for the SELECTED team (season-proof: everything is
  ## derived from the window + max class year, never hardcoded)
  output$vb_home_rank <- renderValueBox({
    board_all <- team_size_summary(size_window() %>%
                                     filter(School %in% conf_pool_slugs()))
    selected_n <- board_all$Players[match(input$g_team, board_all$School)]
    selected_n <- if (length(selected_n)) selected_n[[1]] else 0L
    board <- board_all %>%
      filter(Players >= 8L, is.finite(AvgWeight)) %>%
      arrange(desc(AvgWeight))
    rk <- which(board$School == input$g_team)
    val <- if (length(rk) == 1) paste0("#", rk, " of ", nrow(board)) else "—"
    valueBox(val,
             glue("{input$g_years[1]}–{input$g_years[2]} addition weight rank ",
                  "· n={selected_n}{ifelse(selected_n < 8, ' · limited sample', '')}"),
             icon = icon("weight-hanging"), color = "navy")
  })
  output$vb_home_class <- renderValueBox({
    snap <- home_snapshot()
    if (is.null(snap)) {
      return(valueBox("—", "No players in this window",
                      icon = icon("star"), color = "light-blue"))
    }
    yr <- snap$year
    cls_all <- reach_window() %>%
      filter(Year == yr, School %in% conf_pool_slugs()) %>%
      distinct(School, AthleteKey, Year, Type, .keep_all = TRUE) %>%
      group_by(School) %>%
      summarize(
        rated_n = sum(is.finite(RatingClean)),
        r = ifelse(rated_n > 0, mean(RatingClean, na.rm = TRUE), NA_real_),
        .groups = "drop") %>%
      filter(rated_n > 0, is.finite(r)) %>%
      arrange(desc(r))
    reporting <- nrow(cls_all)
    selected_rated <- cls_all$rated_n[match(input$g_team, cls_all$School)]
    selected_rated <- if (length(selected_rated)) selected_rated[[1]] else 0L
    rank_ready <- !snap$is_open && selected_rated >= 3L &&
      reporting >= ceiling(0.75 * active_conf_n())
    rk <- if (rank_ready) which(cls_all$School == input$g_team) else integer(0)
    rank_lab <- if (length(rk) == 1L) {
      paste0(" · #", rk, " of ", reporting, " in ", active_conf_lab())
    } else if (snap$is_open) {
      paste0(" · provisional · ", reporting, "/", active_conf_n(), " reporting")
    } else {
      " · rank withheld (limited coverage)"
    }
    value <- if (is.finite(snap$avg_rating)) snap$avg_rating else "—"
    valueBox(value,
             glue("Class of {yr} {ifelse(snap$is_open, 'open', 'arriving')} ",
                  "avg · rated n={snap$rated_n}{rank_lab}"),
             icon = icon("star"), color = "light-blue")
  })
  output$vb_home_dev <- renderValueBox({
    if (is.null(roster_now())) {
      return(valueBox("—", "Roster development unavailable for this selection",
                      icon = icon("dumbbell"), color = "orange"))
    }
    gains_all <- wr_data_r() %>%
      filter(School %in% conf_pool_slugs()) %>%
      group_by(School) %>%
      summarize(g = mean(GainPerYr, na.rm = TRUE),
                n = sum(is.finite(GainPerYr)), .groups = "drop")
    selected_n <- gains_all$n[match(input$g_team, gains_all$School)]
    selected_n <- if (length(selected_n)) selected_n[[1]] else 0L
    gains <- gains_all %>%
      filter(n >= 5L, is.finite(g)) %>%
      arrange(desc(g))
    rk <- which(gains$School == input$g_team)
    val <- if (length(rk) == 1) {
      glue("+{round(gains$g[rk], 1)} lbs/yr")
    } else "—"
    valueBox(val,
             glue("Current-roster matched development · n={selected_n} ",
                  "{ifelse(length(rk) == 1, paste0('(#', rk, ' of ',
                  nrow(gains), ')'), '· limited sample')}"),
             icon = icon("dumbbell"), color = "orange")
  })

  ## Arriving-class snapshot; open-cycle coverage lives in the status rail.
  output$class_snap_title <- renderText({
    snap <- home_snapshot()
    if (is.null(snap)) return("Class snapshot")
    glue("{team_label(input$g_team)} Class of {snap$year} ",
         "{ifelse(snap$is_open, 'open-cycle watch', 'arriving-class snapshot')}")
  })
  output$class_snap_note <- renderUI({
    snap <- home_snapshot()
    if (is.null(snap)) return(NULL)
    if (snap$is_open) {
      ctx_note("This cycle is still open. Counts and ratings remain provisional;
                the dashboard withholds its conference rank.")
    } else {
      ctx_note(glue("This is the class arriving in {snap$year}. The Class of
                    {CYCLE_CAP} reporting rate is tracked in the status rail."))
    }
  })
  output$class_snap <- renderUI({
    req(input$g_team)
    snap <- home_snapshot()
    validate(need(!is.null(snap), "No commits in this window."))
    delta_html <- function(d, suffix = "") {
      if (is.na(d)) return("")
      cls <- if (d >= 0) "snap-delta-up" else "snap-delta-down"
      glue("<span class='{cls}'>{ifelse(d >= 0, '+', '')}{d}{suffix}</span>")
    }
    top_name <- htmltools::htmlEscape(snap$top_name)
    top_pos <- htmltools::htmlEscape(snap$top_pos)
    avg_rating <- if (is.finite(snap$avg_rating)) snap$avg_rating else "—"
    pct_instate <- if (is.finite(snap$pct_instate)) paste0(snap$pct_instate, "%") else "—"
    avg_height <- if (length(snap$avg_height) && !is.na(snap$avg_height)) {
      snap$avg_height
    } else {
      "—"
    }
    avg_weight <- if (is.finite(snap$avg_weight)) snap$avg_weight else "—"
    top_rating <- if (is.finite(snap$top_rating)) snap$top_rating else "—"
    HTML(glue(
      "<div class='row gi-snapshot-core'>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{snap$n}</div>",
      "<div class='lbl'>Listed additions</div></div>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{avg_rating}</div>",
      "<div class='lbl'>Avg rating · n={snap$rated_n} {delta_html(snap$d_rating)}</div></div>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{snap$blue}</div>",
      "<div class='lbl'>Blue-chips (90+)</div></div>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{pct_instate}</div>",
      "<div class='lbl'>Listed-origin in-state · n={snap$origin_n}</div></div>",
      "</div>",
      "<div class='row gi-snapshot-detail'>",
      "<div class='col-xs-4 snap-stat'><div class='num'>{avg_height}</div>",
      "<div class='lbl'>Body avg height · n={snap$body_n}</div></div>",
      "<div class='col-xs-4 snap-stat'><div class='num'>{avg_weight}</div>",
      "<div class='lbl'>Body avg lbs · n={snap$body_n} {delta_html(snap$d_weight)}</div></div>",
      "<div class='col-xs-4 snap-stat'><div class='num' style='font-size:16px; padding-top:8px;'>",
      "{top_name}</div>",
      "<div class='lbl'>Headliner ({top_pos}, {top_rating})</div></div>",
      "</div>"))
  })

  output$home_points_title <- renderText({
    glue("{team_label(input$g_team)} insights — ",
         "{str_to_title(g_sport())}, {input$g_years[1]}–{input$g_years[2]}")
  })
  output$home_points <- renderUI({
    req(input$g_team)
    ## the SCORED insights: top 3 by notability (magnitude x recency),
    ## each carrying the size of the pool that backs it (n) so the reader
    ## sees how much data stands behind the claim. make_talking_points()
    ## still drives the fuller Size Lab list unchanged.
    ri <- ranked_insights(size_window(), input$g_team, g_sport())
    validate(need(nrow(ri) > 0, "Not enough data in this window."))
    top <- utils::head(ri, 3)
    items <- paste0(
      "<li>", htmltools::htmlEscape(top$sentence),
      " <span class='insight-n'>n=", top$n, "</span></li>",
      collapse = "")
    HTML(paste0("<ul class='talking-points'>", items, "</ul>"))
  })

  ## ---- WHAT CHANGED SINCE THE LAST VISIT -----------------------------------
  ## The device stores a compact class snapshot (localStorage key
  ## 'gi_snapshot', written by the saveSnapshot observer below). On the next
  ## visit the stored copy comes back once through input$stored_snapshot,
  ## gets validated HARD (forged localStorage must neither error the app nor
  ## inject HTML -- every field is type-checked and only validated numbers
  ## ever surface), and the Home strip compares it against TODAY'S
  ## recomputed numbers under the same team + sport + player pool.
  prior_snap <- reactiveVal(NULL)
  observeEvent(input$stored_snapshot, once = TRUE, {
    s <- input$stored_snapshot
    ok <- tryCatch({
      chr1 <- function(x) is.character(x) && length(x) == 1 &&
        !is.na(x) && nzchar(x)
      num1 <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
      is.list(s) &&
        all(c("team", "sport", "pool", "year", "n", "blue", "avg",
              "visited") %in% names(s)) &&
        chr1(s$team) && s$team %in% TEAM_CONFIG$slug &&
        chr1(s$sport) && s$sport %in% c("football", "basketball") &&
        chr1(s$pool) && s$pool %in% c("commit", "both", "transfer") &&
        num1(s$year) && num1(s$n) && num1(s$blue) && num1(s$avg) &&
        chr1(s$visited) && !is.na(as.Date(s$visited))
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      prior_snap(list(
        team = s$team, sport = s$sport, pool = s$pool,
        year = as.integer(s$year), n = as.integer(s$n),
        blue = as.integer(s$blue), avg = round(as.numeric(s$avg), 1),
        visited = as.Date(s$visited)))
    }
  })

  ## keep the device's snapshot current under whatever settings are active
  ## (the strip compares against the copy captured BEFORE these overwrites)
  observe({
    req(input$g_team)
    snap <- tryCatch(home_snapshot(),
                     error = function(e) NULL)
    if (is.null(snap) || !is.finite(snap$avg_rating)) return(invisible(NULL))
    session$sendCustomMessage("saveSnapshot", list(
      team = input$g_team, sport = g_sport(),
      pool = input$g_type %||% "both",
      year = snap$year, n = snap$n, blue = snap$blue,
      avg = snap$avg_rating, visited = as.character(Sys.Date())))
  })

  output$last_visit_strip <- renderUI({
    p <- prior_snap()
    if (is.null(p)) return(NULL)
    req(input$g_team)
    ## only compare like with like: same team, sport, and player pool,
    ## stored on an earlier day, about the same class year
    if (!identical(p$team, input$g_team) ||
        !identical(p$sport, g_sport()) ||
        !identical(p$pool, input$g_type %||% "both")) return(NULL)
    if (!isTRUE(p$visited < Sys.Date())) return(NULL)
    snap <- tryCatch(home_snapshot(),
                     error = function(e) NULL)
    if (is.null(snap) ||
        !identical(as.integer(snap$year), p$year)) return(NULL)
    parts <- character(0)
    ## before/after, never "additions": the delta is NET (a +1 can be two
    ## commits and a decommit), so the honest wording is the size change
    if (snap$n != p$n) {
      parts <- c(parts, glue(
        "class size <b>{p$n} &rarr; {snap$n}</b>"))
    }
    if (is.finite(snap$avg_rating) &&
        abs(snap$avg_rating - p$avg) >= 0.05) {
      parts <- c(parts, glue(
        "class avg <b>{sprintf('%.1f', p$avg)} &rarr; ",
        "{sprintf('%.1f', snap$avg_rating)}</b>"))
    }
    if (snap$blue != p$blue) {
      parts <- c(parts, glue(
        "blue-chips <b>{p$blue} &rarr; {snap$blue}</b>"))
    }
    if (length(parts) == 0) return(NULL)
    date_lab <- sub(" 0", " ", format(p$visited, "%b %d"), fixed = TRUE)
    div(class = "lastvisit-strip",
        role = "status", "aria-live" = "polite",
        tags$span(class = "lv-lead",
                  glue("Since your last visit ({date_lab}):")),
        HTML(paste(parts, collapse = " &middot; ")),
        tags$span(class = "lv-pool",
                  glue("Class of {snap$year}, {players_lab()}")))
  })

  ## ---- SIZE LAB --------------------------------------------------------------------
  output$vb_height <- renderValueBox({
    rows <- team_rows()
    val <- if (nrow(rows) > 0) format_height(mean(rows$Height_in)) else "—"
    valueBox(val, glue("{team_label(input$g_team)} avg height"),
             icon = icon("ruler-vertical"), color = "light-blue")
  })
  output$vb_weight <- renderValueBox({
    rows <- team_rows()
    val <- if (nrow(rows) > 0) paste0(round(mean(rows$Weight), 0), " lbs") else "—"
    valueBox(val, glue("{team_label(input$g_team)} avg weight"),
             icon = icon("weight-hanging"), color = "navy")
  })
  output$vb_lbsin <- renderValueBox({
    rows <- team_rows()
    val <- if (nrow(rows) > 0) sprintf("%.2f", mean(rows$LbsPerInch)) else "—"
    valueBox(val, "Pounds per inch (girth index)",
             icon = icon("compress"), color = "orange")
  })
  output$vb_rank <- renderValueBox({
    ## beef rank within the active team's conference (Phase 0: all 16 members)
    board <- team_size_summary(size_window() %>%
                                 filter(School %in% conf_pool_slugs())) %>%
      arrange(desc(AvgWeight))
    rk <- which(board$School == input$g_team)
    val <- if (length(rk) == 1) paste0("#", rk, " of ", nrow(board)) else "—"
    valueBox(val, "Beef rank (by avg weight)",
             icon = icon("trophy"), color = "yellow")
  })

  ## position isolation buttons above the Body Map (per sport)
  observeEvent(input$g_sport, {
    groups <- setdiff(position_levels(input$g_sport), "Other")
    ## as.list: named vectors trip jsonlite's keep_vec_names warning on
    ## every update (named lists serialize cleanly)
    updateRadioButtons(session, "body_pos",
                       choices = as.list(c("All positions" = "All",
                                           setNames(groups, groups))),
                       selected = "All", inline = TRUE)
  })

  output$body_map <- renderGirafe({
    req(input$g_team)
    ## the default view ships precomputed -- first paint is a readRDS
    if (at_defaults(input$body_pos)) {
      key <- paste0("body_map_", ifelse(is_phone(), "phone", "desktop"))
      if (!is.null(PRE[[key]])) {
        message("serving precomputed ", key)
        return(PRE[[key]])
      }
    }
    validate(need(nrow(size_window()) > 0, "No commits in this window."))
    keep <- if (is.null(input$body_pos) || input$body_pos == "All") {
      NULL
    } else input$body_pos
    girafe_try(girafe_wrap(
      plot_body_map(size_window(), input$g_team, g_sport(), pos_keep = keep, players_note = players_lab(),
                    logo_path = file.path(
                      "www", TEAM_CONFIG$logo[match(input$g_team,
                                                    TEAM_CONFIG$slug)])),
      h = 7, name = png_name("body-map")), "body map")
  }) %>% bindCache(input$g_team, g_sport(), g_years_d(), input$g_type,
                   input$body_pos, (input$client_w %||% 1200) < 700)

  output$dna_plot <- renderGirafe({
    if (at_defaults(cmp_sensitive = TRUE)) {
      key <- paste0("dna_", ifelse(is_phone(), "phone", "desktop"))
      if (!is.null(PRE[[key]])) {
        message("serving precomputed ", key)
        return(PRE[[key]])
      }
    }
    validate(need(nrow(team_rows()) > 0,
                  "No commits for this team in this window."))
    girafe_try(girafe_wrap(
      plot_position_dna(size_window(), input$g_team, g_sport(),
                        compare_slug = g_cmp(), players_note = players_lab()),
      w = 9.5, h = 6, name = png_name("position-dna")), "position dna")
  }) %>% bindCache(input$g_team, g_sport(), g_years_d(), input$g_type,
                   g_cmp(), (input$client_w %||% 1200) < 700)

  output$talking_points <- renderUI({
    req(input$g_team)
    pts <- make_talking_points(size_window(), input$g_team, g_sport())
    validate(need(length(pts) > 0, "Not enough data in this window."))
    HTML(paste0("<ul class='talking-points'>",
                paste0("<li>", pts, "</li>", collapse = ""),
                "</ul>"))
  })

  ## ---- CONFERENCE BEEF ------------------------------------------------------------------
  ## "Commit classes" = the year window of signees; "Current roster" = the
  ## players actually on campus now (the closest available proxy for who
  ## plays; true starting lineups need usage data)
  beef_source_data <- reactive({
    if (input$size_source == "roster") {
      validate(need(!is.null(roster_now()),
                    "Run scripts/scrapeRosters.R to add current rosters."))
      roster_size()
    } else {
      size_window()
    }
  })
  beef_source_label <- reactive({
    if (input$size_source == "roster") {
      glue("current {str_to_title(g_sport())} rosters ",
           "({unique(roster_now()$RosterYear)[1]})")
    } else NULL
  })

  ## "<conf> Beef Board" -- the leaderboard is the active conference's, so the
  ## title tracks conf_label (Phase 0: "Big 12 Beef Board", unchanged). Matches
  ## the chart title in plot_beef_board and the twin-table caption below.
  output$beef_board_conf_title <- renderText(glue("{active_conf_lab()} Beef Board"))
  outputOptions(output, "beef_board_conf_title", suspendWhenHidden = FALSE)

  ## backcast-honesty note. Name/count/"whole" year all come from CONF_CONFIG so
  ## the caveat can't go stale. The anchor conference (shipped Big 12) keeps its
  ## hand-written realignment detail verbatim -- at Phase 0 this branch always
  ## runs and, with conf_lab "Big 12" / n 16 / whole 2024, reproduces the
  ## original note. Any later conference gets a generic (still honest) note
  ## instead of inheriting the Pac-12-four detail as a lie.
  output$beef_ctx_note <- renderUI({
    conf <- active_conf(); cl <- active_conf_lab(); n <- active_conf_n()
    whole <- CONF_CONFIG$conf_whole[match(conf, CONF_CONFIG$conf)]
    anchor <- CONF_CONFIG$conf[which.min(CONF_CONFIG$order)]
    note <- if (identical(conf, anchor)) {
      glue("'{cl}' means today's {n} members, applied retroactively — ",
           "Arizona, ASU, Colorado, and Utah joined in {whole}, so their ",
           "earlier classes were signed in the Pac-12.")
    } else {
      glue("'{cl}' means today's {n} members, applied retroactively — any ",
           "class a member signed before it joined (through {whole}) is a ",
           "backcast from that program's former league.")
    }
    ctx_note(note)
  })
  outputOptions(output, "beef_ctx_note", suspendWhenHidden = FALSE)

  output$beef_board <- renderGirafe({
    req(input$size_metric, input$size_pos, input$size_source)
    ## default Conference Beef view ships precomputed (AvgWeight, all
    ## positions, commit classes, vs ASU) -- cold-start first paint is a
    ## readRDS instead of a fragile ~3s SVG build
    if (at_defaults(cmp_sensitive = TRUE) &&
        identical(input$size_metric, "AvgWeight") &&
        identical(input$size_pos, "All") &&
        identical(input$size_source, "commits")) {
      key <- paste0("beef_board_", ifelse(is_phone(), "phone", "desktop"))
      if (!is.null(PRE[[key]])) {
        message("serving precomputed ", key)
        return(PRE[[key]])
      }
    }
    validate(need(nrow(filter_pos(beef_source_data(), input$size_pos)) > 0,
                  "No players for this position filter."))
    girafe_try(girafe_wrap(
      plot_beef_board(beef_source_data(), input$g_team, g_sport(),
                      metric = input$size_metric, pos_filter = input$size_pos,
                      compare_slug = g_cmp(),
                      source_label = beef_source_label(),
                      players_note = players_lab()),
      w = 8, h = 9,
      name = png_name(glue("beef-board-{input$size_source}"))), "beef board")
  }) %>% bindCache(input$g_team, g_sport(), g_years_d(), input$g_type,
                   g_cmp(), input$size_metric, input$size_pos,
                   input$size_source, (input$client_w %||% 1200) < 700)

  ## ---- CONFERENCE LAB: all four leagues, distribution-first -----------------
  ## conference-wide (not team-scoped): uses the global sport, year window, and
  ## Players toggle. The metric comes from CONF_COMPARE_POLICY via the selector,
  ## so a RED metric can never reach the builder.
  output$conf_spread <- renderGirafe({
    req(input$conf_metric)
    yr <- g_years_d()
    girafe_try(girafe_wrap(
      plot_conf_talent_spread(size_all(), metric = input$conf_metric,
                              year_min = yr[1], year_max = yr[2],
                              sport = g_sport(), type = input$g_type %||% "both",
                              highlight_team = input$g_team),
      w = 10.5, h = 6.5,
      name = png_name(glue("conference-lab-{tolower(input$conf_metric)}"))),
      "conference lab")
  }) %>% bindCache(g_sport(), input$conf_metric, g_years_d(),
                   input$g_type, input$g_team,
                   (input$client_w %||% 1200) < 700)

  ## amber caveat banner, shown only for a YELLOW context metric (in-state /
  ## portal share) — surfaces the "reads geography, not talent" warning above
  ## the chart, not just in the plot caption
  output$conf_caveat <- renderUI({
    req(input$conf_metric)
    pol <- CONF_COMPARE_POLICY[[input$conf_metric]]
    if (is.null(pol) || !identical(pol$tier, "YELLOW")) return(NULL)
    div(class = "gi-caveat",
        icon("triangle-exclamation"),
        tags$span(tags$strong(pol$label, " is a context metric. "), pol$caveat))
  })
  outputOptions(output, "conf_caveat", suspendWhenHidden = FALSE)

  output$conf_twin <- renderUI({
    req(isTRUE(input$twin_conf_spread), input$conf_metric)
    yr <- g_years_d()
    tbl <- conf_spread_table(size_all(), metric = input$conf_metric,
                             year_min = yr[1], year_max = yr[2],
                             type = input$g_type %||% "both")
    pol <- CONF_COMPARE_POLICY[[input$conf_metric]]
    cap_note <- paste0(
      str_to_title(g_sport()), ", ", yr[1], "–", yr[2], ". ",
      if (identical(pol$tier, "YELLOW"))
        "Context metric — reads geography/strategy, not talent." else
        "Aggregates over today's membership; realignment-honest.")
    HTML(conf_twin_html(
      tbl, caption = glue("Conference Lab — {pol$label} — table view"),
      caption_note = cap_note))
  })
  outputOptions(output, "conf_twin", suspendWhenHidden = FALSE)

  output$size_trend <- renderGirafe({
    req(input$size_metric, input$size_pos)
    trend_data <- filter_pos(size_window(), input$size_pos)
    validate(need(
      nrow(dplyr::filter(trend_data, School == input$g_team)) > 0,
      "No commits for this team + position filter."))
    girafe_try(girafe_wrap(
      plot_size_trend(size_window(), input$g_team, g_sport(),
                      metric = input$size_metric, pos_filter = input$size_pos,
                      compare_slug = g_cmp(), players_note = players_lab()),
      w = 10.5, h = 4.5, name = png_name("size-trend")), "size trend")
  })

  output$h2h_plot <- renderGirafe({
    req(input$size_source)
    validate(need(!is.null(g_cmp()),
                  "Pick a 'Compare to' team in the top bar."))
    girafe_try(girafe_wrap(
      plot_head_to_head(beef_source_data(), input$g_team, g_cmp(), g_sport(),
                        source_label = beef_source_label(),
                        players_note = players_lab()),
      w = 10.5, h = 4.5,
      name = png_name(glue("h2h-vs-{g_cmp()}"))), "head to head")
  })

  output$matchup_h2h_plot <- renderGirafe({
    cmp <- g_cmp()
    validate(need(!is.null(cmp),
                  "Pick a Compare to program in the top bar to build this matchup."))
    girafe_try(girafe_wrap(
      plot_head_to_head(size_window(), input$g_team, cmp, g_sport(),
                        source_label = paste(players_lab(), "recruiting additions"),
                        players_note = players_lab()),
      w = 10.5, h = 4.8,
      name = png_name(glue("matchup-h2h-vs-{cmp}"))), "matchup head to head")
  })
  ## ---- WEIGHT ROOM ---------------------------------------------------------------------
  wr_team <- reactive({
    wr_data_r() %>% filter(School == input$g_team)
  })

  output$vb_wr_gain <- renderValueBox({
    if (is.null(roster_now())) return(valueBox("—", "Roster not scraped",
                                               icon = icon("dumbbell"), color = "red"))
    rows <- wr_team()
    val <- if (nrow(rows) > 0) {
      paste0("+", round(mean(rows$GainPerYr), 1), " lbs/yr")
    } else "—"
    valueBox(val, glue("{team_label(input$g_team)} avg gain per year on campus"),
             icon = icon("dumbbell"), color = "navy")
  })
  output$vb_wr_rank <- renderValueBox({
    req(!is.null(roster_now()))
    ## weight-room rank within the active conference (Phase 0: all 16 members)
    gains <- wr_data_r() %>%
      filter(School %in% conf_pool_slugs()) %>%
      group_by(School) %>%
      summarize(g = mean(GainPerYr), .groups = "drop") %>%
      arrange(desc(g))
    rk <- which(gains$School == input$g_team)
    val <- if (length(rk) == 1) paste0("#", rk, " of ", nrow(gains)) else "—"
    valueBox(val, "Weight room rank",
             icon = icon("trophy"), color = "orange")
  })
  ## box titles + the spotlight player flip with the gainers/slim-downs radio
  output$wr_board_box_title <- renderText({
    if ((input$wr_direction %||% "gain") == "gain") {
      "The Weight Room Effect: Pounds Added per Year"
    } else "The Cut Room: Pounds Trimmed per Slimmer"
  })
  output$wr_players_box_title <- renderText({
    if ((input$wr_direction %||% "gain") == "gain") {
      "Biggest Transformations Since Commit Day"
    } else "Biggest Slim-Downs Since Commit Day"
  })

  output$vb_wr_gainer <- renderValueBox({
    req(!is.null(roster_now()))
    rows <- wr_team()
    if (nrow(rows) == 0) return(valueBox("—", "No matched signees",
                                         icon = icon("user"), color = "light-blue"))
    if ((input$wr_direction %||% "gain") == "gain") {
      big <- rows %>% slice_max(WeightGain, n = 1, with_ties = FALSE)
      valueBox(glue("+{big$WeightGain} lbs"),
               glue("Biggest gainer: {big$Name} ({big$Position})"),
               icon = icon("arrow-trend-up"), color = "light-blue")
    } else {
      big <- rows %>% slice_min(WeightGain, n = 1, with_ties = FALSE)
      valueBox(glue("{big$WeightGain} lbs"),
               glue("Biggest slim-down: {big$Name} ({big$Position})"),
               icon = icon("arrow-trend-down"), color = "light-blue")
    }
  })
  output$vb_wr_shrink <- renderValueBox({
    req(!is.null(roster_now()))
    hc <- height_check_stats(wr_data_r(), input$g_team)
    val <- if (is.na(hc$pct_shrunk_team)) "—" else paste0(hc$pct_shrunk_team, "%")
    valueBox(val, glue("of {team_label(input$g_team)} signees 'shrunk' ",
                       "({active_conf_lab()}: {hc$pct_shrunk_conf}%)"),
             icon = icon("ruler"), color = "yellow")
  })

  output$wr_board <- renderGirafe({
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    validate(need(nrow(wr_data_r()) > 0, "No matched signees in this window."))
    if ((input$wr_direction %||% "gain") == "loss") {
      validate(need(any(wr_data_r()$WeightGain < 0),
                    "No slimmed-down signees in this window."))
    }
    girafe_try(girafe_wrap(
      plot_weight_room_board(wr_data_r(), input$g_team, g_sport(),
                             compare_slug = g_cmp(),
                             direction = input$wr_direction %||% "gain"),
      w = 8, h = 8,
      name = png_name(ifelse((input$wr_direction %||% "gain") == "gain",
                             "weight-room", "cut-room"))), "weight room board")
  })
  output$wr_footer <- renderUI({
    req(!is.null(roster_now()))
    scraped <- unique(roster_now()$ScrapedAt)[1]
    HTML(glue("<em style='color:#888;'>Matched players = HS signees from the
      selected class window still on the current roster (transfers excluded).
      Gains are normalized to pounds per year on campus so young rosters
      aren't penalized. All weights are as reported by programs and listed
      by 247Sports. Rosters scraped {scraped}.</em>"))
  })

  output$wr_players <- renderGirafe({
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    validate(need(nrow(wr_team()) > 0, "No matched signees in this window."))
    if ((input$wr_direction %||% "gain") == "loss") {
      validate(need(any(wr_team()$WeightGain < 0),
                    glue("No slimmed-down {team_label(input$g_team)} signees ",
                         "in this window.")))
    }
    girafe_try(girafe_wrap(
      plot_weight_room_players(wr_data_r(), input$g_team, g_sport(),
                               direction = input$wr_direction %||% "gain"),
      w = 8, h = 8,
      name = png_name(ifelse((input$wr_direction %||% "gain") == "gain",
                             "biggest-gainers", "biggest-slim-downs"))), "weight room players")
  })

  output$height_check <- renderPlot({
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    validate(need(sum(!is.na(wr_data_r()$HeightDelta)) > 0,
                  "No height pairs available."))
    plot_height_check(wr_data_r(), input$g_team, g_sport())
  })
  output$height_check_text <- renderUI({
    req(!is.null(roster_now()))
    hc <- height_check_stats(wr_data_r(), input$g_team)
    shr <- if (!is.na(hc$biggest_shrinker)) {
      glue("Biggest {team_label(input$g_team)} 'shrinker': {hc$biggest_shrinker}.")
    } else ""
    HTML(glue("<em style='color:#888;'>{hc$pct_shrunk_conf}% of matched {active_conf_lab()}
      signees are listed SHORTER on the roster than on their recruiting profile
      — recruiting heights run optimistic. {shr}
      Players who left (transfer/NFL/graduated) can't be checked.</em>"))
  })

  ## ---- COACH ERAS -------------------------------------------------------------------------
  ## era comparison always uses the full history (windowing it to a couple of
  ## classes would make era-vs-era comparison meaningless)
  era_data <- reactive({
    size_all() %>% filter(School == input$g_team)
  })

  output$era_timeline <- renderGirafe({
    req(input$era_metric)
    ## default coach-era view ships precomputed (Arizona, football, AvgRating);
    ## the chart is year-window independent, so a default cold-start paint is
    ## a readRDS instead of a fragile SVG build
    if (at_defaults() && identical(input$era_metric, "AvgRating")) {
      key <- paste0("era_timeline_", ifelse(is_phone(), "phone", "desktop"))
      if (!is.null(PRE[[key]])) {
        message("serving precomputed ", key)
        return(PRE[[key]])
      }
    }
    validate(need(nrow(era_data()) > 0, "No commits for this team."))
    girafe_try(girafe_wrap(plot_era_timeline(size_all(), input$g_team, g_sport(),
                                  metric = input$era_metric,
                                  players_note = players_lab()), h = 6,
                name = glue("{input$g_team}-{g_sport()}-coach-eras-",
                            "{input$era_metric}")), "era timeline")
  }) %>% bindCache(input$g_team, g_sport(), input$g_type,
                   input$era_metric, (input$client_w %||% 1200) < 700)

  output$era_mix <- renderGirafe({
    validate(need(nrow(era_data()) > 0, "No commits for this team."))
    girafe_try(girafe_wrap(plot_era_position_mix(size_all(), input$g_team, g_sport(),
                                      players_note = players_lab()),
                w = 8.5, h = 6.2,
                name = glue("{input$g_team}-{g_sport()}-era-position-mix")), "era position mix")
  })

  output$era_table <- renderDT({
    validate(need(nrow(era_data()) > 0, "No commits for this team."))
    datatable(era_summary_table(size_all(), input$g_team, g_sport()),
              options = list(dom = "t", ordering = FALSE, scrollX = TRUE),
              rownames = FALSE)
  })

  ## ---- ANALYST BRIEF (DEFENSIVE WAR ROOM) ----------------------------------------------------
  ## the ARRIVING class's additions (HS + portal, ALL types regardless of
  ## the global radio) that aren't on the 247 roster page yet -- the bodies
  ## showing up this fall (e.g. June portal adds). Pinned to war_room_class,
  ## NOT the newest cycle in the db: once next year's cycle is scraped, its
  ## signees enroll a season out and don't belong on this fit board.
  incoming_adds <- reactive({
    req(input$g_team)
    if (g_sport() != "football" || is.null(roster_now())) return(NULL)
    nkey <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))
    ros_keys <- nkey(roster_now()$Name[roster_now()$School == input$g_team])
    size_football %>%
      filter(School == input$g_team, Year == war_room_class,
             !nkey(Name) %in% ros_keys)
  })

  output$roster_335 <- renderGirafe({
    validate(need(g_sport() == "football",
                  "The 3-3-5 lens applies to football — switch sport above."))
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    validate(need(
      nrow(dplyr::filter(roster_now(), School == input$g_team)) > 0,
      "No roster rows for this team."))
    girafe_try(girafe_wrap(plot_roster_335(roster_now(), input$g_team,
                                incoming = incoming_adds(),
                                ## the gold stack names the PINNED arriving
                                ## class, matching incoming_adds() above
                                incoming_label = paste0("'", war_room_class %% 100,
                                                        " ADDS"),
                                proj_gain = proj_gain_r()),
                w = 8.5, h = 6.4, name = png_name("335-fit-board")), "335 fit board")
  })

  output$def_profile <- renderGirafe({
    validate(need(g_sport() == "football",
                  "The 3-3-5 lens applies to football — switch sport above."))
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    girafe_try(girafe_wrap(plot_def_size_profile(roster_size(), input$g_team,
                                      incoming = incoming_adds(),
                                      proj_gain = proj_gain_r()),
                w = 8.5, h = 6.4, name = png_name("def-bodies-335")), "defense profile")
  })

  output$roster_constr <- renderGirafe({
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    validate(need(
      nrow(dplyr::filter(roster_now(), School == input$g_team)) > 0,
      "No roster rows for this team."))
    girafe_try(girafe_wrap(
      plot_roster_construction(roster_now(), input$g_team, g_sport()),
      w = 8.5, h = 6.4, name = png_name("roster-construction")), "roster construction")
  })

  output$state_retention <- renderGirafe({
    st <- team_state(input$g_team)
    validate(need(
      nrow(dplyr::filter(size_window(), State == st)) > 0,
      glue("No {st} high-school commits in this window.")))
    girafe_try(girafe_wrap(
      plot_state_retention(size_window(), input$g_team, g_sport(),
                           compare_slug = g_cmp(),
                           players_note = players_lab()),
      w = 8.5, h = 6.4, name = png_name("state-retention")), "state retention")
  })

  ## class retention: % of each signing class still on the roster
  output$class_retention <- renderGirafe({
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    src <- if (g_sport() == "football") size_football else size_basketball
    girafe_try(girafe_wrap(
      plot_class_retention(src %>% dplyr::filter(Type == "Commit"),
                           roster_now(), input$g_team,
                           compare_slug = g_cmp()),
      w = 8, h = 9, name = png_name("class-retention")), "class retention")
  })

  output$analyst_notes_out <- renderUI({
    req(input$g_team)
    ## defense first (the war-room lead), then the general brief
    d_notes <- if (g_sport() == "football" && !is.null(roster_now())) {
      defense_notes(roster_now(), size_window(), input$g_team,
                    incoming = incoming_adds(), proj_gain = proj_gain_r())
    } else character(0)
    notes <- c(d_notes,
               analyst_notes(size_window(), roster_now(), input$g_team,
                             g_sport(), compare_slug = g_cmp()))
    validate(need(length(notes) > 0, "Not enough data in this window."))
    HTML(paste0("<ul class='talking-points'>",
                paste0("<li>", notes, "</li>", collapse = ""),
                "</ul>"))
  })

  ## ---- TALENT vs RESULTS ---------------------------------------------------------------------
  output$talent_quadrant <- renderGirafe({
    validate(need(g_sport() == "football",
                  "Season outcomes are football-only for now."))
    validate(need(!is.null(team_seasons),
                  "Run scripts/fetchOutcomes.R (needs a free CFBD key) to add season records."))
    ## the quadrant follows the global class-year window (seasons exist
    ## through 2025, so a 2026-only window has no completed seasons yet).
    ## girafe outputs don't reliably display validate() messages, so the
    ## empty case renders as a message chart instead
    yrs <- g_years_d()
    ts_window <- team_seasons %>%
      filter(year >= yrs[1], year <= yrs[2])
    if (nrow(ts_window) == 0) {
      msg <- ggplot() +
        annotate("text", x = 0, y = 0, size = 6, color = "#46535E",
                 label = glue("No completed seasons in ",
                              "{yrs[1]}–{yrs[2]}.\n",
                              "Widen the year window to see the quadrant.")) +
        theme_void()
      return(girafe_wrap(msg, w = 10.5, h = 6.2, name = "no-seasons"))
    }
    girafe_try(girafe_wrap(
      plot_talent_results(ts_window,
                          size_football %>% filter(Year <= max(ts_window$year)),
                          input$g_team, compare_slug = g_cmp()),
      w = 10.5, h = 6.2,
      name = png_name("talent-vs-results-quadrant")), "talent quadrant")
  })

  ## WAT LADDER: same football-only guards + season window as the quadrant.
  ## Not bindCache'd (nor precomputed) -- it draws the whole conference from
  ## a fresh quasibinomial fit that must reflect the exact windowed panel.
  output$wat_ladder <- renderGirafe({
    validate(need(g_sport() == "football",
                  "Season outcomes are football-only for now."))
    validate(need(!is.null(team_seasons),
                  "Run scripts/fetchOutcomes.R (needs a free CFBD key) to add season records."))
    yrs <- g_years_d()
    ts_window <- team_seasons %>%
      filter(year >= yrs[1], year <= yrs[2])
    if (nrow(ts_window) == 0) {
      msg <- ggplot() +
        annotate("text", x = 0, y = 0, size = 6, color = "#46535E",
                 label = glue("No completed seasons in ",
                              "{yrs[1]}-{yrs[2]}.\n",
                              "Widen the year window to see the ladder.")) +
        theme_void()
      return(girafe_wrap(msg, w = 10.5, h = 6.2, name = "no-seasons"))
    }
    girafe_try(girafe_wrap(
      plot_wat(ts_window,
               size_football %>% filter(Year <= max(ts_window$year)),
               input$g_team, compare_slug = g_cmp()),
      w = 10.5, h = 6.2,
      name = png_name("wins-above-talent-ladder")), "wins above talent ladder")
  })

  ## ---- TABLE TWINS: the numbers view behind the four boards ----------------
  ## Each twin is built from the SAME *_data() call, with the SAME reactive
  ## args, as its chart render above -- the two views cannot disagree. The
  ## req(input$twin_*) gate plus suspendWhenHidden = FALSE means the table
  ## computes exactly when its toggle is on: never while the chart view is
  ## active, and without relying on Shiny's visibility polling of the
  ## display:none container.
  output$beef_twin <- renderUI({
    req(isTRUE(input$twin_beef_board))
    req(input$size_metric, input$size_pos, input$size_source)
    validate(need(nrow(filter_pos(beef_source_data(), input$size_pos)) > 0,
                  "No players for this position filter."))
    b <- beef_board_data(beef_source_data(), input$g_team, g_sport(),
                         metric = input$size_metric,
                         pos_filter = input$size_pos,
                         compare_slug = g_cmp(),
                         source_label = beef_source_label(),
                         players_note = players_lab())
    ## carry the chart's scope onto the table: source + window + pool in
    ## commit mode; roster mode has no year window, so say that instead
    cap_note <- if (is.null(beef_source_label())) {
      glue("{str_to_title(g_sport())} commits {attr(b, 'yr_rng')}. ",
           "Showing: {players_lab()}.")
    } else {
      glue("{beef_source_label()}; the year window does not apply to ",
           "current roster weights.")
    }
    external_note <- attr(b, "external_note") %||% ""
    if (nzchar(external_note)) cap_note <- paste(cap_note, external_note)
    HTML(twin_table_html(
      b, caption = glue("{active_conf_lab()} Beef Board - ",
                        "{pos_filter_label(input$size_pos)} - table view"),
      caption_note = cap_note))
  })
  outputOptions(output, "beef_twin", suspendWhenHidden = FALSE)

  output$retention_twin <- renderUI({
    req(isTRUE(input$twin_class_retention))
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    src <- if (g_sport() == "football") size_football else size_basketball
    b <- retention_board_data(src %>% dplyr::filter(Type == "Commit"),
                              roster_now(), input$g_team,
                              compare_slug = g_cmp())
    ## the chart's subtitle names the four classes measured -- so must the
    ## table (cls_years = the last four completed cycles, from the frame)
    cy <- attr(b, "cls_years")
    HTML(twin_table_html(
      b, caption = "Class Retention: who keeps their signees? - table view",
      caption_note = glue("HS signees from the {min(cy)}-{max(cy)} classes ",
                          "vs the current roster.")))
  })
  outputOptions(output, "retention_twin", suspendWhenHidden = FALSE)

  output$wr_twin <- renderUI({
    req(isTRUE(input$twin_wr_board))
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    validate(need(nrow(wr_data_r()) > 0, "No matched signees in this window."))
    dir <- input$wr_direction %||% "gain"
    if (dir == "loss") {
      validate(need(any(wr_data_r()$WeightGain < 0),
                    "No slimmed-down signees in this window."))
    }
    b <- wr_board_data(wr_data_r(), input$g_team, g_sport(),
                       compare_slug = g_cmp(), direction = dir)
    ## the chart's pool: the global year window, HS signees only (the
    ## Weight Room is defined as HS-signee development -- see wr_data_r)
    yrs <- g_years_d()
    HTML(twin_table_html(
      b, caption = ifelse(
        dir == "gain",
        "The Weight Room Effect: pounds added per year - table view",
        "The Cut Room: pounds trimmed per slimmer - table view"),
      caption_note = glue("Classes {yrs[1]}-{yrs[2]}; matched HS signees ",
                          "still on the roster only.")))
  })
  outputOptions(output, "wr_twin", suspendWhenHidden = FALSE)

  output$quadrant_twin <- renderUI({
    req(isTRUE(input$twin_talent_quadrant))
    validate(need(g_sport() == "football",
                  "Season outcomes are football-only for now."))
    validate(need(!is.null(team_seasons),
                  "Run scripts/fetchOutcomes.R (needs a free CFBD key) to add season records."))
    yrs <- g_years_d()
    ts_window <- team_seasons %>%
      filter(year >= yrs[1], year <= yrs[2])
    if (nrow(ts_window) == 0) {
      return(div(class = "twin-empty",
                 glue("No completed seasons in {yrs[1]}-{yrs[2]} - widen ",
                      "the year window to see the table.")))
    }
    b <- quadrant_data(ts_window,
                       size_football %>% filter(Year <= max(ts_window$year)),
                       input$g_team, compare_slug = g_cmp())
    extras <- list("Win %" = function(d) sprintf("%.0f%%", d$win_pct))
    if ("sp" %in% names(b)) {
      extras[["Avg SP+"]] <- function(d) {
        ifelse(is.finite(d$sp), sprintf("%.1f", d$sp), "n/a")
      }
    }
    HTML(twin_table_html(
      b, caption = glue("The Over/Underachiever Quadrant ",
                        "({attr(b, 'yr_rng')}) - table view"),
      value_col = "talent", n_col = "seasons_n",
      n_chip = function(n) paste0(n, ifelse(n == 1, " season", " seasons")),
      extras = extras,
      ## neutral navy: the quadrant chart reserves Okabe-Ito blue and
      ## vermillion for over/underachiever, so a blue->red bar ramp here
      ## would collide with that vocabulary
      bar_ramp = c("#d8e0ea", "#0C234B")))
  })
  outputOptions(output, "quadrant_twin", suspendWhenHidden = FALSE)

  output$wat_twin <- renderUI({
    req(isTRUE(input$twin_wat_ladder))
    validate(need(g_sport() == "football",
                  "Season outcomes are football-only for now."))
    validate(need(!is.null(team_seasons),
                  "Run scripts/fetchOutcomes.R (needs a free CFBD key) to add season records."))
    yrs <- g_years_d()
    ts_window <- team_seasons %>%
      filter(year >= yrs[1], year <= yrs[2])
    if (nrow(ts_window) == 0) {
      return(div(class = "twin-empty",
                 glue("No completed seasons in {yrs[1]}-{yrs[2]} - widen ",
                      "the year window to see the table.")))
    }
    b <- wat_data(ts_window,
                  size_football %>% filter(Year <= max(ts_window$year)),
                  input$g_team, compare_slug = g_cmp())
    ## value is SIGNED WAT, so the twin ranks overachievers (+) to
    ## underachievers (-) top-to-bottom; value_fmt_fn on the frame renders
    ## "+2.1" / "-1.4" to match the ladder's row labels
    HTML(twin_table_html(
      b, caption = glue("Wins Above Talent ",
                        "({attr(b, 'yr_rng')}) - table view"),
      value_col = "value", n_col = "seasons_n",
      n_chip = function(n) paste0(n, ifelse(n == 1, " season", " seasons")),
      extras = list(
        "Actual %" = function(d) sprintf("%.0f%%", d$actual),
        "Expected %" = function(d) sprintf("%.0f%%", d$expected)),
      caption_note = glue("Seasons {yrs[1]}-{yrs[2]}; wins per season above ",
                          "(+) or below (-) the league talent-to-wins fit ",
                          "({attr(b, 'model_note')})."),
      ## navy neutral ramp: the ladder's colored dots already spend the
      ## over/under-achiever vocabulary, so the twin's percentile bars stay
      ## neutral (same choice as the quadrant twin)
      bar_ramp = c("#d8e0ea", "#0C234B")))
  })
  outputOptions(output, "wat_twin", suspendWhenHidden = FALSE)

  output$team_scoreboard <- renderGirafe({
    validate(need(g_sport() == "football",
                  "Season outcomes are football-only for now."))
    validate(need(!is.null(team_seasons),
                  "Run scripts/fetchOutcomes.R (needs a free CFBD key) to add season records."))
    girafe_try(girafe_wrap(
      plot_team_scoreboard(team_seasons,
                           size_football %>% filter(Year <= max(team_seasons$year)),
                           input$g_team),
      w = 10.5, h = 4.4,
      name = glue("{input$g_team}-season-scoreboard")), "team scoreboard")
  })

  ## ---- TALENT ORIGINS --------------------------------------------------------
  ## One shared set of aggregate frames powers each chart and its semantic
  ## table twin. Team/compare/type are intentionally absent from these pools:
  ## this page is the 67-destination HS/prep origin universe.
  origin_board_r <- reactive({
    req(input$origin_metric, input$origin_pos, input$origin_state)
    origin_state_board(
      origin_pool_r(),
      metric = input$origin_metric,
      position = input$origin_pos,
      top_n = 15,
      selected_state = input$origin_state
    )
  })

  origin_positions_r <- reactive({
    req(input$origin_metric)
    origin_position_board(origin_pool_r(), metric = input$origin_metric,
                          top_n = 3)
  })

  origin_trend_r <- reactive({
    req(input$origin_state, input$origin_metric, input$origin_pos)
    origin_year_board(
      origin_pool_r(),
      state = input$origin_state,
      metric = input$origin_metric,
      position = input$origin_pos
    )
  })

  observeEvent(input$origin_state_click, {
    state <- toupper(input$origin_state_click %||% "")
    if (!state %in% ORIGIN_US_CODES) return(invisible(NULL))
    updateSelectInput(session, "origin_state", selected = state)
    updateRadioButtons(session, "origin_view", selected = "trend")
  })

  output$origin_board <- renderGirafe({
    board <- origin_board_r()
    validate(need(nrow(board) > 0,
                  "No state meets this measure's sample rule in the selected window."))
    girafe_try(
      girafe_wrap(
        plot_origin_state_board(board, g_sport(), input$origin_state),
        w = 10.8, h = 7.8,
        name = glue("power4-{g_sport()}-talent-origins-state-board")
      ),
      "talent origins state board"
    )
  }) %>% bindCache(g_sport(), g_years_d(), input$origin_metric,
                   input$origin_pos, input$origin_state,
                   (input$client_w %||% 1200) < 700)

  output$origin_positions <- renderGirafe({
    board <- origin_positions_r()
    validate(need(nrow(board) > 0,
                  "No position group meets this measure's sample rule."))
    phone <- is_phone()
    girafe_try(
      girafe_wrap(
        plot_origin_position_board(board, g_sport(), phone = phone),
        w = 11, h = if (phone) 9.5 else 7.2,
        name = glue("power4-{g_sport()}-talent-origins-position-hotbeds")
      ),
      "talent origins position hotbeds"
    )
  }) %>% bindCache(g_sport(), g_years_d(), input$origin_metric,
                   (input$client_w %||% 1200) < 700)

  output$origin_trend <- renderGirafe({
    trend <- origin_trend_r()
    validate(need(any(is.finite(trend$Value)),
                  "This state does not meet the yearly sample rule in the selected window."))
    girafe_try(
      girafe_wrap(
        plot_origin_trend(trend, input$origin_state, g_sport()),
        w = 10.8, h = 5.3,
        name = glue("power4-{g_sport()}-{tolower(input$origin_state)}-origin-trend")
      ),
      "talent origins trend"
    )
  }) %>% bindCache(g_sport(), g_years_d(), input$origin_metric,
                   input$origin_pos, input$origin_state,
                   (input$client_w %||% 1200) < 700)

  output$origin_board_twin <- renderUI({
    req(isTRUE(input$twin_origin_board))
    board <- origin_board_r()
    open_note <- attr(board, "open_note") %||% "No open class is included."
    HTML(origin_state_table_html(
      board,
      glue("{str_to_title(g_sport())}, {g_years_d()[1]}-{g_years_d()[2]}; ",
           "last listed HS/prep location. {open_note}")
    ))
  })
  outputOptions(output, "origin_board_twin", suspendWhenHidden = FALSE)

  output$origin_positions_twin <- renderUI({
    req(isTRUE(input$twin_origin_positions))
    board <- origin_positions_r()
    open_note <- attr(board, "open_note") %||% "No open class is included."
    HTML(origin_position_table_html(
      board,
      glue("{str_to_title(g_sport())}, {g_years_d()[1]}-{g_years_d()[2]}; ",
           "top three qualifying states inside each position group. {open_note}")
    ))
  })
  outputOptions(output, "origin_positions_twin", suspendWhenHidden = FALSE)

  output$origin_trend_twin <- renderUI({
    req(isTRUE(input$twin_origin_trend))
    trend <- origin_trend_r()
    HTML(origin_trend_table_html(
      trend,
      glue("{str_to_title(g_sport())}; last listed HS/prep location. ",
           "Open future class is not treated as complete.")
    ))
  })
  outputOptions(output, "origin_trend_twin", suspendWhenHidden = FALSE)

  output$origin_story <- renderUI({
    view <- input$origin_view %||% "board"
    base <- origin_pool_r()
    d <- if (identical(view, "positions")) base else
      .origin_filter_position(base, input$origin_pos %||% "All")
    validate(need(nrow(d) > 0, "No qualifying HS/prep origins in this window."))
    open_text <- origin_open_cycle_note(d)
    open_note <- if (nzchar(open_text)) paste0(" ", open_text) else ""

    sentence <- if (identical(view, "positions")) {
      board <- origin_positions_r()
      leaders <- board %>%
        dplyr::filter(Rank == 1) %>%
        dplyr::count(StateName, sort = TRUE, name = "GroupsLed")
      if (nrow(leaders)) {
        glue("{leaders$StateName[1]} leads {leaders$GroupsLed[1]} of ",
             "{length(unique(board$PosGroup))} position boards on ",
             "{tolower(attr(board, 'metric_label'))}.{open_note}")
      } else {
        paste0("No position group clears the current sample rule.", open_note)
      }
    } else if (identical(view, "trend")) {
      trend <- origin_trend_r()
      complete <- trend %>%
        dplyr::filter(!IsOpenCycle, is.finite(Value)) %>%
        dplyr::arrange(Year)
      if (nrow(complete) >= 2) {
        first <- complete[1, ]
        last <- complete[nrow(complete), ]
        delta <- last$Value - first$Value
        direction <- if (abs(delta) < 0.05) "held steady" else
          if (delta > 0) "rose" else "fell"
        glue("{unique(trend$StateName)} {tolower(attr(trend, 'metric_label'))} ",
             "{direction} from {first$ValueLabel} in {first$Year} to ",
             "{last$ValueLabel} in {last$Year}.{open_note}")
      } else {
        glue("{unique(trend$StateName)} has {nrow(complete)} complete class",
             "{ifelse(nrow(complete) == 1, '', 'es')} clearing the sample rule ",
             "in this window.{open_note}")
      }
    } else {
      board <- origin_board_r() %>% dplyr::arrange(FieldRank)
      metric <- input$origin_metric %||% "blue_n"
      if (!nrow(board)) {
        paste0("No state clears the sample rule for this measure.", open_note)
      } else {
        leader <- board[1, ]
        lead_text <- switch(
          metric,
          commit_n = glue("{leader$StateName} leads with ",
                          "{format(leader$N, big.mark = ',')} captured signees ",
                          "({sprintf('%.1f%%', 100 * leader$N / nrow(d))} of this view)."),
          blue_n = {
            pool_blue <- sum(d$IsBlueChip, na.rm = TRUE)
            glue("{leader$StateName} leads with ",
                 "{format(leader$BlueN, big.mark = ',')} blue-chip signees",
                 "{ifelse(pool_blue > 0, paste0(' (', sprintf('%.1f%%', 100 * leader$BlueN / pool_blue), ' of this view)'), '')}.")
          },
          blue_share = glue("{leader$StateName} leads qualifying states at ",
                            "{leader$ValueLabel} blue chips (rated n=",
                            "{leader$RatedN} of {leader$N})."),
          median_rating = glue("{leader$StateName} leads qualifying states with ",
                               "a {leader$ValueLabel} median rating (rated n=",
                               "{leader$RatedN} of {leader$N})."),
          glue("{leader$StateName} leads this measure at {leader$ValueLabel}.")
        )
        focus_state <- input$origin_state %||% "AZ"
        focus_note <- ""
        if (!focus_state %in% board$StateClean) {
          focus <- .origin_state_summary(d) %>%
            dplyr::filter(StateClean == focus_state)
          info <- origin_metric_info(metric, "board")
          focus_note <- if (nrow(focus) && isTRUE(info$quality)) {
            glue(" Focus state {origin_state_name(focus_state)} is withheld for ",
                 "this quality measure (rated n={focus$RatedN}; minimum ",
                 "{attr(board, 'min_n')}).")
          } else {
            glue(" Focus state {origin_state_name(focus_state)} has no captured ",
                 "signees in this view.")
          }
        }
        paste0(lead_text, focus_note, open_note)
      }
    }

    div(
      class = "gi-origin-insight",
      role = "status", `aria-live` = "polite", `aria-atomic` = "true",
      icon("lightbulb"),
      p(strong("What stands out: "), sentence)
    )
  })

  output$origin_deeper <- renderUI({
    d <- origin_pool_r()
    validate(need(nrow(d) > 0, "No deeper signals in this window."))
    concentration <- origin_concentration(d)
    factories <- origin_factory_board(d, top_n = 3)
    state <- input$origin_state %||% "AZ"
    signature <- origin_position_signature(d, state)
    state_total <- sum(signature$StateN, na.rm = TRUE)
    signature <- signature %>%
      dplyr::filter(StateN >= ifelse(g_sport() == "football", 5, 3))

    all_adds <- origin_window() %>%
      dplyr::filter(Type %in% c("Commit", "Transfer"),
                    Year <= as.integer(format(Sys.Date(), "%Y"))) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarize(
        Additions = dplyr::n(),
        NonPortalShare = 100 * sum(Type == "Commit") / Additions,
        .groups = "drop"
      ) %>%
      dplyr::arrange(Year)

    factory_text <- if (nrow(factories)) {
      glue("{factories$FactoryLabel[1]} is the leading listed talent factory ",
           "in this window ({format(factories$N[1], big.mark = ',')} unique signees).")
    } else "No listed school clears the factory view in this window."

    signature_text <- if (state_total > 0 && nrow(signature)) {
      top <- signature[1, ]
      glue("{origin_state_name(state)} leans most toward {top$PosGroup}: ",
           "{sprintf('%.1f%%', top$StateShare)} of its pool versus ",
           "{sprintf('%.1f%%', top$PoolShare)} across core Power-4 position groups.")
    } else {
      glue("{origin_state_name(state)} has too little position volume for a stable signature.")
    }

    acquisition_text <- if (nrow(all_adds) >= 2) {
      first <- all_adds[1, ]
      last <- all_adds[nrow(all_adds), ]
      glue("Non-portal commitment/transfer records moved from ",
           "{sprintf('%.1f%%', first$NonPortalShare)} of tracked additions in ",
           "{first$Year} to {sprintf('%.1f%%', last$NonPortalShare)} in ",
           "{last$Year}; this is roster-acquisition mix, not a causal claim.")
    } else if (nrow(all_adds) == 1) {
      glue("Non-portal records are {sprintf('%.1f%%', all_adds$NonPortalShare[1])} ",
           "of tracked additions in {all_adds$Year[1]}.")
    } else "No complete class is available for acquisition-mix context."

    source_rows <- origin_window() %>% dplyr::filter(Type == "Commit")
    kind_n <- table(factor(source_rows$OriginKind,
                           levels = c("hs_prep", "juco", "needs_review",
                                      "outside_us")))
    coverage_text <- glue(
      "{format(kind_n[['hs_prep']], big.mark = ',')} HS/prep records; ",
      "{format(kind_n[['juco']], big.mark = ',')} high-confidence JUCO and ",
      "{format(kind_n[['needs_review']], big.mark = ',')} College-name review ",
      "records excluded; {format(kind_n[['outside_us']], big.mark = ',')} ",
      "outside 50 states + DC."
    )

    tagList(
      tags$ul(
        class = "gi-origin-signal-list",
        tags$li(strong("Concentration. "),
                glue("The top four state/prep locations supply ",
                     "{sprintf('%.1f%%', concentration$top4_share)} of this pool ",
                     "({sprintf('%.1f', concentration$effective_states)} effective states).")),
        tags$li(strong("Talent factory. "), factory_text),
        if (!identical(input$origin_view, "positions"))
          tags$li(strong("Position signature. "), signature_text),
        tags$li(strong("Acquisition shift. "), acquisition_text)
      ),
      p(class = "gi-origin-coverage", strong("Coverage: "), coverage_text)
    )
  })
  ## ---- Program Reach: selected-window table + map + distance plots ---------
  filtered_data <- reactive({
    req(input$g_team, input$g_years)
    validate(need(input$g_team %in% TEAM_CONFIG$slug, "Unknown team."))
    d <- reach_window() %>% filter(School == input$g_team)
    validate(need(nrow(d) > 0, "No players in this window."))
    d %>% mutate(
      Ranking = suppressWarnings(as.numeric(Ranking)),
      NationalRank = suppressWarnings(as.numeric(NationalRank)),
      NationalRank = ifelse(NationalRank > 150, NA, NationalRank),
      University = TeamName)
  })

  ## Pipeline map built from the raw-origin frame (main + compare footprints
  ## in team colors); unlike body charts, geography does not require size data.
  output$gridPlot <- renderLeaflet({
    req(input$g_team, input$g_years)
    team_w <- reach_window() %>% filter(School == input$g_team)
    validate(need(nrow(team_w) > 0, "No recruits in this window."))
    mapped <- is.finite(suppressWarnings(as.numeric(team_w$lat))) &
      is.finite(suppressWarnings(as.numeric(team_w$long)))
    validate(need(sum(mapped) > 0,
                  "No recruits with mapped listed origins in this window."))
    ## say on the map itself how many window players CAN'T be mapped yet
    ## (transfers appear once the nightly profile backfill captures a listed
    ## origin; a few non-portal recruits still lack geocodes)
    n_unmapped <- sum(!mapped)
    build_pipeline_map(reach_window(), input$g_team, g_sport(),
                       compare_slug = g_cmp(), n_unmapped = n_unmapped)
  })

  ## v4.4: themed interactive rebuild (the sourced scripts/box_plot.R is
  ## retired); transfers appear once the nightly profile backfill captures a
  ## listed origin -- until then there is no distance to measure
  output$box_plot <- renderGirafe({
    req(input$g_team, input$g_years)
    d <- reach_window() %>%
      filter(School == input$g_team, !is.na(miles_away))
    validate(need(nrow(d) > 0,
                  "No recruits with mapped listed origins in this window."))
    girafe_try(girafe_wrap(plot_distance_box(reach_window(), input$g_team, g_sport()),
                w = 10.5, h = 4.2, name = png_name("distance-box")), "distance box")
  })

  ## v3.7: interactive distance scatter (hover = recruit card, click = 247
  ## profile); the sourced scripts/plot.R version is retired
  output$distance_plot <- renderGirafe({
    req(input$g_team, input$g_years, input$show_outliers)
    d <- reach_window() %>%
      filter(School == input$g_team, !is.na(miles_away))
    validate(need(nrow(d) > 0, "No recruits with mapped listed origins in this window."))
    girafe_try(girafe_wrap(plot_distance_lab(reach_window(), input$g_team, g_sport(),
                                  show_outliers = input$show_outliers),
                name = png_name("distance-lab")), "distance lab")
  })

  output$summary_stats <- renderDT({
    req(input$g_team, input$g_years)
    if (nrow(filtered_data()) == 0) {
      return(data.frame(Message =
        "No recruits found for the selected filters. Please adjust your selections."))
    }
    d <- filtered_data() %>%
      select(Name, miles_away, Location, University, Ranking,
             NationalRank, Position, Height, Weight, Year) %>%
      arrange(desc(miles_away))

    datatable(
      as.data.frame(d),
      colnames = c("Recruit", "Distance from Listed Origin (miles)",
                   "Listed Origin", "Destination Program", "247Sports Rating",
                   "National Ranking", "Position", "Height", "Weight", "Year"),
      options = list(pageLength = 10, lengthChange = FALSE),
      rownames = FALSE)
  })

}

shinyApp(ui, server)
