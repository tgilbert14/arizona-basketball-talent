## ===========================================================================
## Big 12 Talent Lab — v3
## New in v3:
##   * GLOBAL settings in the sidebar: your team, compare-to team, sport,
##     class-year range — every tab obeys them
##   * Home tab that leads with the insights (conference superlatives, your
##     team's talking points, logo quick-pick)
##   * Weight Room tab — pounds added per signee (commit day -> current
##     roster) + the Measurement Reality Check (listed-height honesty)
##   * Coach Eras tab — how each head coach recruits differently (rating,
##     size, footprint, in-state share, position mix) with era-shaded charts
##   * Compare-to team gets a secondary highlight on boards/trends/DNA
##   * Football is now the default sport; sport modal replaced by a sidebar
##     radio; v1/v2 preserved as app_v1.R / app_v2.R
## Plot builders: R/girth_plots.R | Eras: R/coach_eras.R | Team meta:
## R/team_config.R (conference + state columns = multi-conference ready)
## ===========================================================================

#rsconnect::deployApp()

# connect to .db (works locally + on shinyapps.io) -->
db_path <- here("data", "recruiting.db")
conn <- dbConnect(RSQLite::SQLite(), db_path)

## preload + prep both sports once at startup (small tables, fast)
size_football <- safe_query(conn, "SELECT * FROM recruit_class_football") %>%
  prep_size_data("football")
size_basketball <- safe_query(conn, "SELECT * FROM recruit_class_basketball") %>%
  prep_size_data("basketball")

## current rosters (from scripts/scrapeRosters.R); NULL if not scraped yet
load_roster <- function(tbl) {
  if (tbl %in% dbListTables(conn)) dbGetQuery(conn, paste0("SELECT * FROM ", tbl))
  else NULL
}
roster_football <- load_roster("roster_football")
roster_basketball <- load_roster("roster_basketball")

## CFBD season outcomes (from scripts/fetchOutcomes.R); NULL until fetched
team_seasons <- load_roster("team_seasons_football")

## free what startup allocated -- the deployed worker has a hard 1GB ceiling
invisible(gc())

SIZE_YEARS <- range(c(size_football$Year, size_basketball$Year))

## the default window = the 4-class "roster window": those classes supply
## ~92% of current Big 12 rosters in the portal era (the 5th year back adds
## only ~4%), and the smaller default keeps first renders fast
DEFAULT_YEARS <- c(SIZE_YEARS[2] - 3, SIZE_YEARS[2])

## named choices for team pickers (slug values, pretty labels)
team_choices <- setNames(TEAM_CONFIG$slug, TEAM_CONFIG$team_name)

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
  actionLink(id, label = NULL, icon = icon("circle-info"),
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

## public app: visitors see a generic error message, never raw R errors
## (full errors still reach the server logs for debugging)
options(shiny.sanitize.errors = TRUE)

## render cache: flipping back to settings you've already viewed is instant.
## disk (not memory) so it survives across sessions within a worker -- on
## hosted tiers the worker restarts after sleep, which wipes a memory cache
## before most visitors ever benefit from it
shinyOptions(cache = cachem::cache_disk(
  file.path(dirname(tempdir()), "girth-cache"), max_size = 120 * 1024^2))

## the sources & methods copy behind each info button (kept out of the UI
## so the boxes stay clean -- the user opens these only when curious)
INFO_MODALS <- list(
  info_size = list(
    title = "Size Lab — sources & methods",
    body = "
      <p><strong>Source:</strong> 247Sports team commit pages, classes
      2016–2026, all 16 Big 12 programs (scraped Jan 2026; the 2026 class
      re-scraped June 2026 via <code>scripts/refreshClassYear.R</code>).</p>
      <p><strong>What counts:</strong> high-school commits by default; the
      'Players' control in the top bar can add portal transfers or isolate
      them (transfers exist for 2021 onward).</p>
      <p><strong>Caveat:</strong> heights/weights are as listed at commit
      time. Recruiting heights run optimistic — about a quarter of Big 12
      signees are listed shorter on the roster later (see Weight Room →
      Reality Check). Treat any listed height as ±1 inch.</p>
      <p><strong>Girth index:</strong> pounds per inch of height = weight ÷
      height. BMI = 703 × weight ÷ height².</p>"),
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
    body = "
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
      by class standing; home-state commits by Big 12 signing school
      (out-of-conference destinations arrive with the Power-4 expansion).</p>
      <p><strong>The brief:</strong> auto-written, defense first — nothing is
      hand-curated.</p>"),
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
  info_map = list(
    title = "Recruiting Map — sources & methods",
    body = "
      <p><strong>Locations:</strong> each commit's high school, geocoded from
      the 247Sports profile location. Shaded shapes = your team's state
      footprint (smoothed convex hulls).</p>
      <p><strong>Gaps:</strong> transfers have no HS location; players added
      in the June 2026 refresh appear in stats but not on the map until the
      geocoding pipeline runs for them.</p>"),
  info_distance = list(
    title = "Distance Lab — sources & methods",
    body = "
      <p><strong>Distance:</strong> geodesic (great-circle) miles from the
      recruit's high school to campus, from geocoded 247 locations.</p>
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
                        "Average Miles from Home" = "AvgMiles",
                        "% In-State Commits" = "PctInState")

## UI ========================================================================
ui <- dashboardPage(

  dashboardHeader(title = "Big 12 Girth Index"),
  skin = "blue",

  ## ---- sidebar: navigation only (global controls live in the top bar) -----
  dashboardSidebar(
    width = 230,
    collapsed = FALSE,

    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("house")),
                menuItem("Size Lab", tabName = "sizelab", icon = icon("ruler-combined")),
                menuItem("Conference Beef", tabName = "beef", icon = icon("dumbbell")),
                menuItem("Weight Room", tabName = "weightroom", icon = icon("weight-hanging")),
                menuItem("Coach Eras", tabName = "eras", icon = icon("user-tie")),
                menuItem("War Room (3-3-5)", tabName = "brief", icon = icon("shield-halved")),
                menuItem("Talent vs Results", tabName = "results", icon = icon("trophy")),
                menuItem("Recruiting Map", tabName = "summary", icon = icon("map")),
                menuItem("Distance Lab", tabName = "compare", icon = icon("clock")),
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
      tags$style(HTML("
      body, .content-wrapper { font-family: 'Rubik', 'Helvetica Neue', sans-serif; }
      .main-header .logo { font-family: 'Rubik', sans-serif; font-weight: 800;
        letter-spacing: 0.5px; }

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

      /* ---- mobile polish ---- */
      @media (max-width: 767px) {
        .hero { padding: 14px 16px; border-radius: 8px; }
        .hero h1 { font-size: 19px; }
        .hero p { font-size: 13px; }
        .hero .btn { margin-top: 8px; margin-right: 4px; padding: 4px 8px;
          font-size: 12px; }
        #hero_team img { height: 40px !important; }
        .small-box h3 { font-size: 20px; }
        .small-box p { font-size: 12px; }
        .control-bar { padding: 0 10px; top: 50px; }
        .cb-summary-text { font-size: 12px; }
        .box-header .box-title { font-size: 15px; }
        .talking-points li { font-size: 13px; }
        .snap-stat .num { font-size: 19px; }
        .content { padding: 8px; }
        .pinned-card { max-width: 86vw; }
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
              if (!box || box.querySelector('.tap-badge')) return;
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
            if (!el || !el.closest('.girafe')) return;
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
                /* UI chrome never ships in an export: toasts, badges, and
                   the pinned cards' close/grip/hint controls */
                return !(n.classList && (n.classList.contains('gi-toast') ||
                                         n.classList.contains('tap-badge') ||
                                         n.classList.contains('pin-close') ||
                                         n.classList.contains('pin-resize') ||
                                         n.classList.contains('pin-hint')));
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
            var pane = document.querySelector('.tab-pane.active') || document.body;
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
      "))),

    ## ---- THE CONTROL BAR: global settings, visible on every tab ------------
    ## a slim summary strip is always shown; tapping it expands/collapses the
    ## full controls (collapsed by default on phones)
    div(class = "control-bar",
        div(class = "cb-head",
            uiOutput("cb_summary", inline = TRUE),
            tags$button(id = "snap_view", class = "snap-btn", type = "button",
                        title = paste("Save a PNG of this view —",
                                      "pinned cards included"),
                        icon("camera")),
            tags$span(class = "cb-chevron", icon("chevron-up"))),
        div(class = "cb-body",
            fluidRow(
              ## no inline logos here -- the summary strip carries them, and
              ## floating images broke the layout at mid widths
              column(width = 2,
                     selectInput("g_team", "Your team",
                                 choices = team_choices,
                                 selected = "arizona", width = "100%")),
              column(width = 2,
                     selectInput("g_compare", "Compare to",
                                 choices = c("— none —" = "", team_choices),
                                 selected = "arizona-state",
                                 width = "100%")),
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
                     radioButtons("g_type", "Players",
                                  choices = c("HS commits" = "commit",
                                              "Commits + transfers" = "both",
                                              "Transfers only" = "transfer"),
                                  selected = "both"),
                     checkboxInput("show_context", "Context notes",
                                   value = TRUE))
            ))),

    tabItems(

      ## HOME ------------------------------------------------------------------
      tabItem(tabName = "home",
              div(class = "hero",
                  uiOutput("hero_team"),
                  p("Who are the biggest boys in the Big 12, how does each class measure up,
                     and how does every coach recruit..."),
                  actionButton("go_sizelab", tagList(icon("ruler-combined"), "Open the Size Lab"),
                               class = "btn-warning"),
                  actionButton("go_beef", tagList(icon("dumbbell"), "Conference Beef"),
                               class = "btn-default"),
                  actionButton("go_wr", tagList(icon("weight-hanging"), "Weight Room"),
                               class = "btn-default"),
                  actionButton("go_eras", tagList(icon("user-tie"), "Coach Eras"),
                               class = "btn-default"),
                  actionButton("go_warroom",
                               tagList(icon("shield-halved"), "War Room (3-3-5)"),
                               class = "btn-default")
              ),
              fluidRow(
                ## each status number is a door into the tab behind it
                column(width = 4, div(
                  class = "vb-link", title = "Open Conference Beef",
                  onclick = "document.querySelector('a[href=\"#shiny-tab-beef\"]').click()",
                  valueBoxOutput("vb_home_rank", width = NULL))),
                column(width = 4, div(
                  class = "vb-link", title = "Open Coach Eras",
                  onclick = "document.querySelector('a[href=\"#shiny-tab-eras\"]').click()",
                  valueBoxOutput("vb_home_class", width = NULL))),
                column(width = 4, div(
                  class = "vb-link", title = "Open the Weight Room",
                  onclick = "document.querySelector('a[href=\"#shiny-tab-weightroom\"]').click()",
                  valueBoxOutput("vb_home_dev", width = NULL)))
              ),
              fluidRow(
                column(width = 12, div(class = "howto-strip",
                  tags$span(class = "howto-title", "Build your own graphic:"),
                  tags$span(class = "howto-step",
                            HTML("<b>1</b> tap a chart point to pin its card")),
                  tags$span(class = "howto-step",
                            HTML("<b>2</b> drag to place &middot; corner grip to resize")),
                  tags$span(class = "howto-step",
                            HTML("<b>3</b> tap a highlighted name for the full player card")),
                  tags$span(class = "howto-step",
                            HTML("<b>4</b> download from the chart &mdash; pinned cards included"))
                ))
              ),
              fluidRow(
                box(
                  title = textOutput("class_snap_title"),
                  status = "danger", solidHeader = TRUE, width = 5,
                  htmlOutput("class_snap"),
                  ctx_note("The newest cycle stays open through signing day —
                    counts and average ratings move as commits land and
                    247 re-ranks."),
                  footer = HTML("<em style='color:#888;'>The newest class in
                    your selected window vs the three classes before it.</em>")
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
                box(
                  title = "Pick your team", status = "primary",
                  solidHeader = TRUE, width = 12,
                  div(style = "text-align:center;",
                      lapply(seq_len(nrow(TEAM_CONFIG)), function(i) {
                        actionButton(
                          inputId = paste0("select_",
                                           gsub("-", "_", TEAM_CONFIG$slug[i])),
                          label = img(src = TEAM_CONFIG$logo[i], height = "62px"),
                          style = "background:transparent; border:none; padding:8px 14px;"
                        )
                      })),
                  footer = HTML("<em style='color:#888;'>Clicking a logo sets
                    your team everywhere and opens the Size Lab.</em>")
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
                         title = "Big 12 Beef Board",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("beef_board", height = "640px"),
                                     color = "#0C234B"),
                         ctx_note("'Big 12' means today's 16 members, applied
                           retroactively — Arizona, ASU, Colorado, and Utah
                           joined in 2024, so their earlier classes were
                           signed in the Pac-12.")
                       )),
                column(width = 7,
                       box(
                         title = "Size Over Time vs the Conference",
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("size_trend", height = "300px"),
                                     color = "#0C234B"),
                         ctx_note("Portal transfers exist in the data from 2021",
                                  " on — with 'Commits + transfers' selected,",
                                  " earlier years show HS classes only.")
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
                                         info_btn("info_wr")),
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE,
                         spin(girafeOutput("wr_board", height = "560px"),
                                     color = "#0C234B"),
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
                                  info_btn("info_retention")),
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  spin(girafeOutput("class_retention", height = "560px"),
                       color = "#0C234B"),
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
                               HTML("<em>Always shows the full 2016–2026
                                    history. Tap or hover a class dot for its top-5
                                    signees; click to open the class on
                                    247Sports.</em>")))
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
                         title = "Home-State Talent: Keep the Fence Up",
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
                  title = "The Over/Underachiever Quadrant",
                  status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  spin(girafeOutput("talent_quadrant", height = "520px"),
                              color = "#0C234B"),
                  ctx_note("2020 was the COVID season — most programs played
                    5–10 games, so windows that include 2020 mix shortened
                    seasons into the win percentages."),
                  footer = HTML("<em style='color:#888;'>Follows the class-year
                    window in the control bar (completed seasons only).</em>")
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

      ## RECRUITING MAP (legacy) -----------------------------------------------------
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  title = tagList("Recruiting Pipelines: click any dot for the player card",
                                  info_btn("info_map")),
                  footer = HTML("<span style='color:#888;'>
                    <em>Shaded shapes = your team's state footprint; the
                    compare-to team's pipeline shows in its color.</em></span>"),
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = FALSE,
                  spin(leafletOutput("gridPlot", height = "420px"),
                              color = "#0C234B")),
                box(
                  title = "Distance Traveled from High School to College (Farthest to Closest)",
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = FALSE,
                  DTOutput("summary_stats", height = "230px")
                )
              )
      ),

      ## DISTANCE LAB (legacy) ---------------------------------------------------------
      tabItem(tabName = "compare",
              fluidRow(
                box(
                  fluidRow(
                    column(width = 4,
                           selectInput("show_outliers", label = NULL,
                                       selectize = FALSE, multiple = FALSE,
                                       choices = c("Show Outliers" = "show",
                                                   "Hide Outliers" = "hide"),
                                       selected = "show", width = "100%"))
                  ),
                  title = tagList("Distance Lab: how far does your class travel?",
                                  info_btn("info_distance")),
                  footer = HTML("<span style='color:#888;'>
                    Tap or hover any dot for the recruit card; pin it to open their
                    247Sports page.</span>"),
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = FALSE,
                  spin(
                    girafeOutput("distance_plot", height = "470px"),
                    color = "#0C234B")),
                box(
                  title = "Miles from Home by Position",
                  status = "primary",
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = FALSE,
                  footer = HTML("<span style='color:#888;'>
                    Distances need a high-school hometown, so portal transfers
                    can't appear here.</span>"),
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
                  HTML("
                    <p style='font-size:13px; color:#777; border-left: 3px solid
                      #FFD200; padding-left: 10px;'>
                      Everything here depends on what programs report and what
                      247Sports lists — heights, weights, ratings, and rosters
                      are best-available numbers, not certified measurements.
                      Treat small differences between teams accordingly.</p>
                    <ul style='font-size:14px; line-height:1.7;'>
                      <li><strong>Recruiting classes</strong>: 247Sports commit
                        lists, classes 2016–2026, all 16 Big 12 programs,
                        football and basketball. Portal transfers are included
                        for 2021–2026; the 'Players' control switches between
                        HS commits, commits + transfers, or transfers only.
                        Every chart caption states which pool it shows.</li>
                      <li><strong>Current rosters</strong>: 247Sports team
                        roster pages, scraped June 2026.</li>
                      <li><strong>Season records and SP+</strong>:
                        CollegeFootballData.com, seasons 2016–2025 (football).</li>
                      <li><strong>Sizes</strong>: heights and weights are as
                        listed. Recruiting heights run optimistic — about a
                        quarter of Big 12 signees are listed shorter on the
                        roster than they were as recruits (Weight Room →
                        Reality Check). Treat any listed height as ±1 inch.</li>
                      <li><strong>Locations</strong>: high schools are geocoded,
                        and each result is checked against its claimed state
                        before it can appear on the map. Transfers have no
                        high-school origin, so they don't appear in
                        distance-based views.</li>
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
                    </ul>")
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
                    <a href='mailto:desertdatalabs@gmail.com?subject=Big%2012%20Girth%20Index'>
                    <strong>desertdatalabs@gmail.com</strong></a></p>
                    <p style='font-size:13px; color:#888;'>Custom dashboards,
                    scrapers, and analytics for sports and beyond.</p>")
                ),
                box(
                  title = "What's next", status = "warning",
                  solidHeader = TRUE, width = 6,
                  HTML("
                    <p style='font-size:14px; line-height:1.7;'>
                    The team table (<code>R/team_config.R</code>) already
                    carries a conference column, so expansion is mostly adding
                    rows and re-running the scrapers:</p>
                    <ol style='font-size:14px; line-height:1.7;'>
                      <li><strong>All Power conferences</strong> — add SEC, Big
                        Ten, and ACC schools; every board, map, and lab picks
                        them up. This also completes the home-state retention
                        picture (right now a recruit who leaves for USC or
                        Oregon is invisible).</li>
                      <li><strong>Conference vs conference</strong> — the same
                        size, development, and retention metrics aggregated at
                        the conference level.</li>
                      <li><strong>Player-level outcomes</strong> — team results
                        are in (Talent vs Results tab). The next step is
                        per-player: snap counts, all-conference honors, and
                        draft picks joined to each signee, so classes can be
                        graded on what players became rather than how they
                        were rated at 18.</li>
                    </ol>")
                )
              )
      )

    ), ## end tabItems

    ## site footer: who built this + how to reach us
    div(class = "ddl-footer",
        HTML(paste0(
          "Built by <strong>Desert Data Labs</strong> · feedback, bug reports, ",
          "or want something like this built for your program? ",
          "<a href='mailto:desertdatalabs@gmail.com?subject=Big%2012%20Girth%20Index'>",
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
  girafe_wrap <- function(p, w = 11.5, h = 6.5, name = "big12-girth-index") {
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
  girafe_try <- function(expr, what = "chart") {
    tryCatch(expr, error = function(e) {
      message("render failed (", what, "): ", conditionMessage(e))
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
  ## the precomputed dna is Arizona-vs-ASU, and serving it for another
  ## compare pick would store the wrong chart under that bindCache key.
  at_defaults <- function(pos_input = NULL, cmp_sensitive = FALSE) {
    identical(input$g_team, "arizona") &&
      g_sport() == "football" &&
      identical(as.integer(g_years_d()), as.integer(DEFAULT_YEARS)) &&
      identical(input$g_type %||% "both", "both") &&
      (!cmp_sensitive ||
         identical(input$g_compare %||% "arizona-state", "arizona-state")) &&
      (is.null(pos_input) || pos_input == "All")
  }

  ## chart export filename: team + sport + chart + window, e.g.
  ## "arizona-football-beef-board-2022-2026" (debounced years -- must match
  ## what the chart actually shows)
  png_name <- function(chart) {
    yrs <- g_years_d()
    glue("{input$g_team}-{g_sport()}-{chart}-{yrs[1]}-{yrs[2]}")
  }

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
      url = p247_url(hit$Name[1], hit$Year[1], g_sport(), hit$Type[1]),
      urlLabel = ifelse(identical(hit$Type[1], "Transfer"),
                        "Find their 247Sports profile →",
                        "Full 247Sports profile →"),
      src = "Size + rating as listed by 247Sports at commitment"))
  }, error = function(e) {
    ## a malformed request must never take the session down
    message("player card lookup failed: ", conditionMessage(e))
  }))

  ## ---- TEAM MEMORY: restore the saved team, or ask once ------------------
  observeEvent(input$stored_team, once = TRUE, {
    st <- input$stored_team
    if (!is.null(st) && st %in% TEAM_CONFIG$slug) {
      updateSelectInput(session, "g_team", selected = st)
    } else if (identical(st, "none")) {
      showModal(modalDialog(
        title = NULL, easyClose = TRUE, footer = NULL, size = "l",
        div(style = "text-align:center;",
            h2("Who's your team?",
               style = "font-weight:800; color:#0C234B; margin-top:4px;"),
            p("Saved on this device — change it any time in the bar up top.",
              style = "color:#888;"),
            div(style = "display:flex; flex-wrap:wrap; gap:6px;
                         justify-content:center;",
                lapply(seq_len(nrow(TEAM_CONFIG)), function(i) {
                  actionButton(
                    paste0("pick_", gsub("-", "_", TEAM_CONFIG$slug[i])),
                    label = tagList(
                      img(src = TEAM_CONFIG$logo[i], height = "34px"),
                      div(TEAM_CONFIG$team_name[i],
                          style = "font-size:11px; font-weight:600;")),
                    class = "btn-default",
                    style = "width:108px; padding:8px 2px;")
                })))
      ))
    }
  })
  lapply(seq_len(nrow(TEAM_CONFIG)), function(i) {
    slug <- TEAM_CONFIG$slug[i]
    observeEvent(input[[paste0("pick_", gsub("-", "_", slug))]], {
      updateSelectInput(session, "g_team", selected = slug)
      removeModal()
    })
  })
  ## persist every team change to the device
  observeEvent(input$g_team, {
    session$sendCustomMessage("saveTeam", input$g_team)
  })

  ## info buttons -> sources & methods modals
  lapply(names(INFO_MODALS), function(id) {
    observeEvent(input[[id]], {
      showModal(modalDialog(
        title = INFO_MODALS[[id]]$title,
        HTML(INFO_MODALS[[id]]$body),
        easyClose = TRUE, footer = modalButton("Got it")
      ))
    })
  })

  ## the collapsed bar's one-line summary of every global setting
  output$cb_summary <- renderUI({
    req(input$g_team, input$g_years)
    logo <- function(slug) img(src = TEAM_CONFIG$logo[match(slug, TEAM_CONFIG$slug)])
    type_word <- switch(input$g_type %||% "both",
                        commit = "HS commits",
                        transfer = "transfers only",
                        "commits + transfers")
    tags$span(class = "cb-summary-text",
         logo(input$g_team), team_label(input$g_team),
         if (!is.null(g_cmp())) tags$span(class = "cb-dim", "vs"),
         if (!is.null(g_cmp())) logo(g_cmp()),
         tags$span(class = "cb-dim",
              glue("· {str_to_title(g_sport())} · ",
                   "{input$g_years[1]}–{input$g_years[2]} · {type_word}")))
  })

  ## ---- global reactives ------------------------------------------------------
  g_sport <- reactive(tolower(input$g_sport))

  ## compare slug or NULL (none / same as main team)
  g_cmp <- reactive({
    if (is.null(input$g_compare) || input$g_compare == "" ||
        input$g_compare == input$g_team) NULL else input$g_compare
  })

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
    if (g_sport() == "football") roster_football else roster_basketball
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
  })

  ## ---- navigation -------------------------------------------------------------
  ## logo quick-pick: set the global team + open Size Lab
  lapply(seq_len(nrow(TEAM_CONFIG)), function(i) {
    slug <- TEAM_CONFIG$slug[i]
    btn_id <- paste0("select_", gsub("-", "_", slug))
    observeEvent(input[[btn_id]], {
      updateSelectInput(session, "g_team", selected = slug)
      updateTabItems(session, "tabs", "sizelab")
    })
  })

  ## hero buttons
  observeEvent(input$go_sizelab, updateTabItems(session, "tabs", "sizelab"))
  observeEvent(input$go_beef, updateTabItems(session, "tabs", "beef"))
  observeEvent(input$go_wr, updateTabItems(session, "tabs", "weightroom"))
  observeEvent(input$go_eras, updateTabItems(session, "tabs", "eras"))
  observeEvent(input$go_warroom, updateTabItems(session, "tabs", "brief"))

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
  ## the hero carries the selected team's logo (the app is ABOUT your team)
  output$hero_team <- renderUI({
    req(input$g_team)
    div(style = "display:flex; align-items:center; gap:16px;",
        img(src = TEAM_CONFIG$logo[match(input$g_team, TEAM_CONFIG$slug)],
            height = "64px",
            style = "background:white; border-radius:10px; padding:5px;"),
        h1(glue("{team_label(input$g_team)} — Big 12 Girth Index")))
  })

  ## current-status boxes for the SELECTED team (season-proof: everything is
  ## derived from the window + max class year, never hardcoded)
  output$vb_home_rank <- renderValueBox({
    board <- team_size_summary(size_window()) %>% arrange(desc(AvgWeight))
    rk <- which(board$School == input$g_team)
    val <- if (length(rk) == 1) paste0("#", rk, " of ", nrow(board)) else "—"
    valueBox(val,
             glue("{team_label(input$g_team)} beef rank, ",
                  "{input$g_years[1]}–{input$g_years[2]} window"),
             icon = icon("weight-hanging"), color = "navy")
  })
  output$vb_home_class <- renderValueBox({
    snap <- class_snapshot(size_window(), input$g_team)
    if (is.null(snap)) {
      return(valueBox("—", "No players in this window",
                      icon = icon("star"), color = "light-blue"))
    }
    ## rank the newest class among the conference's newest classes
    yr <- snap$year
    cls_rank <- size_window() %>%
      filter(Year == yr) %>%
      group_by(School) %>%
      summarize(r = mean(Ranking, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(r))
    rk <- which(cls_rank$School == input$g_team)
    valueBox(glue("{snap$avg_rating}"),
             glue("Class of {yr} avg rating — #{rk} of {nrow(cls_rank)} ",
                  "in the Big 12, {snap$blue} blue-chip",
                  "{ifelse(snap$blue == 1, '', 's')}"),
             icon = icon("star"), color = "light-blue")
  })
  output$vb_home_dev <- renderValueBox({
    if (is.null(roster_now())) {
      return(valueBox("—", "Weight room data not scraped yet",
                      icon = icon("dumbbell"), color = "orange"))
    }
    gains <- wr_data_r() %>%
      group_by(School) %>%
      summarize(g = mean(GainPerYr), .groups = "drop") %>%
      arrange(desc(g))
    rk <- which(gains$School == input$g_team)
    val <- if (length(rk) == 1) {
      glue("+{round(gains$g[rk], 1)} lbs/yr")
    } else "—"
    valueBox(val,
             glue("{team_label(input$g_team)} weight room: gain per year ",
                  "{ifelse(length(rk) == 1, paste0('(#', rk, ' of ',
                  nrow(gains), ')'), '')}"),
             icon = icon("dumbbell"), color = "orange")
  })

  ## class snapshot card (newest class in the window vs the 3 before it)
  output$class_snap_title <- renderText({
    snap <- class_snapshot(size_window(), input$g_team)
    if (is.null(snap)) return("Class snapshot")
    glue("{team_label(input$g_team)} Class of {snap$year} at a glance")
  })
  output$class_snap <- renderUI({
    req(input$g_team)
    snap <- class_snapshot(size_window(), input$g_team)
    validate(need(!is.null(snap), "No commits in this window."))
    delta_html <- function(d, suffix = "") {
      if (is.na(d)) return("")
      cls <- if (d >= 0) "snap-delta-up" else "snap-delta-down"
      glue("<span class='{cls}'>{ifelse(d >= 0, '+', '')}{d}{suffix}</span>")
    }
    HTML(glue(
      "<div class='row'>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{snap$n}</div>",
      "<div class='lbl'>Players added</div></div>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{snap$avg_rating}</div>",
      "<div class='lbl'>Avg rating {delta_html(snap$d_rating)}</div></div>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{snap$blue}</div>",
      "<div class='lbl'>Blue-chips (90+)</div></div>",
      "<div class='col-xs-3 snap-stat'><div class='num'>{snap$pct_instate}%</div>",
      "<div class='lbl'>In-state</div></div>",
      "</div>",
      "<div class='row'>",
      "<div class='col-xs-4 snap-stat'><div class='num'>{snap$avg_height}</div>",
      "<div class='lbl'>Avg height</div></div>",
      "<div class='col-xs-4 snap-stat'><div class='num'>{snap$avg_weight}</div>",
      "<div class='lbl'>Avg lbs {delta_html(snap$d_weight)}</div></div>",
      "<div class='col-xs-4 snap-stat'><div class='num' style='font-size:16px; padding-top:8px;'>",
      "{snap$top_name}</div>",
      "<div class='lbl'>Headliner ({snap$top_pos}, {snap$top_rating})</div></div>",
      "</div>"))
  })

  output$home_points_title <- renderText({
    glue("{team_label(input$g_team)} insights — ",
         "{str_to_title(g_sport())}, {input$g_years[1]}–{input$g_years[2]}")
  })
  output$home_points <- renderUI({
    req(input$g_team)
    pts <- make_talking_points(size_window(), input$g_team, g_sport())
    validate(need(length(pts) > 0, "Not enough data in this window."))
    HTML(paste0("<ul class='talking-points'>",
                paste0("<li>", head(pts, 4), "</li>", collapse = ""),
                "</ul>"))
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
    board <- team_size_summary(size_window()) %>% arrange(desc(AvgWeight))
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

  output$beef_board <- renderGirafe({
    req(input$size_metric, input$size_pos, input$size_source)
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

  output$size_trend <- renderGirafe({
    req(input$size_metric, input$size_pos)
    trend_data <- filter_pos(size_window(), input$size_pos)
    validate(need(
      nrow(dplyr::filter(trend_data, School == input$g_team)) > 0,
      "No commits for this team + position filter."))
    girafe_wrap(
      plot_size_trend(size_window(), input$g_team, g_sport(),
                      metric = input$size_metric, pos_filter = input$size_pos,
                      compare_slug = g_cmp(), players_note = players_lab()),
      w = 10.5, h = 4.5, name = png_name("size-trend"))
  })

  output$h2h_plot <- renderGirafe({
    req(input$size_source)
    validate(need(!is.null(g_cmp()),
                  "Pick a 'Compare to' team in the top bar."))
    girafe_wrap(
      plot_head_to_head(beef_source_data(), input$g_team, g_cmp(), g_sport(),
                        source_label = beef_source_label(),
                        players_note = players_lab()),
      w = 10.5, h = 4.5,
      name = png_name(glue("h2h-vs-{g_cmp()}")))
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
    gains <- wr_data_r() %>%
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
                       "(Big 12: {hc$pct_shrunk_conf}%)"),
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
    girafe_wrap(
      plot_weight_room_board(wr_data_r(), input$g_team, g_sport(),
                             compare_slug = g_cmp(),
                             direction = input$wr_direction %||% "gain"),
      w = 8, h = 8,
      name = png_name(ifelse((input$wr_direction %||% "gain") == "gain",
                             "weight-room", "cut-room")))
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
    girafe_wrap(
      plot_weight_room_players(wr_data_r(), input$g_team, g_sport(),
                               direction = input$wr_direction %||% "gain"),
      w = 8, h = 8,
      name = png_name(ifelse((input$wr_direction %||% "gain") == "gain",
                             "biggest-gainers", "biggest-slim-downs")))
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
    HTML(glue("<em style='color:#888;'>{hc$pct_shrunk_conf}% of matched Big 12
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
    girafe_wrap(plot_era_position_mix(size_all(), input$g_team, g_sport(),
                                      players_note = players_lab()),
                w = 8.5, h = 6.2,
                name = glue("{input$g_team}-{g_sport()}-era-position-mix"))
  })

  output$era_table <- renderDT({
    validate(need(nrow(era_data()) > 0, "No commits for this team."))
    datatable(era_summary_table(size_all(), input$g_team, g_sport()),
              options = list(dom = "t", ordering = FALSE, scrollX = TRUE),
              rownames = FALSE)
  })

  ## ---- ANALYST BRIEF (DEFENSIVE WAR ROOM) ----------------------------------------------------
  ## the newest cycle's additions (HS + portal, ALL types regardless of the
  ## global radio) that aren't on the 247 roster page yet -- the bodies that
  ## are arriving (e.g. June portal adds)
  incoming_adds <- reactive({
    req(input$g_team)
    if (g_sport() != "football" || is.null(roster_now())) return(NULL)
    nkey <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))
    ros_keys <- nkey(roster_now()$Name[roster_now()$School == input$g_team])
    size_football %>%
      filter(School == input$g_team, Year == SIZE_YEARS[2],
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
    girafe_wrap(plot_roster_335(roster_now(), input$g_team,
                                incoming = incoming_adds(),
                                incoming_label = paste0("'", SIZE_YEARS[2] %% 100,
                                                        " ADDS"),
                                proj_gain = proj_gain_r()),
                w = 8.5, h = 6.4, name = png_name("335-fit-board"))
  })

  output$def_profile <- renderGirafe({
    validate(need(g_sport() == "football",
                  "The 3-3-5 lens applies to football — switch sport above."))
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    girafe_wrap(plot_def_size_profile(roster_size(), input$g_team,
                                      incoming = incoming_adds(),
                                      proj_gain = proj_gain_r()),
                w = 8.5, h = 6.4, name = png_name("def-bodies-335"))
  })

  output$roster_constr <- renderGirafe({
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    validate(need(
      nrow(dplyr::filter(roster_now(), School == input$g_team)) > 0,
      "No roster rows for this team."))
    girafe_wrap(
      plot_roster_construction(roster_now(), input$g_team, g_sport()),
      w = 8.5, h = 6.4, name = png_name("roster-construction"))
  })

  output$state_retention <- renderGirafe({
    st <- team_state(input$g_team)
    validate(need(
      nrow(dplyr::filter(size_window(), State == st)) > 0,
      glue("No {st} high-school commits in this window.")))
    girafe_wrap(
      plot_state_retention(size_window(), input$g_team, g_sport(),
                           compare_slug = g_cmp(),
                           players_note = players_lab()),
      w = 8.5, h = 6.4, name = png_name("state-retention"))
  })

  ## class retention: % of each signing class still on the roster
  output$class_retention <- renderGirafe({
    validate(need(!is.null(roster_now()),
                  "Run scripts/scrapeRosters.R to add current rosters."))
    src <- if (g_sport() == "football") size_football else size_basketball
    girafe_wrap(
      plot_class_retention(src %>% dplyr::filter(Type == "Commit"),
                           roster_now(), input$g_team,
                           compare_slug = g_cmp()),
      w = 8, h = 9, name = png_name("class-retention"))
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
    girafe_wrap(
      plot_talent_results(ts_window,
                          size_football %>% filter(Year <= max(ts_window$year)),
                          input$g_team, compare_slug = g_cmp()),
      w = 10.5, h = 6.2,
      name = png_name("talent-vs-results-quadrant"))
  })

  output$team_scoreboard <- renderGirafe({
    validate(need(g_sport() == "football",
                  "Season outcomes are football-only for now."))
    validate(need(!is.null(team_seasons),
                  "Run scripts/fetchOutcomes.R (needs a free CFBD key) to add season records."))
    girafe_wrap(
      plot_team_scoreboard(team_seasons,
                           size_football %>% filter(Year <= max(team_seasons$year)),
                           input$g_team),
      w = 10.5, h = 4.4,
      name = glue("{input$g_team}-season-scoreboard"))
  })

  ## ---- LEGACY: filtered data + map + distance plots ---------------------------------------
  filtered_data <- reactive({
    req(input$g_team, input$g_years)
    ## inputs reach this SQL as strings -- accept only known values
    ## (a forged websocket message must not become a query fragment)
    validate(need(input$g_team %in% TEAM_CONFIG$slug, "Unknown team."))
    sp <- g_sport()
    validate(need(sp %in% c("football", "basketball"), "Unknown sport."))
    db_table <- if (sp == "basketball") "recruit_class_basketball" else "recruit_class_football"

    ## transfers carry no HS location, so they only matter here when included
    ## (NULL-fallback matches the radio's startup default "both")
    type_clause <- switch(input$g_type %||% "both",
                          commit = " AND Type = 'Commit'",
                          transfer = " AND Type = 'Transfer'",
                          "")
    geting_data <- paste0(
      "Select * from ", db_table, " where sport = '", sp,
      "' AND School = '", input$g_team, "'", type_clause,
      " AND Year >= ", as.integer(input$g_years[1]),
      " AND Year <= ", as.integer(input$g_years[2]),
      " ORDER BY Ranking, NationalRank desc, StateRank desc, PositionRank desc, Name")

    all_data <- safe_query(conn, geting_data)
    validate(need(nrow(all_data) > 0, "No players in this window."))

    all_data$lat <- as.numeric(all_data$lat)
    all_data$long <- as.numeric(all_data$long)
    all_data$college_lat <- as.numeric(all_data$college_lat)
    all_data$college_long <- as.numeric(all_data$college_long)
    all_data$Ranking <- as.numeric(all_data$Ranking)
    all_data$NationalRank <- as.numeric(all_data$NationalRank)
    all_data$PositionRank <- as.numeric(all_data$PositionRank)
    all_data$StateRank <- as.numeric(all_data$StateRank)

    ## miles from high school to campus
    big12_data <- all_data %>%
      mutate(disFromHS_m =
               distGeo(p1 = cbind(long, lat),
                       p2 = cbind(college_long, college_lat)))
    meters_per_mile <- 1609.34
    big12_data_wDis <- big12_data %>%
      mutate(miles_away = round(disFromHS_m / meters_per_mile, 0))

    ## fix top 150 national ranks
    big12_data_wDis <- big12_data_wDis %>%
      mutate(NationalRank = ifelse(NationalRank > 150, NA, NationalRank))

    big12_data_wDis %>%
      mutate(University = pretty_university(School))
  })

  ## v3.5: pipeline map built from the prepped size data (main + compare
  ## team footprints in team colors); scripts/map.R kept for reference
  output$gridPlot <- renderLeaflet({
    req(input$g_team, input$g_years)
    team_w <- size_window() %>% filter(School == input$g_team)
    validate(need(nrow(team_w) > 0, "No recruits in this window."))
    ## say on the map itself how many window players CAN'T be mapped
    ## (transfers have no HS hometown; a few HS commits lack geocodes)
    n_unmapped <- sum(is.na(suppressWarnings(as.numeric(team_w$lat))) |
                        is.na(suppressWarnings(as.numeric(team_w$long))))
    build_pipeline_map(size_window(), input$g_team, g_sport(),
                       compare_slug = g_cmp(), n_unmapped = n_unmapped)
  })

  ## v4.4: themed interactive rebuild (the sourced scripts/box_plot.R is
  ## retired); transfers can't appear -- no high-school origin to measure from
  output$box_plot <- renderGirafe({
    req(input$g_team, input$g_years)
    d <- size_window() %>%
      filter(School == input$g_team, !is.na(miles_away))
    validate(need(nrow(d) > 0,
                  "No recruits with mapped high schools in this window."))
    girafe_wrap(plot_distance_box(size_window(), input$g_team, g_sport()),
                w = 10.5, h = 4.2, name = png_name("distance-box"))
  })

  ## v3.7: interactive distance scatter (hover = recruit card, click = 247
  ## profile); the sourced scripts/plot.R version is retired
  output$distance_plot <- renderGirafe({
    req(input$g_team, input$g_years, input$show_outliers)
    d <- size_window() %>%
      filter(School == input$g_team, !is.na(miles_away))
    validate(need(nrow(d) > 0, "No recruits with mapped high schools in this window."))
    girafe_wrap(plot_distance_lab(size_window(), input$g_team, g_sport(),
                                  show_outliers = input$show_outliers),
                name = png_name("distance-lab"))
  })

  output$summary_stats <- renderDT({
    req(input$g_team, input$g_years)
    if (nrow(filtered_data()) == 0) {
      return(data.frame(Message =
        "No recruits found for the selected filters. Please adjust your selections."))
    }
    d <- filtered_data() %>%
      select(Name, miles_away, Location, University, School_City, Ranking,
             NationalRank, Position, Height, Weight, Year) %>%
      arrange(desc(miles_away))

    datatable(
      as.data.frame(d),
      colnames = c("Recruit", "Distance Traveled (miles)", "From", "To", "City",
                   "247Sports Ranking", "National Ranking", "Position",
                   "Height", "Weight", "Year"),
      options = list(pageLength = 10, lengthChange = FALSE),
      rownames = FALSE)
  })

}

shinyApp(ui, server)
