## ---------------------------------------------------------------------------
## girth_analysis.R
## Standalone insight pack: renders the Size Lab / Weight Room / Coach Era
## visualizations to insights/ as PNGs and prints talking points.
## Run from the project root:  Rscript scripts/girth_analysis.R
## The Shiny app uses the exact same builders (R/girth_plots.R).
## ---------------------------------------------------------------------------

source(here::here("R", "coach_eras.R"))      # era config (needed by prep)
source(here::here("R", "functions.R"))       # libraries + safe_query
source(here::here("R", "team_config.R"))     # TEAM_CONFIG + lookups
source(here::here("R", "girth_functions.R")) # parsing + metrics
source(here::here("R", "girth_plots.R"))     # plot builders

out_dir <- here::here("insights")
dir.create(out_dir, showWarnings = FALSE)

## logos resolve relative to the project root when running this script
logo_prefix <- paste0(here::here("www"), "/")

## the featured matchup for the static pack
TEAM <- "arizona"
RIVAL <- "arizona-state"

## ---- load + prep both sports ----------------------------------------------
## HS commits only -- the db also carries portal transfers for refreshed years
conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
fb <- safe_query(conn, "SELECT * FROM recruit_class_football WHERE Type = 'Commit'") %>%
  prep_size_data("football")
bb <- safe_query(conn, "SELECT * FROM recruit_class_basketball WHERE Type = 'Commit'") %>%
  prep_size_data("basketball")
roster_fb <- if ("roster_football" %in% dbListTables(conn)) {
  dbGetQuery(conn, "SELECT * FROM roster_football")
} else NULL
dbDisconnect(conn)

cat("Football rows:", nrow(fb), "| Basketball rows:", nrow(bb), "\n")

save_plot <- function(p, file, w = 12, h = 8) {
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 150,
         bg = "white")
  cat("saved:", file, "\n")
}

## ---- 1. Body Map -----------------------------------------------------------
save_plot(plot_body_map(fb, TEAM, "football"),
          "01_body_map_arizona_football.png", w = 12, h = 8.5)

## ---- 2. Beef Boards (compare-team highlighted) ------------------------------
save_plot(plot_beef_board(fb, TEAM, "football", metric = "AvgWeight",
                          pos_filter = "All", compare_slug = RIVAL,
                          logo_prefix = logo_prefix),
          "02_beef_board_football_weight.png", w = 11, h = 9)

save_plot(plot_beef_board(fb, TEAM, "football", metric = "AvgWeight",
                          pos_filter = "Trenches (OL + DL/Edge)",
                          compare_slug = RIVAL, logo_prefix = logo_prefix),
          "03_beef_board_football_trenches.png", w = 11, h = 9)

## ---- 3. Size over time (rival line + coach-era marks) -----------------------
save_plot(plot_size_trend(fb, TEAM, "football", metric = "AvgWeight",
                          pos_filter = "Trenches (OL + DL/Edge)",
                          compare_slug = RIVAL),
          "04_trend_arizona_trench_weight.png", w = 12, h = 7.5)

## ---- 4. Position DNA (rival mean diamonds) ----------------------------------
save_plot(plot_position_dna(fb, TEAM, "football", compare_slug = RIVAL),
          "05_position_dna_arizona.png", w = 12.5, h = 8)

## ---- 5. Head to head: Territorial Cup ---------------------------------------
save_plot(plot_head_to_head(fb, TEAM, RIVAL, "football"),
          "06_h2h_arizona_vs_asu.png", w = 11.5, h = 8)

## ---- 6. Basketball versions --------------------------------------------------
save_plot(plot_body_map(bb, TEAM, "basketball"),
          "07_body_map_arizona_basketball.png", w = 12, h = 8.5)

save_plot(plot_beef_board(bb, TEAM, "basketball", metric = "AvgHeight",
                          pos_filter = "All", compare_slug = RIVAL,
                          logo_prefix = logo_prefix),
          "08_beef_board_basketball_height.png", w = 11, h = 9)

## ---- 7. Weight Room Effect (needs roster_football from scrapeRosters.R) -----
if (!is.null(roster_fb)) {
  wr <- weight_room_data(fb, roster_fb)
  cat("Weight-room matched players:", nrow(wr), "\n")

  save_plot(plot_weight_room_board(wr, TEAM, "football",
                                   compare_slug = RIVAL,
                                   logo_prefix = logo_prefix),
            "09_weight_room_board.png", w = 11.5, h = 9)
  save_plot(plot_weight_room_players(wr, TEAM, "football"),
            "10_weight_room_arizona.png", w = 11.5, h = 9)
  save_plot(plot_height_check(wr, TEAM, "football"),
            "11_height_reality_check.png", w = 11.5, h = 7)
  print(height_check_stats(wr, TEAM))
} else {
  cat("roster_football table not found; run scripts/scrapeRosters.R first\n")
}

## ---- 8. Coach Eras ------------------------------------------------------------
save_plot(plot_era_timeline(fb, TEAM, "football", metric = "AvgRating"),
          "12_era_timeline_rating.png", w = 12, h = 7.5)
save_plot(plot_era_timeline(fb, TEAM, "football", metric = "PctInState"),
          "13_era_timeline_instate.png", w = 12, h = 7.5)
save_plot(plot_era_position_mix(fb, TEAM, "football"),
          "14_era_position_mix.png", w = 12, h = 7.5)
save_plot(plot_era_timeline(fb, TEAM, "football", metric = "BlueChips"),
          "15_era_timeline_bluechips.png", w = 12, h = 7.5)

## ---- 8b. Analyst Brief ---------------------------------------------------------
if (!is.null(roster_fb)) {
  save_plot(plot_roster_construction(roster_fb, TEAM, "football"),
            "16_roster_construction.png", w = 10.5, h = 8)
}
save_plot(plot_state_retention(fb, TEAM, "football", compare_slug = RIVAL,
                               logo_prefix = logo_prefix),
          "17_state_retention.png", w = 10.5, h = 8.5)

cat("\n========= ANALYST NOTES (Arizona football) =========\n")
cat(paste0("* ", analyst_notes(fb, roster_fb, TEAM, "football")), sep = "\n")

cat("\n========= ERA REPORT CARD (Arizona football) =========\n")
print(as.data.frame(era_summary_table(fb, TEAM, "football")))

## ---- 9. Talent vs Results (needs team_seasons_football from fetchOutcomes.R)
conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
if ("team_seasons_football" %in% dbListTables(conn)) {
  ts <- dbGetQuery(conn, "SELECT * FROM team_seasons_football")
  fb_all <- safe_query(conn, "SELECT * FROM recruit_class_football") %>%
    prep_size_data("football") %>%
    filter(Year <= max(ts$year))
  save_plot(plot_talent_results(ts, fb_all, TEAM, compare_slug = RIVAL),
            "18_talent_vs_results.png", w = 12, h = 7.5)
  save_plot(plot_team_scoreboard(ts, fb_all, TEAM),
            "19_arizona_scoreboard.png", w = 12, h = 5.5)
} else {
  cat("team_seasons_football not found; run scripts/fetchOutcomes.R\n")
}
dbDisconnect(conn)

## ---- 10. Talking points ---------------------------------------------------------
cat("\n========= ARIZONA FOOTBALL TALKING POINTS =========\n")
tp <- make_talking_points(fb, TEAM, "football")
cat(paste0("* ", tp, collapse = "\n"), "\n")
writeLines(paste0("* ", tp), file.path(out_dir, "talking_points_football.txt"))

cat("\n========= ARIZONA BASKETBALL TALKING POINTS =========\n")
tpb <- make_talking_points(bb, TEAM, "basketball")
cat(paste0("* ", tpb, collapse = "\n"), "\n")
writeLines(paste0("* ", tpb), file.path(out_dir, "talking_points_basketball.txt"))

cat("\nDone. PNGs in insights/\n")
