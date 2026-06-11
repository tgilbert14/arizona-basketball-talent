## ---------------------------------------------------------------------------
## precomputeDefaults.R -- render the DEFAULT view's heaviest charts to
## precomputed/*.rds at deploy time, so a fresh visitor's first Size Lab
## paint costs a readRDS instead of a ~3s ggplot/SVG build.
##
## Defaults mirrored from app.R: Arizona, football, the 4-class roster
## window, commits + transfers, all positions. Desktop + phone canvases.
##
## Run after every data refresh (refreshAll.R calls this), then redeploy.
## The app serves these ONLY when every control sits at its default, and
## falls back to live rendering if the files are missing or stale.
## ---------------------------------------------------------------------------

source(here::here("R", "coach_eras.R"))
source(here::here("R", "functions.R"))
source(here::here("R", "team_config.R"))
source(here::here("R", "girth_functions.R"))
source(here::here("R", "girth_plots.R"))

conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
fb <- safe_query(conn, "SELECT * FROM recruit_class_football") %>%
  prep_size_data("football")
dbDisconnect(conn)

## the app's startup defaults (keep in sync with app.R)
TEAM <- "arizona"
yr_max <- max(fb$Year)
DEFAULT_YEARS <- c(yr_max - 3, yr_max)
w <- fb %>% filter(Year >= DEFAULT_YEARS[1], Year <= DEFAULT_YEARS[2])
note <- "HS commits + portal transfers"
png_base <- glue("{TEAM}-football")
yrs_tag <- glue("{DEFAULT_YEARS[1]}-{DEFAULT_YEARS[2]}")

dir.create(here::here("precomputed"), showWarnings = FALSE)
out <- function(g, file) {
  saveRDS(g, here::here("precomputed", file))
  cat("  wrote", file, "(",
      round(file.size(here::here("precomputed", file)) / 1024), "KB )\n")
}

cat("Precomputing default view:", TEAM, "football",
    paste(DEFAULT_YEARS, collapse = "-"), "\n")

body_p <- plot_body_map(w, TEAM, "football", players_note = note,
                        logo_path = here::here("www", "arizona.png"))
out(girafe_build(body_p, h = 7,
                 name = glue("{png_base}-body-map-{yrs_tag}")),
    "body_map_desktop.rds")
out(girafe_build(body_p, h = 7, phone = TRUE,
                 name = glue("{png_base}-body-map-{yrs_tag}")),
    "body_map_phone.rds")

dna_p <- plot_position_dna(w, TEAM, "football",
                           compare_slug = "arizona-state",
                           players_note = note)
out(girafe_build(dna_p, w = 9.5, h = 6,
                 name = glue("{png_base}-position-dna-{yrs_tag}")),
    "dna_desktop.rds")
out(girafe_build(dna_p, w = 9.5, h = 6, phone = TRUE,
                 name = glue("{png_base}-position-dna-{yrs_tag}")),
    "dna_phone.rds")

cat("Done. Commit precomputed/ and redeploy.\n")
