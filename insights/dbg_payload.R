source(here::here("R", "coach_eras.R"))
source(here::here("R", "functions.R"))
source(here::here("R", "team_config.R"))
source(here::here("R", "girth_functions.R"))
source(here::here("R", "girth_plots.R"))
conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
fb <- safe_query(conn, "SELECT * FROM recruit_class_football") %>% prep_size_data("football")
dbDisconnect(conn)
## the deployed default view: last-5 window, commits + transfers
w <- fb %>% filter(Year >= 2022, Year <= 2026)
cat("window rows:", nrow(w), "\n")
t0 <- Sys.time()
g <- girafe(ggobj = plot_body_map(w, "arizona", "football",
                                  logo_path = here::here("www", "arizona.png")),
            width_svg = 11.5, height_svg = 7)
t1 <- Sys.time()
cat("render time:", round(as.numeric(t1 - t0, units = "secs"), 1), "s\n")
cat("svg payload:", round(nchar(g$x$html) / 1e6, 2), "MB\n")
cat("peak object sizes ok; mem used:", round(sum(gc()[, 2]), 0), "MB\n")
