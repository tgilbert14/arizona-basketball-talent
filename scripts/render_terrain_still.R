# The Conference, From Above — data-true terrain still (test render)
# Mesa height = 2026 roster mass (lbs) per Big 12 school, from recruiting.db.
# Layout: deterministic jittered grid (synthetic for the test; geocoded in session 2).
# Output: work_terrain_still.png (16:9) — the conditioning frame for the AI dive test.

suppressMessages({ library(rayshader) })
set.seed(12)

d <- read.csv("work_terrain_mass.csv", stringsAsFactors = FALSE)
d <- d[order(-d$mass_lbs), ]
n <- nrow(d)

W <- 960; H <- 540                      # heightmap resolution (render upscales)
grid <- expand.grid(x = seq_len(W), y = seq_len(H))

# --- layout: 4x4 jittered grid, tallest mesas toward the back for aerial depth ---
cols <- 4; rows <- ceiling(n / cols)
cx <- rep(seq(0.16, 0.84, length.out = cols), times = rows)[1:n] * W
cy <- rep(seq(0.72, 0.24, length.out = rows), each = cols)[1:n] * H   # tall = far (back)
cx <- cx + runif(n, -0.045, 0.045) * W
cy <- cy + runif(n, -0.05, 0.05) * H

# --- heights: mass -> meters-ish; floor basin so mesas rise from a living desert ---
h_peak <- 60 + 240 * (d$mass_lbs - min(d$mass_lbs)) / diff(range(d$mass_lbs))  # 60..300
sigma  <- (14 + 10 * sqrt(d$players / max(d$players))) * (W / 400)             # footprint

hm <- matrix(0, nrow = W, ncol = H)
xs <- matrix(grid$x, nrow = W); ys <- matrix(grid$y, nrow = W)
for (i in seq_len(n)) {
  d2 <- (xs - cx[i])^2 + (ys - cy[i])^2
  # true mesa: steep superellipse walls + clamped plateau top with rim erosion
  bump <- h_peak[i] * exp(-(d2 / (2 * sigma[i]^2))^2.6)
  cap  <- 0.88 * h_peak[i]
  bump <- pmin(bump, cap + (bump - cap) * 0.12)          # flatten the crown
  hm <- pmax(hm, bump)
}
# desert basin: dunes, braided washes, talus noise on slopes
fx   <- outer(sin(seq(0, 9, length.out = W)), cos(seq(0, 7, length.out = H))) * 5
wash1 <- 7 * exp(-((ys - (0.55 * H + 34 * sin(xs / 80)))^2) / (2 * (H / 55)^2))
wash2 <- 5 * exp(-((ys - (0.30 * H + 26 * cos(xs / 65)))^2) / (2 * (H / 70)^2))
slope <- hm > 20 & hm < 0.8 * max(hm)
ero   <- matrix(rnorm(W * H, 0, 1.3), W, H) * slope       # rough talus on mesa walls
hm <- hm + 14 + fx - wash1 - wash2 + ero + matrix(rnorm(W * H, 0, 0.8), W, H)

# --- render: dawn desert grade, low 3/4 aerial, terrain past every frame edge ---
pal <- grDevices::colorRampPalette(c("#6e5138", "#96714a", "#b58c5a", "#cba86f",
                                     "#dec48f", "#ecd9ae"))(256)
hm |>
  height_shade(texture = pal) |>
  add_shadow(lamb_shade(hm, sunaltitude = 14, sunangle = 60, zscale = 1), 0.45) |>
  add_shadow(ambient_shade(hm, zscale = 1), 0.30) |>
  plot_3d(hm, zscale = 1.05, fov = 58, theta = 20, phi = 31, zoom = 0.47,
          solid = FALSE, background = "#e8d9bd", windowsize = c(1600, 900))
Sys.sleep(1)
render_camera(theta = 20, phi = 31, zoom = 0.47, fov = 58)
render_snapshot("work_terrain_still.png", software_render = TRUE,
                width = 2688, height = 1512)
cat("written: work_terrain_still.png\n")
