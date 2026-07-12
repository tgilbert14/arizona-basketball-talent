## ===========================================================================
## makeOgCard.R -- render the branded 1200x630 social-share card to
## docs/og-card.png (the og:image / twitter:image the landing page points at).
##
## One-time but regenerable: pure ggplot2, no external assets, no fonts to
## install, and NO scraped numbers on the card -- understated by design, so it
## never goes stale or overclaims. House navy background, one cardinal accent
## rule, plain default sans (Rubik-adjacent). Run from the repo root:
##   Rscript scripts/makeOgCard.R
## ===========================================================================

suppressMessages({
  library(ggplot2)
})

navy     <- "#0C234B"   # house background
cardinal <- "#AB0520"   # the single accent
mist     <- "#c3d0e6"   # muted blue-white for secondary text
dim      <- "#7f93b4"   # dimmer still for the footer

title_txt <- "Big 12 Girth Index"
sub_txt   <- paste0("Size, talent and recruiting flow -- all 16 programs, ",
                    "updated nightly")
foot_txt  <- "Desert Data Labs   |   girthindex.desertdatalab.com"

card <- ggplot() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
  ## the one cardinal accent: a short rule between wordmark and tagline
  annotate("segment", x = 6, xend = 33, y = 50, yend = 50,
           colour = cardinal, linewidth = 3, lineend = "round") +
  annotate("text", x = 6, y = 67, hjust = 0, vjust = 0.5,
           label = title_txt, colour = "white", fontface = "bold",
           family = "sans", size = 20) +
  annotate("text", x = 6, y = 38, hjust = 0, vjust = 0.5,
           label = sub_txt, colour = mist, family = "sans", size = 7.5) +
  annotate("text", x = 6, y = 13, hjust = 0, vjust = 0.5,
           label = foot_txt, colour = dim, family = "sans", size = 5.5) +
  theme_void() +
  theme(plot.background  = element_rect(fill = navy, colour = navy),
        panel.background = element_rect(fill = navy, colour = navy),
        plot.margin = margin(0, 0, 0, 0))

out <- here::here("docs", "og-card.png")
## width x height x dpi = 12 x 6.3 x 100 -> exactly 1200 x 630 px
ggsave(out, card, width = 12, height = 6.3, dpi = 100, bg = navy)
cat("Wrote", out, "\n")
