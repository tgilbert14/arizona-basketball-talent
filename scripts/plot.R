# get data
rec_data <- filtered_data()

# ## for testing
# rec_data <<- rec_data
# stop()

college_label <- rec_data$University[1]

ordered_recruits <- rec_data %>%
  arrange(Year, desc(miles_away), Name)

all_recruits <- rec_data %>% 
  dplyr::filter(!is.na(Name))

all_recruits <- unique(all_recruits)

# plot individual miles_aways with year on x-axis -->
# compute average by year
avg_dis <- all_recruits %>%
  group_by(Year) %>%
  summarize(Average_Distance = mean(miles_away, na.rm = TRUE))

# Count recruits per miles-year combo
dot_sizes <- all_recruits %>%
  group_by(Year, miles_away) %>%
  summarize(count = n(), .groups = "drop")

# Merge with main data
all_recruits <- all_recruits %>%
  left_join(dot_sizes, by = c("Year", "miles_away"))

top_recruits <- all_recruits %>%
  group_by(Year) %>%
  arrange(desc(miles_away)) %>%
  slice_max(order_by = miles_away, n = 1, with_ties = TRUE) %>%
  ungroup() %>%
  mutate(label = paste0(Name, "\n(", miles_away, " mi)"))

bottom_recruits <- all_recruits %>%
  group_by(Year) %>%
  arrange(miles_away) %>%
  slice_max(order_by = desc(miles_away), n = 1, with_ties = TRUE) %>%
  ungroup() %>%
  mutate(label = paste0(Name, "\n(", miles_away, " mi)"))

# Add percentiles
percentile_data <- all_recruits %>%
  group_by(Year) %>%
  summarize(
    p25 = quantile(miles_away, 0.25, na.rm = TRUE),
    p50 = quantile(miles_away, 0.50, na.rm = TRUE),
    p75 = quantile(miles_away, 0.75, na.rm = TRUE)
  )

year_lines <- unique(all_recruits$Year)
## colors for plots and table

bold_blue <- "#0992B2"
light_blue <- "#21B4E9"
bold_dark <- "#111D14"
deep_orange <- "#D15E10"
my_yellow <- "#F0E442"
sport <- input$sport_modal

#View(all_recruits)

base_plot <- ggplot(all_recruits, aes(x = Year, y = miles_away, color = Type))

final_plot <- base_plot +
  # ribbons with quartiles
  geom_ribbon(data = percentile_data, aes(x = Year, ymin = p25, ymax = p75),
              inherit.aes = FALSE, fill = my_yellow, alpha = 0.4) +
  # add year divider lines
  geom_vline(xintercept = year_lines, color = "gray90", linetype = "solid", size = 0.7) +
  geom_smooth(method = "loess", se = F, color = "firebrick",
              fill = "salmon", linetype = "dashed", alpha = 0.2) +
  
  # median distance
  geom_hline(yintercept = median(all_recruits$miles_away), linetype = "dotted", color = "seagreen") +
  annotate("text", x = min(all_recruits$Year)-1.2, y = median(all_recruits$miles_away)+30,
           label = paste0("Median (",round(median(all_recruits$miles_away),0)," mi)"),
           hjust = 0, color = "seagreen", size = 4) +
  # mean distance
  geom_hline(yintercept = mean(all_recruits$miles_away), linetype = "dotted", color = "seagreen") +
  annotate("text", x = min(all_recruits$Year)-1.2, y = mean(all_recruits$miles_away)+30,
           label = paste0("Mean (",round(mean(all_recruits$miles_away),0)," mi)"),
           hjust = 0, color = "seagreen", size = 4) +
  
  scale_color_manual(values = c("Commit" = bold_blue, "Transfer" = "firebrick")) +
  scale_size_continuous(range = c(2.5, 6), name = "Recruits at Rating") +
  scale_y_continuous(limits = c(min(all_recruits$miles_away)-1, max(all_recruits$miles_away)+1)) +
  scale_x_continuous(breaks = seq(min(all_recruits$Year), max(all_recruits$Year), 1),
                     labels = seq(min(all_recruits$Year), max(all_recruits$Year), 1),
                     expand = expansion(mult = c(0.04, 0.04))) +
  # Yearly average line and points
  geom_line(data = avg_dis, aes(x = Year, y = Average_Distance), 
            inherit.aes = FALSE, color = deep_orange, size = 1.5, alpha = .4) +
  geom_point(data = avg_dis, aes(x = Year, y = Average_Distance), 
             inherit.aes = FALSE, color = deep_orange, size = 3.5, alpha = .4) +
  
  # Recruit dots with jitter
  geom_point(position = position_jitter(width = 0.12), alpha = 0.3, size = 3.5) +
  
  # Label recruits using ggrepel
  geom_text_repel(
    data = top_recruits,
    aes(x = Year, y = miles_away, label = label),
    size = 4,
    fontface = "bold",
    segment.color = bold_blue,
    box.padding = 0.2,
    point.padding = 0.1,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  #geom_smooth(method = "lm", se = T, color = my_yellow) +
  labs(
    title = paste0(college_label, " ", str_to_title(sport), " Commits Distance Traveled (", min(all_recruits$Year), "–", max(all_recruits$Year), ")"),
    x = "Class Year",
    y = "Miles Away",
    subtitle =
    "    *Yellow band = 25th-75th percentile range
    *Orange line = yearly average distance
    *Blue = Recruit(s) that traveled the farthest for each year
    *Red dotted line = loess smoothed trend line",
    color = "Recruit Type"
  ) + theme(
    plot.title = element_text(color = bold_blue,
                              face = "bold",
                              size = 18),
    plot.subtitle = element_text(color = light_blue,
                                 face = "italic",
                                 size = 14),
    plot.caption = element_text(color = light_blue,
                                face = "italic",
                                size = 14),
    panel.background = element_rect(fill = "white"),
    legend.title = element_text(color = bold_blue),
    axis.title.x = element_text(color = light_blue),
    axis.title.y = element_text(color = light_blue)
  )
