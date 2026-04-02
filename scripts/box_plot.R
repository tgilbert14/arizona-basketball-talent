# get all data (not from selection) ??


rec_data <- filtered_data()

college_label <- rec_data$University[1]

# test <<- rec_data
# stop()
data <- rec_data

data$travelSummary <- paste0(data$State," to ",data$School_City)

## everything looks reasonable - trim down wanted columns for view
cleaned_data <- data %>% 
  select(Year, Ranking, NationalRank, Position, School, University,
         Location_Clean, loc_inside_parens, State, University, School_City,
         college_long, college_lat, long, lat, miles_away, travelSummary)

total_data <- nrow(cleaned_data)

# ## get outliers
# possible_outliers <- get_Outliers(cleaned_data)
# possible_outliers$miles_away
## see how much will be hidden - always cutting off at 3100

## cutting off at certain distance for view - but keeping for data analysis
hidden <- cleaned_data %>% 
  filter(miles_away >= 3100) %>% 
  arrange(miles_away)
hide <- nrow(hidden)
perc_removed <- paste0("~",round(hide/total_data*100, 0),"%")
removed <- paste0("*",hide," outliers hidden (",perc_removed," of data)")

if (tolower(sp) == "football") {
  ## set order for later
  plot_order <- c("Offense","Defence","Special Teams")
  ## group positions
  final_data <- cleaned_data %>%
    mutate(`Position Group` = ifelse(grepl("OT|OG|OC|IOL|WR|DUAL|RB|APB|OC|TE|PRO|FB|QB|ATH",
                                           Position),"Offense",Position))
  
  final_data <- final_data %>%
    mutate(`Position Group`=ifelse(grepl("DE|DT|OLB|S|SDE|Edge|DL|WDE|ILB|CB|LB|ATH",Position),
                                   "Defence",ifelse(`Position Group`!="Offense","Special Teams",`Position Group`)))
  } else if (tolower(sp) == "basketball"){
    ## set order for later
    plot_order <- c("Guard","Forward","Center")
   ## group positions
   final_data <- cleaned_data %>%
     mutate(`Position Group` = ifelse(grepl("PG|SG|G|PG/SG|SG/SF|PG/SG/SF",
                                            Position),"Guard",
                                      Position))
   
   final_data <- final_data %>%
     mutate(`Position Group`=ifelse(grepl("SF|PF|F|SF/PF|PF/C|SF/PF/C",
                                        Position),
                                   "Forward",ifelse(`Position Group`!="Guard",
                                                    "Center",`Position Group`)))
 } else {
   ## set order for later
   plot_order <- c("All")
   final_data <- cleaned_data %>%
     mutate(`Position Group` = "All")
 }


the_data <- final_data

## set order by median
order <- the_data  %>% 
  group_by(University) %>% 
  summarize(med = median(miles_away)) %>% 
  arrange(desc(med))

## set order
the_data$University <- factor(the_data$University, levels = order$University)
the_data$`Position Group` <- factor(the_data$`Position Group`,
                                    levels = c(plot_order))


## group by year ranges selected...
sorted_years <- sort(unique(the_data$Year))
## newest year 
curr_year <- sorted_years[length(sorted_years)]
## historic years
his_range <- paste0(sorted_years[1],"-",sorted_years[length(sorted_years)-1])

the_data <- the_data %>% 
  mutate(`Time span` = ifelse(Year >= sorted_years[1] & Year <= sorted_years[length(sorted_years)-1],
                              his_range,curr_year))

## create html links to team logos
logo_df <- data.frame(
  University = c(
    "Arizona State University",
    "University of Arizona",
    "Baylor University",
    "BYU",
    "University of Central Florida",
    "University of Cincinnati",
    "University of Colorado",
    "University of Houston",
    "Iowa State University",
    "Kansas State University",
    "University of Kansas",
    "Oklahoma State University",
    "TCU",
    "Texas Tech University",
    "University of Utah",
    "West Virginia University"),
  logo = c(
    "www/arizona-state.png",
    "www/arizona.png",
    "www/baylor.png",
    "www/byu.png",
    "www/ucf.png",
    "www/cincinnati.png",
    "www/colorado.png",
    "www/houston.png",
    "www/iowa-state.png",
    "www/kansas-state.png",
    "www/kansas.png",
    "www/oklahoma-state.png",
    "www/tcu.png",
    "www/texas-tech.png",
    "www/utah.png",
    "www/west-virginia.png"))


logo_labels <- setNames(paste0("<img src='",logo_df$logo,"' width='42'/>"),
                        logo_df$University)
oki_colors <- c("#0992B2","#D15E10","#F0E442","#21B4E9","#111D14",
                "#E69F00","#56B4E9","#009E73","#000000","#0072B2")

## label states in this range
state_labels <- the_data %>% 
  filter(miles_away > 2700 & miles_away < 3100) %>% 
  group_by(State) %>%
  summarize(miles_away = max(miles_away))

my_box_plot <- ggplot(the_data, aes(x = University, y = miles_away))+
  labs(x = "", y = "Miles Away",
       title = paste0(college_label," ",str_to_title(sp)," by Position"),
       subtitle = paste0("○ Historic (",his_range,") vs Most Current (",curr_year,") ●"))+
  geom_boxplot(aes(fill=University), alpha = .3, width = 1, color = oki_colors[5],
               fill = oki_colors[4], outliers = F, median.linewidth = 3, show.legend = T)+
  # historical range
  geom_point(data = the_data %>% filter(`Time span` == his_range),
             color = oki_colors[10], shape = 1,
             position = position_jitter(width = .3),
             size = 7, alpha = .8)+
  # most recent year
  geom_point(data = the_data %>% filter(`Time span` == curr_year),
             color = "red", shape = 16,
             position = position_jitter(width = .25),
             size = 6, alpha = 1)+
  coord_flip()+
  scale_x_discrete(labels = logo_labels)+
  theme(
    axis.text.y = element_markdown(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 18, face = "italic"),
    #legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic"),
    panel.grid.major.y = element_line(color = "grey"),
    axis.text.x = element_text(size = 14, color = "black")
  )+
  ylim(0, 3100)+
  annotate("text",label=state_labels$State, x = 1.2, y = state_labels$miles_away+25,
           hjust = 0, size = 4, color = "black")+
  
  annotate("text",label=removed, x = .6, y = 3100, hjust = 1,
           size = 3, color = "black")+
  facet_wrap(~`Position Group`, ncol = 1)

