library(tidyverse)

data <- read_csv("csvs/basketball/recruits_cleaned.csv")
View(data)

## Where do the to HS basketball recruits come from? -->

## separated by year
df_byYear <- data %>% 
  group_by(`high.school`,year) %>% 
  summarise(countByYear= n()) %>% 
  arrange(desc(countByYear))
  
## all time (10 years) stats
df_bySchool <- data %>% 
  group_by(`high.school`) %>% 
  summarise(countAllTimeForHS= n()) %>% 
  arrange(desc(countAllTimeForHS))
## filter out top schools... ?
df_bySchool <- df_bySchool %>% 
  filter(countAllTimeForHS > 10) %>% 
  arrange(desc(countAllTimeForHS))

## this is schools with more that 10 top 150 recruits in the last 10 years ##

## look at az...
df_bySchool %>% 
  filter(grepl("AZ", `high.school`))


df_bySchool_2 <- df_bySchool %>% 
  mutate(avgrecruitsTotal_perYear = countAllTimeForHS/10)

df2 <- left_join(df_bySchool_2, df_byYear, by = "high.school")

## get stat data
withStats <- data %>% 
  group_by(high.school, year) %>% 
  summarise(avgScoreByHS_perYear = mean(as.numeric(score)), recruitsTotal_perYear=n()) %>% 
  arrange(desc(recruitsTotal_perYear))

df2_withStats <- left_join(df2, withStats, by = c("high.school", "year")) %>% 
  select(-countAllTimeForHS, -countByYear)


## group data in 2 year periods to get more data points for each school and get
## average score and average recruits per year for each school in those 2 years


df3 <- df2_withStats %>% 
  group_by(high.school, period_start = 2 * ((year- 2016)%/%2) + 2016) %>% 
  summarise(avgScoreByHS_per2yr = mean(avgScoreByHS_perYear, na.rm = TRUE), 
            recruitsTotal_per2yr = sum(recruitsTotal_perYear, na.rm = TRUE), .groups = "drop") %>% 
  mutate(period_end = pmin(period_start + 1, 2025), YearRange = paste0(period_start, "-", period_end)) %>% 
  arrange(high.school, YearRange) %>% 
  select(high.school, YearRange, recruitsTotal_per2yr, avgScoreByHS_per2yr)

library(gt)
library(reshape2)
df4 <- df3 %>%
  #arrange(desc(recruitsTotal_per2yr)) %>% 
  select(high.school, YearRange, recruitsTotal_per2yr) %>% 
  pivot_wider(names_from = YearRange, values_from = recruitsTotal_per2yr) %>% 
  gt(rowname_col = "high.school") %>% 
  tab_header(title = "Where Do the Top HS Basketball Recruits Come From?", 
             subtitle = "Average Number of Top 150 Recruits Per Year by High School and 2-Year Period (2016-2025)") %>% 
  cols_align(align = "left", columns = everything()) %>% 
  fmt_integer(columns = c("2016-2017", "2018-2019", "2020-2021", "2022-2023", "2024-2025")) %>% 
  tab_options(latex.tbl.pos = "H", 
              table.width = pct(100), 
              heading.title.font.weight = "bold", 
              heading.title.font.size = px(20), 
              heading.subtitle.font.size = px(12), 
              heading.align = "left", 
              table.font.size = px(9.5), 
              data_row.padding = px(3), 
              heading.padding = px(1), 
              column_labels.font.weight = "bold") %>% 
  cols_width(stub() ~ px(220), everything() ~ px(70))

## pivot data to add totals

df3 <- df3 %>%
  #arrange(desc(recruitsTotal_per2yr)) %>% 
  select(high.school, YearRange, recruitsTotal_per2yr) 

## temp rename for merge
df_totals <- df_bySchool_2 %>% select(high.school, countAllTimeForHS) %>% 
  rename("recruitsTotal_per2yr" = countAllTimeForHS)

df_totals <- df_totals %>% 
  pivot_longer(cols = recruitsTotal_per2yr,
               names_to = "YearRange",
               values_to = "recruitsTotal_per2yr" ) %>%
  mutate(YearRange = "(2016-2025)")


df6 <- full_join(df3, df_totals)%>% 
  pivot_wider(names_from = YearRange, values_from = recruitsTotal_per2yr)
#View(df6)
## change all NA to 0

table <- df6  %>% 
  gt(rowname_col = "high.school") %>% 
  tab_header(title = "Where Do the Top HS Basketball Recruits Come From?", 
             subtitle = "Number of Top 150 Recruits by Top High Schools over 2-Year Periods (2016-2025)") %>% 
  cols_align(align = "left", columns = everything()) %>% 
  fmt_integer(columns = c("2016-2017", "2018-2019", "2020-2021", "2022-2023", "2024-2025", "(2016-2025)")) %>% 
  tab_options(latex.tbl.pos = "H", 
              table.width = pct(100), 
              heading.title.font.weight = "bold", 
              heading.title.font.size = px(20), 
              heading.subtitle.font.size = px(12), 
              heading.align = "left", 
              table.font.size = px(9.5), 
              data_row.padding = px(3), 
              heading.padding = px(1), 
              column_labels.font.weight = "bold") %>% 
  cols_width(stub() ~ px(220), everything() ~ px(70))


## make a map
state <- map_data("state")
arizona <- state %>%
  filter(region == "arizona")

## get coord data
d2 <- read_csv("csvs/big12_recruiting_cleaned.csv")
d2 <- d2 %>% select(Location, lat, long)
names(d2)[1] <- "high.school"

loc <- left_join(df6, d2, by = "high.school")
View(loc)
write_csv(loc, "locationCoords.csv")
## 
ggplot(arizona, aes(x = long, y = lat, group = group))+
  geom_polygon(col = "red")+
  coord_map(projection = "mercator")
