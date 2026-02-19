data_to_map <- filtered_data()
#data_to_map <<- data_to_map

## need to set up jitter if there are multiple players from same school ->
data_to_map <- data_to_map %>% 
  arrange(Location_Clean)
#View(data_to_map)
i=1
while (i < nrow(data_to_map)) {
  
  if (data_to_map$Location_Clean[i] == data_to_map$Location_Clean[i+1]) {
    random_num <- runif(1, 0.010, 0.013)
    data_to_map$lat[i] <- jitter(data_to_map$lat[i], amount = random_num)
    data_to_map$long[i] <- jitter(data_to_map$long[i], amount = random_num)
    }
  i=i+1
}
#View(data_to_map)

sport <- input$sport_modal
# create URLs for each player search
# e.g., https://247sports.com/season/2023-basketball/recruits/?&Player.FullName=conrad%20martinez
data_to_map <- data_to_map %>% 
  mutate(URL = paste0("https://247sports.com/season/",Year,
                      "-",sport,"/recruits/?&Player.FullName=", 
                      str_replace_all(Name, " ", "%20")))
## colors
c1 <- "#0072B2"
c2 <- "#E69F00"
c3 <- "#F0E442"
c4 <- "#D55E00"
c5 <- "#CC79A7"
c6 <- "#56B4E9"
c7 <- "#009E73"
c8 <- "#000000"
teal <- "#008080"

hull <- data_to_map %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  group_by(State) %>% 
  summarise(geometry = st_combine(geometry)) %>%
  st_buffer(dis = 60000) %>%
  st_convex_hull()
smooth_h <- smooth(hull, method = "chaikin")

# labels for recruits
labs <- lapply(seq(nrow(data_to_map)), function(i) {
  paste0('<strong>',data_to_map[i, "Name"], '</strong> (',
         data_to_map[i, "Position"],') <br>Height: ',data_to_map[i, "Height"],', Weight: ',
         data_to_map[i, "Weight"],'<br>',
         'From: ',data_to_map[i, "Location"], ' (',data_to_map[i, "Year"],')<br>',
         'National Rank: ',data_to_map[i, "NationalRank"],'<br>',
         '247Ranking(1-100+): ',data_to_map[i, "Ranking"], '</br>',
         '<em><a href="', data_to_map[i, "URL"], '" target="_blank">View Profile</a></em>')
})

#National Rank:",NationalRank

map <- leaflet(data_to_map) %>%
  addTiles() %>%
  #addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  # polygon layer...
  addPolygons(
    data = smooth_h,
    weight = 2, color = c1, opacity = 0.8,
    fillColor = c6, fillOpacity = 0.2) %>% 
  # High School locations...
  addCircleMarkers(lng = ~long,
                   lat = ~lat,
                   radius = 4.5,
                   color = c4,
                   stroke = TRUE,
                   #fillOpacity = 0.6,
                   label = lapply(labs, htmltools::HTML), # for hoover
                   popup = lapply(labs, htmltools::HTML), # for click
                   layerId = ~Name # for clicking summary table
                   ) %>%
  # College destinations...
  addCircleMarkers(lng = ~college_long,
                   lat = ~college_lat,
                   radius = 6,
                   color = teal,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   label = ~paste(University)
                   )

# set view/zoom
zoom_long <- data_to_map$college_long[data_to_map$School==input$team][1]
zoom_lat <- data_to_map$college_lat[data_to_map$School==input$team][1]

final_map <- map %>% 
  setView(lng = zoom_long, lat = zoom_lat, zoom = 4.5) %>% 
  # footer
  addControl(html = tags$div(
    style = "
      background-color: rgb(235, 246, 246);
      padding: 1% 2%;
      margin-top: 2px;
      text-align: bottom;
      border-radius: 4px;
      box-shadow: 0 2px 2px rgba(0, 0, 0, 0.1);
      display: block;",
    tags$small("Data Source: ",
               tags$a(href = "https://247sports.com/season/2024-basketball/RecruitRankings/?InstitutionGroup=highschool",
                      "247Sports", target = "_blank"))
    ),position = "bottomleft")


#%>% addPopups(zoom_long, zoom_lat, paste0('<b>',data_to_map$School_City,'</b>'))


