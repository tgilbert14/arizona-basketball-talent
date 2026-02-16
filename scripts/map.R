data_to_map <- filtered_data()
data_to_map <<- data_to_map

university <- unique(str_to_title(data_to_map$School))

# test <<- data_to_map
# stop()
# View(test)

## create polygon shape -->
# data_df <- data_to_map %>% 
#   select(long, lat) %>% 
#   distinct() %>% 
#   drop_na()
# # Convert to a polygon sf object
# poly1 <- st_as_sf(data_df, coords = c("long","lat"), crs = 4326)
# # rounded, then smooth out
# hull <- st_convex_hull(st_union(poly1))
# smooth_h <- smooth(hull, method = "ksmooth", smoothness = .2)

hull <- data_to_map %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  group_by(State) %>% 
  summarise(geometry = st_combine(geometry)) %>%
  st_buffer(dis = 60000) %>%
  st_convex_hull()
smooth_h <- smooth(hull, method = "ksmooth", smoothness = 3)

map <- leaflet(data_to_map) %>%
  addTiles() %>%
  # polygon layer...
  addPolygons(
    data = smooth_h,
    group = "Recruiting Area",
    weight = 3, color = "#E69F00",
    fillColor = "#0072B2", fillOpacity = 0.1,
    # Options to customise polygon highlighting
    highlightOptions = highlightOptions(
      # Highlight stroke parameters
      weight = 3, color = "white",
      fillColor = "#0072B2", fillOpacity = 0.2)) %>% 
  # dots for High School locations...
  addCircleMarkers(lng = ~long,
                   lat = ~lat,
                   #group = "Recruited from HS",
                   radius = 5,
                   color = "blue",
                   stroke = TRUE,
                   fillOpacity = 0.7,
                   label = ~paste0(Name,": ",Location_Clean)) %>%
  # dots for College destinations...
  addCircleMarkers(lng = ~college_long,
                   lat = ~college_lat,
                   radius = 5,
                   color = "red",
                   stroke = TRUE,
                   fillOpacity = 0.7,
                   label = ~paste(School_Clean))

map_with_legend <- map %>%
  addLegend("bottomright",
            pal = colorFactor(c("blue","red"), domain = c("Recruited from HS", university)),
            values = c("Recruited from HS",university),
            title = "Location Type",
            opacity = 0.7)

styled_map <- prependContent(map,
                             # Prepend header styling and subtitle
                             prependContent(
                               tagList(
                                 tags$style(HTML("
        .subtitle {
          color: grey;
          text-align: center;
          padding: 2px;
        }
        .leaflet-bottom.leaflet-right {
          margin-bottom: 50px;
          margin-right: 10px;
        }
        @media screen and (max-width: 600px) {
          h2 { font-size: 18px; }
          h4.subtitle { font-size: 14px; }
        }
      ")),
                                 tags$div(
                                   style = "
          background-color: rgb(235, 246, 246);
          padding: 1% 2%;
          margin-bottom: 2px;
          text-align: center;
          border-radius: 4px;
          box-shadow: 0 2px 2px rgba(0, 0, 0, 0.1);
          display: block;",
                                   tags$h2(paste0(university, " Recruiting Map (", min(data_to_map$Year), "-", max(data_to_map$Year), ")")),
                                   tags$h4(class = "subtitle", "Recruiting Pipeline from Commits (via 247Sports)"),
                                   tags$div(style = "text-align:center; margin-top:0px;",
                                            tags$small("⬤ HS", style = "color:blue; margin-right:2px;"),
                                            tags$small(paste0("⬤ ", university), style = "color:red;")
                                   )
                                 )
                               )
                             )
)
# set view/zoom
zoom_long <- data_to_map$college_long[data_to_map$School==input$team][1]
zoom_lat <- data_to_map$college_lat[data_to_map$School==input$team][1]

final_map <- styled_map %>% 
  setView(lng = zoom_long, lat = zoom_lat, zoom = 5.5)
#%>% addPopups(zoom_long, zoom_lat, paste0('<b>',data_to_map$School_City,'</b>'))

