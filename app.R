# use this for shiny io deployment (connect to .db) -->
db_path <- here("data", "recruiting.db")
conn <- dbConnect(RSQLite::SQLite(), db_path)

# Pre-compute your choices from database
team_selections.1 <- safe_query(conn, "SELECT DISTINCT School FROM recruit_class_football")
team_selections.2 <- safe_query(conn, "SELECT DISTINCT School FROM recruit_class_basketball")
team_selections <- union(team_selections.1,team_selections.2)

sport_selections.1 <- safe_query(conn, "SELECT DISTINCT sport FROM recruit_class_football")
sport_selections.2 <- safe_query(conn, "SELECT DISTINCT sport FROM recruit_class_basketball")
sport_selections <- union(sport_selections.1,sport_selections.2)

## UI -->
ui <- dashboardPage(
  
  dashboardHeader(title = "Big 12 Recruits"),
  #dashboardHeader(title = "Big 12 Recruiting Map"),
  #skin = "yellow",
 
  dashboardSidebar(
    width = 120,
    collapsed = TRUE,
    
    sidebarMenu(id = "tabs",
                menuItem("Recruits",    tabName = "filters", icon = icon("filter")),
                menuItem("Pipeline Map", tabName = "summary", icon = icon("chart-bar")),
                menuItem("Comparison", tabName = "compare", icon = icon("clock"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
  
    tabItems(
      ## Filters tab
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  title = "Where do Big 12 recruits come from?",
                  status = "info",
                  solidHeader = F,
                  width = 3,
                  #background = "blue",
                  footer = "This app allows you to visualize Big 12 football and
                  basketball recruiting classes from 2016 to 2025. You can explore
                  where recruits came from (their high schools) and how far they
                  traveled to reach their college destinations or view how 
                  distance traveled by recruits has changed over time!",
                  
                  ## first selections -->
                  selectInput(
                    "vizType", label = NULL,
                    choices = c("Distance Traveled by Recruits", "Distance Traveled Over Time"),
                    selectize = FALSE,
                    width = "100%",
                    size = 2
                  ),
                  actionButton(
                    inputId = "choose_sport",
                    label = "Select a Sport",
                    width = "100%",
                    class = "btn-warning"
                  ),
                  
                  ## for distance traveled, show these filters -->
                  dateRangeInput(
                    "year_range", "Pick Date Range",
                    start = "2016-01-01",
                    end = "2025-12-25",
                    format = "yyyy",
                    startview = "year",
                    separator = " to ",
                    width = "100%",
                    min = "2016-01-01",
                    max = "2026-12-31"
                  ),
                  selectInput(
                    "team", "Pick Big 12 Team",
                    choices = sort(team_selections$School),
                    selectize = FALSE,
                    selected = T,
                    width = "100%",
                    size = 3
                  ),
                  actionButton(inputId = "make_map",
                               label = tagList("Create", tags$span(style = "margin-left: 8px; color: orange;", icon("map"))),
                               width = "100%")

                ), # end of selection box(s)
                box(
                  title = NULL,  status = "info",
                  background = "navy",
                  solidHeader = T, width = 9, collapsed = F,
                  DTOutput("summary_preview", height = "400px")
                )
              )
      ), ## end of filters tab (tab 1)
      
      ## Summary tab
      tabItem(tabName = "summary",
              
              fluidRow(
                
                box(
                  ## update selection options for map
                  fluidRow(
                    column(width = 3,
                           selectInput(
                             "team", "Check other schools",
                             choices = c("",sort(team_selections$School)),
                             selectize = F,
                             width = "100%")),
                    column(
                      width = 9,
                      actionButton(inputId = "switch_to_plot",
                                   label = 
                                     tagList(icon("chart-line"), "View change over time"),
                                   class = "btn-warning", style = "margin-top: 25px; width: 100%; align: center;")
                    )
                  ), # end of fluid row
                  title = "Click each dot to reveal more information about each recruit!",
                  footer = "This map shows where each recruit came from (High School) as well as player scores, rankings and other metadata. Data was scrapped from 247Sports as of July 2025.",
                  status = "info",
                  background = "navy",
                  solidHeader = TRUE, width = 12,
                  collapsible = T, collapsed = F,
                  withSpinner(
                    leafletOutput("gridPlot", height = "300px"), color = "orange"
                  )
                ),
                
                box(
                  title = "Distance Traveled from High School to College (Farthest to Closest)",
                  status = "primary",
                  background = "aqua",
                  solidHeader = T, width = 12,
                  collapsible = T, collapsed = F,
                  DTOutput("summary_stats", height = "200px")
                )
              )
      ), ## end of summary tab (tab 2)
      
      ## Compare tab
      tabItem(tabName = "compare",
              
              fluidRow(
                box(
                  ## update selection options
                  fluidRow(
                    column(width = 3,
                           selectInput(
                             "team", "Check other schools",
                             choices = c("",sort(team_selections$School)),
                             selectize = FALSE,
                             width = "100%")),
                    column(width = 9,
                           actionButton(inputId = "switch_to_map",
                                        label = 
                                          tagList(icon("map"), "View map locations"),
                                        class = "btn-warning", style = "margin-top: 25px; width: 100%; ; align: center;")
                           )
                    ), # end of fluid row
                  title = "Compare recruiting classes across years to see how 
                  they stack up in terms of 247 player rankings scores (1-100+)",
                  footer = "Data was scrapped from 247Sports as of July 2025.",
                  status = "info",
                  background = "navy",
                  solidHeader = TRUE, width = 12,
                  collapsed = F,
                  withSpinner(
                    plotOutput("plot", height = "500px"), color = "orange"
                  )
                )
              )
      ) ## end of compare tab (tab 3)
      
    ) ## end of tab items
  ) ## end of dashboard body
)

server <- function(input, output, session) {
  # hold the sport choice
  chosenSport <- reactiveVal(NULL)
  chosenSchool <- reactiveVal(NULL)
  chosenYearRange <- reactiveVal(NULL)
  
  # hide initial other tabs
  shinyjs::hide("year_range")
  shinyjs::hide("team")
  shinyjs::hide("make_map")
  
  ## launch modal on button click
  observeEvent(input$choose_sport, {
    showModal(modalDialog(
      title = "Pick your sport",
      radioButtons(
        "sport_modal", NULL,
        choices = sort(sport_selections$sport)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_sport", "Confirm")
      ),
      easyClose = FALSE
    ))
  })
  
  ## confirm & lock in choice
  observeEvent(input$confirm_sport, {
    req(input$sport_modal)
    # save reactive values
    chosenSport(input$sport_modal)
    chosenSchool(input$team)
    chosenYearRange(input$year_range)
    
    removeModal()
    disable("choose_sport")
    
    ## alert
    shinyalert(
      title = "Sport Selected",
      text = paste("You selected", input$sport_modal, "as your sport...
                   Now pick a date range, team, then Create!"),
      type = "success",
      confirmButtonText = "Great!",
      showCancelButton = FALSE,
      timer = 6000
    )
    
    # show hidden tabs
    shinyjs::show("year_range")
    shinyjs::show("team")
    shinyjs::show("make_map")
  })
  
  ## switcher for moving between tabs
  observeEvent(input$switch_to_plot, {
    updateTabItems(session, "tabs", "compare")
  })
  observeEvent(input$switch_to_map, {
    updateTabItems(session, "tabs", "summary")
  })
  
  ## reactive filtering uses chosenSport()
  filtered_data <- reactive({
    req(chosenSport(), input$team, input$year_range)
    yrs <- as.integer(format(input$year_range, "%Y"))
    
    # set database based on sport ->
    if (chosenSport() == "basketball") {
      db_table = "recruit_class_basketball"
    }
    if (chosenSport() == "football") {
      db_table = "recruit_class_football"
    }
    
    geting_data <- paste0("Select * from ",db_table," where sport = '",chosenSport(), 
                          "' AND School = '",input$team,"' AND Year >= ",yrs[1]," AND Year <= ",yrs[2],
                          " ORDER BY Ranking, NationalRank desc, StateRank desc, PositionRank desc, Name")
    
    all_data <- safe_query(conn, geting_data)
    
    all_data$lat <- as.numeric(all_data$lat)
    all_data$long <- as.numeric(all_data$long)
    all_data$college_lat <- as.numeric(all_data$college_lat)
    all_data$college_long <- as.numeric(all_data$college_long)
    all_data$Ranking <- as.numeric(all_data$Ranking)
    all_data$NationalRank <- as.numeric(all_data$NationalRank)
    all_data$PositionRank <- as.numeric(all_data$PositionRank)
    all_data$StateRank <- as.numeric(all_data$PositionRank)
  
    ## calculate miles away
    big12_data <- all_data %>% 
      mutate(disFromHS_m = 
               distGeo(p1 = cbind(long, lat),
                       p2 = cbind(college_long, college_lat)))
    
    ## lets change distance to miles instead of meters -->
    meters_per_mile <- 1609.34
    big12_data_wDis <- big12_data %>% 
      mutate(miles_away = round(disFromHS_m / meters_per_mile,0))
    
    ## clean names
    u_of_schools <- c("arizona","utah","kansas","houston",
                      "colorado","cincinnati","central-florida")
    
    data_final <- big12_data_wDis %>%
      mutate(University = ifelse(
        nchar(School)==3,
        yes = paste0(toupper(School)),
        no = ifelse(School %in% c(u_of_schools),
                    yes = paste0("University of ",str_to_title(School)),
                    no = paste0(str_to_title(School)," University"))
      ))
    data_final
  })

  ## render outputs
  output$selections <- renderPrint({
    req(chosenSport())
    
    cat("Selection preview of recruiting classes from",
      format(input$year_range[1], "%Y"),
      "to", format(input$year_range[2], "%Y"),
      "for", input$team, chosenSport(),"...\n")
      cat("Update selections and hit action button to proceed! \n")
  })
  
  
  output$summary_preview <- renderDT({
    
    if (nrow(filtered_data()) == 0) {
      return(data.frame(Message =
                          "No recruits found for the selected filters. Please adjust your selections."))
    }
    
    if (nrow(filtered_data()) > 0) {
      d <- filtered_data() %>% 
        select(Name, Year, Position, Location, Ranking, NationalRank) %>%
        arrange(desc(Ranking), NationalRank, Name)
      d %>% 
        datatable(
          options = list(pageLength = 10,
                         lengthChange = FALSE),
          height = "400px", class = "stripe hover",
          colnames = c("Recruit", "Class Year", "Position", "High School", "247Sports Ranking", "National Ranking"),
          style = "bootstrap4",
          caption = "247Sports Ranking (1-100+) graded to by 247Sports (100+ being the best).
          National Ranking ranks top recruits in the country for each year (1 being the best).",
          rownames = FALSE
        )
    }

  })
  
  output$summary_stats <- renderDT({
    if (nrow(filtered_data()) == 0) {
      return(data.frame(Message =
      "No recruits found for the selected filters. Please adjust your selections."))
    } else {
      
      big12_data_wDis <- filtered_data()

      d <-  big12_data_wDis %>% 
        select(Name, miles_away, Location, University, School_City, Ranking, NationalRank,
               Position, Height, Weight, Year) %>%
        #mutate("Measurements" = paste0("(",Position,") Height: ",Height," - Weight: ",Weight)) %>%
        arrange(desc(miles_away))
      
      d2 <- as.data.frame(d) %>% 
        datatable(
          colnames = c("Recruit", "Distance Traveled (miles)", "From", "To", "City",
          "247Sports Ranking", "National Ranking", "Position", "Height", "Weight", "Year"),
          options = list(pageLength = 10,
                         #className = "dt-center",
                         lengthChange = FALSE),
          rownames = FALSE)
      d2
    }
  })

  ## create map/plot -->
  observeEvent(input$make_map, {
    req(chosenSport(), input$team, input$year_range)
    
    ## expand sidebar menu
    #shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    
    # move tabs based on selection
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
    
    # render map
    output$gridPlot <- renderLeaflet({
      # sourced map code
      source("scripts/map.R", local = T)
      final_map
    })
    
    ## for plot
    output$selections_map <- renderPrint({
      
      years <- sort(input$year_range)
      
      if(length(years) == 2){
        cat("You selected to compare recruiting classes from",
            years[1], "to", years[2], "for", input$team, chosenSport(),"...\n")
      } else {
        cat("Error: Did not find 2 years to compare... \n")
      }
    })

    ## render plot
    output$plot <- renderPlot ({
      source("scripts/plot.R", local = T)
      final_plot

    })
    
  })
  
 

  
  output$summary <- renderTable({
    filtered_data()
  })
  
  # # Close connection when app stops
  # session$onStop(function() {
  #   if (dbIsValid(conn)) {
  #     dbDisconnect(conn)
  #     cat("Database connection closed.\n")
  #   }
  # })
  
}

shinyApp(ui, server)