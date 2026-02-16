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
  dashboardHeader(title = "Recruit-Tracker"),
  
  dashboardSidebar(
    width = 120,
    collapsed = TRUE,
    
    tags$head(
      tags$style(HTML("
        .box .control-label,
        .shiny-input-container > label { color: black !important; }
      "))
    ),
    sidebarMenu(id = "tabs",
                menuItem("Recruits",    tabName = "filters", icon = icon("filter")),
                menuItem("Pipeline Map", tabName = "summary", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      
      # Filters tab
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  #title = "Select a sport",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  #background = "blue",
                  #footer = "test text",
                  
                  #"Select a Sport",
                  actionButton(
                    inputId = "choose_sport",
                    label = "Select Sport",
                    width   = "100%"
                  ),
                  
                  br(),br(),
                  
                  dateRangeInput(
                    "year_range", "Select Class Date Range",
                    start     = Sys.Date() - years(1),
                    end       = Sys.Date(),
                    format    = "yyyy",
                    startview = "year",
                    separator = " to ",
                    width = "100%",
                    min = "2016-01-01",
                    max = "2026-12-31"
                  ),
                  
                  selectInput(
                    "team", "Select Big 12 Team",
                    choices   = sort(team_selections$School),
                    selectize = FALSE,
                    width     = "100%",
                    size      = 3
                  ),
                  
                  actionButton(inputId = "make_map",
                               label = tagList("Create Map", tags$span(style = "margin-left: 10px; color: green;", icon("map"))),
                               width = "100%")
                ),
                
                box(
                  title = "You selected:", status = "success",
                  solidHeader = TRUE, width = 8,
                  verbatimTextOutput("selections")
                ),
                
                box(
                  title = "Recruit info:", status = "success",
                  solidHeader = TRUE, width = 8,
                  DTOutput("summary_preview", height = "400px")
                )
              )
              
      ),
      
      # Summary tab
      tabItem(tabName = "summary",
              
              fluidRow(
                box(
                  title = "Click each blue dot to reveal recruit and where they are from!", status = "info",
                  solidHeader = TRUE, width = 12,
                  leafletOutput("gridPlot", height = "600px")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # hold the one‐time sport choice
  chosenSport <- reactiveVal(NULL)
  
  # launch modal on button click
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
  
  # confirm & lock in choice
  observeEvent(input$confirm_sport, {
    req(input$sport_modal)
    chosenSport(input$sport_modal)
    removeModal()
    disable("choose_sport")
  })
  
  # reactive filtering uses chosenSport()
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
    
    # all_data <- all_data %>%
    #   mutate(
    #     Height_in = str_extract(Height, "[0-9]+") %>% as.numeric() * 12 +
    #       str_extract(Height, "(?<=-)[0-9.]+") %>% as.numeric()
    #     )
    
    all_data$lat <- as.numeric(all_data$lat)
    all_data$long <- as.numeric(all_data$long)
    all_data$college_lat <- as.numeric(all_data$college_lat)
    all_data$college_long <- as.numeric(all_data$college_long)
    all_data$Ranking <- as.numeric(all_data$Ranking)
    all_data$NationalRank <- as.numeric(all_data$NationalRank)
    all_data$PositionRank <- as.numeric(all_data$PositionRank)
    all_data$StateRank <- as.numeric(all_data$PositionRank)
    
    all_data
    
  })
  
  # render outputs
  output$selections <- renderPrint({
    req(chosenSport())
    cat(
      "Selected recruiting classes from",
      format(input$year_range[1], "%Y"),
      "to", format(input$year_range[2], "%Y"),"\n",
      "for", input$team, chosenSport()
    )
  })
  
  output$summary_preview <- renderTable({
    
    if (nrow(filtered_data()) == 0) {
      return(data.frame(Message =
                          "No recruits found for the selected filters. Please adjust your selections."))
      } else {
        filtered_data() %>% 
          arrange(desc(Ranking), NationalRank, StateRank, PositionRank, Name)
    }

  })
  
  # create map button
  observeEvent(input$make_map, {
    req(chosenSport(), input$team, input$year_range)
    
    # move tabs
    updateTabItems(session, "tabs", "summary")  # switch to Summary tab
    
    # render plot
    output$gridPlot <- renderLeaflet({
  
      # sourced map code
      source("scripts/map.R", local = T)
      final_map
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