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
  
  dashboardHeader(title = "Big 12"),
  #dashboardHeader(title = "Big 12 Recruiting Map"),
  skin = "yellow",
 
  dashboardSidebar(
    width = 250,
    collapsed = TRUE,
    
    sidebarMenu(id = "tabs",
                menuItem("Teams", tabName = "filters", icon = icon("filter")),
                menuItem("Pipeline Map", tabName = "summary", icon = icon("chart-bar")),
                menuItem("Comparison", tabName = "compare", icon = icon("clock"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    ## to center box titles for logos
    tags$head(
      tags$style(HTML("
    .box-header .box-title {
      width: 100%;
      text-align: center;
    }"))
    ),
    
  
    tabItems(
      ## Filters tab
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  title = HTML("<span style='color: teal; font-size: 20px; font-weight: bold;'>
                  <em>Where do Big 12 recruits come from?</em></span><br>
                               </span>"),
                  status = "warning",
                  solidHeader = F,
                  width = 3,
                  #background = "aqua",
                  
                  footer = HTML("<span style='color: teal; font-size: 14px;'>
                  This app allows you to <strong>visualize Big 12 football
                  and basketball recruiting classes.</strong> You can explore
                  where recruits came from (distance from high school to college 
                  destinations) via a map by selecting <em>'Distance Traveled 
                  by Recruits'</em> and calculate distance traveled for each 
                  recruit. <em>'Distance Traveled Over Time'</em> creates a scatter 
                  plot showing distance traveled by recruits. To get started, 
                  <strong>click a team</strong>.</span>"),
                  
                  # first selections -->
                  
                  ## for distance traveled, show these filters -->
                  dateRangeInput(
                    "year_range", label = NULL,
                    start = "2016-01-01",
                    end = "2026-01-31",
                    format = "yyyy",
                    startview = "year",
                    separator = " to ",
                    width = "100%",
                    min = "2016-01-01",
                    max = "2026-12-31"
                  ),
                  selectInput(
                    "vizType", label = NULL,
                    choices = c("Distance Traveled by Recruits", "Distance Traveled Over Time"),
                    selectize = FALSE,
                    selected = "Distance Traveled Over Time",
                    width = "100%",
                    size = 2
                  ),
                  selectInput(
                    "team", "Pick Big 12 Team",
                    choices = sort(c("",team_selections$School)),
                    selectize = FALSE,
                    selected = FALSE,
                    width = "100%",
                    size = 3
                  ),
                  actionButton(
                    inputId = "choose_sport",
                    label = "Select Sport",
                    width = "100%", 
                    class = "btn-info"
                  )
                ), # end of selection box(s) for initial selections
                
                # selection summary
                box(
                  title = NULL,  status = "info",
                  background = "navy",
                  solidHeader = T, width = 9, collapsed = F,
                  
                  ## render Big 12 school logos on condition no school is selected -->
                  #conditionalPanel(condition = "input.team == ''", # got rid of condition...
                  
                  ## add title to run across logos
                  # box(width = 12, background = "light-blue", solidHeader = T,height = "40px",
                  #     title = "Click a logo to start!"),
                  actionButton(
                    inputId = "select_arizona",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "arizona.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_arizona_state",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "arizona-state.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_baylor",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "baylor.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  
                  actionButton(
                    inputId = "select_byu",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "byu.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_cincinnati",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "cincinnati.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_colorado",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "colorado.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_houston",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "houston.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_iowa_state",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "iowa-state.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_kansas",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "kansas.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_kansas_state",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "kansas-state.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_oklahoma_state",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "oklahoma-state.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_tcu",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "tcu.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_texas_tech",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "texas-tech.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_ucf",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "ucf.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_utah",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "utah.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),
                  actionButton(
                    inputId = "select_west_virginia",
                    label = div(
                      style = "text-align:center; margin-top: 10px; magin-bottom: 10px;",
                      img(src = "west-virginia.png", height = "80px")
                    ),
                    style = "background-color: transparent; border: none; width: 24%;"
                  ),


                  #), # end of conditional panel
                  
                  ## team summary from selection - dont need anymore
                  #DTOutput("summary_preview", height = "600px")
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
                           actionButton("switch_school", "Switch School",
                                        class = "btn-warning", width = "100%")),
                    column(width = 3,
                           actionButton("choose_sport", "Switch Sport",
                                        class = "btn-info", width = "100%",
                                        style = "margin-bottom: 10px; align: left; margin-right: 0;")),

                    column(width = 6,
                      actionButton(inputId = "switch_to_plot",
                                   label =
                                     tagList(icon("chart-line"), "View change over time"),
                                   class = "btn-warning", width = "100%",
                                   style = "margin-bottom: 10px; align: left; margin-right: 0;")
                    )
                  ), # end of fluid row
                  title = "Click each dot to reveal more information about each recruit!",
                  footer = HTML("<span style='color: #FFA500;'>
                    <em>*Polygons grouped by State. Only commits with reported High Schools mapped (Transfers not included). 
                    Data from 247Sports as of Jan 2026.</em>
                                </span>"),
                  status = "info",
                  background = "navy",
                  solidHeader = TRUE, width = 12,
                  collapsible = T, collapsed = F,

                  withSpinner(
                    leafletOutput("gridPlot", height = "320px"), color = "orange"
                  )
                ),
                
                box(
                  title = "Distance Traveled from High School to College (Farthest to Closest)",
                  status = "primary",
                  background = "aqua",
                  solidHeader = T, width = 12,
                  collapsible = T, collapsed = F,
                  DTOutput("summary_stats", height = "230px")
                )
              )
      ), ## end of summary tab (tab 2)
      
      ## Compare tab
      tabItem(tabName = "compare",
              
              fluidRow(
                box(
                  ## update selection options
                  fluidRow(
                    column(width = 2,
                           actionButton("switch_school", "Switch School",
                                        class = "btn-warning", width = "100%")),
                    column(width = 3,
                           actionButton("choose_sport", "Switch Sport",
                                        class = "btn-info", width = "100%",
                                        style = "margin-bottom: 10px; align: left; margin-right: 0;")),

                    column(width = 3,
                           actionButton(inputId = "switch_to_map",
                                        label = 
                                          tagList(icon("map"), "View map locations"),
                                        class = "btn-warning", width = "100%",
                                        style = "margin-bottom: 10px; align: left; margin-right: 0;")
                           ),
                    column(width = 4,
                           selectInput(
                             "show_outliers", label = NULL,
                             choices = c("Show Outliers" = "show", "Hide Outliers" = "hide"),
                             selected = "show", width = "100%")
                    )
                    ), # end of fluid row
                  title = "Comparing Distance Traveled by Recruits Over Time",
                  footer = HTML("<span style='color: #FFA500;'>
                  Data was scrapped from 247Sports as of Jan 2026.
                                </span>"),
                  status = "info",
                  background = "navy",
                  solidHeader = TRUE, width = 12,
                  collapsible = T,
                  collapsed = F,
                  withSpinner(
                    plotOutput("plot", height = "450px"), color = "orange"
                  )
                ),
                box(
                  title = "Distance Traveled from High School to College (Farthest to Closest)",
                  status = "primary",
                  background = "aqua",
                  solidHeader = T, width = 12,
                  collapsible = T, collapsed = F,
                  DTOutput("summary_stats", height = "230px")
                )
              )
      ) ## end of compare tab (tab 3)
      
    ) ## end of tab items
  ) ## end of dashboard body
)

server <- function(input, output, session) {
  # hold the sport choice
  chosenSport <- reactiveVal('basketball')
  chosenSchool <- reactiveVal(NULL)
  chosenYearRange <- reactiveVal(NULL)
  
  # default to basketball, but can switch to football with button
  #disable("choose_sport")
  
  # hide initial other tabs
  #shinyjs::hide("year_range")
  shinyjs::hide("team")
  shinyjs::hide("make_map")
  
  ## for switching schools, takes back to 'teams' tab (filters)
  observeEvent(input$switch_school, {
    updateTabItems(session, "tabs", "filters")  # switch to filters tab

  })
  
  ## update selections if click logo ------------->
  observeEvent(input$select_arizona, {
    req(chosenSport())
    # update reactive value for team
    chosenSchool("arizona")
    updateSelectInput(session, "team", selected = "arizona")
    # move tabs based on selection
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_arizona_state, {
    req(chosenSport())
    chosenSchool("arizona-state")
    updateSelectInput(session, "team", selected = "arizona-state")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_baylor, {
    req(chosenSport())
    chosenSchool("baylor")
    updateSelectInput(session, "team", selected = "baylor")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_byu, {
    req(chosenSport())
    chosenSchool("byu")
    updateSelectInput(session, "team", selected = "byu")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_cincinnati, {
    req(chosenSport())
    chosenSchool("cincinnati")
    updateSelectInput(session, "team", selected = "cincinnati")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_colorado, {
    req(chosenSport())
    chosenSchool("colorado")
    updateSelectInput(session, "team", selected = "colorado")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_houston, {
    req(chosenSport())
    chosenSchool("houston")
    updateSelectInput(session, "team", selected = "houston")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_iowa_state, {
    req(chosenSport())
    chosenSchool("iowa-state")
    updateSelectInput(session, "team", selected = "iowa-state")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_kansas, {
    req(chosenSport())
    chosenSchool("kansas")
    updateSelectInput(session, "team", selected = "kansas")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_kansas_state, {
    req(chosenSport())
    chosenSchool("kansas-state")
    updateSelectInput(session, "team", selected = "kansas-state")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_oklahoma_state, {
    req(chosenSport())
    chosenSchool("oklahoma-state")
    updateSelectInput(session, "team", selected = "oklahoma-state")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_tcu, {
    req(chosenSport())
    chosenSchool("tcu")
    updateSelectInput(session, "team", selected = "tcu")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_texas_tech, {
    req(chosenSport())
    chosenSchool("texas-tech")
    updateSelectInput(session, "team", selected = "texas-tech")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_ucf, {
    req(chosenSport())
    chosenSchool("central-florida")
    updateSelectInput(session, "team", selected = "central-florida")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_utah, {
    req(chosenSport())
    chosenSchool("utah")
    updateSelectInput(session, "team", selected = "utah")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  observeEvent(input$select_west_virginia, {
    req(chosenSport())
    chosenSchool("west-virginia")
    updateSelectInput(session, "team", selected = "west-virginia")
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")  # switch to summary tab
    } else {
      updateTabItems(session, "tabs", "compare")  # switch to compare tab
    }
  })
  
  ## launch modal on button click
  observeEvent(input$choose_sport, {
    showModal(modalDialog(
      title = "Change Sport",
      radioButtons(
        "sport_modal", NULL,
        choices = sort(str_to_title(sport_selections$sport))
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
    #chosenSchool(input$team)
    chosenYearRange(input$year_range)
    
    removeModal()
    
  
    # show hidden tabs
    #shinyjs::show("year_range")
    #shinyjs::show("team")
    #shinyjs::show("make_map")
  })
  
  observeEvent(input$team, {
    if (input$team != "") {
      chosenSchool(input$team)
    }
  })
  
  ## switcher for moving between tabs
  observeEvent(input$switch_to_plot, {
    #chosenSchool(input$team)
    #updateSelectInput(session, "team", selected = chosenSchool())
    updateTabItems(session, "tabs", "compare")
    
  })
  observeEvent(input$switch_to_map, {
    #chosenSchool(input$team)
    #updateSelectInput(session, "team", selected = chosenSchool())
    updateTabItems(session, "tabs", "summary")
    
  })
  
  ## reactive filtering uses chosenSport()
  filtered_data <- reactive({
    req(chosenSport(), input$team, input$year_range)
    yrs <- as.integer(format(input$year_range, "%Y"))
    
    sp <- tolower(chosenSport())
    
    # set database based on sport ->
    if (sp == "basketball") {
      db_table = "recruit_class_basketball"
    }
    if (sp == "football") {
      db_table = "recruit_class_football"
    }
    
    geting_data <- paste0("Select * from ",db_table," where sport = '",sp, 
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
    
    ## fix top 150 national ranks - usually only rank 150 so limit to 150
    big12_data_wDis <- big12_data_wDis %>% 
      mutate(NationalRank = ifelse(NationalRank > 150, NA, NationalRank))
    
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
  
  ## not using summary preview anymore
  # output$summary_preview <- renderDT({
  #   if (nrow(filtered_data()) == 0) {
  #     return(data.frame(Message =
  #                         "No recruits found for the selected filters. Please adjust your selections."))
  #   }
  #   if (nrow(filtered_data()) > 0) {
  #     d <- filtered_data() %>% 
  #       select(Name, Year, Position, Location, Ranking, NationalRank) %>%
  #       arrange(desc(Ranking), NationalRank, Name)
  #     d %>% 
  #       datatable(
  #         options = list(pageLength = 10,
  #                        lengthChange = FALSE),
  #         height = "400px", class = "stripe hover",
  #         colnames = c("Recruit", "Class Year", "Position", "High School", "247Sports Ranking", "National Ranking"),
  #         style = "bootstrap4",
  #         caption = '"247Sports Ranking" are player scores from 1 to 100 (100 being best).
  #         "National Ranking" ranks top 150 recruits in the country each year (1 being the best).',
  #         rownames = FALSE
  #       )
  #   }
  # })
  
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
  #observeEvent(input$make_map, {
  observe({
    
    req(chosenSport(),input$year_range, input$team)
    
    # chosenSchool(input$team)
    # updateSelectInput(session, "team", selected = chosenSchool())
    
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
      req(input$show_outliers)
      
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