## ===========================================================================
## Big 12 Talent Pathways — v2 "Size Lab" edition
## New in v2:
##   * Size Lab (Girth) tab — interactive height x weight Body Map, value
##     boxes, position DNA, auto-generated talking points
##   * Conference Beef tab — Beef Board leaderboards (logos), size-over-time
##     vs the conference band, head-to-head position-group comparison
##   * Team logo grid + handlers generated from R/team_config.R (was 16
##     copy-pasted blocks), shared metrics in R/girth_functions.R
##   * Bug fixes: StateRank was assigned from PositionRank; duplicate
##     button input IDs across tabs
## The previous app is preserved as app_v1.R.
## Plot builders live in R/girth_plots.R (also used by scripts/girth_analysis.R)
## ===========================================================================

# use this for shiny io deployment (connect to .db) -->
db_path <- here("data", "recruiting.db")
conn <- dbConnect(RSQLite::SQLite(), db_path)

# Pre-compute choices from database
team_selections.1 <- safe_query(conn, "SELECT DISTINCT School FROM recruit_class_football")
team_selections.2 <- safe_query(conn, "SELECT DISTINCT School FROM recruit_class_basketball")
team_selections <- union(team_selections.1, team_selections.2)

sport_selections.1 <- safe_query(conn, "SELECT DISTINCT sport FROM recruit_class_football")
sport_selections.2 <- safe_query(conn, "SELECT DISTINCT sport FROM recruit_class_basketball")
sport_selections <- union(sport_selections.1, sport_selections.2)

## preload + prep the size data once at startup (small tables, fast queries)
size_football <- safe_query(conn, "SELECT * FROM recruit_class_football") %>%
  prep_size_data("football")
size_basketball <- safe_query(conn, "SELECT * FROM recruit_class_basketball") %>%
  prep_size_data("basketball")

SIZE_YEARS <- range(c(size_football$Year, size_basketball$Year))

## named choices for team pickers (slug values, pretty labels)
team_choices <- setNames(TEAM_CONFIG$slug, TEAM_CONFIG$team_name)

## position-group filter choices per sport
pos_choices <- function(sport) {
  if (tolower(sport) == "football") {
    c("All", "Trenches (OL + DL/Edge)",
      setdiff(position_levels("football"), "Other"))
  } else {
    c("All", setdiff(position_levels("basketball"), "Other"))
  }
}

## girth metric choices
metric_choices <- c("Average Weight" = "AvgWeight",
                    "Average Height" = "AvgHeight",
                    "Pounds per Inch" = "AvgLbsPerIn",
                    "Average BMI" = "AvgBMI")

## UI -->
ui <- dashboardPage(

  dashboardHeader(title = "Big 12"),
  skin = "yellow",

  dashboardSidebar(
    width = 250,
    collapsed = TRUE,

    sidebarMenu(id = "tabs",
                menuItem("Teams", tabName = "filters", icon = icon("filter")),
                menuItem("Pipeline Map", tabName = "summary", icon = icon("chart-bar")),
                menuItem("Distance Comparison", tabName = "compare", icon = icon("clock")),
                menuItem("Size Lab (Girth)", tabName = "sizelab", icon = icon("weight-hanging")),
                menuItem("Conference Beef", tabName = "beef", icon = icon("dumbbell"))
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
    }
    .small-box h3 { font-size: 26px; }
    .talking-points li { margin-bottom: 9px; font-size: 15px; }"))
    ),

    tabItems(
      ## Filters tab ---------------------------------------------------------
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  title = HTML("<span style='color: teal; font-size: 20px; font-weight: bold;'>
                  <em>Where do Big 12 recruits come from &mdash; and how big are they?</em></span><br>
                               </span>"),
                  status = "warning",
                  solidHeader = F,
                  width = 3,

                  footer = HTML("<span style='color: teal; font-size: 14px;'>
                  This app lets you <strong>explore Big 12 football and
                  basketball recruiting classes.</strong> Map where commits
                  come from (<em>'Distance Traveled by Recruits'</em>), chart
                  distance over time (<em>'Distance Traveled Over Time'</em>),
                  or break down how much size each program signs in the
                  <em>'Big 12 Size Lab'</em>. To get started,
                  <strong>click a team</strong>.</span>"),

                  # first selections -->
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
                    choices = c("Distance Traveled by Recruits",
                                "Distance Traveled Over Time",
                                "Big 12 Size Lab"),
                    selectize = FALSE,
                    selected = "Distance Traveled Over Time",
                    width = "100%",
                    size = 3
                  ),
                  selectInput(
                    "team", "Pick Big 12 Team",
                    choices = sort(c("", team_selections$School)),
                    selectize = FALSE,
                    selected = FALSE,
                    width = "100%",
                    size = 3
                  ),
                  actionButton(
                    inputId = "choose_sport_filters",
                    label = "Select Sport",
                    width = "100%",
                    class = "btn-info"
                  )
                ), # end of selection box

                ## LOGO grid generated from TEAM_CONFIG
                box(
                  title = NULL, status = "info",
                  background = "navy",
                  solidHeader = T, width = 9, collapsed = F,

                  lapply(seq_len(nrow(TEAM_CONFIG)), function(i) {
                    actionButton(
                      inputId = paste0("select_",
                                       gsub("-", "_", TEAM_CONFIG$slug[i])),
                      label = div(
                        style = "text-align:center; margin-top: 10px; margin-bottom: 10px;",
                        img(src = TEAM_CONFIG$logo[i], height = "80px")
                      ),
                      style = "background-color: transparent; border: none; width: 24%;"
                    )
                  })
                )
              )
      ), ## end of filters tab

      ## Summary tab --> the pipeline map ------------------------------------
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  fluidRow(
                    column(width = 3,
                           actionButton("switch_school_summary", "Switch School",
                                        class = "btn-warning", width = "100%")),
                    column(width = 3,
                           actionButton("choose_sport_summary", "Switch Sport",
                                        class = "btn-info", width = "100%",
                                        style = "margin-bottom: 10px;")),
                    column(width = 6,
                           actionButton(inputId = "switch_to_plot",
                                        label = tagList(icon("chart-line"), "View change over time"),
                                        class = "btn-warning", width = "100%",
                                        style = "margin-bottom: 10px;"))
                  ),

                  title = "Click each dot to reveal more information about each recruit!",
                  footer = HTML("<span style='color: #FFA500;'>
                    <em>*Polygons grouped by State. Only commits with reported high schools mapped (Transfers not included).
                    Data from 247Sports as of Jan 2026.</em>
                                </span>"),
                  status = "info",
                  background = "navy",
                  solidHeader = TRUE, width = 12,
                  collapsible = T, collapsed = F,
                  withSpinner(leafletOutput("gridPlot", height = "320px"),
                              color = "orange")),
                ## data table
                box(
                  title = "Distance Traveled from High School to College (Farthest to Closest)",
                  status = "primary",
                  background = "aqua",
                  solidHeader = T, width = 12,
                  collapsible = T, collapsed = F,
                  DTOutput("summary_stats", height = "230px")
                )
              )
      ), ## end of summary tab

      ## Compare tab --> the distance scatter plot ---------------------------
      tabItem(tabName = "compare",
              fluidRow(
                box(
                  fluidRow(
                    column(width = 2,
                           actionButton("switch_school_compare", "Switch School",
                                        class = "btn-warning", width = "100%")),
                    column(width = 3,
                           actionButton("choose_sport_compare", "Switch Sport",
                                        class = "btn-info", width = "100%",
                                        style = "margin-bottom: 10px;")),
                    column(width = 3,
                           actionButton(inputId = "switch_to_map",
                                        label = tagList(icon("map"), "View map locations"),
                                        class = "btn-warning", width = "100%",
                                        style = "margin-bottom: 10px;")),
                    column(width = 4,
                           selectInput("show_outliers", label = NULL,
                                       selectize = FALSE, multiple = FALSE,
                                       choices = c("Show Outliers" = "show",
                                                   "Hide Outliers" = "hide"),
                                       selected = "show", width = "100%"))
                  ),

                  title = "Comparing Distance Traveled by Recruits Over Time",
                  footer = HTML("<span style='color: #FFA500;'>
                  Data was scraped from 247Sports as of Jan 2026.</span>"),
                  status = "info",
                  background = "navy",
                  solidHeader = TRUE, width = 12,
                  collapsible = T, collapsed = F,
                  withSpinner(
                    plotOutput("plot", height = "450px"), color = "orange")),
                ## box plot
                box(
                  title = "Distance Traveled from High School to College (Box Plot)",
                  status = "primary",
                  background = "aqua",
                  solidHeader = T, width = 12,
                  collapsible = T, collapsed = F,
                  footer = HTML("<span style='color: #FFA500;'>
                  Transfers not included in plot.</span>"),
                  plotOutput("box_plot", height = "230px"))
              )
      ), ## end of compare tab

      ## Size Lab tab --> NEW in v2 ------------------------------------------
      tabItem(tabName = "sizelab",
              fluidRow(
                box(
                  width = 12, status = "info", background = "navy",
                  solidHeader = TRUE,
                  title = "Size Lab: how big are the bodies your program signs?",
                  fluidRow(
                    column(width = 3,
                           selectInput("size_team", NULL, choices = team_choices,
                                       selected = "arizona", width = "100%")),
                    column(width = 3,
                           actionButton("choose_sport_sizelab", "Switch Sport",
                                        class = "btn-info", width = "100%")),
                    column(width = 6,
                           sliderInput("size_years", NULL,
                                       min = SIZE_YEARS[1], max = SIZE_YEARS[2],
                                       value = SIZE_YEARS, step = 1, sep = "",
                                       width = "100%"))
                  ),
                  footer = HTML("<span style='color: #FFA500;'><em>Heights and
                  weights are 247Sports recruiting measurements at commit time
                  (high school commits only, transfers excluded).</em></span>")
                )
              ),
              fluidRow(
                valueBoxOutput("vb_height", width = 3),
                valueBoxOutput("vb_weight", width = 3),
                valueBoxOutput("vb_lbsin", width = 3),
                valueBoxOutput("vb_rank", width = 3)
              ),
              fluidRow(
                box(
                  title = "The Body Map: every commit, height x weight (hover the dots!)",
                  status = "info", background = "navy", solidHeader = TRUE,
                  width = 12, collapsible = T,
                  withSpinner(girafeOutput("body_map", height = "560px"),
                              color = "orange")
                )
              ),
              fluidRow(
                box(
                  title = "Position DNA vs the Conference",
                  status = "primary", background = "aqua", solidHeader = T,
                  width = 7, collapsible = T,
                  withSpinner(plotOutput("dna_plot", height = "430px"),
                              color = "orange")
                ),
                box(
                  title = "Auto-Generated Talking Points (take these to the message board)",
                  status = "warning", solidHeader = T,
                  width = 5, collapsible = T,
                  htmlOutput("talking_points")
                )
              )
      ), ## end of sizelab tab

      ## Conference Beef tab --> NEW in v2 -----------------------------------
      tabItem(tabName = "beef",
              fluidRow(
                box(
                  width = 12, status = "info", background = "navy",
                  solidHeader = TRUE,
                  title = "Conference Beef: who signs the biggest classes in the Big 12?",
                  fluidRow(
                    column(width = 2,
                           selectInput("size_metric", NULL,
                                       choices = metric_choices,
                                       selected = "AvgWeight", width = "100%")),
                    column(width = 3,
                           selectInput("size_pos", NULL,
                                       choices = pos_choices("basketball"),
                                       selected = "All", width = "100%")),
                    column(width = 3,
                           selectInput("h2h_team", "Compare against:",
                                       choices = team_choices,
                                       selected = "arizona-state", width = "100%")),
                    column(width = 2,
                           sliderInput("beef_years", NULL,
                                       min = SIZE_YEARS[1], max = SIZE_YEARS[2],
                                       value = SIZE_YEARS, step = 1, sep = "",
                                       width = "100%")),
                    column(width = 2,
                           actionButton("choose_sport_beef", "Switch Sport",
                                        class = "btn-info", width = "100%"))
                  )
                )
              ),
              fluidRow(
                column(width = 5,
                       box(
                         title = "Big 12 Beef Board",
                         status = "primary", background = "aqua", solidHeader = T,
                         width = NULL, collapsible = T,
                         withSpinner(plotOutput("beef_board", height = "640px"),
                                     color = "orange")
                       )),
                column(width = 7,
                       box(
                         title = "Size Over Time vs the Conference",
                         status = "primary", background = "aqua", solidHeader = T,
                         width = NULL, collapsible = T,
                         withSpinner(plotOutput("size_trend", height = "300px"),
                                     color = "orange")
                       ),
                       box(
                         title = "Head to Head: Position-Group Weigh-In",
                         status = "primary", background = "aqua", solidHeader = T,
                         width = NULL, collapsible = T,
                         withSpinner(plotOutput("h2h_plot", height = "300px"),
                                     color = "orange")
                       ))
              )
      ) ## end of beef tab

    ) ## end of tab items
  ) ## end of dashboard body
) ## end of UI

server <- function(input, output, session) {
  # hold the sport + school choices
  chosenSport <- reactiveVal("basketball")
  chosenSchool <- reactiveVal("arizona")  # default so Size Lab works instantly
  chosenYearRange <- reactiveVal(NULL)

  # hide initial controls
  shinyjs::hide("team")
  shinyjs::hide("make_map")

  ## ---- shared navigation helpers -----------------------------------------

  ## jump to the tab matching the chosen viz type
  go_to_viz <- function() {
    if (input$vizType == "Distance Traveled by Recruits") {
      updateTabItems(session, "tabs", "summary")
    } else if (input$vizType == "Distance Traveled Over Time") {
      updateTabItems(session, "tabs", "compare")
    } else {
      updateTabItems(session, "tabs", "sizelab")
    }
  }

  ## one handler per team logo, generated from TEAM_CONFIG
  lapply(seq_len(nrow(TEAM_CONFIG)), function(i) {
    slug <- TEAM_CONFIG$slug[i]
    btn_id <- paste0("select_", gsub("-", "_", slug))
    observeEvent(input[[btn_id]], {
      req(chosenSport())
      chosenSchool(slug)
      updateSelectInput(session, "team", selected = slug)
      updateSelectInput(session, "size_team", selected = slug)
      go_to_viz()
    })
  })

  ## "Switch School" buttons (one per tab to keep IDs unique)
  lapply(c("switch_school_summary", "switch_school_compare"), function(id) {
    observeEvent(input[[id]], {
      updateTabItems(session, "tabs", "filters")
    })
  })

  ## "Switch Sport" buttons -> launch modal
  lapply(c("choose_sport_filters", "choose_sport_summary",
           "choose_sport_compare", "choose_sport_sizelab",
           "choose_sport_beef"), function(id) {
    observeEvent(input[[id]], {
      showModal(modalDialog(
        title = "Change Sport",
        radioButtons(
          "sport_modal", NULL,
          choices = sort(str_to_title(sport_selections$sport)),
          selected = str_to_title(chosenSport())
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_sport", "Confirm")
        ),
        easyClose = FALSE
      ))
    })
  })

  ## confirm & lock in sport choice
  observeEvent(input$confirm_sport, {
    req(input$sport_modal)
    chosenSport(input$sport_modal)
    chosenYearRange(input$year_range)
    ## position-group filter depends on sport
    updateSelectInput(session, "size_pos",
                      choices = pos_choices(input$sport_modal),
                      selected = "All")
    removeModal()
  })

  observeEvent(input$team, {
    if (input$team != "") {
      chosenSchool(input$team)
      updateSelectInput(session, "size_team", selected = input$team)
    }
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

    sp <- tolower(chosenSport())

    # set database table based on sport ->
    if (sp == "basketball") {
      db_table = "recruit_class_basketball"
    }
    if (sp == "football") {
      db_table = "recruit_class_football"
    }

    geting_data <- paste0("Select * from ", db_table, " where sport = '", sp,
                          "' AND School = '", input$team, "' AND Year >= ", yrs[1], " AND Year <= ", yrs[2],
                          " ORDER BY Ranking, NationalRank desc, StateRank desc, PositionRank desc, Name")

    all_data <- safe_query(conn, geting_data)

    all_data$lat <- as.numeric(all_data$lat)
    all_data$long <- as.numeric(all_data$long)
    all_data$college_lat <- as.numeric(all_data$college_lat)
    all_data$college_long <- as.numeric(all_data$college_long)
    all_data$Ranking <- as.numeric(all_data$Ranking)
    all_data$NationalRank <- as.numeric(all_data$NationalRank)
    all_data$PositionRank <- as.numeric(all_data$PositionRank)
    all_data$StateRank <- as.numeric(all_data$StateRank)  # v1 bug: was PositionRank

    ## calculate miles away
    big12_data <- all_data %>%
      mutate(disFromHS_m =
               distGeo(p1 = cbind(long, lat),
                       p2 = cbind(college_long, college_lat)))

    ## change distance to miles instead of meters -->
    meters_per_mile <- 1609.34
    big12_data_wDis <- big12_data %>%
      mutate(miles_away = round(disFromHS_m / meters_per_mile, 0))

    ## fix top 150 national ranks - usually only rank 150 so limit to 150
    big12_data_wDis <- big12_data_wDis %>%
      mutate(NationalRank = ifelse(NationalRank > 150, NA, NationalRank))

    ## clean names (shared helper in R/girth_functions.R)
    data_final <- big12_data_wDis %>%
      mutate(University = pretty_university(School))
    data_final
  })

  ## jump to the right viz once selections are ready (same UX as v1)
  observe({
    req(chosenSport(), input$year_range, input$team)
    go_to_viz()
  })

  ## ---- legacy renders (sourced scripts, same contract as v1) -------------

  # render map
  output$gridPlot <- renderLeaflet({
    req(chosenSport(), input$year_range, input$team)
    sp <- chosenSport()
    source("scripts/map.R", local = T)
    final_map
  })

  # render box plot
  output$box_plot <- renderPlot({
    req(chosenSport(), input$year_range, input$team)
    sp <- chosenSport()
    source("scripts/box_plot.R", local = T)
    my_box_plot
  })

  ## render scatter plot
  output$plot <- renderPlot({
    req(chosenSport(), input$year_range, input$team, input$show_outliers)
    sp <- chosenSport()
    source("scripts/plot.R", local = T)
    final_plot
  })

  ## render data table
  output$summary_stats <- renderDT({
    req(chosenSport(), input$year_range, input$team)
    if (nrow(filtered_data()) == 0) {
      return(data.frame(Message =
                          "No recruits found for the selected filters. Please adjust your selections."))
    } else {

      big12_data_wDis <- filtered_data()

      d <- big12_data_wDis %>%
        select(Name, miles_away, Location, University, School_City, Ranking, NationalRank,
               Position, Height, Weight, Year) %>%
        arrange(desc(miles_away))

      d2 <- as.data.frame(d) %>%
        datatable(
          colnames = c("Recruit", "Distance Traveled (miles)", "From", "To", "City",
                       "247Sports Ranking", "National Ranking", "Position", "Height", "Weight", "Year"),
          options = list(pageLength = 10,
                         lengthChange = FALSE),
          rownames = FALSE)
      d2
    }
  })

  ## ---- Size Lab reactives + renders (NEW in v2) ---------------------------

  ## full prepped table for the current sport (loaded once at startup)
  size_all <- reactive({
    if (tolower(chosenSport()) == "football") size_football else size_basketball
  })

  ## windowed by the Size Lab year slider
  size_lab_data <- reactive({
    req(input$size_years)
    size_all() %>%
      filter(Year >= input$size_years[1], Year <= input$size_years[2])
  })

  ## windowed by the Conference Beef year slider
  beef_data <- reactive({
    req(input$beef_years)
    size_all() %>%
      filter(Year >= input$beef_years[1], Year <= input$beef_years[2])
  })

  ## the Size Lab team's rows
  size_team_rows <- reactive({
    req(input$size_team)
    size_lab_data() %>% filter(School == input$size_team)
  })

  ## value boxes
  output$vb_height <- renderValueBox({
    rows <- size_team_rows()
    val <- if (nrow(rows) > 0) format_height(mean(rows$Height_in)) else "—"
    valueBox(val, paste0(team_label(input$size_team), " Avg Height"),
             icon = icon("ruler-vertical"), color = "light-blue")
  })
  output$vb_weight <- renderValueBox({
    rows <- size_team_rows()
    val <- if (nrow(rows) > 0) paste0(round(mean(rows$Weight), 0), " lbs") else "—"
    valueBox(val, paste0(team_label(input$size_team), " Avg Weight"),
             icon = icon("weight-hanging"), color = "navy")
  })
  output$vb_lbsin <- renderValueBox({
    rows <- size_team_rows()
    val <- if (nrow(rows) > 0) sprintf("%.2f", mean(rows$LbsPerInch)) else "—"
    valueBox(val, "Pounds per Inch (girth index)",
             icon = icon("compress"), color = "orange")
  })
  output$vb_rank <- renderValueBox({
    board <- team_size_summary(size_lab_data()) %>% arrange(desc(AvgWeight))
    rk <- which(board$School == input$size_team)
    val <- if (length(rk) == 1) paste0("#", rk, " of ", nrow(board)) else "—"
    valueBox(val, "Beef Rank (by avg weight)",
             icon = icon("trophy"), color = "yellow")
  })

  ## the interactive Body Map
  output$body_map <- renderGirafe({
    req(input$size_team)
    validate(need(nrow(size_lab_data()) > 0,
                  "No commits found for this window."))
    p <- plot_body_map(size_lab_data(), input$size_team,
                       tolower(chosenSport()))
    girafe(
      ggobj = p, width_svg = 11.5, height_svg = 7,
      options = list(
        opts_tooltip(css = paste0(
          "background-color:#0C234B;color:white;padding:8px;",
          "border-radius:6px;font-size:13px;")),
        opts_hover(css = "stroke:#0C234B;stroke-width:2px;"),
        opts_toolbar(saveaspng = TRUE)
      )
    )
  })

  ## position DNA
  output$dna_plot <- renderPlot({
    req(input$size_team)
    validate(need(nrow(size_team_rows()) > 0,
                  "No commits found for this team in this window."))
    plot_position_dna(size_lab_data(), input$size_team, tolower(chosenSport()))
  })

  ## auto talking points
  output$talking_points <- renderUI({
    req(input$size_team)
    pts <- make_talking_points(size_lab_data(), input$size_team,
                               tolower(chosenSport()))
    validate(need(length(pts) > 0, "Not enough data in this window."))
    HTML(paste0("<ul class='talking-points'>",
                paste0("<li>", pts, "</li>", collapse = ""),
                "</ul>"))
  })

  ## ---- Conference Beef renders -------------------------------------------

  output$beef_board <- renderPlot({
    req(input$size_team, input$size_metric, input$size_pos)
    validate(need(nrow(filter_pos(beef_data(), input$size_pos)) > 0,
                  "No commits found for this position filter."))
    plot_beef_board(beef_data(), input$size_team, tolower(chosenSport()),
                    metric = input$size_metric, pos_filter = input$size_pos)
  })

  output$size_trend <- renderPlot({
    req(input$size_team, input$size_metric, input$size_pos)
    trend_data <- filter_pos(beef_data(), input$size_pos)
    validate(need(
      nrow(dplyr::filter(trend_data, School == input$size_team)) > 0,
      "No commits found for this team + position filter."))
    plot_size_trend(beef_data(), input$size_team, tolower(chosenSport()),
                    metric = input$size_metric, pos_filter = input$size_pos)
  })

  output$h2h_plot <- renderPlot({
    req(input$size_team, input$h2h_team)
    validate(need(input$size_team != input$h2h_team,
                  "Pick two different teams to compare."))
    plot_head_to_head(beef_data(), input$size_team, input$h2h_team,
                      tolower(chosenSport()))
  })

}

shinyApp(ui, server)
