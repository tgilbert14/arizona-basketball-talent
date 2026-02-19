# Load the necessary library
#library(rvest)
#library(ggplot2)
#library(ggrepel)
#library(stringr)

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(DBI)
library(DT)
library(RSQLite)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(here)
library(sf)
library(smoothr)
library(gt)
library(geosphere)
library(ggplot2)
library(ggrepel)

# Link to icon
#my_icon = "www/icons8-location-48.png"

safe_query <- function(conn, query) {
  tryCatch({
    result <- dbGetQuery(conn, query)
    if (nrow(result) == 0) return(data.frame(sport = character(0)))
    result
  }, error = function(e) {
    warning("Query failed: ", conditionMessage(e))
    data.frame(sport = character(0))
  })
}




