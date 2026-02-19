## easy way to deploy app
#rsconnect::deployApp()


## load libraries
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(dplyr)
library(stringr)
library(readr)
library(DBI)
library(RSQLite)
library(leaflet)
library(here)
library(sf)
library(smoothr)
library(gt)
library(geosphere)
library(ggplot2)
library(ggrepel)
#library(plotly)

## query for getting data
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

## Link to icon
#my_icon = "www/icons8-location-48.png"
