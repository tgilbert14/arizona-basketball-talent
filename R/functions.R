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

## must have column 'miles_away'
remove_Outliers <- function(data) {
  ## look at data outside of Quartile1/3 +- 1.5 (IQR)
  Q1 <- quantile(data$miles_away)[[2]] # 25%
  Q3 <- quantile(data$miles_away)[[4]] # 75%
  IQR = Q3 - Q1
  ## set bounds
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  
  ## filter data outside of bounds
  clean_data <- data %>% 
    filter(miles_away >= lower_bound & miles_away <= upper_bound) %>% 
    arrange(miles_away)
  
  # ## look at only outlier data
  # possible_outliers <- data %>% 
  #   filter(miles_away < lower_bound | miles_away > upper_bound) %>% 
  #   arrange(desc(miles_away))

  return(clean_data)
}

## must have column 'miles_away'
get_Outliers <- function(data) {
  ## look at data outside of Quartile1/3 +- 1.5 (IQR)
  Q1 <- quantile(data$miles_away)[[2]] # 25%
  Q3 <- quantile(data$miles_away)[[4]] # 75%
  IQR = Q3 - Q1
  ## set bounds
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  
  # ## filter data outside of bounds
  # clean_data <- data %>% 
  #   filter(miles_away >= lower_bound & miles_away <= upper_bound) %>% 
  #   arrange(miles_away)
  
  ## look at only outlier data
  possible_outliers <- data %>% 
    filter(miles_away < lower_bound | miles_away > upper_bound) %>% 
    arrange(desc(miles_away))
  
  return(possible_outliers)
}
