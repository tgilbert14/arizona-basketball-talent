# Ranked UA recruiting evaluations from 2010 to present (unranked players not included)
# this is also excluding transfers

# Load the necessary library
library(rvest)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(stringr)


## set environment path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
app_path<-getwd()


sport <- "basketball"
#sport <- "football"

## dont do newest year! Have undecided players...

# years to evaluate, rankings started on 247 in 2010
year <- c(2016:2025)

#https://247sports.com/season/2024-basketball/RecruitRankings/?InstitutionGroup=highschool

## df to store player data
recruit_data <- data.frame(
  recruit = character(),
  ranking = integer(),
  `high school` = character(),
  position = character(),
  score = numeric(),
  year = integer())

s=1
# for each school
while (s <= length(year)) {
  
  call <- paste0("https://247sports.com/season/",year[s],"-",sport,"/RecruitRankings/?InstitutionGroup=highschool")
  page <- read_html(call)
  
  scores <- page %>%
    html_nodes(".wrapper") %>%
    html_text(trim = TRUE)
  
  playerRankings <- scores
  
  i=1
  while(i <= length(playerRankings)) {
    ## default no shift
    shift = 0
    
    # if extra fields (movement score or percent prediction)
    if (length(str_split(playerRankings[i], "\\s{2,}")[[1]]) > 8 && !grepl("%",playerRankings[i])) {
      ## shift if extra score value
      shift = 1
    }
    
    rank <- str_split(playerRankings[i], "\\s{2,}")[[1]][1]
    name <- str_split(playerRankings[i], "\\s{2,}")[[1]][3+shift]
    hs <- str_split(playerRankings[i], "\\s{2,}")[[1]][4+shift]
    pos <- str_split(playerRankings[i], "\\s{2,}")[[1]][5+shift]
    sco <- str_split(playerRankings[i], "\\s{2,}")[[1]][7+shift]
    y <- year[s]
    
    recruit_data <- rbind(recruit_data, data.frame(
      recruit = name,
      ranking = rank,
      `high school` = hs,
      position = pos,
      score = sco,
      year = y
    ))
    
    # goes player to player
    i=i+1
  }
  cat(paste0("Year ",year[s]," complete for ",sport,"... \n"))
  # View(recruit_data)
  # write_csv(recruit_data, paste0("csvs/",sport,"/recruits_",year[s],".csv"))
  # goes year to year
  s=s+1
}

write_csv(recruit_data, paste0("csvs/",sport,"/recruits.csv"))
View(recruit_data)

data <- read_csv("scripts/csvs/basketball/recruits.csv")

## clean up data
unique(data$position)

## to fix...
data[grepl("7-1 / 200", data$position),]

data[grepl("7-1 / 200", data$position),][1] <- "Kai Sotto"
data[grepl("7-1 / 200", data$position),][5] <- "96"
data[grepl("7-1 / 200", data$position),][3] <- "The Skill Factory (Atlanta, GA)"
data[grepl("7-1 / 200", data$position),][4] <- "C"

## clean up data
unique(data$position)

unique(data$ranking)

unique(data$score)

unique(data$year)

nrow(data)
length(unique(data$high.school))

View(data)
write_csv(data, paste0("csvs/",sport,"/recruits_cleaned.csv"))


