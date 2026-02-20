library(DBI)
library(RSQLite)
library(readr)
library(rvest)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)


##### scrape data

sport <- "basketball"
#sport <- "football"

## BIG12 schools
school <- c("arizona","arizona-state","baylor","byu","cincinnati","colorado",
            "houston","iowa-state","kansas","kansas-state","oklahoma-state",
            "tcu","texas-tech","central-florida","utah","west-virginia")
# list of all power confrence schools

# ## BIG10 schools
# school <- c("illinois","indiana","iowa","maryland","michigan","michigan-state",
#             "minnesota","nebraska","northwestern","ohio-state",
#             "penn-state","rutgers","wisconsin", "purdue", "oregon",
#             "washington", "usc", "ucla")
# sort(school)

# years to evaluate, rankings started on 247 in 2010
year <- c(2016:2026)

sport <- tolower(sport)
school <- tolower(school)
#https://247sports.com/college/arizona/season/2025-football/commits/

s=1
# for each school
while (s <= length(school)) {
  
  i=1
  # get player data for each year and create dataframe with all data
  while (i <= length(year)) {
    call <- paste0("https://247sports.com/college/",school[s],"/season/",year[i],"-",sport,"/commits/")
    page <- read_html(call)
    
    # give time to connect to webpage
    #Sys.sleep(10)
    
    scores <- page %>%
      html_nodes(".ri-page__star-and-score .score , .ri-page__name-link , .wrapper .position , .wrapper .metrics , .posrank , .withDate , .meta , .sttrank , .natrank") %>%
      html_text(trim = TRUE)
    
    # getting rid of commits with no scores
    playerRankings <- scores[scores != "Rating"]
    #playerRankings <- playerRankings[!grepl("^N/A",playerRankings)]
    playerRankings <- playerRankings[!grepl("^Commit",playerRankings)]
    #playerRankings <- playerRankings[!grepl("\\(HS\\)",playerRankings)]
    #playerRankings <- playerRankings[!grepl("\\(T\\)",playerRankings)]
    #playerRankings <- playerRankings[!is.na(playerRankings)]
    
    if (length(playerRankings) > 0) {
      
      # Reshape into a data frame by alternating pairs - if there is data
      name_vec <- playerRankings[seq(1, length(playerRankings), by = 8)]
      rank_vec <- playerRankings[seq(4, length(playerRankings), by = 8)]
      natRank_vec <- playerRankings[seq(5, length(playerRankings), by = 8)]
      posRank_vec <- playerRankings[seq(6, length(playerRankings), by = 8)]
      stateRank_vec <- playerRankings[seq(7, length(playerRankings), by = 8)]
      pos_vec <- playerRankings[seq(8, length(playerRankings), by = 8)]
      # splitting out height and weights
      meta_vec <- playerRankings[seq(3, length(playerRankings), by = 8)]
      meta_vec.clean <- gsub(" ", "", meta_vec)
      split_values <- strsplit(meta_vec.clean, "/")
      split_values.unlist <- unlist(split_values)
      height_vec <- split_values.unlist[seq(1, length(split_values.unlist), by = 2)]
      weight_vec <- split_values.unlist[seq(2, length(split_values.unlist), by = 2)]
      # splitting out state from location
      loc_vec <- playerRankings[seq(2, length(playerRankings), by = 8)]
      loc_vec.clean <- gsub(" ", "", loc_vec)
      split_state <- strsplit(loc_vec.clean, ",")
      split_state.unlist <- unlist(split_state)
      state_vec <- gsub(")","",split_state.unlist[seq(2, length(split_state.unlist), by = 2)])
      
      player_df <- data.frame(
        Name = name_vec,
        Location = loc_vec,
        Height = height_vec,
        Weight = weight_vec,
        Ranking = rank_vec,
        NationalRank = natRank_vec,
        PositionRank = posRank_vec,
        StateRank = stateRank_vec,
        State = state_vec,
        Position = pos_vec,
        stringsAsFactors = FALSE)
      #View(player_df)
      
      # clean up unranked players
      player_df <- player_df[player_df$Ranking != 'NA',]
      
      if (nrow(player_df) == 0) {
        # in case there was players but they dont have ratings
        player_df <- data.frame(
          Name = NA,
          Location = NA,
          Height = NA,
          Weight = 0,
          Ranking = 0,
          NationalRank = NA,
          PositionRank = NA,
          StateRank = NA,
          State = NA,
          Position = NA,
          Year = year[i],
          School = school[s],
          Type = "Transfer",
          stringsAsFactors = FALSE)
        
      } else {
        # add year
        player_df$Year <- year[i]
        # add school[s]
        player_df$School <- school[s]
        # add recuit type
        player_df$Type <- "Commit"
      }
      
      # check for transfers -->
      transfer.meta <- page %>%
        html_nodes(".player .score , .portal-list_itm .position , .player .metrics , .player a") %>%
        html_text(trim = TRUE)
      
      # Find indices of all T-ratings
      t_indices <- which(str_detect(transfer.meta, "\\(T\\)"))
      
      # Extract each recruit’s full info from T rating location
      recruits_clean <- lapply(t_indices, function(i) {
        name <- transfer.meta[i - 2]
        size <- transfer.meta[i - 1]
        rating <- transfer.meta[i]
        position <- transfer.meta[i + 2]
        tibble(Name = name, Size = size, Rating = rating, Position = position)
      })
      
      # Combine into a clean dataframe
      transfer.playerRankings <- bind_rows(recruits_clean)
      #print(transfer.playerRankings)
      
      if(nrow(transfer.playerRankings) > 0) {
        # Reshape into a data frame by alternating pairs - if there is data
        name_vec <- transfer.playerRankings$Name
        pos_vec <- transfer.playerRankings$Position
        
        # clean up scores
        rank_vec.raw <- transfer.playerRankings$Rating
        rank_vec.num <- gsub(" \\(.*\\)", "", rank_vec.raw)
        rank_vec.numeric <- as.numeric(rank_vec.num)
        rank_vec <- round(rank_vec.numeric*100, 0)
        
        # splitting out height and weights
        meta_vec <- transfer.playerRankings$Size
        meta_vec.clean <- gsub(" ", "", meta_vec)
        split_values <- strsplit(meta_vec.clean, "/")
        split_values.unlist <- unlist(split_values)
        height_vec <- split_values.unlist[seq(1, length(split_values.unlist), by = 2)]
        weight_vec <- split_values.unlist[seq(2, length(split_values.unlist), by = 2)]
        
        player_df.transfers <- data.frame(
          Name = name_vec,
          Location = NA,
          Height = height_vec,
          Weight = weight_vec,
          Ranking = rank_vec,
          NationalRank = NA,
          PositionRank = NA,
          StateRank = NA,
          State = NA,
          Position = pos_vec,
          stringsAsFactors = FALSE)
        
        # clean up unranked players
        player_df.transfers <- player_df.transfers[player_df.transfers$Ranking != 'NA',]
        # add year
        player_df.transfers$Year <- year[i]
        # add school
        player_df.transfers$School <- school[s]
        # add recuit type
        player_df.transfers$Type <- "Transfer"
        
        # bind to other commits
        player_df <- rbind(player_df, player_df.transfers)
      }
      
      # create new dataframe or add to it
      if (i==1) {
        rec_data <- player_df
      } else {
        rec_data <-rbind(rec_data, player_df)
      }
    }
    #View(rec_data)
    print(paste0("Year ",year[i]," complete for ",school[s],"..."))
    # move on to next year
    i = i+1
    
    # If last scrap, create plot
    if (i==length(year)+1) {
      
      # Convert Ranking to numeric (if it's a character)
      rec_data <- rec_data %>%
        mutate(
          Ranking = as.numeric(Ranking),
          Weight = as.numeric(Weight),
          NationalRank = as.numeric(NationalRank),
          PositionRank = as.numeric(PositionRank),
          StateRank = as.numeric(StateRank)
        )
      
      ordered_recruits <- rec_data %>%
        arrange(Year, desc(Ranking), Name)
      
      all_recruits <- rec_data %>% 
        dplyr::filter(!is.na(Name))
      
      all_recruits <- unique(all_recruits)
      

      # Save school[s] data as csv for comparison later...
      school_clean <- tolower(gsub(" ", "_", school[s]))
      var_name <- paste0("all_recruits_", school_clean)
      assign(var_name, all_recruits)
      file_loc <- paste0("data/csvs/",sport,"/",var_name,".csv")
      # Save CSV
      write_csv(all_recruits, file_loc)
      #shell.exec(file_loc)
      
      print("Complete!")
    }
    
  }
  # move to next school
  s=s+1
  
  if (s == length(school)+1) {
    print("All schools processed")
  }
}


# Compare schools
files_here <- paste0("data/csvs/",sport,"/")
files <- dir(files_here)

x=1
while (x <= length(files)) {
  if (x==1) {
    all_data <- read.csv(paste0(files_here,"/",files[x]))
  }else {
    more_data <- read.csv(paste0(files_here,"/",files[x]))
    all_data <-rbind(all_data, more_data)
  }
  x=x+1
}

write_csv(all_data, paste0("data/csvs/",sport,"/","all_recruits_all_schools.csv"))


## check for errors and clean
football <- read_csv("data/csvs/football/all_recruits_all_schools.csv")
basketball <- read_csv("data/csvs/basketball/all_recruits_all_schools.csv")

## get rid of duplicates
football <- football %>% 
  distinct(Name, Year, School, .keep_all = TRUE) %>% 
  filter(!is.na(Name))
basketball <- basketball %>% 
  distinct(Name, Year, School, .keep_all = TRUE) %>%
  filter(!is.na(Name))

this_data <- basketball
#this_data <- football

## football first
data_cleaned <- this_data %>%
  mutate(
    # Extract what's inside parentheses
    loc_inside_parens = str_extract(Location, "(?<=\\()[^)]+"),
    # Extract name before parentheses and trim
    location_name = str_trim(str_extract(Location, "^[^(]+")),
    # Final cleaned version: add "High School," before the parentheses content
    Location_Clean = paste0(location_name, " High School, ", loc_inside_parens)
  )
View(data_cleaned)

# clean and geocode high school locations
hs_data <- data_cleaned %>%
  #distinct(Location_Clean, .keep_all = TRUE) %>%
  mutate(Location_Clean = str_trim(Location_Clean))

# only want commits
hs_data <- hs_data %>%
  filter(Type == "Commit")

library(tidygeocoder)

# have to do in sections..
hs_data.1 <- hs_data[1:250,]
hs_data.2 <- hs_data[281:nrow(hs_data),]

# use geo code to get lat/long coordinates
hs_geo_1 <- hs_data.1 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_2 <- hs_data.2 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

# bind together
t1 <- rbind(hs_geo_1,hs_geo_2)

data_cleaned_raw <- t1

data_cleaned <- data_cleaned_raw %>% mutate(
  School_Clean = paste0("University of ", School)
)

# add city col, then fill
data_cleaned$School_City <- NA
data_cleaned$School_City[data_cleaned$School=="arizona-state"] <- "Tempe, AZ"
data_cleaned$School_City[data_cleaned$School=="arizona"] <- "Tucson, AZ"
data_cleaned$School_City[data_cleaned$School=="baylor"] <- "Waco, TX"
data_cleaned$School_City[data_cleaned$School=="byu"] <- "Provo, UT"
data_cleaned$School_City[data_cleaned$School=="texas-tech"] <- "Lubbock, TX"
data_cleaned$School_City[data_cleaned$School=="utah"] <- "Salt Lake City, UT"
data_cleaned$School_City[data_cleaned$School=="west-virginia"] <- "Morgantown, WV"
data_cleaned$School_City[data_cleaned$School=="central-florida"] <- "Orlando, FL"
data_cleaned$School_City[data_cleaned$School=="cincinnati"] <- "Cincinnati, OH"
data_cleaned$School_City[data_cleaned$School=="colorado"] <- "Boulder, CO"
data_cleaned$School_City[data_cleaned$School=="houston"] <- "Houston, TX"
data_cleaned$School_City[data_cleaned$School=="iowa-state"] <- "Ames, IA"
data_cleaned$School_City[data_cleaned$School=="kansas-state"] <- "Manhattan, KS"
data_cleaned$School_City[data_cleaned$School=="kansas"] <- "Lawrence, KS"
data_cleaned$School_City[data_cleaned$School=="oklahoma-state"] <- "Stillwater, OK"
data_cleaned$School_City[data_cleaned$School=="tcu"] <- "Fort Worth, TX"

#View(data_cleaned)
# try to do the same for college/school destinations (or look it up)
school_geo <- data_cleaned %>%
  distinct(School, .keep_all = TRUE) %>%
  mutate(School = str_trim(School)) %>%
  geocode(address = School_City, method = "osm", lat = college_lat, long = college_long)

school_geo_cleaned <- school_geo %>% 
  select(School, School_Clean, School_City, college_lat, college_long)

# merge data_cleaned back with school coordinates...
data_merged <- left_join(data_cleaned, school_geo_cleaned, by=c("School", "School_Clean", "School_City"))
#View(data_merged)

# # check weight
# no_w <- data_merged %>% 
#   filter(Weight==0)
# View(no_w)

data_merged$Weight[data_merged$Name=="De'Quon Lake"] <- 225
data_merged$Weight[data_merged$Name=="Conrad Martinez"] <- 174
data_merged$Weight[data_merged$Name=="Kani Coles"] <- 245
data_merged$Weight[data_merged$Name=="Rashawn Fredericks"] <- 200
data_merged$Weight[data_merged$Name=="D'Angelo Hunter"] <- 190
data_merged$Weight[data_merged$Name=="Brandon Averette"] <- 185
data_merged$Weight[data_merged$Name=="Davide Moretti"] <- 180
data_merged$Weight[data_merged$Name=="Malik Ondigo"] <- 215
data_merged$Weight[data_merged$Name=="D'Angelo Hunter"] <- 190

# need locations for some players
no_coor <- data_merged %>% 
  filter(is.na(lat))
View(no_coor)


data_merged$lat[data_merged$Name=="Elias Valtonen"] <- "60.1699"
data_merged$long[data_merged$Name=="Elias Valtonen"] <- "24.9384"

data_merged$lat[data_merged$Name=="Frankie Collins"] <- "36.1716"
data_merged$long[data_merged$Name=="Frankie Collins"] <- "-115.1391"
data_merged$Location[data_merged$Name=="Frankie Collins"] <- "Coronado (Las Vegas, Nevada)"

data_merged$lat[data_merged$Name=="Azuolas Tubelis"] <- "55.1694"
data_merged$long[data_merged$Name=="Azuolas Tubelis"] <- "23.8813"

data_merged$lat[data_merged$Name=="Kerr Kriisa"] <- "58.5953"
data_merged$long[data_merged$Name=="Kerr Kriisa"] <- "25.0136"

data_merged$lat[data_merged$Name=="Jakub Dombek"] <- "49.8175"
data_merged$long[data_merged$Name=="Jakub Dombek"] <- "15.4730"

data_merged$lat[data_merged$Name=="Paul Mbiya"] <- "46.2276"
data_merged$long[data_merged$Name=="Paul Mbiya"] <- "2.2137"

data_merged$lat[data_merged$Name=="Lat Mayen"] <- "-35.282"
data_merged$long[data_merged$Name=="Lat Mayen"] <- "149.129"

data_merged$lat[data_merged$Name=="Norbert Thelissen"] <- "52.1326"
data_merged$long[data_merged$Name=="Norbert Thelissen"] <- "5.2913"

data_merged$lat[data_merged$Name=="Tautvilas Tubelis"] <- "55.1694"
data_merged$long[data_merged$Name=="Tautvilas Tubelis"] <- "23.8813"

data_merged$lat[data_merged$Name=="Paulius Murauskas"] <- "55.1694"
data_merged$long[data_merged$Name=="Paulius Murauskas"] <- "23.8813"

data_merged$lat[data_merged$Name=="Motiejus Krivas"] <- "55.1694"
data_merged$long[data_merged$Name=="Motiejus Krivas"] <- "23.8813"

data_merged$lat[data_merged$Name=="Henri Veesaar"] <- "58.5953"
data_merged$long[data_merged$Name=="Henri Veesaar"] <- "25.0136"

data_merged$lat[data_merged$Name=="Mikael Jantunen"] <- "60.1699"
data_merged$long[data_merged$Name=="Mikael Jantunen"] <- "24.9384"

## for locations....
data_merged$lat[data_merged$Location=="Helsinki Roosters (Finland, FINL)"] <- "63.2467777"
data_merged$long[data_merged$Location=="Helsinki Roosters (Finland, FINL)"] <- "25.9209164"

data_merged$lat[data_merged$Location=="Tidehaven (El Maton, TX)"] <- "33.44323"
data_merged$long[data_merged$Location=="Tidehaven (El Maton, TX)"] <- "-94.069166"

data_merged$lat[data_merged$Location=="SC Derby (Donji Kokoti, TGD)"] <- "42.44111"
data_merged$long[data_merged$Location=="SC Derby (Donji Kokoti, TGD)"] <- "19.26361"

# need locations for some players
no_coor <- data_merged %>% 
  filter(is.na(lat))
View(no_coor)

#write_csv(data_merged, "data/csvs/football/all_data_football_FINAL.csv")
write_csv(data_merged, "data/csvs/basketball/all_data_basketball_FINAL.csv")


## putting in in sql database...
library(readxl)
library(DBI)
library(RSQLite)
library(readr)


# connTemp <- dbConnect(SQLite(), "C:/ProgramData/VGSData/VGS50.db")
# df <- read.csv("data/missingKansasStateData.csv")

conn <- dbConnect(RSQLite::SQLite(), "data/recruiting.db")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS [recruit_class_basketball](
  [Name] VARCHAR(50), 
  [Location] VARCHAR(50), 
  [Height] VARCHAR(15), 
  [Weight] INT, 
  [Ranking] INT, 
  [NationalRank] INT, 
  [PositionRank] INT, 
  [StateRank] INT, 
  [State] VARCHAR2(15), 
  [Position] VARCHAR2(15), 
  [Year] INT(4), 
  [School] VARCHAR(50), 
  [Type] VARCHAR(15), 
  [count] INT, 
  [sport] VARCHAR(50), 
  [loc_inside_parens] VARCHAR(50), 
  [location_name] VARCHAR(50), 
  [Location_Clean] VARCHAR(50), 
  [lat] FLOAT, 
  [long] FLOAT, 
  [School_Clean] VARCHAR(50), 
  [School_City] VARCHAR(50), 
  [college_lat] FLOAT, 
  [college_long] FLOAT);")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS [recruit_class_football](
  [Name] VARCHAR(50), 
  [Location] VARCHAR(50), 
  [Height] VARCHAR(15), 
  [Weight] INT, 
  [Ranking] INT, 
  [NationalRank] INT, 
  [PositionRank] INT, 
  [StateRank] INT, 
  [State] VARCHAR2(15), 
  [Position] VARCHAR2(15), 
  [Year] INT(4), 
  [School] VARCHAR(50), 
  [Type] VARCHAR(15), 
  [count] INT, 
  [sport] VARCHAR(50), 
  [loc_inside_parens] VARCHAR(50), 
  [location_name] VARCHAR(50), 
  [Location_Clean] VARCHAR(50), 
  [lat] FLOAT, 
  [long] FLOAT, 
  [School_Clean] VARCHAR(50), 
  [School_City] VARCHAR(50), 
  [college_lat] FLOAT, 
  [college_long] FLOAT);")

## final insert into database
football <- read_csv("data/csvs/football/all_data_football_FINAL.csv")
basketball <- read_csv("data/csvs/basketball/all_data_basketball_FINAL.csv")

# This appends your data to the existing table
dbWriteTable(conn, "recruit_class_basketball", basketball, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "recruit_class_football", football, append = TRUE, row.names = FALSE)

dbGetQuery(conn, "SELECT * FROM recruit_class_basketball LIMIT 5")

dbDisconnect(conn)

