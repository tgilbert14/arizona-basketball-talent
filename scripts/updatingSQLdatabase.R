library(DBI)
library(RSQLite)
library(readr)

connTemp <- dbConnect(SQLite(), "C:/ProgramData/VGSData/VGS50.db")

df <- read.csv("data/missingKansasStateData.csv")



data_cleaned <- df %>%
  mutate(
    # Extract what's inside parentheses
    loc_inside_parens = str_extract(Location, "(?<=\\()[^)]+"),
    # Extract name before parentheses and trim
    location_name = str_trim(str_extract(Location, "^[^(]+")),
    # Final cleaned version: add "High School," before the parentheses content
    Location_Clean = paste0(location_name, " High School, ", loc_inside_parens)
  )
View(data_cleaned)
#test <- data_cleaned[data_cleaned$School == "houston",]

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
View(data_merged)

# check weight
no_w <- data_merged %>% 
  filter(Weight==0)
View(no_w)

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

data_merged <- unique(data_merged)
write_csv(data_merged, "recruit_csvs/all_data_basketball.csv")

## putting in in sql database...
library(readxl)      # For reading Excel files
library(DBI)         # For database interface
library(RSQLite)     # For SQLite backend

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

# This appends your Excel data to the existing table
dbWriteTable(connTemp, "recruit_class_basketball", data_merged, append = TRUE, row.names = FALSE)

dbGetQuery(connTemp, "SELECT * FROM recruit_class_basketball LIMIT 5")

dbDisconnect(connTemp)

