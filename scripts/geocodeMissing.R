## ===========================================================================
## geocodeMissing.R
## Geocode recruits that have a Location string but no lat/long (players
## added by refreshClassYear.R). Built defensively because free geocoders
## confidently return the WRONG city (e.g. "Valencia, CA" -> Valencia, Mexico):
##
##   1. the query is pinned: "City, ST, USA"
##   2. a result is accepted ONLY if it falls inside the claimed state's
##      bounding box (padded); everything else is rejected, left NA, and
##      written to backups/geocode_rejects_<date>.csv for manual review
##   3. rows with no parseable "City, ST" (international players etc.) are
##      skipped and reported, never guessed
##
## Run from the project root (OSM is rate-limited to ~1 query/second):
##   Rscript scripts/geocodeMissing.R            # both sports
## ===========================================================================

suppressMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(DBI)
  library(RSQLite)
  library(tidygeocoder)
})

## generous (padded) lat/long bounding boxes per state -- validation only
STATE_BBOX <- read.csv(text = "st,lat_min,lat_max,lon_min,lon_max
AL,30.1,35.1,-88.6,-84.8
AK,51.0,71.5,-179.9,-129.0
AZ,31.2,37.1,-115.0,-108.9
AR,32.9,36.6,-94.7,-89.6
CA,32.4,42.1,-124.6,-114.0
CO,36.9,41.1,-109.2,-101.9
CT,40.9,42.1,-73.8,-71.7
DE,38.4,39.9,-75.8,-74.9
DC,38.7,39.1,-77.2,-76.8
FL,24.4,31.1,-87.7,-79.9
GA,30.3,35.1,-85.7,-80.7
HI,18.8,22.3,-160.3,-154.7
ID,41.9,49.1,-117.3,-110.9
IL,36.9,42.6,-91.6,-87.4
IN,37.7,41.8,-88.2,-84.7
IA,40.3,43.6,-96.7,-90.0
KS,36.9,40.1,-102.2,-94.5
KY,36.4,39.2,-89.7,-81.9
LA,28.8,33.1,-94.1,-88.7
ME,42.9,47.5,-71.2,-66.8
MD,37.8,39.8,-79.6,-74.9
MA,41.2,42.9,-73.6,-69.8
MI,41.6,48.4,-90.5,-82.3
MN,43.4,49.5,-97.3,-89.4
MS,30.0,35.1,-91.7,-88.0
MO,35.9,40.7,-95.9,-89.0
MT,44.3,49.1,-116.2,-103.9
NE,39.9,43.1,-104.2,-95.2
NV,34.9,42.1,-120.1,-113.9
NH,42.6,45.4,-72.7,-70.5
NJ,38.8,41.4,-75.7,-73.8
NM,31.2,37.1,-109.2,-102.9
NY,40.4,45.1,-79.9,-71.7
NC,33.7,36.7,-84.5,-75.3
ND,45.8,49.1,-104.2,-96.4
OH,38.3,42.0,-85.0,-80.4
OK,33.5,37.1,-103.1,-94.3
OR,41.9,46.4,-124.7,-116.4
PA,39.6,42.4,-80.6,-74.6
RI,41.1,42.1,-71.9,-71.0
SC,32.0,35.3,-83.5,-78.4
SD,42.4,46.0,-104.2,-96.3
TN,34.9,36.8,-90.4,-81.5
TX,25.7,36.6,-106.8,-93.4
UT,36.9,42.1,-114.2,-108.9
VT,42.6,45.1,-73.5,-71.4
VA,36.4,39.6,-83.8,-75.1
WA,45.4,49.1,-124.9,-116.8
WV,37.1,40.7,-82.7,-77.6
WI,42.4,47.1,-93.0,-86.7
WY,40.9,45.1,-111.2,-104.0",
  stringsAsFactors = FALSE)

## "School Name (City, ST)" -> "City, ST, USA" (+ the parts for validation)
parse_loc <- function(location) {
  inside <- str_match(location, "\\(([^)]*)\\)")[, 2]
  parts <- str_split_fixed(inside, ",", 2)
  city <- str_trim(parts[, 1])
  st <- str_trim(parts[, 2])
  data.frame(city = city, st = st,
             query = ifelse(!is.na(st) & st %in% STATE_BBOX$st,
                            paste0(city, ", ", st, ", USA"), NA_character_),
             stringsAsFactors = FALSE)
}

conn <- dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
dir.create(here::here("backups"), showWarnings = FALSE)
all_rejects <- list()

for (tbl in c("recruit_class_football", "recruit_class_basketball")) {
  cat("\n=====", tbl, "=====\n")
  todo <- dbGetQuery(conn, paste0(
    "SELECT rowid AS rid, Name, School, Year, Location, State FROM ", tbl,
    " WHERE (lat IS NULL OR lat = '' OR lat = 'NA')",
    " AND Location IS NOT NULL AND Location != ''",
    " AND Type = 'Commit'"))
  if (nrow(todo) == 0) { cat("nothing to geocode\n"); next }

  todo <- cbind(todo, parse_loc(todo$Location))
  skipped <- todo %>% filter(is.na(query))
  queue <- todo %>% filter(!is.na(query)) %>% distinct(query, .keep_all = FALSE)
  cat(nrow(todo), "players missing coords |", nrow(queue),
      "unique places to geocode |", nrow(skipped),
      "skipped (no parseable 'City, ST')\n")
  if (nrow(queue) == 0) next

  ## one query per unique place (OSM throttles to ~1/sec internally)
  coded <- tidygeocoder::geocode(queue, address = query, method = "osm",
                                 quiet = TRUE)

  res <- todo %>%
    filter(!is.na(query)) %>%
    left_join(coded, by = "query") %>%
    left_join(STATE_BBOX, by = "st") %>%
    mutate(ok = !is.na(lat) & !is.na(long) &
             lat >= lat_min & lat <= lat_max &
             long >= lon_min & long <= lon_max)

  accepted <- res %>% filter(ok)
  rejected <- res %>% filter(!ok & !is.na(lat))
  failed <- res %>% filter(is.na(lat))

  cat("accepted:", nrow(accepted), "| rejected (outside claimed state):",
      nrow(rejected), "| no result:", nrow(failed), "\n")
  if (nrow(rejected) > 0) {
    cat("  e.g.", paste(head(rejected$query, 3), collapse = " | "), "\n")
    all_rejects[[tbl]] <- rejected %>%
      mutate(table = tbl) %>%
      select(table, Name, School, Year, Location, query, lat, long)
  }

  ## write only the validated coordinates back
  if (nrow(accepted) > 0) {
    for (i in seq_len(nrow(accepted))) {
      dbExecute(conn, paste0(
        "UPDATE ", tbl, " SET lat = ", accepted$lat[i],
        ", long = ", accepted$long[i], " WHERE rowid = ", accepted$rid[i]))
    }
    cat("wrote", nrow(accepted), "validated coordinates to", tbl, "\n")
  }
}

if (length(all_rejects) > 0) {
  rej_path <- here::here("backups", paste0("geocode_rejects_",
                                           format(Sys.Date()), ".csv"))
  write_csv(bind_rows(all_rejects), rej_path)
  cat("\nRejects written for manual review:", rej_path, "\n")
}
dbDisconnect(conn)
cat("\nDone. Rejected/failed rows stay off the map rather than guessing.\n")
