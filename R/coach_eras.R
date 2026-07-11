## ---------------------------------------------------------------------------
## coach_eras.R
## Head-coach eras by RECRUITING CLASS YEAR (2016-2026 window).
## first_class / last_class = the class years attributed to that coach.
##
## ATTRIBUTION RULE: a class belongs to the staff that ran its MAIN signing
## window -- the December early-signing period for 2021+ classes (when ~90%
## of commits sign), the February NSD for earlier classes. So e.g. Arizona's
## 2024 class is FISCH's (signed Dec 2023; Brennan retained it after Jan
## 2024), while the 2018 class is Sumlin's (assembled for Feb 2018 NSD after
## the December group scattered). Mid-window firings remain judgment calls;
## edit freely. NA last_class = current coach.
##
## Used by: Era Compare tab, era annotations on time-series plots.
## To extend to more conferences: just add rows (slug must match TEAM_CONFIG).
## ---------------------------------------------------------------------------

COACH_ERAS <- rbind(
  ## ---- FOOTBALL ------------------------------------------------------------
  ## 2026-cycle changes: Gundy->Morris (fired 11/2025), Klieman->Klein
  ## (retired 12/2025), Whittingham->Scalley, Campbell->Rogers (to Penn St).
  data.frame(sport = "football", rbind(
    data.frame(slug = "arizona", coach = c("Rich Rodriguez", "Kevin Sumlin", "Jedd Fisch", "Brent Brennan"),
               first_class = c(2016, 2018, 2021, 2025), last_class = c(2017, 2020, 2024, NA)),
    data.frame(slug = "arizona-state", coach = c("Todd Graham", "Herm Edwards", "Kenny Dillingham"),
               first_class = c(2016, 2018, 2023), last_class = c(2017, 2022, NA)),
    data.frame(slug = "baylor", coach = c("Art Briles", "Matt Rhule", "Dave Aranda"),
               first_class = c(2016, 2017, 2020), last_class = c(2016, 2019, NA)),
    data.frame(slug = "byu", coach = "Kalani Sitake",
               first_class = 2016, last_class = NA),
    data.frame(slug = "central-florida", coach = c("Scott Frost", "Josh Heupel", "Gus Malzahn", "Scott Frost II"),
               first_class = c(2016, 2018, 2022, 2025), last_class = c(2017, 2021, 2024, NA)),
    data.frame(slug = "cincinnati", coach = c("Tommy Tuberville", "Luke Fickell", "Scott Satterfield"),
               first_class = c(2016, 2017, 2023), last_class = c(2016, 2022, NA)),
    data.frame(slug = "colorado", coach = c("Mike MacIntyre", "Mel Tucker", "Karl Dorrell", "Deion Sanders"),
               first_class = c(2016, 2019, 2021, 2023), last_class = c(2018, 2020, 2022, NA)),
    data.frame(slug = "houston", coach = c("Tom Herman", "Major Applewhite", "Dana Holgorsen", "Willie Fritz"),
               first_class = c(2016, 2017, 2019, 2024), last_class = c(2016, 2018, 2023, NA)),
    data.frame(slug = "iowa-state", coach = c("Matt Campbell", "Jimmy Rogers"),
               first_class = c(2016, 2026), last_class = c(2025, NA)),
    data.frame(slug = "kansas", coach = c("David Beaty", "Les Miles", "Lance Leipold"),
               first_class = c(2016, 2019, 2022), last_class = c(2018, 2021, NA)),
    data.frame(slug = "kansas-state", coach = c("Bill Snyder", "Chris Klieman", "Collin Klein"),
               first_class = c(2016, 2019, 2026), last_class = c(2018, 2025, NA)),
    data.frame(slug = "oklahoma-state", coach = c("Mike Gundy", "Eric Morris"),
               first_class = c(2016, 2026), last_class = c(2025, NA)),
    data.frame(slug = "tcu", coach = c("Gary Patterson", "Sonny Dykes"),
               first_class = c(2016, 2022), last_class = c(2021, NA)),
    data.frame(slug = "texas-tech", coach = c("Kliff Kingsbury", "Matt Wells", "Joey McGuire"),
               first_class = c(2016, 2019, 2022), last_class = c(2018, 2021, NA)),
    data.frame(slug = "utah", coach = c("Kyle Whittingham", "Morgan Scalley"),
               first_class = c(2016, 2026), last_class = c(2025, NA)),
    data.frame(slug = "west-virginia", coach = c("Dana Holgorsen", "Neal Brown", "Rich Rodriguez"),
               first_class = c(2016, 2019, 2025), last_class = c(2018, 2024, NA))
  )),
  ## ---- BASKETBALL ----------------------------------------------------------
  data.frame(sport = "basketball", rbind(
    data.frame(slug = "arizona", coach = c("Sean Miller", "Tommy Lloyd"),
               first_class = c(2016, 2022), last_class = c(2021, NA)),
    data.frame(slug = "arizona-state", coach = c("Bobby Hurley", "Randy Bennett"),
               first_class = c(2016, 2026), last_class = c(2025, NA)),
    data.frame(slug = "baylor", coach = "Scott Drew",
               first_class = 2016, last_class = NA),
    data.frame(slug = "byu", coach = c("Dave Rose", "Mark Pope", "Kevin Young"),
               first_class = c(2016, 2020, 2025), last_class = c(2019, 2024, NA)),
    data.frame(slug = "central-florida", coach = c("Donnie Jones", "Johnny Dawkins"),
               first_class = c(2016, 2017), last_class = c(2016, NA)),
    data.frame(slug = "cincinnati", coach = c("Mick Cronin", "John Brannen", "Wes Miller"),
               first_class = c(2016, 2020, 2022), last_class = c(2019, 2021, NA)),
    data.frame(slug = "colorado", coach = "Tad Boyle",
               first_class = 2016, last_class = NA),
    data.frame(slug = "houston", coach = "Kelvin Sampson",
               first_class = 2016, last_class = NA),
    data.frame(slug = "iowa-state", coach = c("Steve Prohm", "T.J. Otzelberger"),
               first_class = c(2016, 2022), last_class = c(2021, NA)),
    data.frame(slug = "kansas", coach = "Bill Self",
               first_class = 2016, last_class = NA),
    data.frame(slug = "kansas-state", coach = c("Bruce Weber", "Jerome Tang"),
               first_class = c(2016, 2023), last_class = c(2022, NA)),
    data.frame(slug = "oklahoma-state", coach = c("Travis Ford", "Brad Underwood", "Mike Boynton", "Steve Lutz"),
               first_class = c(2016, 2017, 2018, 2025), last_class = c(2016, 2017, 2024, NA)),
    data.frame(slug = "tcu", coach = c("Trent Johnson", "Jamie Dixon"),
               first_class = c(2016, 2017), last_class = c(2016, NA)),
    data.frame(slug = "texas-tech", coach = c("Tubby Smith", "Chris Beard", "Mark Adams", "Grant McCasland"),
               first_class = c(2016, 2017, 2022, 2024), last_class = c(2016, 2021, 2023, NA)),
    data.frame(slug = "utah", coach = c("Larry Krystkowiak", "Craig Smith", "Alex Jensen"),
               first_class = c(2016, 2022, 2026), last_class = c(2021, 2025, NA)),
    data.frame(slug = "west-virginia", coach = c("Bob Huggins", "Josh Eilert", "Darian DeVries", "Ross Hodge"),
               first_class = c(2016, 2024, 2025, 2026), last_class = c(2023, 2024, 2025, NA))
  ))
)

## current window cap: treat NA last_class as "through the ACTIVE recruiting
## cycle". Class of N signs Dec N-1/Feb N, so by mid-year the N+1 cycle is
## already live on 247 (2027 pages have commits in July 2026) and the scraper
## can land rows a year ahead of the calendar. A hardcoded cap would strip
## coach attribution from those rows and drop them from every era chart.
## Open-ended eras (NA last_class) extend to this cap automatically; actual
## coaching CHANGES stay curated rows above -- edit those by hand.
ERA_MAX_CLASS <- as.integer(format(Sys.Date(), "%Y")) + 1L

## coach for each school/sport/class-year (vectorized over year)
coach_for_class <- function(slug, sport, year) {
  eras <- COACH_ERAS[COACH_ERAS$slug == slug &
                       COACH_ERAS$sport == tolower(sport), ]
  if (nrow(eras) == 0) return(rep(NA_character_, length(year)))
  eras$last_class[is.na(eras$last_class)] <- ERA_MAX_CLASS
  out <- rep(NA_character_, length(year))
  for (i in seq_len(nrow(eras))) {
    hit <- year >= eras$first_class[i] & year <= eras$last_class[i]
    out[hit] <- eras$coach[i]
  }
  out
}

## add a Coach column to prepped size data (one sport at a time)
add_coach_era <- function(df, sport) {
  schools <- unique(df$School)
  lookup <- do.call(rbind, lapply(schools, function(s) {
    yrs <- sort(unique(df$Year[df$School == s]))
    data.frame(School = s, Year = yrs,
               Coach = coach_for_class(s, sport, yrs))
  }))
  dplyr::left_join(df, lookup, by = c("School", "Year"))
}

## era table for one team (for annotations + Era Compare): coach, from, to
team_eras <- function(slug, sport) {
  eras <- COACH_ERAS[COACH_ERAS$slug == slug &
                       COACH_ERAS$sport == tolower(sport), ]
  if (nrow(eras) == 0) return(NULL)
  eras$last_class[is.na(eras$last_class)] <- ERA_MAX_CLASS
  eras[order(eras$first_class),
       c("coach", "first_class", "last_class")]
}
