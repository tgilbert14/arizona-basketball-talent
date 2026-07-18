## ===========================================================================
## qcSweep.R -- data QC sweep + the qc_flags ledger
##
##   Rscript scripts/qcSweep.R                 # sweep, upsert flags, report
##   Rscript scripts/qcSweep.R --report-only   # sweep + report, write nothing
##
## Runs rule-based QC over the recruit/roster tables and records suspects in
## the qc_flags ledger table so the nightly can MARK (and where safe, FIX)
## possible errors on every auto-update without alarm fatigue:
##
##   * a flag's identity is (rule, tbl, row_key) -- re-sweeps update
##     last_seen and never duplicate
##   * status lifecycle: open -> accepted | fixed. An ACCEPTED flag is a
##     human-verified legitimate outlier (e.g. a Hawaii recruit at Arizona
##     -- real recruiting, not an error) and is NEVER re-raised or counted
##     as new. A row that stops matching its rule auto-closes to fixed.
##   * exit code: 0 = no NEW open high-severity flags, 2 = new HIGH flags
##     found this sweep (the nightly surfaces that in its manifest/alert)
##
## Rules (severity):
##   GEO-STATE      (high) geocoded pin lands outside the bbox of the claimed
##                         US state/territory -- the wrong-Canton failure mode
##   STATE-CONFLICT  (med) State column vs hometown text disagree on the US
##                         state (the 247 team-page wrong-state bug)
##   GEO-BOUNDS      (med) pin outside every US state/territory box on a row
##                         with NO parseable state claim (international rows
##                         with international pins are correct, not flagged)
##   GEO-COLLAPSE    (med) one rounded coordinate shared by many DIFFERENT
##                         CITIES -- the Nominatim centroid-collapse signature
##                         (many schools in ONE city sharing a pin is by design)
##   DUP-ROW        (high) duplicate Name+School+Year+Type rows
##   COMP-THIN       (med) an onboarded school's football class-year with
##                         suspiciously few commits (completed cycles only)
##   RANGE-HW        (low) height/weight outside plausibility bands
##   TYPE-ERA        (med) transfers recorded before 2019 (2019-20 grad
##                         transfers are real and verified)
##
## AUTO-FIX (unless --report-only): 247 placeholder zeros -- Weight <= 20 ->
## NULL, Height '0-0' -> NULL. Deterministic missing-data markers that would
## otherwise drag class-average weights.
##
## The state bboxes mirror scripts/geocodeMissing.R (the ingest-time guard);
## this sweep re-checks the WHOLE table so legacy rows that predate the
## guard get the same scrutiny as new ones.
## ===========================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
})

args        <- commandArgs(trailingOnly = TRUE)
report_only <- "--report-only" %in% args

repo_root <- here::here()
db_path   <- file.path(repo_root, "data", "recruiting.db")
stopifnot(file.exists(db_path))

## ---------------------------------------------------------------------------
## state bounding boxes (contiguous + AK/HI/DC), generous edges -- identical
## philosophy to geocodeMissing.R: a box miss on a claimed state is a wrong
## geocode, not a wrong player
## ---------------------------------------------------------------------------
STATE_BBOX <- read.csv(text = "st,lat_min,lat_max,lon_min,lon_max
AL,30.1,35.1,-88.6,-84.8
AK,51.0,71.5,-179.9,-129.9
AZ,31.2,37.1,-114.9,-108.9
AR,32.9,36.6,-94.7,-89.6
CA,32.4,42.1,-124.5,-114.0
CO,36.9,41.1,-109.2,-102.0
CT,40.9,42.1,-73.8,-71.7
DE,38.4,39.9,-75.8,-74.9
DC,38.7,39.1,-77.2,-76.8
FL,24.4,31.1,-87.7,-79.9
GA,30.3,35.1,-85.7,-80.7
HI,18.8,22.3,-160.3,-154.7
ID,41.9,49.1,-117.3,-110.9
IL,36.9,42.6,-91.6,-87.0
IN,37.7,41.8,-88.2,-84.7
IA,40.3,43.6,-96.7,-90.1
KS,36.9,40.1,-102.1,-94.5
KY,36.4,39.2,-89.6,-81.9
LA,28.8,33.1,-94.1,-88.7
ME,42.9,47.5,-71.2,-66.8
MD,37.8,39.8,-79.6,-74.9
MA,41.2,43.0,-73.6,-69.8
MI,41.6,48.4,-90.5,-82.3
MN,43.4,49.5,-97.4,-89.4
MS,30.1,35.1,-91.7,-88.0
MO,35.9,40.7,-95.9,-89.0
MT,44.3,49.1,-116.2,-103.9
NE,39.9,43.1,-104.2,-95.2
NV,34.9,42.1,-120.1,-113.9
NH,42.6,45.4,-72.7,-70.5
NJ,38.8,41.4,-75.7,-73.8
NM,31.2,37.1,-109.2,-102.9
NY,40.4,45.1,-79.9,-71.7
NC,33.7,36.7,-84.4,-75.3
ND,45.8,49.1,-104.2,-96.4
OH,38.3,42.0,-84.9,-80.4
OK,33.5,37.1,-103.1,-94.3
OR,41.9,46.4,-124.7,-116.4
PA,39.6,42.4,-80.6,-74.6
RI,41.1,42.1,-71.9,-71.1
SC,32.0,35.3,-83.4,-78.4
SD,42.4,46.0,-104.2,-96.3
TN,34.9,36.8,-90.4,-81.6
TX,25.7,36.6,-106.7,-93.4
UT,36.9,42.1,-114.2,-108.9
VT,42.6,45.1,-73.5,-71.4
VA,36.4,39.6,-83.8,-75.1
WA,45.4,49.1,-124.9,-116.9
WV,37.1,40.7,-82.7,-77.6
WI,42.4,47.1,-93.0,-86.7
WY,40.9,45.1,-111.2,-104.0
AS,-14.7,-13.7,-171.3,-169.1
PR,17.7,18.7,-67.6,-65.0
GU,13.1,13.8,144.5,145.1
VI,17.5,18.6,-65.3,-64.4",
  stringsAsFactors = FALSE)

## a pin is US-plausible if it falls inside ANY state/territory box (GU sits
## east of the antimeridian, so a single lat/lon envelope cannot work)
in_any_bbox <- function(lat, lon) {
  ok <- rep(FALSE, length(lat))
  for (i in seq_len(nrow(STATE_BBOX))) {
    ok <- ok | (lat >= STATE_BBOX$lat_min[i] & lat <= STATE_BBOX$lat_max[i] &
                lon >= STATE_BBOX$lon_min[i] & lon <= STATE_BBOX$lon_max[i])
  }
  ok
}

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

## ---------------------------------------------------------------------------
## the ledger
## ---------------------------------------------------------------------------
dbExecute(con, "CREATE TABLE IF NOT EXISTS qc_flags (
  rule       TEXT NOT NULL,
  tbl        TEXT NOT NULL,
  row_key    TEXT NOT NULL,
  severity   TEXT NOT NULL,
  details    TEXT,
  first_seen TEXT NOT NULL,
  last_seen  TEXT NOT NULL,
  status     TEXT NOT NULL DEFAULT 'open',
  PRIMARY KEY (rule, tbl, row_key)
)")

now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

## collected this sweep: data.frame(rule, tbl, row_key, severity, details)
found <- list()
add_flags <- function(rule, tbl, row_key, severity, details) {
  if (length(row_key) == 0) return(invisible())
  found[[length(found) + 1]] <<- data.frame(
    rule = rule, tbl = tbl, row_key = row_key, severity = severity,
    details = details, stringsAsFactors = FALSE)
}

## row identity that survives re-scrapes (rowids don't)
rk <- function(d) paste(d$Name, d$School, d$Year, d$Type, sep = "|")

state_from_row <- function(State, Location_Clean) {
  st <- toupper(trimws(ifelse(is.na(State), "", State)))
  fb <- sub(".*,\\s*([A-Za-z]{2})\\s*$", "\\1",
            ifelse(is.na(Location_Clean), "", Location_Clean))
  st <- ifelse(nchar(st) == 2, st, toupper(fb))
  ifelse(nchar(st) == 2, st, NA_character_)
}

## ---------------------------------------------------------------------------
## sweep the two recruit tables
## ---------------------------------------------------------------------------
for (tbl in c("recruit_class_football", "recruit_class_basketball")) {
  d <- dbGetQuery(con, paste0(
    "SELECT Name, School, Year, Type, State, Location_Clean, location_name,",
    " lat, long, Height, Weight, ProfileUrl FROM ", tbl))
  if (nrow(d) == 0) next
  d$st  <- state_from_row(d$State, d$Location_Clean)
  d$key <- rk(d)

  has_geo <- !is.na(d$lat) & !is.na(d$long)
  known_us <- !is.na(d$st) & d$st %in% STATE_BBOX$st

  ## GEO-BOUNDS: a pin outside every US state/territory box is only judgeable
  ## when the row does NOT claim an international hometown -- ProKick
  ## Australia punters and German linemen belong abroad (correct pins, real
  ## recruits). Flag it when no state claim exists at all: unverifiable.
  ## US-state-claimed rows are covered precisely by GEO-STATE below.
  oob <- has_geo & !in_any_bbox(d$lat, d$long)
  ob_flag <- oob & is.na(d$st)
  add_flags("GEO-BOUNDS", tbl, d$key[ob_flag], "med",
            sprintf("pin (%.3f, %.3f) outside all US bounds with no parseable state claim; hometown '%s'",
                    d$lat[ob_flag], d$long[ob_flag], d$Location_Clean[ob_flag]))

  ## GEO-STATE: pin outside the claimed state's bbox
  m <- match(d$st, STATE_BBOX$st)
  chk <- has_geo & known_us
  bad_st <- chk & (d$lat  < STATE_BBOX$lat_min[m] |
                   d$lat  > STATE_BBOX$lat_max[m] |
                   d$long < STATE_BBOX$lon_min[m] |
                   d$long > STATE_BBOX$lon_max[m])
  bad_st[is.na(bad_st)] <- FALSE
  add_flags("GEO-STATE", tbl, d$key[bad_st], "high",
            sprintf("pin (%.3f, %.3f) not in claimed state %s; hometown '%s'",
                    d$lat[bad_st], d$long[bad_st], d$st[bad_st],
                    d$Location_Clean[bad_st]))

  ## STATE-CONFLICT: the scrape's State column and the hometown text disagree
  ## (both claiming a US state). Catches the 247 team-page wrong-state bug
  ## (e.g. 'Huber Heights, TX' for the Dayton OH suburb) even when the pin
  ## happens to agree with the wrong side.
  txt_st <- toupper(sub(".*,\\s*([A-Za-z]{2})\\s*$", "\\1",
                        ifelse(is.na(d$Location_Clean), "", d$Location_Clean)))
  txt_st[!(txt_st %in% STATE_BBOX$st)] <- NA
  col_st <- toupper(trimws(ifelse(is.na(d$State), "", d$State)))
  col_st[!(col_st %in% STATE_BBOX$st)] <- NA
  conf <- !is.na(txt_st) & !is.na(col_st) & txt_st != col_st
  add_flags("STATE-CONFLICT", tbl, d$key[conf], "med",
            sprintf("State column says %s but hometown text '%s' says %s",
                    col_st[conf], d$Location_Clean[conf], txt_st[conf]))

  ## GEO-COLLAPSE: one rounded coordinate claimed by many DIFFERENT CITIES.
  ## Geocoding is city-granularity by design, so 30 Houston high schools
  ## sharing the Houston pin is correct -- the signature of a real centroid
  ## collapse is distinct CITY names on one coordinate.
  g <- d[has_geo & !is.na(d$Location_Clean), , drop = FALSE]
  if (nrow(g) > 0) {
    g$city <- tolower(trimws(sub("^.*?,\\s*([^,]+),\\s*[A-Za-z]{2}\\s*$",
                                 "\\1", g$Location_Clean)))
    g$city[g$city == g$Location_Clean] <- NA  # pattern didn't match
    g <- g[!is.na(g$city), , drop = FALSE]
    if (nrow(g) > 0) {
      g$coord <- paste(round(g$lat, 3), round(g$long, 3))
      cities_per_coord <- tapply(g$city, g$coord,
                                 function(x) length(unique(x)))
      hot <- names(cities_per_coord)[cities_per_coord >= 8]
      if (length(hot) > 0) {
        hit <- g$coord %in% hot
        add_flags("GEO-COLLAPSE", tbl, g$key[hit], "med",
                  sprintf("coordinate %s shared by %d distinct cities",
                          g$coord[hit],
                          as.integer(cities_per_coord[g$coord[hit]])))
      }
    }
  }

  ## DUP-ROW
  dup <- duplicated(d$key) | duplicated(d$key, fromLast = TRUE)
  if (any(dup)) {
    ukeys <- unique(d$key[dup])
    add_flags("DUP-ROW", tbl, ukeys, "high",
              "same Name+School+Year+Type appears more than once")
  }

  ## TYPE-ERA: transfers before 2019 are outside anything 247 records
  ## (2019-2020 grad transfers are real -- Cade Mays, Brenton Cox et al.
  ## verified -- so the portal-era cutoff for SUSPICION is 2019, not 2021)
  te <- d$Type == "Transfer" & d$Year < 2019
  add_flags("TYPE-ERA", tbl, d$key[te], "med",
            sprintf("Transfer recorded in %d (pre-2019)", d$Year[te]))

  ## AUTO-FIX: 247 placeholder zeros. A Weight of 0 (or <= 20) and a Height
  ## of '0-0' are missing-data markers, not measurements -- left in place
  ## they silently drag class-average weights. NULL them (the safe,
  ## deterministic fix); the sweep reports what it fixed.
  junk_w <- !is.na(d$Weight) & d$Weight <= 20
  junk_h <- !is.na(d$Height) & d$Height == "0-0"
  if (!report_only && any(junk_w | junk_h)) {
    if (any(junk_w)) {
      dbExecute(con, paste0("UPDATE ", tbl,
        " SET Weight = NULL WHERE Weight IS NOT NULL AND Weight <= 20"))
    }
    if (any(junk_h)) {
      dbExecute(con, paste0("UPDATE ", tbl,
        " SET Height = NULL WHERE Height = '0-0'"))
    }
    cat(sprintf("[auto-fix] %s: NULLed %d placeholder Weight(s) <= 20, %d Height(s) '0-0'\n",
                tbl, sum(junk_w), sum(junk_h)))
    d$Weight[junk_w] <- NA
    d$Height[junk_h] <- NA
  }

  ## RANGE-HW: parse height "6-2.5" -> inches; flag implausible values
  ## that remain after the auto-fix (these need a human look)
  hw <- regmatches(d$Height, regexec("^([4-7])-([0-9]{1,2}(\\.[0-9])?)$",
                                     ifelse(is.na(d$Height), "", d$Height)))
  inches <- vapply(hw, function(p)
    if (length(p) == 4) as.numeric(p[2]) * 12 + as.numeric(p[3])
    else NA_real_, numeric(1))
  h_bad <- !is.na(inches) & (inches < 60 | inches > 90)
  w_lo <- if (grepl("basketball", tbl)) 130 else 130
  w_hi <- if (grepl("basketball", tbl)) 320 else 420
  w_bad <- !is.na(d$Weight) & (d$Weight < w_lo | d$Weight > w_hi)
  rb <- h_bad | w_bad
  add_flags("RANGE-HW", tbl, d$key[rb], "low",
            sprintf("Height '%s' / Weight %s outside plausibility",
                    d$Height[rb], d$Weight[rb]))
}

## ---------------------------------------------------------------------------
## COMP-THIN: onboarded schools' completed football cycles with < 10 commits
## (open cycles exempt -- classes fill through signing day)
## ---------------------------------------------------------------------------
cfg_path <- file.path(repo_root, "data", "team_config.csv")
if (file.exists(cfg_path)) {
  cfg <- read.csv(cfg_path, stringsAsFactors = FALSE)
  ob  <- cfg$slug[as.logical(cfg$onboarded)]
  cycle_cap  <- as.integer(format(Sys.Date(), "%Y")) + 1L
  last_done  <- as.integer(format(Sys.Date(), "%Y")) - 1L  # completed cycles
  cc <- dbGetQuery(con, paste0(
    "SELECT School, Year, COUNT(*) n FROM recruit_class_football",
    " WHERE Type = 'Commit' AND Year BETWEEN 2016 AND ", last_done,
    " GROUP BY School, Year"))
  grid <- expand.grid(School = ob, Year = 2016:last_done,
                      stringsAsFactors = FALSE)
  grid <- merge(grid, cc, all.x = TRUE)
  grid$n[is.na(grid$n)] <- 0L
  thin <- grid[grid$n < 10, , drop = FALSE]
  add_flags("COMP-THIN", "recruit_class_football",
            paste(thin$School, thin$Year, sep = "|"), "med",
            sprintf("only %d football commits in completed cycle %d",
                    thin$n, thin$Year))
}

## ---------------------------------------------------------------------------
## upsert into the ledger + auto-close resolved flags
## ---------------------------------------------------------------------------
f <- if (length(found) > 0) do.call(rbind, found) else
  data.frame(rule = character(0), tbl = character(0), row_key = character(0),
             severity = character(0), details = character(0))

existing <- dbGetQuery(con, "SELECT rule, tbl, row_key, status FROM qc_flags")
exist_id <- paste(existing$rule, existing$tbl, existing$row_key, sep = "\r")
f_id     <- paste(f$rule, f$tbl, f$row_key, sep = "\r")

new_mask  <- !(f_id %in% exist_id)
new_flags <- f[new_mask, , drop = FALSE]

if (!report_only) {
  ## refresh re-observed flags: last_seen AND severity + details (status
  ## untouched -> accepted stays). Rewriting severity is load-bearing: when a
  ## rule's severity is edited in code (GEO-BOUNDS high -> med when it went
  ## international-aware), a last_seen-only upsert leaves every pre-existing
  ## flag carrying the STALE severity, so a ledger query for "open high" reads
  ## a label the code no longer assigns. details refreshes for the same reason
  ## (a row's coordinates/hometown can change between sweeps).
  seen_old <- f[!new_mask, , drop = FALSE]
  if (nrow(seen_old) > 0) {
    dbExecute(con,
      "UPDATE qc_flags SET last_seen = ?, severity = ?, details = ? WHERE rule = ? AND tbl = ? AND row_key = ?",
      params = list(rep(now, nrow(seen_old)), seen_old$severity,
                    seen_old$details, seen_old$rule, seen_old$tbl,
                    seen_old$row_key))
  }
  if (nrow(new_flags) > 0) {
    dbExecute(con,
      "INSERT INTO qc_flags (rule, tbl, row_key, severity, details, first_seen, last_seen, status)
       VALUES (?, ?, ?, ?, ?, ?, ?, 'open')",
      params = list(new_flags$rule, new_flags$tbl, new_flags$row_key,
                    new_flags$severity, new_flags$details,
                    rep(now, nrow(new_flags)), rep(now, nrow(new_flags))))
  }
  ## auto-close: open flags whose row no longer matches any rule this sweep
  open_now <- existing[existing$status == "open", , drop = FALSE]
  open_id  <- paste(open_now$rule, open_now$tbl, open_now$row_key, sep = "\r")
  gone     <- open_now[!(open_id %in% f_id), , drop = FALSE]
  if (nrow(gone) > 0) {
    dbExecute(con,
      "UPDATE qc_flags SET status = 'fixed', last_seen = ? WHERE rule = ? AND tbl = ? AND row_key = ? AND status = 'open'",
      params = list(rep(now, nrow(gone)), gone$rule, gone$tbl, gone$row_key))
  }
}

## ---------------------------------------------------------------------------
## report
## ---------------------------------------------------------------------------
cat("\n==== QC SWEEP ", now, if (report_only) " (report-only)", " ====\n",
    sep = "")
if (nrow(f) == 0) {
  cat("No rule matches anywhere. Clean sweep.\n")
} else {
  tab <- aggregate(row_key ~ rule + severity, f, length)
  tab <- tab[order(match(tab$severity, c("high", "med", "low"))), ]
  for (i in seq_len(nrow(tab)))
    cat(sprintf("  %-12s %-5s %d row(s)\n", tab$rule[i], tab$severity[i],
                tab$row_key[i]))
  cat("\nNEW this sweep (not previously in the ledger): ", nrow(new_flags),
      "\n", sep = "")
  if (nrow(new_flags) > 0) {
    show <- head(new_flags, 25)
    for (i in seq_len(nrow(show)))
      cat(sprintf("  [%s|%s] %s -- %s\n", show$severity[i], show$rule[i],
                  show$row_key[i], show$details[i]))
    if (nrow(new_flags) > 25)
      cat("  ... and ", nrow(new_flags) - 25, " more (see qc_flags)\n",
          sep = "")
  }
}
accepted_n <- sum(existing$status == "accepted")
cat("\nLedger: ", nrow(existing), " prior flag(s), ", accepted_n,
    " accepted (never re-raised as new).\n", sep = "")
cat("To accept a verified-legit outlier:\n",
    "  UPDATE qc_flags SET status='accepted' WHERE rule='GEO-STATE' AND row_key='<Name|school|Year|Type>';\n",
    sep = "")

new_high <- sum(new_flags$severity == "high")
if (new_high > 0) {
  cat("\n", new_high, " NEW high-severity flag(s) -- exiting 2 so the caller",
      " can escalate.\n", sep = "")
  quit(save = "no", status = 2)
}
cat("\nNo new high-severity flags.\n")
