## ===========================================================================
## buildTeamConfig.R  --  PHASE 1 team-config emitter
##
## Turns the validated slug map + one CFBD /teams/fbs call into
## data/team_config.csv, the new source of truth for TEAM_CONFIG. The existing
## 16 Big 12 rows are preserved BYTE-FOR-BYTE from R/team_config.R (slug,
## name, logo, colors, conference, state, conf_since) and marked onboarded=TRUE;
## every other Power-4 team is added onboarded=FALSE so it is PRESENT in the
## config but HIDDEN from the app until its data is backfilled per-conference.
##
## For each NEW team it also downloads the CFBD primary logo (logos[[1]],
## http -> https) to www/<slug>.png (skipped if the file already exists).
##
## conf_since:
##   * existing 16      : preserved exactly from R/team_config.R
##   * 2024 realignment : the nine movers hardcoded to 2024 (Texas, Oklahoma
##                        -> SEC; USC, UCLA, Oregon, Washington -> Big Ten;
##                        California, Stanford, SMU -> ACC). CFBD does not
##                        report a join year, so these are hardcoded.
##   * every other new  : 2012 legacy default (predates the app's 2016 window)
##
## CSV column order (contract):
##   slug, team_name, logo, primary, secondary, conference, state,
##   conf_since, cfbd_name, campus_lat, campus_long, onboarded
##
## Idempotent + re-runnable: overwrites data/team_config.csv, skips logos that
## already exist. Run from the project root, AFTER validateSlugs.R:
##   Rscript scripts/buildTeamConfig.R
## Needs CFBD_API_KEY in ~/.Renviron.
## ===========================================================================

suppressMessages({
  library(httr)
  library(jsonlite)
})

source(here::here("R", "team_config.R"))

key <- Sys.getenv("CFBD_API_KEY")
if (key == "") {
  cat("No CFBD_API_KEY found.\n",
      "1. Get a free key: https://collegefootballdata.com/key\n",
      "2. Add CFBD_API_KEY=<key> to your .Renviron (then restart R)\n",
      "3. Re-run: Rscript scripts/buildTeamConfig.R\n")
  quit(save = "no", status = 1)
}

cfbd_get <- function(path, ...) {
  resp <- GET(paste0("https://api.collegefootballdata.com", path),
              add_headers(Authorization = paste("Bearer", key)),
              query = list(...), timeout(30))
  if (status_code(resp) == 401) stop("CFBD key rejected (401) -- check .Renviron")
  if (status_code(resp) != 200) stop("CFBD ", path, " returned ", status_code(resp))
  fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}

map_path <- here::here("data", "p4_slug_map.csv")
if (!file.exists(map_path)) {
  stop("data/p4_slug_map.csv not found -- run scripts/validateSlugs.R first")
}
slug_map <- read.csv(map_path, stringsAsFactors = FALSE)
stopifnot(all(c("cfbd_name", "conference", "candidate_slug", "valid") %in%
                names(slug_map)))

## ---------------------------------------------------------------------------
## existing-16 cfbd_name (mirrors CFBD_NAMES / fetchOutcomes.R). Authoritative
## for the existing rows' cfbd_name + their CFBD campus join, so preservation
## never depends on the slug map's contents. Guarded against TEAM_CONFIG.
## ---------------------------------------------------------------------------
EXISTING_CFBD <- c(
  "arizona" = "Arizona", "arizona-state" = "Arizona State",
  "baylor" = "Baylor", "byu" = "BYU", "central-florida" = "UCF",
  "cincinnati" = "Cincinnati", "colorado" = "Colorado",
  "houston" = "Houston", "iowa-state" = "Iowa State", "kansas" = "Kansas",
  "kansas-state" = "Kansas State", "oklahoma-state" = "Oklahoma State",
  "tcu" = "TCU", "texas-tech" = "Texas Tech", "utah" = "Utah",
  "west-virginia" = "West Virginia")
## the 16 canonical Big 12 slugs -- the IDENTITY of the preserved rows. This is
## the anchor that keeps the build idempotent: once data/team_config.csv exists,
## R/team_config.R loads it and TEAM_CONFIG becomes all 67 P4 rows, so we must
## NEVER treat "everything in TEAM_CONFIG" as existing -- only these 16 are.
EXISTING_SLUGS <- names(EXISTING_CFBD)
stopifnot(all(EXISTING_SLUGS %in% TEAM_CONFIG$slug))

## the nine 2024 realignment movers, keyed on CFBD school name (stable)
MOVERS_2024 <- c("Texas", "Oklahoma",                       # -> SEC
                 "USC", "UCLA", "Oregon", "Washington",     # -> Big Ten
                 "California", "Stanford", "SMU")           # -> ACC
LEGACY_SINCE <- 2012L

P4_CONF <- c("Big 12", "Big Ten", "SEC", "ACC")

## full-name -> USPS abbreviation, a safety net in case CFBD ever returns a
## spelled-out state (it normally ships 2-letter codes, kept as-is below)
STATE_ABBR <- c(
  "Alabama"="AL","Alaska"="AK","Arizona"="AZ","Arkansas"="AR","California"="CA",
  "Colorado"="CO","Connecticut"="CT","Delaware"="DE","District of Columbia"="DC",
  "Florida"="FL","Georgia"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL",
  "Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA",
  "Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI",
  "Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT",
  "Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ",
  "New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND",
  "Ohio"="OH","Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA",
  "Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD","Tennessee"="TN",
  "Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA",
  "West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY")
norm_state <- function(x) {
  x <- trimws(as.character(x))
  ifelse(is.na(x) | !nzchar(x), NA_character_,
         ifelse(nchar(x) == 2, toupper(x), unname(STATE_ABBR[x])))
}

norm_hex <- function(x) {
  x <- toupper(trimws(as.character(x)))
  ifelse(is.na(x) | !nzchar(x), NA_character_,
         ifelse(startsWith(x, "#"), x, paste0("#", x)))
}

## ---------------------------------------------------------------------------
## CFBD data for every P4 team
## ---------------------------------------------------------------------------
cat("Fetching CFBD /teams/fbs (year 2025)...\n")
teams <- as.data.frame(cfbd_get("/teams/fbs", year = 2025))
need <- c("school", "conference", "color", "alternateColor",
          "location.latitude", "location.longitude", "location.state")
miss <- setdiff(need, names(teams))
if (length(miss) > 0) stop("CFBD /teams/fbs missing field(s): ",
                           paste(miss, collapse = ", "))
if (!"logos" %in% names(teams)) stop("CFBD /teams/fbs missing 'logos'")

## first logo url for a given CFBD school (logos is a list-column)
logo_url_for <- function(school) {
  idx <- match(school, teams$school)
  if (is.na(idx)) return(NA_character_)
  lg <- teams$logos[[idx]]
  if (is.null(lg) || length(lg) == 0 || is.na(lg[1])) return(NA_character_)
  as.character(lg[1])
}

cfbd_val <- function(school, col) {
  idx <- match(school, teams$school)
  if (is.na(idx)) return(NA)
  teams[[col]][idx]
}

## ---------------------------------------------------------------------------
## (1) existing 16 -- preserved exactly, onboarded = TRUE. Subset TEAM_CONFIG to
## the 16 canonical slugs (works whether TEAM_CONFIG is the inline 16 fallback
## OR the CSV-loaded 67); their preserved values are the same either way.
## ---------------------------------------------------------------------------
base16 <- TEAM_CONFIG[match(EXISTING_SLUGS, TEAM_CONFIG$slug), , drop = FALSE]
existing <- data.frame(
  slug        = base16$slug,
  team_name   = base16$team_name,
  logo        = base16$logo,
  primary     = base16$primary,
  secondary   = base16$secondary,
  conference  = base16$conference,
  state       = base16$state,
  conf_since  = as.integer(base16$conf_since),
  cfbd_name   = unname(EXISTING_CFBD[base16$slug]),
  stringsAsFactors = FALSE)
existing$campus_lat  <- as.numeric(vapply(existing$cfbd_name,
                          function(s) as.numeric(cfbd_val(s, "location.latitude")),
                          numeric(1)))
existing$campus_long <- as.numeric(vapply(existing$cfbd_name,
                          function(s) as.numeric(cfbd_val(s, "location.longitude")),
                          numeric(1)))
existing$onboarded   <- TRUE
if (any(is.na(existing$campus_lat) | is.na(existing$campus_long))) {
  cat("[WARN] missing CFBD campus coords for existing team(s): ",
      paste(existing$slug[is.na(existing$campus_lat) |
                            is.na(existing$campus_long)], collapse = ", "), "\n")
}

## ---------------------------------------------------------------------------
## (2) new teams -- every P4 team that is NOT one of the 16 canonical Big 12
## programs. Split on EXISTING_SLUGS (the fixed 16), never on TEAM_CONFIG$slug,
## which becomes all 67 once the CSV is loaded (that would yield ZERO new teams).
## ---------------------------------------------------------------------------
new_map <- slug_map[slug_map$conference %in% P4_CONF &
                      !(slug_map$candidate_slug %in% EXISTING_SLUGS), ,
                    drop = FALSE]
## a new team with an unresolved slug (needs_review) stays in the config as a
## hidden placeholder, but is flagged: onboarding is gated later on a real slug
review_new <- new_map[!as.logical(new_map$valid), , drop = FALSE]
## onboarded preservation: a plain re-run must never silently UN-onboard a
## conference a later backfill already flipped. If a new team is already in the
## current config as onboarded, keep it; a brand-new team defaults FALSE (hidden).
prev_onb <- if ("onboarded" %in% names(TEAM_CONFIG)) {
  as.logical(TEAM_CONFIG$onboarded[match(new_map$candidate_slug, TEAM_CONFIG$slug)])
} else rep(NA, nrow(new_map))
new_onboarded <- ifelse(is.na(prev_onb), FALSE, prev_onb)

new <- data.frame(
  slug        = new_map$candidate_slug,
  team_name   = new_map$cfbd_name,
  logo        = paste0(new_map$candidate_slug, ".png"),
  primary     = norm_hex(vapply(new_map$cfbd_name,
                          function(s) as.character(cfbd_val(s, "color")), character(1))),
  secondary   = norm_hex(vapply(new_map$cfbd_name,
                          function(s) as.character(cfbd_val(s, "alternateColor")), character(1))),
  conference  = new_map$conference,
  state       = norm_state(vapply(new_map$cfbd_name,
                          function(s) as.character(cfbd_val(s, "location.state")), character(1))),
  conf_since  = ifelse(new_map$cfbd_name %in% MOVERS_2024, 2024L, LEGACY_SINCE),
  cfbd_name   = new_map$cfbd_name,
  campus_lat  = as.numeric(vapply(new_map$cfbd_name,
                          function(s) as.numeric(cfbd_val(s, "location.latitude")), numeric(1))),
  campus_long = as.numeric(vapply(new_map$cfbd_name,
                          function(s) as.numeric(cfbd_val(s, "location.longitude")), numeric(1))),
  onboarded   = new_onboarded,
  stringsAsFactors = FALSE)
## order new teams by conference (Big Ten/SEC/ACC) then name for a stable CSV
new <- new[order(match(new$conference, P4_CONF), new$team_name), , drop = FALSE]

## ---------------------------------------------------------------------------
## (3) combine (existing first, exact order) + write
## ---------------------------------------------------------------------------
col_order <- c("slug", "team_name", "logo", "primary", "secondary",
               "conference", "state", "conf_since", "cfbd_name",
               "campus_lat", "campus_long", "onboarded")
out <- rbind(existing[, col_order], new[, col_order])

## hard guard: the 16 canonical rows must survive byte-for-byte on every
## preserved field (the "shipped experience stays byte-identical" contract).
## Compare against base16 -- the 16 canonical rows -- NOT the whole TEAM_CONFIG,
## which is all 67 P4 rows once the CSV is loaded.
chk <- out[match(EXISTING_SLUGS, out$slug), ]
for (col in c("slug", "team_name", "logo", "primary", "secondary",
              "conference", "state")) {
  if (!identical(as.character(chk[[col]]), as.character(base16[[col]]))) {
    stop("existing-16 preservation FAILED on column '", col, "'")
  }
}
if (!identical(as.integer(chk$conf_since), as.integer(base16$conf_since))) {
  stop("existing-16 preservation FAILED on conf_since")
}
if (!all(chk$onboarded)) stop("a canonical Big 12 team is not onboarded=TRUE")
stopifnot(!any(duplicated(out$slug)))

dir.create(here::here("data"), showWarnings = FALSE)
cfg_path <- here::here("data", "team_config.csv")
write.csv(out, cfg_path, row.names = FALSE)

## ---------------------------------------------------------------------------
## (4) download NEW teams' logos to www/<slug>.png (skip existing)
## ---------------------------------------------------------------------------
dir.create(here::here("www"), showWarnings = FALSE)
logo_fail <- character(0)
downloaded <- 0L
skipped <- 0L
for (i in seq_len(nrow(new))) {
  dest <- here::here("www", new$logo[i])
  if (file.exists(dest) && file.size(dest) > 0) { skipped <- skipped + 1L; next }
  url <- logo_url_for(new$cfbd_name[i])
  if (is.na(url)) { logo_fail <- c(logo_fail, new$slug[i]); next }
  url <- sub("^http://", "https://", url)
  resp <- try(GET(url, timeout(30), write_disk(dest, overwrite = TRUE)),
              silent = TRUE)
  ok <- !inherits(resp, "try-error") && status_code(resp) == 200 &&
    file.exists(dest) && file.size(dest) > 0
  if (ok) {
    downloaded <- downloaded + 1L
  } else {
    if (file.exists(dest)) unlink(dest)   # never leave a 0-byte/failed logo
    logo_fail <- c(logo_fail, new$slug[i])
  }
  Sys.sleep(runif(1, 0.3, 0.8))
}

## ---------------------------------------------------------------------------
## summary
## ---------------------------------------------------------------------------
cat("\n==== TEAM CONFIG SUMMARY ====\n")
cat("wrote:", cfg_path, "\n")
cat("total rows      :", nrow(out), "\n")
cat("onboarded (TRUE):", sum(out$onboarded), "\n")
cat("hidden  (FALSE) :", sum(!out$onboarded), "\n\n")
cat("by conference (onboarded / total):\n")
for (cf in P4_CONF) {
  sub <- out[out$conference == cf, ]
  cat(sprintf("  %-8s %2d / %2d\n", cf, sum(sub$onboarded), nrow(sub)))
}
cat("\nlogos: ", downloaded, " downloaded, ", skipped,
    " already present, ", length(logo_fail), " failed\n", sep = "")
if (length(logo_fail) > 0) {
  cat("  logo download FAILED (needs_review): ",
      paste(logo_fail, collapse = ", "), "\n")
}
if (nrow(review_new) > 0) {
  cat("\n[WARN] new team(s) with an UNRESOLVED slug -- written hidden, but the",
      "slug must be fixed (validateSlugs.R) before onboarding:\n")
  cat("  ", paste(review_new$cfbd_name, collapse = ", "), "\n")
}
## CFBD is meant to be 100% populated on P4 campus coords, but the odd school
## comes back NULL. Report (do not guess) -- campus_lat/long stays NA and the
## roster scrape supplies per-player college coords when the team is backfilled.
no_campus <- out$slug[!out$onboarded & (is.na(out$campus_lat) |
                                          is.na(out$campus_long))]
if (length(no_campus) > 0) {
  cat("\n[WARN] new team(s) with NO CFBD campus coords (left NA, not guessed): ",
      paste(no_campus, collapse = ", "), "\n")
}
cat("\nExisting 16 preserved byte-for-byte; every new team is onboarded=FALSE",
    "\n(present in the config, hidden from the app until per-conference backfill).\n")
