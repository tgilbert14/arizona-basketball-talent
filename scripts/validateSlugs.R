## ===========================================================================
## validateSlugs.R  --  PHASE 1 slug-map builder (247 landing-page sweep)
##
## Builds a CANDIDATE 247Sports slug for every Power-4 team CFBD reports and
## validates it against the live 247 college landing page. This gates
## onboarding: a wrong slug fails SAFE (non-200 -> needs_review -> the team is
## never scraped) instead of silently pulling the wrong school's recruits.
##
## Candidate slug rules (per the Phase 1 design):
##   * existing 16 Big 12 teams REUSE their known-good slugs from
##     R/team_config.R -- never re-derived (central-florida != "ucf")
##   * a hardcoded QUIRK map covers the known non-derivable new ones
##     (Miami FL -> miami, NC State -> north-carolina-state, etc.)
##   * everything else derives: lowercase, drop & . ' , spaces -> hyphens
##
## For each of the 67 candidates it fetches https://247sports.com/college/<slug>/
## with the scraper's exact user agent (mirrors refreshClassYear.R), a 1-2s
## polite sleep between teams and a 3-try retry, and records the HTTP status.
##
## Emits data/p4_slug_map.csv (one row per P4 team):
##   cfbd_name, conference, candidate_slug, http_status, valid, needs_review
## and prints a summary + the needs-review list LOUDLY. It does NOT write
## team_config -- buildTeamConfig.R consumes this map.
##
## Run from the project root (~67 polite fetches, one-time, a few minutes):
##   Rscript scripts/validateSlugs.R
## Needs CFBD_API_KEY in ~/.Renviron (same key fetchOutcomes.R uses).
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
      "3. Re-run: Rscript scripts/validateSlugs.R\n")
  quit(save = "no", status = 1)
}

## same Bearer pattern as fetchOutcomes.R
cfbd_get <- function(path, ...) {
  resp <- GET(paste0("https://api.collegefootballdata.com", path),
              add_headers(Authorization = paste("Bearer", key)),
              query = list(...), timeout(30))
  if (status_code(resp) == 401) stop("CFBD key rejected (401) -- check .Renviron")
  if (status_code(resp) != 200) stop("CFBD ", path, " returned ", status_code(resp))
  fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}

## exact scraper user agent (kept byte-identical to refreshClassYear.R so 247
## sees one consistent, polite browser face across the whole pipeline)
UA <- user_agent(paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"))

## the four Power-4 leagues (CFBD's exact conference strings). Notre Dame is
## "FBS Independents" and Miami (OH) is "Mid-American", so both fall out here.
P4_CONF <- c("Big 12", "Big Ten", "SEC", "ACC")

## ---------------------------------------------------------------------------
## existing-16 slug <-> CFBD name. Mirrors CFBD_NAMES in fetchOutcomes.R (the
## Phase-1 seam that becomes the cfbd_name column). Guarded below against
## TEAM_CONFIG so the two can never silently drift.
## ---------------------------------------------------------------------------
EXISTING_CFBD <- c(
  "arizona" = "Arizona", "arizona-state" = "Arizona State",
  "baylor" = "Baylor", "byu" = "BYU", "central-florida" = "UCF",
  "cincinnati" = "Cincinnati", "colorado" = "Colorado",
  "houston" = "Houston", "iowa-state" = "Iowa State", "kansas" = "Kansas",
  "kansas-state" = "Kansas State", "oklahoma-state" = "Oklahoma State",
  "tcu" = "TCU", "texas-tech" = "Texas Tech", "utah" = "Utah",
  "west-virginia" = "West Virginia")
## the 16 canonical slugs must all be known to TEAM_CONFIG. Use `%in%` not
## setequal: once data/team_config.csv exists, R/team_config.R loads it and
## TEAM_CONFIG carries all 67 P4 slugs -- the 16 are a SUBSET of it, not equal.
stopifnot(all(names(EXISTING_CFBD) %in% TEAM_CONFIG$slug))
## reverse lookup: CFBD name -> known-good existing slug
EXISTING_BY_NAME <- setNames(names(EXISTING_CFBD), unname(EXISTING_CFBD))

## ---------------------------------------------------------------------------
## QUIRK map for the NEW teams whose 247 slug is NOT name-derivable, keyed on
## CFBD school name (stable). UCF is not here -- it is an existing team and is
## resolved by EXISTING_BY_NAME above. Anything 247 renames later shows up as
## needs_review in the sweep and gets added here.
## ---------------------------------------------------------------------------
## NOTE: the 2026-07 sweep found 247 had RENAMED two slugs the original design
## recon listed as miami-fl / nc-state -- both now 404. The live-verified slugs
## are "miami" (og:title "Miami Hurricanes", team id Miami-Hurricanes-Football-13,
## NOT Miami OH which 404s at miami-oh) and "north-carolina-state" (og:title
## "NC State Wolfpack"). This is exactly the drift the sweep exists to catch.
QUIRK <- c(
  "Miami"     = "miami",                  # CFBD "Miami" = Miami (FL) Hurricanes
  "Ole Miss"  = "ole-miss",
  "NC State"  = "north-carolina-state",   # 247 renamed away from nc-state
  "Texas A&M" = "texas-am",
  "USC"       = "usc")

## exact spec: lowercase, drop ampersand/period/apostrophe (straight + curly),
## collapse whitespace to single hyphens.
derive_slug <- function(name) {
  s <- tolower(name)
  s <- gsub("[&.'’]", "", s)
  s <- gsub("\\s+", "-", trimws(s))
  s
}

candidate_slug <- function(cfbd_name) {
  if (cfbd_name %in% names(EXISTING_BY_NAME)) return(unname(EXISTING_BY_NAME[cfbd_name]))
  if (cfbd_name %in% names(QUIRK))            return(unname(QUIRK[cfbd_name]))
  derive_slug(cfbd_name)
}

## fetch the 247 landing page; return the final HTTP status (-1 on transient
## failure after retries). 3 tries with a linear backoff; a 200 short-circuits.
fetch_status <- function(slug, tries = 3) {
  url <- paste0("https://247sports.com/college/", slug, "/")
  st <- -1L
  for (attempt in seq_len(tries)) {
    resp <- try(GET(url, UA, timeout(25)), silent = TRUE)
    if (!inherits(resp, "try-error")) {
      st <- status_code(resp)
      if (st == 200) break
    }
    if (attempt < tries) Sys.sleep(2 * attempt)
  }
  st
}

## ---------------------------------------------------------------------------
## build the candidate table
## ---------------------------------------------------------------------------
cat("Fetching CFBD /teams/fbs (year 2025)...\n")
teams <- as.data.frame(cfbd_get("/teams/fbs", year = 2025))
if (!"conference" %in% names(teams) || !"school" %in% names(teams)) {
  stop("CFBD /teams/fbs response missing 'school'/'conference' -- schema drift")
}
p4 <- teams[teams$conference %in% P4_CONF, c("school", "conference")]
p4 <- p4[!is.na(p4$school), , drop = FALSE]
p4 <- p4[order(match(p4$conference, P4_CONF), p4$school), , drop = FALSE]
cat("Power-4 teams from CFBD:", nrow(p4), "\n")
for (cf in P4_CONF) cat("  ", cf, ": ", sum(p4$conference == cf), "\n", sep = "")

p4$candidate_slug <- vapply(p4$school, candidate_slug, character(1))

## duplicate-slug guard: two teams deriving to the same slug would each scrape
## the other's page. Report before the sweep so it is never silent.
dup <- p4$candidate_slug[duplicated(p4$candidate_slug)]
if (length(dup) > 0) {
  cat("\n[WARN] duplicate candidate slug(s) -- add a QUIRK override:\n")
  print(p4[p4$candidate_slug %in% dup, c("school", "conference", "candidate_slug")])
}

## ---------------------------------------------------------------------------
## the polite sweep
## ---------------------------------------------------------------------------
cat("\nValidating", nrow(p4), "candidate slugs against 247 landing pages",
    "(1-2s between fetches)...\n\n")
status <- integer(nrow(p4))
for (i in seq_len(nrow(p4))) {
  status[i] <- fetch_status(p4$candidate_slug[i])
  cat(sprintf("  %-28s %-16s -> %s\n", p4$school[i], p4$candidate_slug[i],
              if (status[i] == 200) "200 OK" else paste0("HTTP ", status[i])))
  if (i < nrow(p4)) Sys.sleep(runif(1, 1, 2))
}

out <- data.frame(
  cfbd_name      = p4$school,
  conference     = p4$conference,
  candidate_slug = p4$candidate_slug,
  http_status    = status,
  valid          = status == 200L,
  needs_review   = status != 200L,
  stringsAsFactors = FALSE)

dir.create(here::here("data"), showWarnings = FALSE)
map_path <- here::here("data", "p4_slug_map.csv")
write.csv(out, map_path, row.names = FALSE)

## ---------------------------------------------------------------------------
## summary -- print the needs-review list LOUDLY
## ---------------------------------------------------------------------------
cat("\n==== SLUG SWEEP SUMMARY ====\n")
cat("wrote:", map_path, "\n")
cat("total P4 teams :", nrow(out), "\n")
cat("valid (200)    :", sum(out$valid), "\n")
cat("needs review   :", sum(out$needs_review), "\n")
if (any(out$needs_review)) {
  cat("\n!!! NEEDS REVIEW (slug did not resolve on 247 -- fix before onboarding) !!!\n")
  nr <- out[out$needs_review, c("cfbd_name", "conference", "candidate_slug",
                                "http_status")]
  for (i in seq_len(nrow(nr))) {
    cat(sprintf("  - %-24s [%-8s] candidate '%s' (HTTP %s)\n",
                nr$cfbd_name[i], nr$conference[i], nr$candidate_slug[i],
                nr$http_status[i]))
  }
  cat("\nAdd a QUIRK override (CFBD name -> real 247 slug) and re-run.\n")
} else {
  cat("\nAll candidate slugs resolved 200 -- every P4 team has a valid slug.\n")
}
