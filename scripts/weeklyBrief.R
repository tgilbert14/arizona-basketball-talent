## ===========================================================================
## weeklyBrief.R
## Auto-write docs/brief/index.html -- the "what changed" page the landing
## site links to. Diffs the live db against a week-old pre-run snapshot:
##
##   baseline = newest backups/pre_run_*.db that is >= 6 days old;
##              if none that old, the OLDEST snapshot on hand (labeled so);
##              if no snapshots at all, the db regenerated from git HEAD
##              (same cat-file + quick_check pattern as auditRefreshHoles.R)
##
## Understated on purpose: every number carries its n, removals are labeled
## "no longer listed" (247 dropping a row usually means a decommit or
## reclassification -- it says nothing about where the player went), and
## per-team movement only prints when a team has n >= 3 commits and a
## nonzero delta. Static, self-contained HTML; no scripts.
##
## Run from the project root:
##   Rscript scripts/weeklyBrief.R
##   Rscript scripts/weeklyBrief.R --db X --baseline Y --out Z   # tests only
## ===========================================================================

suppressMessages({
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

source(here::here("R", "team_config.R"))

## ---------------------------------------------------------------------------
## args
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else NA_character_
}
db_path   <- opt("--db")
base_arg  <- opt("--baseline")
out_path  <- opt("--out")
if (is.na(db_path))  db_path  <- here::here("data", "recruiting.db")
if (is.na(out_path)) out_path <- here::here("docs", "brief", "index.html")

## ---------------------------------------------------------------------------
## baseline resolution
## ---------------------------------------------------------------------------
quick_check <- function(db) {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(conn, "PRAGMA quick_check")[[1]][1]
}

pick_baseline <- function() {
  files <- list.files(here::here("backups"),
                      pattern = "^pre_run_[0-9]{8}_[0-9]{6}\\.db$",
                      full.names = TRUE)
  if (length(files) > 0) {
    stamps <- as.POSIXct(
      sub("^pre_run_([0-9]{8})_([0-9]{6})\\.db$", "\\1\\2", basename(files)),
      format = "%Y%m%d%H%M%S")
    keep <- !is.na(stamps)
    files <- files[keep]; stamps <- stamps[keep]
  }
  if (length(files) > 0) {
    ord <- order(stamps)
    files <- files[ord]; stamps <- stamps[ord]
    old_enough <- which(stamps <= Sys.time() - 6 * 86400)
    if (length(old_enough) > 0) {
      k <- max(old_enough)   # newest snapshot that is at least 6 days old
      return(list(path = files[k], date = as.Date(stamps[k]), mode = "week"))
    }
    return(list(path = files[1], date = as.Date(stamps[1]), mode = "oldest"))
  }
  ## no snapshots at all -- regenerate the committed db straight from git
  cat("No pre_run snapshots found -- regenerating baseline from git HEAD\n")
  tmp <- tempfile(fileext = ".db")
  status <- system2("git",
                    c("-C", shQuote(here::here()),
                      "cat-file", "blob", "HEAD:data/recruiting.db"),
                    stdout = tmp)
  if (status != 0 || !file.exists(tmp) || file.size(tmp) == 0) {
    stop("could not regenerate baseline from git (cat-file exit ", status, ")")
  }
  chk <- quick_check(tmp)
  if (!identical(chk, "ok")) {
    stop("git-regenerated baseline failed PRAGMA quick_check ('", chk, "')")
  }
  list(path = tmp, date = NA, mode = "head")
}

if (!is.na(base_arg)) {
  if (!file.exists(base_arg)) stop("baseline not found: ", base_arg)
  baseline <- list(path = base_arg,
                   date = as.Date(file.mtime(base_arg)), mode = "arg")
} else {
  baseline <- pick_baseline()
}

fmt_date <- function(d) sub(" 0", " ", format(d, "%b %d, %Y"), fixed = TRUE)
window_label <- switch(baseline$mode,
  week   = paste0("since ", fmt_date(baseline$date)),
  oldest = paste0("since ", fmt_date(baseline$date)),
  arg    = paste0("since ", fmt_date(baseline$date)),
  head   = "since the last published dataset (git HEAD)")
window_note <- if (identical(baseline$mode, "oldest")) {
  paste0("Oldest snapshot on hand &mdash; the window grows to a full week ",
         "as nightly snapshots accumulate.")
} else {
  ""
}
cat("Baseline:", baseline$path, "->", window_label, "\n")

## ---------------------------------------------------------------------------
## diff machinery
## ---------------------------------------------------------------------------
## the scraper's own name normalization -- display names drift between runs
name_key <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))

## generational suffixes come and go between scrapes ("Kevin Moorer" vs
## "Kevin Moorer II") -- strip them off the END of a name key so the pair
## matches. Longest alternatives first so "iii" never half-matches as "ii".
strip_gen <- function(k) sub("(iii|ii|iv|jr|sr|v)$", "", k)

read_tbl <- function(db, tbl) {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  tryCatch(dbGetQuery(conn, paste0(
    "SELECT Name, School, Year, Type, Ranking, Position FROM ", tbl)),
    error = function(e) NULL)
}

keyed <- function(d) {
  d %>%
    mutate(.k = paste(name_key(Name), School, Year, Type, sep = "|")) %>%
    distinct(.k, .keep_all = TRUE)
}

## per-sport diff: additions, removals, renames, per-team movement,
## blue-chip adds
diff_sport <- function(cur, base) {
  c2 <- keyed(cur)
  b2 <- keyed(base)
  adds <- anti_join(c2, b2, by = ".k")
  gone <- anti_join(b2, c2, by = ".k")

  ## Coverage honesty applies to BOTH dimensions that can expand between
  ## snapshots: a newly tracked class year and a newly onboarded program.
  ## Every row for a school absent from the baseline is backfill, not weekly
  ## recruiting activity. Exclude the union before any headline, blue-chip,
  ## rename, or mover calculation.
  current_schools <- unique(c2$School[!is.na(c2$School) & nzchar(c2$School)])
  baseline_schools <- unique(b2$School[!is.na(b2$School) & nzchar(b2$School)])
  new_schools <- setdiff(current_schools, baseline_schools)
  old_schools <- setdiff(baseline_schools, current_schools)
  stable_schools <- intersect(current_schools, baseline_schools)
  current_years <- unique(
    c2$Year[c2$School %in% stable_schools & !is.na(c2$Year)])
  baseline_years <- unique(
    b2$Year[b2$School %in% stable_schools & !is.na(b2$Year)])
  new_years <- setdiff(current_years, baseline_years)

  ## Deep-history changes are dataset maintenance, not current recruiting.
  ## The live window includes this calendar year, the immediately prior class,
  ## and the next cycle so late portal/listing updates remain eligible.
  activity_floor <- as.integer(format(Sys.Date(), "%Y")) - 1L
  activity_ceiling <- as.integer(format(Sys.Date(), "%Y")) + 1L

  coverage_school <- adds[adds$School %in% new_schools, , drop = FALSE]
  if (nrow(adds) > 0) {
    adds <- adds[!(adds$School %in% new_schools), , drop = FALSE]
  }
  coverage_year <- adds[adds$Year %in% new_years, , drop = FALSE]
  if (nrow(adds) > 0) {
    adds <- adds[!(adds$Year %in% new_years), , drop = FALSE]
  }
  coverage_history <- adds[
    !is.na(adds$Year) &
      (adds$Year < activity_floor | adds$Year > activity_ceiling), ,
    drop = FALSE]
  if (nrow(adds) > 0) {
    adds <- adds[is.na(adds$Year) |
                   (adds$Year >= activity_floor & adds$Year <= activity_ceiling), ,
                 drop = FALSE]
  }

  ## Mirror the same rule for a retired class year or a program removed from
  ## the tracked universe. Those rows are coverage contraction, not decommits.
  old_years <- setdiff(baseline_years, current_years)
  contraction_school <- gone[gone$School %in% old_schools, , drop = FALSE]
  if (nrow(gone) > 0) {
    gone <- gone[!(gone$School %in% old_schools), , drop = FALSE]
  }
  contraction_year <- gone[gone$Year %in% old_years, , drop = FALSE]
  if (nrow(gone) > 0) {
    gone <- gone[!(gone$Year %in% old_years), , drop = FALSE]
  }
  contraction_history <- gone[
    !is.na(gone$Year) &
      (gone$Year < activity_floor | gone$Year > activity_ceiling), ,
    drop = FALSE]
  if (nrow(gone) > 0) {
    gone <- gone[is.na(gone$Year) |
                   (gone$Year >= activity_floor & gone$Year <= activity_ceiling), ,
                 drop = FALSE]
  }

  ## name-drift collapse: 247 relabeling a player ("Kevin Moorer" ->
  ## "Kevin Moorer II") lands as one add + one remove sharing
  ## School+Year+Type. When one name key is a prefix of the other, or the
  ## keys match after stripping a generational suffix, that pair is the
  ## same player listed under a new name -- report it once, and inflate
  ## neither the addition nor the removal count.
  renamed <- NULL
  if (nrow(adds) > 0 && nrow(gone) > 0) {
    a_key <- name_key(adds$Name)
    g_key <- name_key(gone$Name)
    drop_a <- logical(nrow(adds))
    drop_g <- logical(nrow(gone))
    pairs <- list()
    for (i in seq_len(nrow(adds))) {
      for (j in seq_len(nrow(gone))) {
        if (drop_g[j]) next
        if (!isTRUE(adds$School[i] == gone$School[j]) ||
            !isTRUE(adds$Year[i] == gone$Year[j]) ||
            !isTRUE(adds$Type[i] == gone$Type[j])) next
        a <- a_key[i]
        g <- g_key[j]
        if (!nzchar(a) || !nzchar(g)) next
        if (startsWith(a, g) || startsWith(g, a) ||
            identical(strip_gen(a), strip_gen(g))) {
          pairs[[length(pairs) + 1L]] <- data.frame(
            old = gone$Name[j], new = adds$Name[i],
            School = adds$School[i], Year = adds$Year[i],
            Type = adds$Type[i], stringsAsFactors = FALSE)
          drop_a[i] <- TRUE
          drop_g[j] <- TRUE
          break
        }
      }
    }
    if (length(pairs) > 0) {
      renamed <- bind_rows(pairs)
      adds <- adds[!drop_a, , drop = FALSE]
      gone <- gone[!drop_g, , drop = FALSE]
    }
  }

  yr <- suppressWarnings(max(cur$Year, na.rm = TRUE))
  ## the movers table keys on the newest tracked cycle; never let a stray
  ## beyond-ceiling row (calendar+2) move the goalposts
  yr <- min(yr, as.integer(format(Sys.Date(), "%Y")) + 1L)
  movement_available <- yr %in% current_years && yr %in% baseline_years
  mov <- if (movement_available) {
    full_join(
      cur %>% filter(School %in% stable_schools,
                     Year == yr, Type == "Commit") %>%
        group_by(School) %>%
        summarize(n_now = n(), avg_now = mean(Ranking, na.rm = TRUE),
                  .groups = "drop"),
      base %>% filter(School %in% stable_schools,
                      Year == yr, Type == "Commit") %>%
        group_by(School) %>%
        summarize(n_then = n(), avg_then = mean(Ranking, na.rm = TRUE),
                  .groups = "drop"),
      by = "School") %>%
      mutate(n_now = ifelse(is.na(n_now), 0L, n_now),
             n_then = ifelse(is.na(n_then), 0L, n_then),
             d_n = n_now - n_then,
             d_avg = round(avg_now - avg_then, 1)) %>%
      filter(n_now >= 3, d_n != 0 | (!is.na(d_avg) & d_avg != 0)) %>%
      arrange(desc(abs(d_n)), School)
  } else {
    data.frame()
  }

  blue <- adds %>% filter(!is.na(Ranking), Ranking >= 90)
  list(adds = adds, gone = gone, renamed = renamed, mov = mov, blue = blue,
       yr = yr, movement_available = movement_available,
       coverage_year = coverage_year, coverage_school = coverage_school,
       coverage_history = coverage_history,
       contraction_year = contraction_year,
       contraction_school = contraction_school,
       contraction_history = contraction_history,
       activity_floor = activity_floor, activity_ceiling = activity_ceiling,
       new_schools = new_schools,
       old_schools = old_schools)
}

## ---------------------------------------------------------------------------
## html helpers
## ---------------------------------------------------------------------------
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
team_lab <- function(slug) {
  i <- match(slug, TEAM_CONFIG$slug)
  ifelse(is.na(i), slug, TEAM_CONFIG$team_name[i])
}
rate_lab <- function(r) ifelse(is.na(r), "unrated", sprintf("%.1f", r))
type_lab <- function(t) ifelse(t == "Transfer", "portal transfer", "HS commit")
avg_lab <- function(a) ifelse(is.finite(a), sprintf("%.1f", a), "--")

player_line <- function(d) {
  paste0(esc(d$Name), " (", esc(ifelse(is.na(d$Position), "?", d$Position)),
         ", ", rate_lab(d$Ranking), ", class of ", d$Year, ", ",
         type_lab(d$Type), ")")
}

## one sport's sections
sport_html <- function(sport_name, cur, base, base_label) {
  if (is.null(cur) || nrow(cur) == 0) {
    return(paste0("<h2>", sport_name, "</h2>\n<p class='empty'>No ",
                  tolower(sport_name), " data in the current database.</p>"))
  }
  if (is.null(base) || nrow(base) == 0) {
    return(paste0("<h2>", sport_name, "</h2>\n<p class='empty'>The baseline ",
                  "snapshot holds no ", tolower(sport_name),
                  " rows, so an honest diff is not possible this week.</p>"))
  }
  d <- diff_sport(cur, base)
  h <- c(paste0("<h2>", sport_name, "</h2>"))

  ## Coverage changes appear before activity and never enter its counts.
  if (nrow(d$coverage_year) > 0) {
    cov_yrs <- sort(unique(d$coverage_year$Year))
    h <- c(h, paste0(
      "<p class='coverage'><strong>Coverage expanded:</strong> now tracking ",
      "the ", paste(cov_yrs, collapse = " and "), " class",
      if (length(cov_yrs) > 1) "es" else "", ". ",
      nrow(d$coverage_year), " player rows entered with the new cycle and ",
      "are excluded from this week's recruiting activity.</p>"))
  }
  if (nrow(d$coverage_school) > 0) {
    cov_schools <- sort(unique(d$coverage_school$School))
    h <- c(h, paste0(
      "<p class='coverage'><strong>Program coverage expanded:</strong> ",
      length(cov_schools), " newly tracked program",
      if (length(cov_schools) == 1) "" else "s", " added ",
      nrow(d$coverage_school), " backfilled player rows. Those rows are ",
      "excluded from every activity, blue-chip, and class-movement count ",
      "below.</p>"))
  }
  if (nrow(d$coverage_history) > 0) {
    hist_yrs <- sort(unique(d$coverage_history$Year))
    hist_span <- if (length(hist_yrs) == 1) hist_yrs else
      paste0(min(hist_yrs), "&ndash;", max(hist_yrs))
    hist_schools <- unique(d$coverage_history$School)
    h <- c(h, paste0(
      "<p class='coverage'><strong>Historical coverage repaired:</strong> ",
      nrow(d$coverage_history), " row",
      if (nrow(d$coverage_history) == 1) "" else "s", " across ",
      length(hist_schools), " established program",
      if (length(hist_schools) == 1) "" else "s", " in class",
      if (length(hist_yrs) == 1) " " else "es ", hist_span,
      " were backfilled. This brief's live activity window is ",
      d$activity_floor, "&ndash;", d$activity_ceiling,
      "; those rows are ",
      "excluded from every recruiting-activity total and list.</p>"))
  }

  ## Coverage contraction follows the same honesty rule in reverse.
  if (nrow(d$contraction_year) > 0) {
    con_yrs <- sort(unique(d$contraction_year$Year))
    h <- c(h, paste0(
      "<p class='coverage'><strong>Coverage changed:</strong> no longer ",
      "tracking the ", paste(con_yrs, collapse = " and "), " class",
      if (length(con_yrs) > 1) "es" else "", ". ",
      nrow(d$contraction_year), " rows are excluded from removals.</p>"))
  }
  if (nrow(d$contraction_school) > 0) {
    con_schools <- sort(unique(d$contraction_school$School))
    h <- c(h, paste0(
      "<p class='coverage'><strong>Program coverage contracted:</strong> ",
      length(con_schools), " program",
      if (length(con_schools) == 1) "" else "s", " left the tracked ",
      "universe. Their ", nrow(d$contraction_school), " rows are excluded ",
      "from removals and movement.</p>"))
  }
  if (nrow(d$contraction_history) > 0) {
    hist_yrs <- sort(unique(d$contraction_history$Year))
    hist_span <- if (length(hist_yrs) == 1) hist_yrs else
      paste0(min(hist_yrs), "&ndash;", max(hist_yrs))
    hist_schools <- unique(d$contraction_history$School)
    h <- c(h, paste0(
      "<p class='coverage'><strong>Historical coverage adjusted:</strong> ",
      nrow(d$contraction_history), " row",
      if (nrow(d$contraction_history) == 1) "" else "s", " across ",
      length(hist_schools), " established program",
      if (length(hist_schools) == 1) "" else "s", " in class",
      if (length(hist_yrs) == 1) " " else "es ", hist_span,
      " left the source during data maintenance. They are excluded from ",
      "removal and movement totals.</p>"))
  }

  ## additions, grouped per team, commits and transfers labeled separately
  h <- c(h, paste0("<h3>New additions (n=", nrow(d$adds), ")</h3>"))
  if (nrow(d$adds) == 0) {
    h <- c(h, paste0("<p class='empty'>No new players listed ", base_label,
                     ".</p>"))
  } else {
    h <- c(h, "<ul>")
    schools <- d$adds %>% count(School, sort = TRUE)
    for (s in schools$School) {
      rows <- d$adds %>% filter(School == s) %>% arrange(desc(Ranking))
      lines <- vapply(seq_len(nrow(rows)),
                      function(i) player_line(rows[i, ]), character(1))
      h <- c(h, paste0("<li><strong>", esc(team_lab(s)), "</strong> (n=",
                       nrow(rows), "): ", paste(lines, collapse = "; "),
                       "</li>"))
    }
    h <- c(h, "</ul>")
  }

  ## renames -- collapsed add/remove pairs; only printed when one happened
  if (!is.null(d$renamed) && nrow(d$renamed) > 0) {
    h <- c(h, paste0("<h3>Listed under a new name (n=", nrow(d$renamed),
                     ")</h3>"),
           paste0("<p class='note'>Add/remove pairs on the same team, ",
                  "class, and type whose names differ only by spelling or ",
                  "a generational suffix &mdash; the same player relabeled, ",
                  "so they count as neither an addition nor a removal.</p>"),
           "<ul>")
    for (i in seq_len(nrow(d$renamed))) {
      rn <- d$renamed[i, ]
      h <- c(h, paste0("<li>", esc(rn$old), " is now listed as ",
                       esc(rn$new), " (", esc(team_lab(rn$School)),
                       ", class of ", rn$Year, ", ", type_lab(rn$Type),
                       ")</li>"))
    }
    h <- c(h, "</ul>")
  }

  ## removals -- honest label, never "lost"
  h <- c(h, paste0("<h3>No longer listed (n=", nrow(d$gone), ")</h3>"))
  if (nrow(d$gone) == 0) {
    h <- c(h, paste0("<p class='empty'>No rows dropped off 247's team pages ",
                     base_label, ".</p>"))
  } else {
    h <- c(h, paste0("<p class='note'>Rows that disappeared from 247's team ",
                     "pages &mdash; usually a decommit or reclassification. ",
                     "A disappearing row says nothing about where the ",
                     "player went.</p>"),
           "<ul>")
    gone <- d$gone %>% arrange(School, Name)
    for (i in seq_len(nrow(gone))) {
      g <- gone[i, ]
      h <- c(h, paste0("<li>", esc(g$Name), " (", esc(team_lab(g$School)),
                       ", class of ", g$Year, ", was ", type_lab(g$Type),
                       ")</li>"))
    }
    h <- c(h, "</ul>")
  }

  ## per-team movement, newest cycle only, n >= 3 and nonzero delta
  h <- c(h, paste0("<h3>Class of ", d$yr, " movement (n=", nrow(d$mov),
                   " teams)</h3>"))
  if (!isTRUE(d$movement_available)) {
    h <- c(h, paste0(
      "<p class='coverage'>The baseline does not include the class of ",
      d$yr, " for the stable program set, so class movement is withheld ",
      "until comparable snapshots exist.</p>"))
  } else if (nrow(d$mov) == 0) {
    h <- c(h, paste0("<p class='empty'>No class-size or average-rating ",
                     "movement among teams with at least 3 HS commits.</p>"))
  } else {
    h <- c(h, paste0("<p class='note'>HS commits only; teams shown have at ",
                     "least 3 commits now and a nonzero change ", base_label,
                     ".</p>"),
           "<table><tr><th>Team</th><th>Commits</th>",
           "<th>Avg rating</th></tr>")
    for (i in seq_len(nrow(d$mov))) {
      m <- d$mov[i, ]
      h <- c(h, paste0(
        "<tr><td>", esc(team_lab(m$School)), "</td><td>",
        m$n_then, " &rarr; ", m$n_now,
        " (", sprintf("%+d", m$d_n), ")</td><td>",
        avg_lab(m$avg_then), " &rarr; ", avg_lab(m$avg_now), "</td></tr>"))
    }
    h <- c(h, "</table>")
  }

  ## blue-chip additions
  h <- c(h, paste0("<h3>New blue-chip additions, rating 90+ (n=",
                   nrow(d$blue), ")</h3>"))
  if (nrow(d$blue) == 0) {
    h <- c(h, paste0("<p class='empty'>No new 90+ players ", base_label,
                     ".</p>"))
  } else {
    h <- c(h, "<ul>")
    blue <- d$blue %>% arrange(desc(Ranking))
    for (i in seq_len(nrow(blue))) {
      b <- blue[i, ]
      h <- c(h, paste0("<li>", esc(b$Name), " (",
                       esc(ifelse(is.na(b$Position), "?", b$Position)), ", ",
                       rate_lab(b$Ranking), ") &mdash; ",
                       esc(team_lab(b$School)), ", class of ", b$Year, ", ",
                       type_lab(b$Type), "</li>"))
    }
    h <- c(h, "</ul>")
  }
  paste(h, collapse = "\n")
}

## ---------------------------------------------------------------------------
## build the page
## ---------------------------------------------------------------------------
sports <- list(
  Football   = list(cur = read_tbl(db_path, "recruit_class_football"),
                    base = read_tbl(baseline$path, "recruit_class_football")),
  Basketball = list(cur = read_tbl(db_path, "recruit_class_basketball"),
                    base = read_tbl(baseline$path, "recruit_class_basketball"))
)

## Headline counts use activity-only rows. Coverage changes are logged
## separately so a pipeline run makes every exclusion auditable.
tot_adds <- 0L; tot_gone <- 0L
tot_cov_rows <- 0L; cov_schools <- character(0)
tot_con_rows <- 0L; con_schools <- character(0)
tot_hist_add_rows <- 0L; hist_add_schools <- character(0)
tot_hist_gone_rows <- 0L; hist_gone_schools <- character(0)
activity_windows <- character(0)
for (nm in names(sports)) {
  s <- sports[[nm]]
  if (is.null(s$cur) || is.null(s$base) ||
      nrow(s$cur) == 0 || nrow(s$base) == 0) next
  dd <- diff_sport(s$cur, s$base)
  tot_adds <- tot_adds + nrow(dd$adds)
  tot_gone <- tot_gone + nrow(dd$gone)
  tot_cov_rows <- tot_cov_rows + nrow(dd$coverage_school)
  cov_schools <- union(cov_schools, dd$new_schools)
  tot_con_rows <- tot_con_rows + nrow(dd$contraction_school)
  con_schools <- union(con_schools, dd$old_schools)
  tot_hist_add_rows <- tot_hist_add_rows + nrow(dd$coverage_history)
  hist_add_schools <- union(hist_add_schools,
                            dd$coverage_history$School)
  tot_hist_gone_rows <- tot_hist_gone_rows + nrow(dd$contraction_history)
  hist_gone_schools <- union(hist_gone_schools,
                             dd$contraction_history$School)
  activity_windows <- union(activity_windows,
                            paste0(dd$activity_floor, "-", dd$activity_ceiling))
}
cat("Headline:", tot_adds, "additions,", tot_gone, "removals", window_label,
    "\n")
if (tot_cov_rows > 0) {
  cat("Coverage expansion excluded:", tot_cov_rows, "historical rows across",
      length(cov_schools), "new programs.\n")
}
if (tot_con_rows > 0) {
  cat("Coverage contraction excluded:", tot_con_rows, "historical rows across",
      length(con_schools), "retired programs.\n")
}
if (tot_hist_add_rows > 0) {
  cat("Historical coverage repair excluded:", tot_hist_add_rows,
      "rows across", length(hist_add_schools),
      paste0("established programs (classes outside ",
             paste(sort(unique(activity_windows)), collapse = "/"), ").\n"))
}
if (tot_hist_gone_rows > 0) {
  cat("Historical coverage adjustment excluded:", tot_hist_gone_rows,
      "rows across", length(hist_gone_schools),
      paste0("established programs (classes outside ",
             paste(sort(unique(activity_windows)), collapse = "/"), ").\n"))
}

body <- vapply(names(sports), function(nm) {
  sport_html(nm, sports[[nm]]$cur, sports[[nm]]$base, window_label)
}, character(1))

page <- c(
"<!DOCTYPE html>",
"<html lang=\"en\">",
"<head>",
"  <meta charset=\"utf-8\">",
"  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
"  <title>This week across the Power 4 &mdash; Girth Index brief</title>",
paste0("  <meta name=\"description\" content=\"What changed across all 67 ",
       "Power-4 programs ", esc(window_label), ": new commits, portal ",
       "additions, and class movement, auto-written from the nightly ",
       "refresh.\">"),
"  <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">",
"  <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>",
paste0("  <link href=\"https://fonts.googleapis.com/css2?family=Rubik:wght@",
       "400;600;800&display=swap\" rel=\"stylesheet\">"),
"  <link rel=\"icon\" href=\"../assets/ua.png\">",
"  <style>",
"    :root {",
"      --navy: #0C234B; --navy2: #16386e; --cardinal: #AB0520;",
"      --gold: #FFD200; --ink: #1c2733; --muted: #6b7a89;",
"    }",
"    * { margin: 0; padding: 0; box-sizing: border-box; }",
"    body { font-family: 'Rubik', 'Helvetica Neue', sans-serif;",
"      color: var(--ink); background: #f4f7fb; line-height: 1.55; }",
"    .hero { background: linear-gradient(155deg, var(--navy) 0%,",
"      var(--navy2) 100%); color: white; padding: 40px 20px 34px 20px;",
"      text-align: center; }",
"    .hero h1 { font-size: clamp(24px, 5vw, 38px); font-weight: 800; }",
"    .hero h1 span { color: var(--gold); }",
"    .hero p.win { margin-top: 8px; font-size: 14px; opacity: 0.85; }",
"    .hero a { color: var(--gold); text-decoration: none; }",
"    main { max-width: 820px; margin: 0 auto; padding: 30px 18px 10px 18px; }",
"    h2 { color: var(--navy); font-size: 24px; font-weight: 800;",
"      margin: 26px 0 4px 0; border-bottom: 3px solid var(--cardinal);",
"      display: inline-block; padding-bottom: 2px; }",
"    h3 { color: var(--navy); font-size: 16px; margin: 20px 0 6px 0; }",
"    ul { margin: 6px 0 6px 22px; }",
"    li { font-size: 14px; margin-bottom: 6px; color: #37475a; }",
"    p.note { font-size: 13px; color: var(--muted); margin: 4px 0 8px 0; }",
"    p.empty { font-size: 13.5px; color: var(--muted); margin: 4px 0; }",
"    p.coverage { margin: 10px 0; padding: 10px 12px; border-left: 4px solid",
"      var(--gold); background: #fff8d9; color: #37475a; font-size: 13px; }",
"    table { border-collapse: collapse; margin: 8px 0; width: 100%;",
"      background: white; border-radius: 10px; overflow: hidden;",
"      box-shadow: 0 3px 12px rgba(12,35,75,0.07); font-size: 14px; }",
"    th { background: var(--navy); color: white; text-align: left;",
"      padding: 8px 12px; font-weight: 600; }",
"    td { padding: 7px 12px; border-top: 1px solid #e6ecf3; }",
"    footer { text-align: center; color: var(--muted); font-size: 12.5px;",
"      padding: 28px 16px 38px 16px; }",
"    footer a { color: var(--cardinal); font-weight: 600;",
"      text-decoration: none; }",
"  </style>",
"</head>",
"<body>",
"  <div class=\"hero\">",
"    <h1>This week across the <span>Power 4</span></h1>",
paste0("    <p class=\"win\">", tot_adds, " addition",
       ifelse(tot_adds == 1, "", "s"), " and ", tot_gone, " removal",
       ifelse(tot_gone == 1, "", "s"), " across football and basketball, ",
       esc(window_label), ". Auto-written from the nightly data refresh on ",
       fmt_date(Sys.Date()), ".</p>"),
if (nzchar(window_note)) {
  ## our own copy, already entity-safe -- esc() would mangle &mdash;
  paste0("    <p class=\"win\">", window_note, "</p>")
} else NULL,
"    <p class=\"win\"><a href=\"../\">&larr; back to the Girth Index</a></p>",
"  </div>",
"  <main>",
body,
"  </main>",
"  <footer>",
paste0("    Regenerated nightly by the data pipeline &mdash; sources: ",
       "<a href=\"https://247sports.com/\">247Sports</a> team pages. ",
       "Independent project; not affiliated with the SEC, Big Ten, ACC, ",
       "Big 12, or their member schools."),
"  </footer>",
"</body>",
"</html>")

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
con <- file(out_path, open = "w", encoding = "UTF-8")
writeLines(page, con)
close(con)
cat("Wrote", out_path, "\n")

## ---------------------------------------------------------------------------
## status.json -- the tiny freshness beacon docs/index.html progressively
## enhances from ("data updated {date}"). nightlyRefresh.R writes the SAME
## shape at S6.5; last writer wins harmlessly. Lives one level up from the
## brief (docs/status.json for the default out path) and is fully
## parameterized by --db/--out, so a test run writes a scratch beacon instead
## of clobbering the published one. Counts come straight from the current db.
## ---------------------------------------------------------------------------
fb_cur <- sports$Football$cur
bb_cur <- sports$Basketball$cur
n_rows <- function(d) if (is.null(d)) 0L else nrow(d)
max_yr <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NA_integer_)
  suppressWarnings(as.integer(max(d$Year, na.rm = TRUE)))
}
newest_class <- suppressWarnings(max(c(max_yr(fb_cur), max_yr(bb_cur)),
                                     na.rm = TRUE))
if (!is.finite(newest_class)) newest_class <- NA_integer_
## "updated" = when the DATA was last scraped, not when this brief regenerated
## -- a no-change brief rerun (or a manual regen) must never advance the
## public freshness date. Read the newest ScrapedAt across the tables that
## carry it (rosters always do; recruit tables gain it on their next scrape);
## fall back to today only if nothing is stamped.
updated_date <- local({
  conn <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  vals <- character(0)
  for (t in c("roster_football", "roster_basketball",
              "recruit_class_football", "recruit_class_basketball")) {
    v <- tryCatch(dbGetQuery(conn, paste0(
      "SELECT MAX(ScrapedAt) m FROM ", t))$m, error = function(e) NA_character_)
    if (length(v) == 1 && !is.na(v) && nzchar(v)) vals <- c(vals, v)
  }
  d <- suppressWarnings(as.Date(vals))
  d <- d[!is.na(d)]
  if (length(d) > 0) max(d) else Sys.Date()
})
status <- list(updated         = format(updated_date),
               football_rows   = n_rows(fb_cur),
               basketball_rows = n_rows(bb_cur),
               newest_class    = newest_class,
               brief           = "brief/")
status_path <- file.path(dirname(dirname(out_path)), "status.json")
tryCatch({
  jsonlite::write_json(status, status_path, auto_unbox = TRUE, pretty = TRUE)
  cat("Wrote", status_path, "\n")
}, error = function(e) {
  cat("status.json write skipped (", conditionMessage(e), ")\n", sep = "")
})
