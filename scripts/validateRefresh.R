## ===========================================================================
## validateRefresh.R -- the post-refresh sanity gate. Compares the LIVE db
## against the PRE-RUN baseline snapshot and fails loudly if the night's
## scrape mangled the data:
##
##   (a) recruit_class_* : total rows shrank no more than 5%, and no
##       (School, Year) group that had rows in the baseline lost more
##       than 40% of them. The only exception is an exact, source-reviewed,
##       active-cycle nonzero decommit listed in
##       scripts/refresh-validation-exceptions.csv; zero-row and historical
##       regressions are never exempted.
##   (b) roster_*        : per-RosterYear totals within +/-20% of baseline
##       for years present in BOTH dbs, counted among BASELINE schools only
##       (new schools/years are onboarding growth and pass), and at least
##       75% of onboarded schools in the newest live year
##   (c) team_seasons_football : row count >= baseline count
##   (d) plausibility    : among non-NA values, Weight within 130-420
##       (football) / 130-320 (basketball) and Ranking within 55-110;
##       fail if more than 2% of non-NA values fall outside
##   (d2) non-NA share   : roster/recruit Weight (and recruit Ranking)
##       non-NA share must not drop more than 15 percentage points below
##       baseline -- catches an NA flood that keeps row counts intact
##   (e) PRAGMA integrity_check == 'ok' on the live db
##
## NA-tolerant throughout -- portal transfers carry NA State/Location/geo
## by design and must never trip a check.
##
## Usage:
##   Rscript scripts/validateRefresh.R <baseline_db> [live_db=data/recruiting.db]
## Exit 0 = every check passed, 1 = at least one FAIL.
## ===========================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: Rscript scripts/validateRefresh.R <baseline_db>",
      "[live_db=data/recruiting.db]\n")
  quit(save = "no", status = 1)
}
base_db <- args[1]
live_db <- if (length(args) >= 2) args[2] else "data/recruiting.db"

if (!file.exists(base_db)) {
  cat("[FAIL] baseline db not found:", base_db, "\n")
  quit(save = "no", status = 1)
}
if (!file.exists(live_db)) {
  cat("[FAIL] live db not found:", live_db, "\n")
  quit(save = "no", status = 1)
}

## short-lived connection per query (matches the pipeline's locking rule)
q <- function(db, sql) {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(conn, sql)
}
has_table <- function(db, tbl) {
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  tbl %in% dbListTables(conn)
}
n_rows <- function(db, tbl) {
  if (!has_table(db, tbl)) return(0L)
  as.integer(q(db, paste0("SELECT COUNT(*) AS n FROM ", tbl))$n)
}
has_col <- function(db, tbl, col) {
  col %in% q(db, paste0("PRAGMA table_info(", tbl, ")"))$name
}
## ---------------------------------------------------------------------------
## Exact, source-reviewed active-cycle decommit exceptions
## ---------------------------------------------------------------------------
## A one-player class can legitimately move 2 -> 1 overnight. That should not
## cause a permanent rollback loop, but a broad threshold exception would make
## the safety gate blind to real scraper losses. The registry therefore binds
## a waiver to the exact table, school, year, before/after counts, removed and
## retained commit names, source-verification date, and a short expiry. It can
## never waive a zero-row regression or a historical class.
DECOMMIT_EXCEPTION_PATH <- Sys.getenv(
  "GIRTH_DECOMMIT_EXCEPTION_PATH",
  unset = file.path("scripts", "refresh-validation-exceptions.csv")
)

empty_decommit_exceptions <- function() {
  data.frame(
    tbl = character(), school = character(), year = integer(),
    baseline_rows = integer(), live_rows = integer(),
    removed_name = character(), retained_name = character(),
    expires_on = as.Date(character()), verified_on = as.Date(character()),
    source_url = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}

load_decommit_exceptions <- function(path = DECOMMIT_EXCEPTION_PATH) {
  if (!file.exists(path)) return(empty_decommit_exceptions())

  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                    check.names = FALSE),
    error = function(e) stop("could not read decommit exception registry: ",
                             conditionMessage(e), call. = FALSE)
  )
  required <- c("tbl", "school", "year", "baseline_rows", "live_rows",
                "removed_name", "retained_name", "expires_on", "verified_on",
                "source_url", "reason")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop("decommit exception registry missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- raw[, required, drop = FALSE]
  out$tbl <- trimws(tolower(as.character(out$tbl)))
  out$school <- trimws(tolower(as.character(out$school)))
  out$year <- suppressWarnings(as.integer(out$year))
  out$baseline_rows <- suppressWarnings(as.integer(out$baseline_rows))
  out$live_rows <- suppressWarnings(as.integer(out$live_rows))
  out$removed_name <- trimws(as.character(out$removed_name))
  out$retained_name <- trimws(as.character(out$retained_name))
  out$expires_on <- suppressWarnings(as.Date(out$expires_on))
  out$verified_on <- suppressWarnings(as.Date(out$verified_on))
  out$source_url <- trimws(as.character(out$source_url))
  out$reason <- trimws(as.character(out$reason))

  text_ok <- function(x) !is.na(x) & nzchar(x)
  if (any(!out$tbl %in% c("recruit_class_football",
                           "recruit_class_basketball")) ||
      any(!text_ok(out$school)) || any(!text_ok(out$removed_name)) ||
      any(!text_ok(out$retained_name)) || any(!text_ok(out$source_url)) ||
      any(!text_ok(out$reason)) || any(!is.finite(out$year)) ||
      any(!is.finite(out$baseline_rows)) || any(!is.finite(out$live_rows)) ||
      any(is.na(out$expires_on)) || any(is.na(out$verified_on))) {
    stop("decommit exception registry has an invalid required value", call. = FALSE)
  }
  if (any(out$baseline_rows != out$live_rows + 1L) ||
      any(out$live_rows < 1L)) {
    stop("decommit exceptions must be one-row nonzero losses",
         call. = FALSE)
  }
  if (any(out$expires_on < out$verified_on)) {
    stop("decommit exception expiry cannot precede source verification",
         call. = FALSE)
  }
  key <- paste(out$tbl, out$school, out$year, sep = "\r")
  if (anyDuplicated(key)) {
    stop("decommit exception registry has duplicate table/school/year rows",
         call. = FALSE)
  }
  out
}

active_class_year <- function(year, today = Sys.Date()) {
  current <- as.integer(format(as.Date(today), "%Y"))
  is.finite(year) && year >= current && year <= current + 1L
}

name_key <- function(x) {
  tolower(gsub("\\s+", " ", trimws(as.character(x))))
}

school_year_commits <- function(db, tbl, school, year) {
  if (!has_col(db, tbl, "Name") || !has_col(db, tbl, "Type")) {
    return(data.frame(Name = character(), Type = character(),
                      stringsAsFactors = FALSE))
  }
  conn <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(
    conn,
    paste0("SELECT Name, Type FROM ", tbl,
           " WHERE School = ? AND Year = ?"),
    params = list(school, as.integer(year))
  )
}

approved_nonzero_decommit <- function(exceptions, base_db, live_db, tbl,
                                      school, year, n_base, n_live) {
  if (!nrow(exceptions) || !active_class_year(year)) return(NULL)
  hit <- exceptions[
    exceptions$tbl == tbl & exceptions$school == tolower(school) &
      exceptions$year == as.integer(year) &
      exceptions$baseline_rows == as.integer(n_base) &
      exceptions$live_rows == as.integer(n_live) &
      exceptions$verified_on <= Sys.Date() &
      Sys.Date() <= exceptions$expires_on,
    , drop = FALSE]
  if (nrow(hit) != 1L || as.integer(n_live) < 1L) return(NULL)

  base_players <- school_year_commits(base_db, tbl, school, year)
  live_players <- school_year_commits(live_db, tbl, school, year)
  base_commits <- name_key(base_players$Name[base_players$Type == "Commit"])
  live_commits <- name_key(live_players$Name[live_players$Type == "Commit"])
  removed <- name_key(hit$removed_name[[1]])
  retained <- name_key(hit$retained_name[[1]])
  if (!(removed %in% base_commits) || removed %in% live_commits ||
      !(retained %in% base_commits) ||
      !(retained %in% live_commits)) return(NULL)
  hit
}

decommit_exceptions <- load_decommit_exceptions()
if (nrow(decommit_exceptions)) {
  cat("[INFO] loaded ", nrow(decommit_exceptions),
      " exact active-cycle decommit exception(s)\n", sep = "")
}

## Scale roster coverage with the shipped program universe. This is 51 of 67
## today and automatically moves if onboarding changes again.
onboarded_n <- tryCatch({
  cfg <- utils::read.csv(file.path("data", "team_config.csv"),
                         stringsAsFactors = FALSE)
  sum(toupper(as.character(cfg$onboarded)) == "TRUE", na.rm = TRUE)
}, error = function(e) 16L)
if (!is.finite(onboarded_n) || onboarded_n < 1L) onboarded_n <- 16L
ROSTER_TEAM_FLOOR <- as.integer(ceiling(0.75 * onboarded_n))

fails <- 0L
check <- function(label, ok, detail = "") {
  cat(sprintf("[%s] %s%s\n", if (isTRUE(ok)) "PASS" else "FAIL", label,
              if (nzchar(detail)) paste0(" -- ", detail) else ""))
  if (!isTRUE(ok)) fails <<- fails + 1L
  invisible(ok)
}

cat("Baseline:", base_db, "\nLive    :", live_db, "\n\n")

## ---------------------------------------------------------------------------
## (a) recruit tables: totals + per-(School, Year) retention
## ---------------------------------------------------------------------------
for (tbl in c("recruit_class_football", "recruit_class_basketball")) {
  if (!has_table(live_db, tbl)) {
    check(paste0(tbl, ": table present in live db"), FALSE)
    next
  }
  b_n <- n_rows(base_db, tbl)
  l_n <- n_rows(live_db, tbl)
  check(sprintf("%s: total rows shrank <= 5%% (%d -> %d)", tbl, b_n, l_n),
        l_n >= b_n * 0.95)

  if (b_n > 0) {
    bg <- q(base_db, paste0(
      "SELECT School, Year, COUNT(*) AS n_base FROM ", tbl,
      " GROUP BY School, Year"))
    lg <- q(live_db, paste0(
      "SELECT School, Year, COUNT(*) AS n_live FROM ", tbl,
      " GROUP BY School, Year"))
    m <- merge(bg, lg, by = c("School", "Year"), all.x = TRUE)
    m$n_live[is.na(m$n_live)] <- 0L
    bad <- m[m$n_base > 0 & m$n_live < 0.6 * m$n_base, , drop = FALSE]
    ## A source-reviewed exception can only clear the exact active-cycle,
    ## nonzero drop it names. It cannot waive a zero-row hole, an old class,
    ## a different player, or any total-table regression above.
    if (nrow(bad) > 0 && nrow(decommit_exceptions) > 0) {
      approved <- vector("list", nrow(bad))
      for (i in seq_len(nrow(bad))) {
        approved[[i]] <- approved_nonzero_decommit(
          decommit_exceptions, base_db, live_db, tbl,
          bad$School[i], bad$Year[i], bad$n_base[i], bad$n_live[i]
        )
      }
      allow <- vapply(approved, function(x) !is.null(x), logical(1))
      if (any(allow)) {
        for (i in which(allow)) {
          hit <- approved[[i]]
          cat(sprintf(
            "[INFO] %s: exact verified decommit allowed for %s %s (%d -> %d); verified %s, expires %s\n",
            tbl, bad$School[i], bad$Year[i], bad$n_base[i], bad$n_live[i],
            format(hit$verified_on[[1]]), format(hit$expires_on[[1]])))
          cat("       source:", hit$source_url[[1]], "\n")
        }
        bad <- bad[!allow, , drop = FALSE]
      }
    }
    detail <- if (nrow(bad) > 0) {
      paste0(nrow(bad), " group(s), e.g. ",
             paste(head(sprintf("%s %s (%d -> %d)", bad$School, bad$Year,
                                bad$n_base, bad$n_live), 3),
                   collapse = "; "))
    } else ""
    check(paste0(tbl, ": no unresolved (School, Year) group lost > 40% of its rows"),
          nrow(bad) == 0, detail)
  } else {
    check(paste0(tbl, ": no unresolved (School, Year) group lost > 40% of its rows"),
          TRUE, "baseline empty -- nothing to lose")
  }
}

## ---------------------------------------------------------------------------
## (b) roster tables: per-RosterYear totals within +/-20% for years present
##     in BOTH dbs (a year new in live is the annual append -- growth, pass),
##     and school coverage on the newest live year
## ---------------------------------------------------------------------------
for (tbl in c("roster_football", "roster_basketball")) {
  if (!has_table(live_db, tbl)) {
    check(paste0(tbl, ": table present in live db"), FALSE)
    next
  }
  year_aware <- has_col(live_db, tbl, "RosterYear") &&
    has_table(base_db, tbl) && has_col(base_db, tbl, "RosterYear")

  if (year_aware) {
    by <- q(base_db, paste0(
      "SELECT RosterYear, COUNT(*) AS n_base FROM ", tbl,
      " GROUP BY RosterYear"))
    ly <- q(live_db, paste0(
      "SELECT RosterYear, COUNT(*) AS n_live FROM ", tbl,
      " GROUP BY RosterYear"))
    ## POWER-4 NOTE: onboarding a conference adds rows to the SAME RosterYear
    ## (rosters are current-season only), so an all-schools total comparison
    ## falsely trips on legitimate expansion (SEC 2026: 1729 -> 3487). Scope
    ## the +/-20% band to schools PRESENT IN THE BASELINE: that still catches
    ## what the gate exists for -- a broken scrape wiping or duplicating the
    ## rows we already had -- while brand-new schools count as growth and are
    ## covered by the distinct-school floor below. No behavior change when the
    ## school set is unchanged (the nightly).
    base_schools <- q(base_db, paste0(
      "SELECT DISTINCT School FROM ", tbl))$School
    ly_shared <- if (length(base_schools) > 0) {
      in_list <- paste(sprintf("'%s'", gsub("'", "''", base_schools)),
                       collapse = ",")
      q(live_db, paste0(
        "SELECT RosterYear, COUNT(*) AS n_live FROM ", tbl,
        " WHERE School IN (", in_list, ") GROUP BY RosterYear"))
    } else ly
    shared <- merge(by, ly_shared, by = "RosterYear")
    new_schools <- setdiff(
      q(live_db, paste0("SELECT DISTINCT School FROM ", tbl))$School,
      base_schools)
    if (length(new_schools) > 0) {
      cat(sprintf(
        "[INFO] %s: %d school(s) new in live allowed as growth: %s\n",
        tbl, length(new_schools),
        paste(head(sort(new_schools), 20), collapse = ", ")))
    }
    if (nrow(shared) > 0) {
      bad <- shared[shared$n_live < 0.8 * shared$n_base |
                    shared$n_live > 1.2 * shared$n_base, , drop = FALSE]
      detail <- if (nrow(bad) > 0) {
        paste(sprintf("%s (%d -> %d)", bad$RosterYear, bad$n_base,
                      bad$n_live), collapse = "; ")
      } else ""
      check(sprintf(
        "%s: per-RosterYear totals within +/-20%% among baseline schools (%d shared year(s))",
        tbl, nrow(shared)), nrow(bad) == 0, detail)
    } else {
      check(sprintf("%s: per-RosterYear totals within +/-20%%", tbl), TRUE,
            "no RosterYear present in both dbs -- comparison skipped")
    }
    new_years <- setdiff(ly$RosterYear, by$RosterYear)
    if (length(new_years) > 0) {
      cat(sprintf("[INFO] %s: RosterYear(s) new in live allowed as growth: %s\n",
                  tbl, paste(new_years, collapse = ", ")))
    }
    yrs <- ly$RosterYear[!is.na(ly$RosterYear)]
    yr_new <- if (length(yrs) > 0) max(yrs) else NA
    sc <- if (is.na(yr_new)) 0L else {
      scy <- q(live_db, paste0(
        "SELECT RosterYear, COUNT(DISTINCT School) AS n FROM ", tbl,
        " GROUP BY RosterYear"))
      as.integer(scy$n[match(yr_new, scy$RosterYear)])
    }
    check(sprintf(
      "%s: >= %d of %d onboarded schools in newest live RosterYear %s (%d)",
      tbl, ROSTER_TEAM_FLOOR, onboarded_n, yr_new, sc),
      sc >= ROSTER_TEAM_FLOOR)
  } else {
    ## no RosterYear in one of the dbs -- fall back to the whole-table gate
    b_n <- n_rows(base_db, tbl)
    l_n <- n_rows(live_db, tbl)
    ok_total <- if (b_n == 0) TRUE else (l_n >= b_n * 0.8 && l_n <= b_n * 1.2)
    check(sprintf("%s: total rows within +/-20%% of baseline (%d -> %d)",
                  tbl, b_n, l_n), ok_total,
          if (b_n == 0) "baseline empty -- comparison skipped" else "")
    sc <- as.integer(q(live_db, paste0(
      "SELECT COUNT(DISTINCT School) AS n FROM ", tbl))$n)
    check(sprintf("%s: >= %d of %d onboarded schools present (%d)",
                  tbl, ROSTER_TEAM_FLOOR, onboarded_n, sc),
          sc >= ROSTER_TEAM_FLOOR)
  }
}

## ---------------------------------------------------------------------------
## (c) team_seasons_football: never shrinks
## ---------------------------------------------------------------------------
tbl <- "team_seasons_football"
if (has_table(base_db, tbl) && !has_table(live_db, tbl)) {
  check(paste0(tbl, ": table present in live db"), FALSE)
} else {
  b_n <- n_rows(base_db, tbl)
  l_n <- n_rows(live_db, tbl)
  check(sprintf("%s: row count >= baseline (%d -> %d)", tbl, b_n, l_n),
        l_n >= b_n,
        if (b_n == 0 && l_n == 0) "absent or empty in both" else "")
}

## ---------------------------------------------------------------------------
## (d) plausibility spot-check on the live recruit tables (non-NA values)
## ---------------------------------------------------------------------------
weight_limits <- list(recruit_class_football   = c(130L, 420L),
                      recruit_class_basketball = c(130L, 320L))
for (tbl in names(weight_limits)) {
  if (!has_table(live_db, tbl)) next   # presence already failed in (a)
  vals <- q(live_db, paste0("SELECT Weight, Ranking FROM ", tbl))

  lo <- weight_limits[[tbl]][1]
  hi <- weight_limits[[tbl]][2]
  w <- suppressWarnings(as.numeric(vals$Weight))
  w <- w[!is.na(w)]
  pct_w <- if (length(w) == 0) 0 else 100 * mean(w < lo | w > hi)
  check(sprintf("%s: Weight plausibility %d-%d (%.2f%% of %d non-NA outside)",
                tbl, lo, hi, pct_w, length(w)), pct_w <= 2)

  r <- suppressWarnings(as.numeric(vals$Ranking))
  r <- r[!is.na(r)]
  pct_r <- if (length(r) == 0) 0 else 100 * mean(r < 55 | r > 110)
  check(sprintf("%s: Ranking plausibility 55-110 (%.2f%% of %d non-NA outside)",
                tbl, pct_r, length(r)), pct_r <= 2)
}

## ---------------------------------------------------------------------------
## (d2) non-NA-share regression -- a 247 format change can NA-flood a column
##      while keeping row counts intact; fail if the live non-NA share drops
##      more than 15 percentage points below the baseline share
## ---------------------------------------------------------------------------
na_share_cols <- list(roster_football          = "Weight",
                      roster_basketball        = "Weight",
                      recruit_class_football   = c("Weight", "Ranking"),
                      recruit_class_basketball = c("Weight", "Ranking"))
non_na_share <- function(db, tbl, col) {
  v <- q(db, paste0("SELECT ", col, " AS v FROM ", tbl))$v
  if (length(v) == 0) return(NA_real_)
  mean(!is.na(suppressWarnings(as.numeric(v))))
}
for (tbl in names(na_share_cols)) {
  if (!has_table(base_db, tbl) || !has_table(live_db, tbl)) next
  for (col in na_share_cols[[tbl]]) {
    if (!has_col(base_db, tbl, col)) next   # nothing to regress against
    if (!has_col(live_db, tbl, col)) {
      check(sprintf("%s.%s: column present in live db", tbl, col), FALSE)
      next
    }
    b_s <- non_na_share(base_db, tbl, col)
    l_s <- non_na_share(live_db, tbl, col)
    if (is.na(b_s) || is.na(l_s)) {
      check(sprintf("%s.%s: non-NA share drop <= 15 points", tbl, col),
            TRUE, "empty table -- comparison skipped")
      next
    }
    check(sprintf(
      "%s.%s: non-NA share drop <= 15 points (%.1f%% -> %.1f%%)",
      tbl, col, 100 * b_s, 100 * l_s), l_s >= b_s - 0.15)
  }
}

## ---------------------------------------------------------------------------
## (e) SQLite integrity
## ---------------------------------------------------------------------------
ic <- tryCatch(q(live_db, "PRAGMA integrity_check")[[1]][1],
               error = function(e) paste("error:", conditionMessage(e)))
check("live db PRAGMA integrity_check == 'ok'", identical(ic, "ok"),
      if (identical(ic, "ok")) "" else substr(ic, 1, 120))

## ---------------------------------------------------------------------------
cat("\n", if (fails == 0L) "ALL CHECKS PASSED"
    else paste0(fails, " CHECK(S) FAILED"), "\n", sep = "")
quit(save = "no", status = if (fails > 0L) 1L else 0L)
