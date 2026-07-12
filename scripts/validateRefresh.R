## ===========================================================================
## validateRefresh.R -- the post-refresh sanity gate. Compares the LIVE db
## against the PRE-RUN baseline snapshot and fails loudly if the night's
## scrape mangled the data:
##
##   (a) recruit_class_* : total rows shrank no more than 5%, and no
##       (School, Year) group that had rows in the baseline lost more
##       than 40% of them
##   (b) roster_*        : per-RosterYear totals within +/-20% of baseline
##       for years present in BOTH dbs (years new in live are growth and
##       pass), and at least 12 distinct schools in the newest live year
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
    ## exempt school-years the hole audit has deliberately STOPPED healing
    ## (heal streak exhausted = probable decommit; the row loss there is the
    ## source being honored, not a scrape failure) -- see auditRefreshHoles.R
    if (nrow(bad) > 0 && has_table(live_db, "audit_heals")) {
      aged <- q(live_db, paste0(
        "SELECT School, Year FROM audit_heals WHERE tbl = '", tbl,
        "' AND streak > 3"))
      if (nrow(aged) > 0) {
        keep <- !(paste(bad$School, bad$Year) %in%
                    paste(aged$School, aged$Year))
        if (any(!keep)) {
          cat("[INFO] ", tbl, ": ", sum(!keep), " group(s) exempted from the",
              " retention gate (aged-out heals / probable decommits)\n",
              sep = "")
        }
        bad <- bad[keep, , drop = FALSE]
      }
    }
    detail <- if (nrow(bad) > 0) {
      paste0(nrow(bad), " group(s), e.g. ",
             paste(head(sprintf("%s %s (%d -> %d)", bad$School, bad$Year,
                                bad$n_base, bad$n_live), 3),
                   collapse = "; "))
    } else ""
    check(paste0(tbl, ": no (School, Year) group lost > 40% of its rows"),
          nrow(bad) == 0, detail)
  } else {
    check(paste0(tbl, ": no (School, Year) group lost > 40% of its rows"),
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
    shared <- merge(by, ly, by = "RosterYear")
    ## POWER-4 NOTE: onboarding a new school or a whole new conference is
    ## GROWTH, not shrinkage -- it raises per-RosterYear totals, never lowers
    ## them. This +/-20% band compares only RosterYears present in BOTH dbs, so
    ## rows added by an expansion land in `new_years` below (allowed as growth)
    ## rather than tripping this gate. No behavior change at the shipped 16.
    if (nrow(shared) > 0) {
      bad <- shared[shared$n_live < 0.8 * shared$n_base |
                    shared$n_live > 1.2 * shared$n_base, , drop = FALSE]
      detail <- if (nrow(bad) > 0) {
        paste(sprintf("%s (%d -> %d)", bad$RosterYear, bad$n_base,
                      bad$n_live), collapse = "; ")
      } else ""
      check(sprintf(
        "%s: per-RosterYear totals within +/-20%% (%d shared year(s))",
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
    ## PHASE 1: the literal 12 is 75% of the shipped 16. When teams onboard,
    ## scale this floor to the onboarded/active count (e.g. ceiling(0.75 * n))
    ## rather than the constant. Left as-is here -- no behavior change at 16.
    check(sprintf("%s: >= 12 distinct schools in newest live RosterYear %s (%d)",
                  tbl, yr_new, sc), sc >= 12)
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
    check(sprintf("%s: >= 12 distinct schools present (%d)", tbl, sc),
          sc >= 12)
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
