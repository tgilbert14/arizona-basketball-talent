## ---------------------------------------------------------------------------
## DASHBOARD DATA STATUS
## A dependency-light contract for the Home status rail. Source capture and
## pipeline bookkeeping are deliberately separate: an approved backfill can
## change the bundled database without passing through the nightly ledger.
## ---------------------------------------------------------------------------

.dashboard_date <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(NULL)
  raw <- trimws(as.character(x[[1]]))
  if (!nzchar(raw)) return(NULL)
  out <- tryCatch(as.Date(substr(raw, 1, 10)), error = function(e) NA)
  if (length(out) != 1L || is.na(out)) NULL else out
}

.dashboard_scalar <- function(x, default = NA_integer_) {
  if (is.null(x) || !length(x)) return(default)
  value <- suppressWarnings(as.integer(x[[1]]))
  if (is.na(value)) default else value
}


.dashboard_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  value <- trimws(as.character(x[[1]]))
  if (!nzchar(value)) default else value
}

## The bundled SQLite ledger is authoritative for source rows, but a failed
## refresh deliberately does not ship that mutable database. This tiny static
## sidecar lets the deployed Home rail report the newer pipeline outcome while
## retaining DB-derived capture dates and coverage counts.
dashboard_pipeline_sidecar <- function(path = file.path("www", "pipeline-status.json")) {
  empty <- list(checked_date = NULL, status = NULL, note = "")
  if (is.null(path) || !length(path) || !file.exists(path)) return(empty)

  raw <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(raw)) return(empty)

  status <- .dashboard_text(raw$pipeline_status, default = "")
  list(
    checked_date = .dashboard_date(raw$pipeline_checked),
    status = if (nzchar(status)) tolower(status) else NULL,
    note = .dashboard_text(raw$pipeline_message, default = "")
  )
}
.dashboard_table_snapshot <- function(conn, table, team_col = "School",
                                      stamp_col = "ScrapedAt",
                                      year_col = NULL) {
  empty <- list(date = NULL, teams = 0L, year = NA_integer_, rows = 0L)
  if (is.null(conn) || !DBI::dbIsValid(conn) ||
      !DBI::dbExistsTable(conn, table)) return(empty)

  fields <- DBI::dbListFields(conn, table)
  if (!all(c(team_col, stamp_col) %in% fields)) return(empty)
  qi <- function(x) as.character(DBI::dbQuoteIdentifier(conn, x))
  qt <- as.character(DBI::dbQuoteIdentifier(conn, table))

  if (!is.null(year_col) && year_col %in% fields) {
    year_sql <- paste0("MAX(", qi(year_col), ")")
    query <- paste0(
      "SELECT MAX(", qi(stamp_col), ") AS stamp, ",
      "COUNT(*) AS rows, ", year_sql, " AS active_year, ",
      "COUNT(DISTINCT CASE WHEN ", qi(year_col), " = (SELECT ",
      year_sql, " FROM ", qt, ") THEN ", qi(team_col), " END) AS teams ",
      "FROM ", qt)
  } else {
    query <- paste0(
      "SELECT MAX(", qi(stamp_col), ") AS stamp, COUNT(*) AS rows, ",
      "NULL AS active_year, COUNT(DISTINCT ", qi(team_col), ") AS teams ",
      "FROM ", qt)
  }

  row <- tryCatch(DBI::dbGetQuery(conn, query),
                  error = function(e) data.frame())
  if (!nrow(row)) return(empty)
  list(
    date = .dashboard_date(row$stamp),
    teams = .dashboard_scalar(row$teams, 0L),
    year = .dashboard_scalar(row$active_year, NA_integer_),
    rows = .dashboard_scalar(row$rows, 0L)
  )
}

dashboard_source_meta <- function(conn) {
  recruit_fb <- .dashboard_table_snapshot(conn, "recruit_class_football")
  recruit_bb <- .dashboard_table_snapshot(conn, "recruit_class_basketball")
  roster_fb <- .dashboard_table_snapshot(
    conn, "roster_football", year_col = "RosterYear")
  roster_bb <- .dashboard_table_snapshot(
    conn, "roster_basketball", year_col = "RosterYear")

  outcomes <- list(year = NA_integer_, teams = 0L, rows = 0L)
  if (!is.null(conn) && DBI::dbIsValid(conn) &&
      DBI::dbExistsTable(conn, "team_seasons_football")) {
    fields <- DBI::dbListFields(conn, "team_seasons_football")
    if (all(c("slug", "year") %in% fields)) {
      row <- tryCatch(DBI::dbGetQuery(conn, paste(
        "SELECT MAX(year) AS active_year, COUNT(*) AS rows,",
        "COUNT(DISTINCT CASE WHEN year = (SELECT MAX(year)",
        "FROM team_seasons_football) THEN slug END) AS teams",
        "FROM team_seasons_football")), error = function(e) data.frame())
      if (nrow(row)) {
        outcomes <- list(
          year = .dashboard_scalar(row$active_year, NA_integer_),
          teams = .dashboard_scalar(row$teams, 0L),
          rows = .dashboard_scalar(row$rows, 0L))
      }
    }
  }

  source_dates <- Filter(Negate(is.null), list(
    recruit_fb$date, recruit_bb$date, roster_fb$date, roster_bb$date))
  capture_date <- if (length(source_dates)) {
    max(as.Date(unlist(source_dates), origin = "1970-01-01"))
  } else {
    NULL
  }

  list(
    capture_date = capture_date,
    recruiting = list(football = recruit_fb, basketball = recruit_bb),
    rosters = list(football = roster_fb, basketball = roster_bb),
    outcomes = outcomes
  )
}

.dashboard_refresh_row <- function(conn, where = NULL) {
  if (is.null(conn) || !DBI::dbIsValid(conn) ||
      !DBI::dbExistsTable(conn, "refresh_log")) return(NULL)
  fields <- DBI::dbListFields(conn, "refresh_log")
  if (!all(c("finished_at", "status") %in% fields)) return(NULL)

  clause <- if (is.null(where) || !nzchar(where)) "" else paste("WHERE", where)
  notes <- if ("notes" %in% fields) "notes" else "'' AS notes"
  query <- paste(
    paste0("SELECT finished_at, status, ", notes, " FROM refresh_log"),
    clause,
    "ORDER BY datetime(finished_at) DESC, rowid DESC LIMIT 1"
  )
  out <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
  if (!nrow(out)) NULL else out[1, , drop = FALSE]
}

dashboard_refresh_meta <- function(conn,
                                   status_path = file.path("www", "pipeline-status.json")) {
  source <- dashboard_source_meta(conn)
  sidecar <- dashboard_pipeline_sidecar(status_path)
  latest <- .dashboard_refresh_row(conn)
  updated <- .dashboard_refresh_row(
    conn, "status IN ('ok', 'degraded') AND finished_at IS NOT NULL")

  checked_date <- if (is.null(latest)) NULL else
    .dashboard_date(latest$finished_at)
  updated_date <- if (is.null(updated)) NULL else
    .dashboard_date(updated$finished_at)
  status <- if (is.null(latest) || !"status" %in% names(latest)) {
    "unknown"
  } else {
    tolower(trimws(as.character(latest$status[[1]])))
  }
  note <- if (is.null(latest) || !"notes" %in% names(latest) ||
              is.na(latest$notes[[1]])) "" else
    trimws(as.character(latest$notes[[1]]))

  ## A sidecar only overrides the ledger when it represents an equally recent
  ## or newer check. This prevents an old static file from masking a fresher
  ## deployed database, while allowing a rollback/failure to be visible
  ## without committing mutable SQLite rows.
  sidecar_is_current <- !is.null(sidecar$checked_date) &&
    !is.null(sidecar$status) &&
    (is.null(checked_date) || sidecar$checked_date >= as.Date(checked_date[[1]]))
  if (sidecar_is_current) {
    checked_date <- sidecar$checked_date
    status <- sidecar$status
    note <- sidecar$note
  }

  list(
    checked_date = checked_date,
    updated_date = updated_date,
    capture_date = source$capture_date,
    status = if (nzchar(status)) status else "unknown",
    note = note,
    status_from_sidecar = isTRUE(sidecar_is_current),
    sources = source
  )
}

dashboard_freshness_info <- function(meta, today = Sys.Date()) {
  captured <- if (is.list(meta)) meta$capture_date else NULL
  checked <- if (is.list(meta)) meta$checked_date else NULL
  updated <- if (is.list(meta)) meta$updated_date else NULL
  status <- if (is.list(meta) && length(meta$status)) {
    tolower(as.character(meta$status[[1]]))
  } else {
    "unknown"
  }
  today <- as.Date(today)

  reference <- if (!is.null(captured) && length(captured) &&
                   !is.na(captured[[1]])) captured else checked
  if (is.null(reference) || !length(reference) || is.na(reference[[1]])) {
    return(list(
      state = "unknown", color = "#7B8798", value = "Unknown",
      detail = "source freshness unavailable", age_days = NA_integer_,
      aria = "Data source freshness is unavailable."
    ))
  }

  reference <- as.Date(reference[[1]])
  age <- max(0L, as.integer(today - reference))
  state <- if (age > 3L) {
    "stale"
  } else if (age > 1L) {
    "warning"
  } else {
    "fresh"
  }
  color <- switch(state,
                  fresh = "#1F7A4D",
                  warning = "#D97706",
                  stale = "#AB0520",
                  "#7B8798")
  value <- sub(" 0", " ", format(reference, "%b %d"), fixed = TRUE)
  detail <- if (!is.null(captured)) {
    "latest source capture"
  } else if (age == 0L) {
    "pipeline checked today"
  } else if (age == 1L) {
    "pipeline checked yesterday"
  } else {
    paste("pipeline checked", age, "days ago")
  }

  checked_long <- if (is.null(checked) || !length(checked) ||
                      is.na(checked[[1]])) {
    "unknown"
  } else {
    format(as.Date(checked[[1]]), "%B %d, %Y")
  }
  capture_long <- if (is.null(captured) || !length(captured) ||
                      is.na(captured[[1]])) {
    "unknown"
  } else {
    format(as.Date(captured[[1]]), "%B %d, %Y")
  }
  updated_long <- if (is.null(updated) || !length(updated) ||
                      is.na(updated[[1]])) {
    "unknown"
  } else {
    format(as.Date(updated[[1]]), "%B %d, %Y")
  }
  aria <- paste0(
    "Latest source capture ", capture_long, "; pipeline checked ",
    checked_long, "; latest check status ", status,
    "; pipeline last updated ", updated_long, ".")

  list(state = state, color = color, value = value, detail = detail,
       age_days = age, aria = aria)
}

dashboard_pipeline_info <- function(meta, today = Sys.Date()) {
  checked <- if (is.list(meta)) meta$checked_date else NULL
  captured <- if (is.list(meta)) meta$capture_date else NULL
  status <- if (is.list(meta) && length(meta$status)) {
    tolower(as.character(meta$status[[1]]))
  } else {
    "unknown"
  }
  critical <- status %in% c("failed", "failure", "error")
  degraded <- identical(status, "degraded")
  note <- if (is.list(meta) && isTRUE(meta$status_from_sidecar)) {
    .dashboard_text(meta$note, default = "")
  } else ""

  if (is.null(checked) || !length(checked) || is.na(checked[[1]])) {
    return(list(
      state = "unknown", color = "#7B8798",
      label = "Pipeline check unavailable",
      detail = "Source captures remain visible above."
    ))
  }

  checked <- as.Date(checked[[1]])
  lagging <- !is.null(captured) && length(captured) &&
    !is.na(captured[[1]]) && as.Date(captured[[1]]) > checked
  age <- max(0L, as.integer(as.Date(today) - checked))
  state <- if (critical || age > 3L) {
    "stale"
  } else if (degraded || lagging || age > 1L) {
    "warning"
  } else {
    "fresh"
  }
  color <- switch(state,
                  fresh = "#1F7A4D",
                  warning = "#D97706",
                  stale = "#AB0520",
                  "#7B8798")
  checked_lab <- sub(" 0", " ", format(checked, "%b %d"), fixed = TRUE)
  label <- if (critical) {
    paste("Last pipeline check failed", checked_lab)
  } else if (degraded) {
    paste("Partial pipeline check", checked_lab)
  } else if (lagging) {
    paste("Update record trails captured rows", checked_lab)
  } else if (identical(status, "noop")) {
    paste("Checked", checked_lab, "- no source changes")
  } else {
    paste("Pipeline checked", checked_lab)
  }
  capture_lab <- if (is.null(captured) || !length(captured) ||
                     is.na(captured[[1]])) {
    "the last published source snapshot"
  } else {
    paste0("the ", format(as.Date(captured[[1]]), "%b %d, %Y"),
           " source snapshot")
  }
  detail <- if (critical && nzchar(note)) {
    paste0(note, " This dashboard remains on ", capture_lab, ".")
  } else if (critical) {
    paste0("The refresh was rolled back after its validation gate failed; ",
           "this dashboard remains on ", capture_lab, ".")
  } else if (lagging) {
    "The dashboard is using the newer source capture shown above."
  } else {
    "The dataset is a worker-start snapshot refreshed by the release pipeline."
  }
  list(state = state, color = color, label = label, detail = detail)
}
