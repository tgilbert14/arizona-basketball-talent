## Focused regression contract for conference boards with an outside-league rival.
## Run: Rscript scripts/validateBoardComparators.R

source(here::here("R", "coach_eras.R"))
source(here::here("R", "functions.R"))
source(here::here("R", "dashboard_status.R"))
source(here::here("R", "team_config.R"))
source(here::here("R", "girth_functions.R"))
source(here::here("R", "girth_plots.R"))

app_lines <- readLines(here::here("app.R"), warn = FALSE)
twin_start <- grep("^twin_bar_color <-", app_lines)[1]
twin_end <- grep("^conf_twin_html <-", app_lines)[1] - 1L
eval(parse(text = paste(app_lines[twin_start:twin_end],
                        collapse = "\n")))

conn <- DBI::dbConnect(RSQLite::SQLite(), here::here("data", "recruiting.db"))
on.exit(DBI::dbDisconnect(conn), add = TRUE)
fb <- safe_query(conn, "SELECT * FROM recruit_class_football") %>%
  prep_size_data("football")
roster <- safe_query(conn, "SELECT * FROM roster_football")
roster_year <- suppressWarnings(as.integer(roster$RosterYear))
active_roster_year <- max(roster_year[is.finite(roster_year)])
roster <- roster[is.finite(roster_year) & roster_year == active_roster_year, ]

main <- "arizona"
external <- "georgia"
peer <- "arizona-state"

conference_rows <- function(board, value_col) {
  board %>%
    dplyr::filter(!external_reference) %>%
    dplyr::arrange(School) %>%
    dplyr::select(School, dplyr::all_of(value_col), n)
}

expect_same_field <- function(base, compared, value_col) {
  stopifnot(isTRUE(all.equal(
    conference_rows(base, value_col),
    conference_rows(compared, value_col),
    check.attributes = FALSE
  )))
  stopifnot(isTRUE(all.equal(attr(base, "conf_avg"),
                             attr(compared, "conf_avg"))))
}

expect_external_row <- function(board) {
  ext <- board %>% dplyr::filter(School == external)
  stopifnot(nrow(ext) == 1L, isTRUE(ext$external_reference[[1]]),
            identical(as.character(ext$role[[1]]), "external"),
            isTRUE(attr(board, "external_reference")),
            isTRUE(attr(board, "external_requested")),
            grepl("does not affect", attr(board, "external_note"), fixed = TRUE))
}

expect_shape_cue <- function(plot) {
  stopifnot(any(vapply(plot$layers, function(layer) {
    "shape" %in% names(layer$mapping)
  }, logical(1))))
}

## Class Retention: Georgia appends once; the Big 12 field and weighted
## conference average remain byte-for-byte equivalent to the no-rival board.
commits <- fb %>% dplyr::filter(Type == "Commit")
ret_base <- retention_board_data(commits, roster, main)
ret_ext <- retention_board_data(commits, roster, main, external)
expect_external_row(ret_ext)
expect_same_field(ret_base, ret_ext, "retention")
stopifnot(is.na(attr(ret_ext, "match_note")))
expect_shape_cue(plot_class_retention(commits, roster, main, external))
ret_twin <- twin_table_html(ret_ext, "retention comparator regression")
stopifnot(grepl("N/R", ret_twin, fixed = TRUE),
          grepl("Georgia (external reference)", ret_twin, fixed = TRUE),
          grepl("class=\"twin-external\"", ret_twin, fixed = TRUE))

ret_peer <- retention_board_data(commits, roster, main, peer)
stopifnot(!any(ret_peer$external_reference),
          identical(ret_peer$role[ret_peer$School == peer], "compare"),
          !isTRUE(attr(ret_peer, "external_requested")))
expect_same_field(ret_base, ret_peer, "retention")

## Missing Georgia roster data must not create a synthetic 0% row.
ret_missing <- retention_board_data(
  commits, dplyr::filter(roster, School != external), main, external)
stopifnot(!external %in% ret_missing$School,
          isTRUE(attr(ret_missing, "external_requested")),
          !isTRUE(attr(ret_missing, "external_reference")),
          grepl("no qualifying", attr(ret_missing, "external_note"),
                ignore.case = TRUE),
          grepl("not plotted", attr(ret_missing, "external_note"),
                ignore.case = TRUE))
expect_same_field(ret_base, ret_missing, "retention")

## Weight Room: use the four enrolled classes measured by retention. The
## name-match scope receipt must survive both gain and loss board assembly.
classes <- attr(ret_base, "cls_years")
wr <- weight_room_data(
  fb %>% dplyr::filter(Type == "Commit", Year %in% classes), roster)
stopifnot(nrow(wr) > 0, !is.null(attr(wr, "match_note")))

wr_base <- wr_board_data(wr, main, "football", direction = "gain")
wr_ext <- wr_board_data(wr, main, "football", external, direction = "gain")
expect_external_row(wr_ext)
expect_same_field(wr_base, wr_ext, "AvgGain")
stopifnot(identical(attr(wr_base, "match_note"), attr(wr_ext, "match_note")))
expect_shape_cue(plot_weight_room_board(
  wr, main, "football", external, direction = "gain"))
wr_twin <- twin_table_html(wr_ext, "weight-room comparator regression")
stopifnot(grepl("N/R", wr_twin, fixed = TRUE),
          grepl("Georgia (external reference)", wr_twin, fixed = TRUE),
          grepl("class=\"twin-external\"", wr_twin, fixed = TRUE))

wr_peer <- wr_board_data(wr, main, "football", peer, direction = "gain")
stopifnot(!any(wr_peer$external_reference),
          identical(wr_peer$role[wr_peer$School == peer], "compare"))
expect_same_field(wr_base, wr_peer, "AvgGain")

## Loss direction preserves the conference model. Georgia is either a valid
## unranked slimmer reference or a truthful missing reference, depending on
## this source snapshot.
wr_loss_base <- wr_board_data(wr, main, "football", direction = "loss")
wr_loss_ext <- wr_board_data(wr, main, "football", external, direction = "loss")
expect_same_field(wr_loss_base, wr_loss_ext, "AvgGain")
stopifnot(identical(attr(wr_loss_base, "match_note"),
                    attr(wr_loss_ext, "match_note")))
if (any(wr$School == external & wr$WeightGain < 0, na.rm = TRUE)) {
  expect_external_row(wr_loss_ext)
} else {
  stopifnot(!external %in% wr_loss_ext$School,
            isTRUE(attr(wr_loss_ext, "external_requested")),
            !isTRUE(attr(wr_loss_ext, "external_reference")),
            grepl("no qualifying", attr(wr_loss_ext, "external_note"),
                  ignore.case = TRUE))
}

## Removing Georgia from the matched pool is the explicit missing-gain case.
wr_missing <- wr_board_data(
  dplyr::filter(wr, School != external), main, "football", external,
  direction = "gain")
stopifnot(!external %in% wr_missing$School,
          isTRUE(attr(wr_missing, "external_requested")),
          !isTRUE(attr(wr_missing, "external_reference")),
          grepl("no qualifying", attr(wr_missing, "external_note"),
                ignore.case = TRUE),
          grepl("not plotted", attr(wr_missing, "external_note"),
                ignore.case = TRUE))
expect_same_field(wr_base, wr_missing, "AvgGain")

cat("Board comparator validation passed.\n")
