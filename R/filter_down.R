#' @title Filter until/after first occurrence of a set of conditions
#' @description These two utility functions combine `dplyr::filter` with base
#' `cumsum`, filtering to include only rows before a condition occurs for the
#' first time (`filter_until`) or only rows after a condition has occurred for
#' the first time (`filter_after`). This is mostly
#' useful for pulling data out of spreadsheets or other formats that may have
#' been made more for presentation than for analysis--think unnecessary
#' headings, footnotes, or references.
#' @param .data A data frame.
#' @param ... A set of one or more logical conditions. This follows what is
#' allowed for `dplyr::filter`, though without the comma shorthand to combine
#' multiple conditions.
#' @param .include_first For `filter_after`, logical: if `FALSE` (the default),
#' the row that flags the condition will be excluded, returning only the rows
#' after the occurrence. If `TRUE`, the row that matches the condition is included.
#' If this is multiple rows, only the last is included. Using this will likely
#' leave you with unwanted data, such as headings you'll then want to convert
#' to column names.
#' @return A subset version of the original data frame.
#' @seealso dplyr::filter
#' @examples
#' messy_notes <- tibble::tribble(
#' ~x1,       ~x2,        ~x3,
#' "A",       "dog",      0,
#' "B",       "cat",      1,
#' "Source:", "xyz.com",  NA,
#' "Date:",   "Jan",      2021
#' )
#' messy_summary <- tibble::tribble(
#' ~x1,        ~x2,
#' "A",        1,
#' "B",        5,
#' "C",        9,
#' "Weights",  NA,
#' "A",        0.2,
#' "B",        0.5
#' )
#'
#' # Use filter_until to get the data until the source information starts--there
#' # are several conditions that will work
#' filter_until(messy_notes, is.na(x3))
#' filter_until(messy_notes, grepl("\\:", x1))
#'
#' # Use filter_until to get the data up until the weights table, and
#' # filter_after to get the weights table.
#' filter_until(messy_summary, x1 == "Weights" & is.na(x2))
#' filter_after(messy_summary, x1 == "Weights" & is.na(x2))
#'
#' # In some cases, it would be useful to hold onto those headings:
#' filter_after(messy_summary, x1 == "Weights" & is.na(x2), .include_first = TRUE)
#' @export
#' @rdname filter_down
#'
#' @import rlang
#' @import dplyr
filter_until <- function(.data, ...) {
  q <- rlang::quos(...)
  tryCatch(
    error = function(e) stop("An error occurred--most likely, your conditions are split by commas.\nUnlike dplyr::filter, this function requires '&' or '|' in between conditions, not commas."),
    dplyr::mutate(.data, dummy = cumsum(!!!q) == 0)
  )
  dplyr::filter(.data, cumsum(!!!q) == 0)
}

#' @export
#' @rdname filter_down
filter_after <- function(.data, ..., .include_first = FALSE) {
  q <- rlang::quos(...)
  tryCatch(
    error = function(e) stop("An error occurred--most likely, your conditions are split by commas.\nUnlike dplyr::filter, this function requires '&' or '|' in between conditions, not commas."),
    dplyr::mutate(.data, dummy = cumsum(!!!q) == 0)
  )
  out <- dplyr::filter(.data, cumsum(!!!q) > 0)
  if (.include_first) {
    out
  } else {
    out[-1,]
  }
}
