#' Print unique values from a data frame column, then keep it moving
#'
#' `show_uniq` gets the unique values of a column and their position within that vector, prints them neatly to the console, then returns the original data frame unchanged. It's just a convenience for showing the values in a column without breaking your workflow or train of thought, and is useful for identifying groups for `add_grps`.
#'
#' @param .data A data frame
#' @param col Bare column name of interest
#' @return Original unchanged `.data`
#' @examples
#'
#' @export
show_uniq <- function(.data, col) {
  var <- enquo(col)
  values <- .data %>%
    dplyr::pull(!!var) %>%
    unique() %>%
    purrr::imap_chr(~paste(.y, .x, sep = ": "))
  cat(prettycols(values, requireNamespace("crayon", quietly = TRUE)), fill = TRUE)
  cat("\n")
  return(.data)
}

prettycols <- function(x, cray) {
  w <- floor(options("width")$width * 0.98)
  l <- max(nchar(x))
  cols <- floor(w / l)
  padded <- stringr::str_pad(x, width = l, side = "right")

  if (cray) {
    pink <- crayon::make_style("orchid")
    padded <- pink(padded)
  }
  spl <- rep(1:ceiling(length(x) / cols), each = cols)
  out <- suppressWarnings(split(padded, spl))
  purrr::map_chr(out, paste, collapse = " ")
}
