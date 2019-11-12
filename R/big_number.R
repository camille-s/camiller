#' Abbreviate large numbers by order of magnitude
#'
#' This picks out the order of magnitude of each number in a vector and returns a formatted abbreviation, such as 1230 --> 1.2k. It handles abbreviations through the trillions; after that, it's suggested that you switch over to scientific notation. The convenience of this function over those from the `scales` package is that you can mix & match orders of magnitude.
#'
#' Its sibling `big_money` tacks on a currency symbol, placed after a negative sign if applicable.
#'
#' @param x A numeric vector
#' @param digits Number of digits (significant figures) to keep; defaults to 2
#' @param currency Symbol to use to denote currency; defaults `"$"`
#' @return A character vector
#' @examples
#' big_number(c(123, 12345, 1234567), digits = 3)
#' big_number(c(-12000, 15000, 16000, 77777))
#' big_money(c(-12000, 15000, 16000, 2e6))
#'
#' # take care with your significant digits choices
#' purrr::map_chr(1:3, ~big_number(987, digits = .))

#' @export
big_number <- function(x, digits = 2) {
  abbr <- c("", "k", "M", "B", "T")
  mags <- 10^((seq_along(abbr) - 1) * 3)
  x_s <- signif(x, digits = digits)
  idx <- findInterval(abs(x_s), mags)
  idx <- ifelse(idx == 0, 1, idx)
  suffix <- purrr::map_chr(idx, ~abbr[.])
  if (any(abs(x_s) >= max(mags) * 1e3)) {
    warning("Standard abbreviations only go up to one trillion. Larger numbers will just be shown in terms of trillions; consider scientific notation instead.", call. = FALSE)
  }
  main <- x_s / mags[idx]
  paste0(main, suffix)
}

#' @rdname big_number
#' @export
big_money <- function(x, digits = 2, currency = "$") {
  bn <- big_number(x, digits)
  patt <- paste0("\\1", currency)
  gsub("^(\\-?)", patt, bn)
}
