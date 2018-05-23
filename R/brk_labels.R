#' Create pretty breaks labels
#' @param x A character vector
#' @param format A string ("percent", "dollar", "dollark") giving desired output format, or `NULL`` for no formatting
#' @param mult_by A number by which to multiply values in breaks
#' @param sep A string by which to separate values in breaks
#' @return A character vector of formatted break labels
#' @examples
#' brks <- c("[0.04,0.15]", "(0.15,0.25]", "(0.25,0.4]", "(0.4,0.55]", "(0.55,0.6]")
#' brk_labels(brks)
#' brk_labels(brks, format = "percent", mult_by = 100)
#' @export
brk_labels <- function(x, format = NULL, mult_by = 1, sep = " to ") {
  purrr::map_chr(x, function(lab) {
    splits <- stringr::str_split(lab, ",")[[1]]
    x1 <- stringr::str_remove_all(splits[1], "[\\(\\[]") %>%
      as.numeric() %>%
      magrittr::multiply_by(mult_by)
    x2 <- stringr::str_remove_all(splits[2], "[\\)\\]]") %>%
      as.numeric() %>%
      magrittr::multiply_by(mult_by)
    if (is.null(format)) {
      out_nums <- c(x1, x2)
    } else {
      if (format == "percent") out_nums <- c(x1, paste0(x2, "%"))
      if (format == "dollar") out_nums <- paste0("$", c(x1, x2))
      if (format == "dollark") out_nums <- sprintf("$%sk", c(x1, x2))
    }
    paste(out_nums, collapse = sep)
  })
}
