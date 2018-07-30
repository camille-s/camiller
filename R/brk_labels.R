#' Create pretty breaks labels
#'
#' This function maps over a vector of labels, such as those returned by `base::cut`. It works well in formatting `ggplot` scales, and can optionally pass parameters to an underlying call to `base::formatC`.
#' @param x A character vector.
#' @param format A string giving desired output format, or `NULL` (the default) for no formatting. Built-in shorthands are `"percent"`, `"dollar"`, and `"dollark"`, the last of which formats numbers like `$12.3k`. Alternatively, provide a character argument to be used by `base::formatC` and set `custom = TRUE`.
#' @param custom Logical, whether to use custom formatting, triggering a call to `formatC` with the arguments supplied to `format` and `...`. Defaults `FALSE`.
#' @param mult_by A number by which to multiply values in breaks. Defaults to 1, i.e. just the numbers input. Note that multiplication is carried out *before* rounding and formatting.
#' @param round_digits If not `NULL` (the default), the number of digits to round to. Note that this takes place after multiplying by `mult_by`.
#' @param sep A string by which to separate values in breaks.
#' @param ... Any additional arguments to pass on to `base::formatC` if `custom` is `TRUE`.
#' @return A character vector of formatted break labels.
#' @examples
#' percentage_brks <- c("[0.04,0.15]", "(0.15,0.25]", "(0.25,0.4]")
#' brk_labels(percentage_brks)
#' brk_labels(percentage_brks, format = "percent", mult_by = 100)
#'
#' scientific_brks <- c("[-15500,0]", "(0,20000]", "(20000,25000]")
#' brks_labels(scientific_brks, format = "e", custom = TRUE, digits = 2)
#' @export
brk_labels <- function(x, format = NULL, custom = FALSE, mult_by = 1, round_digits = NULL, sep = " to ", ...) {
  assertthat::assert_that(class(x) == "character", msg = "x should be a character vector.")
  purrr::map_chr(x, function(lab) {
    splits <- stringr::str_split(lab, ",") %>% purrr::flatten()
    x1 <- stringr::str_remove_all(splits[1], "[\\(\\[]") %>%
      as.numeric() %>%
      magrittr::multiply_by(mult_by)
    x2 <- stringr::str_remove_all(splits[2], "[\\)\\]]") %>%
      as.numeric() %>%
      magrittr::multiply_by(mult_by)

    if (!is.null(round_digits)) {
      x1 <- round(x1, digits = round_digits)
      x2 <- round(x2, digits = round_digits)
    }

    if (custom) {
      out_nums <- formatC(c(x1, x2), format = format, ...)
    } else if (is.null(format)) {
        out_nums <- c(x1, x2)
    } else {
        out_nums <- dplyr::case_when(
          format == "percent" ~ c(x1, paste0(x2, "%")),
          format == "dollar" ~ paste0("$", c(x1, x2)),
          format == "dollark" ~ sprintf("$%sk", c(x1, x2)),
          TRUE ~ paste0(c(x1, x2))
        )
    }

    paste(out_nums, collapse = sep)
  })
}


