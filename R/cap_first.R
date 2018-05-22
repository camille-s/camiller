#' Capitalize first letter in string
#' @param x A character vector
#'
#' @return A character vector with first character of each capitalized
#' @examples
#' cap_first("camille")
#' cap_first(c("camille", "new haven"))
#' @export
cap_first <- function(x) {
  paste0(
    stringr::str_sub(x, 1, 1) %>% stringr::str_to_upper(),
    stringr::str_sub(x, 2, -1)
  )
}
