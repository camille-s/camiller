#' Collapse variable into subgroup positions
#' @param x Character vector
#' @param grps Named list of variable positions within vector `x`
#' @return Named list of variable values from given positions
#' @examples
#' ages <- c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years",
#'   "18 and 19 years", "20 years", "21 years", "22 to 24 years",
#'   "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")
#' age_grps <- list(
#'   under18 = 1:4,
#'   ages18_24 = 5:8,
#'   ages18_34 = 5:10
#' )
#' make_grps(ages, age_grps)
#' @export
make_grps <- function(x, grps) purrr::map(grps, ~unique(x)[.])
