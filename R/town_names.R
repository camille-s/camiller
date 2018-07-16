#' Clean up town names as returned from ACS
#' @param df A data frame
#' @param name_col Bare column name of town names
#' @return A tibble/data frame with cleaned names and "not defined" towns removed
#' @export
town_names <- function(df, name_col) {
  name_var <- rlang::enquo(name_col)
  df %>%
    dplyr::mutate(!!rlang::quo_name(name_var) := stringr::str_remove(!!name_var, " town(,.)*")) %>%
    dplyr::filter(!stringr::str_detect(!!name_var, "not defined"))
}
