#' Arrange estimates, MOEs, etc into wide format
#' @param .data A data frame
#' @param ... Bare column names of types of measures
#' @param group Bare column name of group or category variable for spreading into wide data
#' @return A tibble/data frame in wide format
#' @examples
#' edu %>%
#'   make_wide(estimate, moe, group = variable)
#' @export
make_wide <- function(.data, ..., group = group) {
  gather_cols <- rlang::quos(...)
  grp_var <- rlang::enquo(group)
  gather_names <- tidyselect::vars_select(names(.data), !!!gather_cols)

  .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := as.character(!!grp_var)) %>%
    tidyr::gather(key = type, value = value, !!!gather_cols) %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := !!grp_var %>% forcats::as_factor() %>% forcats::fct_rev()) %>%
    dplyr::mutate(type = forcats::as_factor(type) %>% forcats::fct_relevel(gather_names)) %>%
    tidyr::unite("TMPGRPCOL", !!grp_var, type, sep = "_", remove = F) %>%
    dplyr::mutate(TMPGRPCOL = forcats::as_factor(TMPGRPCOL) %>% forcats::fct_reorder2(!!grp_var, type, dplyr::first)) %>%
    dplyr::select(-!!grp_var, -type) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::spread(key = TMPGRPCOL, value = value)
}
