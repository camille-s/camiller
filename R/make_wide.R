#' Arrange estimates, MOEs, etc into wide format
#' @param df A data frame
#' @param ... Bare column names of types of measures
#' @param group Bare column name of group or category variable for spreading into wide data
#' @return A tibble/data frame in wide format
#' @examples
#' edu %>%
#'   make_wide(estimate, moe)
#' @export
make_wide <- function(df, ..., group = variable) {
  gather_cols <- rlang::quos(...)
  grp_var <- rlang::enquo(group)
  gather_names <- tidyselect::vars_select(names(df), !!!gather_cols)

  df %>%
    tidyr::gather(key = type, value = value, !!!gather_cols) %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := !!grp_var %>% forcats::fct_inorder() %>% forcats::fct_rev()) %>%
    dplyr::mutate(type = as.factor(type) %>% forcats::fct_relevel(gather_names)) %>%
    tidyr::unite("grp", !!grp_var, type, sep = "_", remove = F) %>%
    dplyr::mutate(grp = as.factor(grp) %>% forcats::fct_reorder2(!!grp_var, type, dplyr::first)) %>%
    dplyr::select(-!!grp_var, -type) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::spread(key = grp, value = value)
}
