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
  grps <- unique(.data[[rlang::as_label(grp_var)]])
  grp_name <- rlang::as_label(grp_var)
  col_order <- paste(rep(grps, each = length(gather_names)), gather_names, sep = "_")

  out <- .data %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = {{ group }}, values_from = c(gather_names),
                       names_glue = sprintf("{ %s }_{.value}", grp_name)) %>%
    dplyr::select(dplyr::everything(), -dplyr::any_of(col_order), dplyr::all_of(col_order))

  # basically copying janitor::remove_empty
  full_cols <- colSums(!is.na(out)) > 0
  out[, full_cols, drop = FALSE]
}

