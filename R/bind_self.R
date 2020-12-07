#' Neatly bind a data frame to itself, with a column changed
#'
#' This function changes the value of a data frame's column, then binds back to itself in order to fit into a pipeline. This preserves grouping, and if `group` is a factor, returns `group` as a factor with expanded levels.
#' @param .data A data frame.
#' @param group Bare column name of the column needing to be changed.
#' @param new_value String giving the new value for column `group`.
#' @param append Logical: should new values be appended to the end of `data`, or prepended to the beginning (the default)?
#' @param .id Optional, inheriting from `[dplyr::bind_rows()]`: if a string, will create IDs for each data frame and create a column of IDs with the name given as `.id`.
#' @return A data frame with twice as many rows as the original.
#' @examples
#' town_populations <- data.frame(
#'   name = c("East Haven", "Hamden", "West Haven"),
#'   pop = c(29015, 61476, 54972)
#' )
#' town_populations %>%
#'   dplyr::mutate(measure = "total_pop") %>%
#'   bind_self(group = name, new_value = "Inner Ring towns")
#'
#' town_populations %>%
#'   dplyr::group_by(name) %>%
#'   bind_self(group = name, new_value = "Inner Ring towns") %>%
#'   dplyr::summarise(pop = sum(pop))
#' @export
bind_self <- function(.data, group, new_value, append = FALSE, .id = NULL) {
  grp_var <- rlang::enquo(group)

  is_grouped <- dplyr::is_grouped_df(.data)
  is_factor_grp <- is.factor(.data[[rlang::as_label(grp_var)]])
  if (is_grouped) {
    grouped_by_vars <- dplyr::groups(.data)
    ungrouped <- .data %>%
      dplyr::ungroup()
  } else {
    ungrouped <- .data
  }

  if (is_factor_grp) {
    all_lvls <- list(factor(new_value), ungrouped[[rlang::as_label(grp_var)]]) %>% forcats::lvls_union()
    new_data <- ungrouped %>%
      dplyr::mutate({{ grp_var }} := factor(new_value, levels = all_lvls))
    ungrouped <- ungrouped %>%
      dplyr::mutate({{ grp_var }} := forcats::fct_expand(!!grp_var, new_value))
  } else {
    new_data <- ungrouped %>%
      dplyr::mutate({{ grp_var }} := new_value)
  }

  if (append) {
    out <- dplyr::bind_rows(ungrouped, new_data, .id = .id)
  } else {
    out <- dplyr::bind_rows(new_data, ungrouped, .id = .id)
  }
  if (is_grouped) {
    out <- out %>%
      dplyr::group_by(!!!grouped_by_vars)
  }
  out
}


