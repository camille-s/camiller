#' Collapse variable into groups and sum
#' @param .data A data frame; will honor grouping
#' @param grp_list A named list of groups to collapse `group` into
#' @param group Bare column name giving groups in data; will be converted to factor
#' @param value Bare column name of values. Replaces `estimate` argument, but (for now) still defaults to a column named `estimate`
#' @param moe Bare column name of margins of error; if supplied, MOEs of sums will be included in output
#' @param estimate *Soft deprecated; use* `value`. Bare column name of estimates or values
#' @return A data frame/tibble with sums of `estimate`. Retains grouping columns
#' @examples
#' edu_list <- list(ages25plus = 1, less_than_high_school = 2:16,
#'     high_school = 17:18, some_college_or_aa = 19:21, bachelors_plus = 22:25)
#'
#' edu_detail %>%
#'   dplyr::group_by(name) %>%
#'   add_grps(edu_list, group = variable, value = estimate, moe = moe)
#'
#' edu_detail %>%
#'   dplyr::group_by(name) %>%
#'   add_grps(list(total = "ages25plus",
#'       aa_or_bach = c("Associate's degree", "Bachelor's degree"),
#'       bachelors_plus = c("Bachelor's degree", "Master's degree",
#'                          "Professional school degree", "Doctorate degree")),
#'     value = estimate, group = variable, moe = moe)
#' @export

add_grps <- function(.data, grp_list, group = group, value = estimate, moe = NULL, estimate = NULL) {
  if (!missing(estimate)) {
    warning("argument estimate is deprecated; please use value instead.", call. = TRUE)
    # value <- estimate
    val_var <- rlang::enquo(estimate)
  } else {
    val_var <- rlang::enquo(value)
  }
  grp_var <- rlang::enquo(group)
  # val_var <- rlang::enquo(value)
  group_cols <- dplyr::groups(.data)
  grp_names <- names(grp_list)

  grp_list_chars <- make_grps(.data %>% dplyr::pull(!!grp_var), grp_list)

  group_df <- grp_list_chars %>%
    purrr::imap_dfr(function(grps, grp_name) {
      .data %>%
        dplyr::filter(!!grp_var %in% grps) %>%
        dplyr::mutate(!!rlang::quo_name(grp_var) := grp_name) %>%
        dplyr::group_by(!!!group_cols, !!grp_var)
    })

  if (!rlang::quo_is_null(rlang::enquo(moe))) {
    moe_var <- rlang::enquo(moe)
    out <- group_df %>%
      dplyr::summarise(!!rlang::quo_name(val_var) := sum(!!val_var), !!rlang::quo_name(moe_var) := tidycensus::moe_sum(!!moe_var, !!val_var) %>% round())

  } else {
    out <- group_df %>%
      dplyr::summarise(!!rlang::quo_name(val_var) := sum(!!val_var))
  }
  out %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := as.factor(!!grp_var) %>% forcats::fct_relevel(grp_names)) %>%
    dplyr::arrange(!!!group_cols, !!grp_var) %>%
    dplyr::group_by(!!!group_cols)
}
