#' Collapse variable into groups and sum
#' @param df A data frame; will honor grouping
#' @param grp_list A named list of groups to collapse `group` into
#' @param group Bare column name giving groups in data; will be converted to factor
#' @param estimate Bare column name of estimates or values
#' @param moe Bare column name of margins of error; if supplied, MOEs of sums will be included in output
#' @return A data frame/tibble with sums of `estimate`. Retains grouping columns
#' @examples
#' edu_list <- list(ages25plus = 1, less_than_high_school = 2:16,
#'     high_school = 17:18, some_college_or_aa = 19:21, bachelors_plus = 22:25)
#'
#' edu_detail %>%
#'   dplyr::group_by(name) %>%
#'   add_grps(edu_list, group = variable, moe = moe)
#'
#' edu_detail %>%
#'   dplyr::group_by(name) %>%
#'   add_grps(list(total = "ages25plus",
#'       aa_or_bach = c("Associate's degree", "Bachelor's degree"),
#'       bachelors_plus = c("Bachelor's degree", "Master's degree", "Professional school degree", "Doctorate degree")),
#'     group = variable, moe = moe)
#' @export

add_grps <- function(df, grp_list, group = group, estimate = estimate, moe = NULL) {
  grp_var <- rlang::enquo(group)
  est_var <- rlang::enquo(estimate)
  # moe_var <- enquo(moe)
  group_cols <- dplyr::groups(df)
  grp_names <- names(grp_list)

  grp_list_chars <- make_grps(df %>% dplyr::pull(!!grp_var), grp_list)

  group_df <- grp_list_chars %>%
    purrr::imap_dfr(function(grps, grp_name) {
      df %>%
        dplyr::filter(!!grp_var %in% grps) %>%
        dplyr::mutate(!!rlang::quo_name(grp_var) := grp_name) %>%
        dplyr::group_by(!!!group_cols, !!grp_var)
    })

  if (!rlang::quo_is_null(rlang::enquo(moe))) {
    moe_var <- rlang::enquo(moe)
    out <- group_df %>%
      dplyr::summarise(!!rlang::quo_name(est_var) := sum(!!est_var), !!rlang::quo_name(moe_var) := tidycensus::moe_sum(!!moe_var, !!est_var) %>% round())

  } else {
    out <- group_df %>%
      dplyr::summarise(!!rlang::quo_name(est_var) := sum(!!est_var))
  }
  out %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := as.factor(!!grp_var) %>% forcats::fct_relevel(grp_names)) %>%
    dplyr::arrange(!!!group_cols, !!grp_var) %>%
    dplyr::group_by(!!!group_cols)
}