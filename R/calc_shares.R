#' Make table of rates given a denominator
#'
#' `calc_shares` makes it easy to divide values by some denominator within the same long-shaped data frame. For example, it works well for a table of population groups for multiple locations where you want to divide population counts by some total population. It optionally handles dividing margins of error. Denote locations or other groupings by using a grouped data frame, passing bare column names to `...`, or both.
#'
#' @param df A data frame
#' @param ... Optional; bare column names to be used for groupings.
#' @param group Bare column name where groups are given--that is, the denominator value should be found in this column
#' @param denom String; denominator to filter from `group`
#' @param estimate Bare column name of estimates or values
#' @param moe Bare column name of margins of error; if supplied, MOE of shares will be included in output
#' @param digits Number of digits to round to; defaults to 3.
#' @return A tibble/data frame with shares (and optionally MOE of shares) of subgroup values within a denominator group. Shares given for denominator group will be blank.
#' @examples
#' edu %>%
#'   dplyr::group_by(name) %>%
#'   calc_shares(group = variable, denom = "age25plus", moe = moe)
#'
#' race_pops %>%
#'   calc_shares(region, name, group = variable, denom = "total", moe = moe)
#' @export
calc_shares <- function(df, ..., group = group, denom = "total_pop", estimate = estimate, moe = NULL, digits = 3) {
  grp_var <- rlang::enquo(group)
  est_var <- rlang::enquo(estimate)

  # check for denom in grp_var
  assertthat::assert_that(denom %in% df[[rlang::quo_name(grp_var)]], msg = sprintf("The denominator '%s' doesn\'t seem to be in %s", denom, rlang::quo_name(grp_var)))

  # should be grouped and/or have id
  if (dplyr::is_grouped_df(df)) {
    group_cols <- c(dplyr::groups(df), rlang::quos(...))
  } else {
    assertthat::assert_that(length(rlang::quos(...)) > 0, msg = "Must supply a grouped data frame and/or give column names in ...")
    group_cols <- rlang::quos(...)
  }
  # group by all group_cols
  df_grp <- df %>%
    dplyr::group_by(!!!group_cols)

  join_names <- tidyselect::vars_select(names(df), !!!group_cols)
  join_cols <- rlang::quos(!!!group_cols)

  est_name <- rlang::quo_name(est_var)

  df2 <- df_grp %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := as.character(!!grp_var))

  if (!rlang::quo_is_null(rlang::enquo(moe))) {
    moe_var <- rlang::enquo(moe)
    moe_name <- rlang::quo_name(moe_var)

    calcs <- dplyr::inner_join(
      df2 %>%
        dplyr::filter(!!grp_var == denom) %>%
        dplyr::select(-!!grp_var) %>%
        dplyr::rename(total_est = !!est_var, total_moe = !!moe_var),
      df2 %>%
        dplyr::filter(!!grp_var != denom),
      by = join_names
    ) %>%
      dplyr::mutate(share = round((!!est_var) / total_est, digits = digits)) %>%
      dplyr::mutate(sharemoe = round(tidycensus::moe_prop(!!est_var, total_est, !!moe_var, total_moe), digits = 3))

    bound <- dplyr::bind_rows(
      calcs %>%
        dplyr::select(!!!join_cols, !!rlang::quo_name(est_name) := total_est, !!rlang::quo_name(moe_name) := total_moe) %>%
        dplyr::mutate(!!rlang::quo_name(grp_var) := denom) %>%
        unique(),
      calcs %>% dplyr::select(-dplyr::starts_with("total"))
    )
  } else {
    calcs <- dplyr::inner_join(
      df2 %>%
        dplyr::filter(!!grp_var == denom) %>%
        dplyr::select(-!!grp_var) %>%
        dplyr::rename(total_est = !!est_var),
      df2 %>%
        dplyr::filter(!!grp_var != denom),
      by = join_names
    ) %>%
      dplyr::mutate(share = round((!!est_var) / total_est, digits = digits))

    bound <- dplyr::bind_rows(
      calcs %>%
        dplyr::select(!!!join_cols, !!rlang::quo_name(est_name) := total_est) %>%
        dplyr::mutate(!!rlang::quo_name(grp_var) := denom) %>%
        unique(),
      calcs %>%
        dplyr::select(-dplyr::starts_with("total"))
    )
  }

  out <- bound %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := as.factor(!!grp_var) %>% forcats::fct_inorder() %>% forcats::fct_relevel(denom)) %>%
    dplyr::arrange(!!!join_cols, !!grp_var) %>%
    dplyr::select(!!!join_cols, !!grp_var, dplyr::everything())

  # ungroup if original df wasn't grouped
  if (!dplyr::is_grouped_df(df)) {
    out <- dplyr::ungroup(out)
  }
  out
}
