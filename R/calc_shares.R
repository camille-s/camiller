#' Make table of rates given a denominator
#' @param df A data frame
#' @param ... Bare column names of IDs, town names, etc to exclude from `gather`ing
#' @param group Bare column name where groups are given--that is, the denominator value should be found in this column
#' @param denom String; denominator to filter from `group``
#' @param estimate Bare column name of estimates or values
#' @param moe Bare column name of margins of error; if supplied, MOE of shares will be included in output
#' @return A tibble/data frame with shares (and optionally MOE of shares) of subgroup values within a denominator group. Shares given for denominator group will be blank.
#' @examples
#' edu %>%
#'   dplyr::rename(est = estimate) %>%
#'   calc_shares(name, denom = "age25plus", estimate = est, moe = moe)
#' @export
calc_shares <- function(df, ..., group = variable, denom = "Total", estimate = estimate, moe = NULL) {
  grp_var <- rlang::enquo(group)
  est_var <- rlang::enquo(estimate)
  group_cols <- dplyr::groups(df)
  join_cols <- rlang::quos(...)
  # join_cols <- quos(c(!!!group_cols), c(!!!quos(...)))
  join_names <- purrr::map_chr(rlang::exprs(...), as.character)
  est_name <- rlang::quo_name(est_var)

  df2 <- df %>%
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
      dplyr::mutate(share = round((!!est_var) / total_est, digits = 3)) %>%
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
      dplyr::mutate(share = round((!!est_var) / total_est, digits = 3))

    bound <- dplyr::bind_rows(
      calcs %>%
        dplyr::select(!!!join_cols, !!rlang::quo_name(est_name) := total_est) %>%
        dplyr::mutate(!!rlang::quo_name(grp_var) := denom) %>%
        unique(),
      calcs %>%
        dplyr::select(-dplyr::starts_with("total"))
    )
  }

  bound %>%
    # dplyr::mutate(!!rlang::quo_name(grp_var) := ifelse(is.factor(!!grp_var), !!grp_var, as.factor(!!grp_var))) %>%
    dplyr::mutate(!!rlang::quo_name(grp_var) := as.factor(!!grp_var) %>% forcats::fct_inorder() %>% forcats::fct_relevel(denom)) %>%
    dplyr::arrange(!!!join_cols, !!grp_var) %>%
    dplyr::select(!!!join_cols, !!grp_var, dplyr::everything())
}
