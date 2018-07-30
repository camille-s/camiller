#' Significance testing of differences with MOEs
#'
#' This function uses 2 estimates and 2 margins of error to do significance testing. It optionally returns the input data frame with the calculations used for testing, or just the data frame with results attached.
#' @param df A data frame
#' @param est1 Estimate for first group
#' @param moe1 Margin of error for first group
#' @param est2 Estimate for second group
#' @param moe2 Margin of error for second group
#' @param cl Confidence level used in calculating MOEs given; defaults to 0.9, per ACS data
#' @param alpha Alpha used for significance testing; defaults to 0.05
#' @param show_calc Logical, whether to keep intermediary calculations (default) or only result of testing
#' @return A tibble/data frame with testing-related columns added
#' @examples
#' med_age <- data.frame(name = c("Hamden", "New Haven"),
#'     men_est = c(37.2, 29.5), men_moe = c(1.9, 0.8),
#'     women_est = c(37.8, 31.9), women_moe = c(1.9, 0.8))
#' med_age %>%
#'   moe_test(men_est, men_moe, women_est, women_moe, alpha = 0.9, show_calc = TRUE)
#' @export
moe_test <- function(df, est1, moe1, est2, moe2, cl = 0.9, alpha = 0.05, show_calc = TRUE) {
  est1 <- rlang::enquo(est1)
  est2 <- rlang::enquo(est2)
  moe1 <- rlang::enquo(moe1)
  moe2 <- rlang::enquo(moe2)
  z <- 1 - (1 - cl) / 2
  test_z <- 1 - alpha / 2
  lvl <- (1 - alpha) * 100
  sig_name <- rlang::sym(paste("isSig", sprintf("%02g", lvl), sep = "_"))

  out <- df %>%
    dplyr::mutate(diff = !!est2 - !!est1) %>%
    dplyr::mutate(se1 = !!moe1 / stats::qnorm(z), se2 = !!moe2 / stats::qnorm(z)) %>%
    dplyr::mutate(se = sqrt(se1^2 + se2^2)) %>%
    dplyr::mutate(z_score = diff / se) %>%
    dplyr::mutate(!!sig_name := abs(z_score) > abs(stats::qnorm(test_z)))
  if (show_calc) {
    out
  } else {
    out %>% dplyr::select(-diff:-z_score)
  }
}
