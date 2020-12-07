context("test-moe_test")
library(camiller)
library(testthat)

test_that("handles keeping or dropping intermediaries", {
  pov_change <- pov_age_10_16 %>%
    dplyr::mutate(dplyr::across(c(age, ratio), function(x) as.factor(x) %>% forcats::fct_inorder())) %>%
    dplyr::group_by(name, year, ratio) %>%
    add_grps(list(kids = 1:3), group = age, moe = moe) %>%
    dplyr::group_by(name, year, age) %>%
    add_grps(list(determined = 1, low_income = 2:9), group = ratio, moe = moe) %>%
    calc_shares(group = ratio, denom = "determined", moe = moe) %>%
    dplyr::filter(!is.na(share)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = paste0("y", year)) %>%
    tidyr::unite("group", age, ratio) %>%
    dplyr::select(-estimate:-moe) %>%
    make_wide(share:sharemoe, group = year)

  test1 <- pov_change %>%
    moe_test(y2010_share, y2010_sharemoe, y2016_share, y2016_sharemoe, alpha = 0.05, show_calc = T)
  expect_equal(ncol(test1), ncol(pov_change) + 6)

  test2 <- pov_change %>%
    moe_test(y2010_share, y2010_sharemoe, y2016_share, y2016_sharemoe, alpha = 0.05, show_calc = F)
  expect_equal(ncol(test2), ncol(pov_change) + 1)

  expect_equal(nrow(test1), nrow(pov_change))
  expect_equal(nrow(test2), nrow(pov_change))
})

test_that("handles naming based on alpha", {
  pov_change <- pov_age_10_16 %>%
    dplyr::mutate(dplyr::across(c(age, ratio), function(x) as.factor(x) %>% forcats::fct_inorder())) %>%
    dplyr::group_by(name, year, ratio) %>%
    add_grps(list(kids = 1:3), group = age, moe = moe) %>%
    dplyr::group_by(name, year, age) %>%
    add_grps(list(determined = 1, low_income = 2:9), group = ratio, moe = moe) %>%
    calc_shares(group = ratio, denom = "determined", moe = moe) %>%
    dplyr::filter(!is.na(share)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = paste0("y", year)) %>%
    tidyr::unite("group", age, ratio) %>%
    dplyr::select(-estimate:-moe) %>%
    make_wide(share:sharemoe, group = year)

  test05 <- pov_change %>%
    moe_test(y2010_share, y2010_sharemoe, y2016_share, y2016_sharemoe, alpha = 0.05, show_calc = F)
  expect_equal(names(test05)[ncol(test05)], "isSig_95")

  test10 <- pov_change %>%
    moe_test(y2010_share, y2010_sharemoe, y2016_share, y2016_sharemoe, alpha = 0.1, show_calc = F)
  expect_equal(names(test10)[ncol(test10)], "isSig_90")
})
