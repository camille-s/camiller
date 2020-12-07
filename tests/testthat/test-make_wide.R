context("test-make_wide")
library(camiller)
library(testthat)

test_that("makes expected number of columns", {
  edu_rates <- edu %>%
    dplyr::group_by(name) %>%
    calc_shares(group = variable, denom = "age25plus", moe = moe)
  num_groups <- length(unique(edu_rates$variable))
  edu_wide <- edu_rates %>% make_wide(estimate:sharemoe, group = variable)

  expect_equal(nrow(edu_wide), length(unique(edu_rates$name)))
  expect_equal(ncol(edu_wide), 1 + 2 * 1 + 4 * (num_groups - 1))
})

test_that("handles : notation", {
  edu_rates <- edu %>%
    dplyr::group_by(name) %>%
    calc_shares(group = variable, denom = "age25plus", moe = moe)

  edu1 <- edu_rates %>% make_wide(estimate, moe, share, sharemoe, group = variable)
  edu2 <- edu_rates %>% make_wide(estimate:sharemoe, group = variable)

  expect_identical(edu1, edu2)
})

