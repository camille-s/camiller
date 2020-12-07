context("test-calc_shares")
library(camiller)
library(testthat)

race <- utils::read.csv("../gnh_race.csv")

test_that("retains grouping", {
  race_grp <- race %>%
    dplyr::group_by(name, region) %>%
    calc_shares(group = variable, denom = "total", moe = moe)
  race_ungrp <- race %>%
    calc_shares(name, region, group = variable, denom = "total", moe = moe)

  expect_true(dplyr::is_grouped_df(race_grp))
  expect_false(dplyr::is_grouped_df(race_ungrp))
})

test_that("handles null moe", {
  edu_est <- edu %>%
    dplyr::select(-moe) %>%
    calc_shares(name, group = variable, denom = "age25plus")

  edu_moe <- edu %>%
    calc_shares(name, group = variable, denom = "age25plus", moe = moe)

  expect_equal(edu_moe$share, edu_est$share)
  expect_equal(ncol(edu_est), ncol(edu_moe) - 2)
})

test_that("handles ... and group_by", {
  race1 <- race %>%
    dplyr::group_by(region, name) %>%
    calc_shares(group = variable, denom = "total", moe = moe)

  race2 <- race %>%
    calc_shares(region, name, group = variable, denom = "total", moe = moe)

  race3 <- race %>%
    dplyr::group_by(region) %>%
    calc_shares(name, group = variable, denom = "total", moe = moe)

  expect_setequal(race2$share, race1$share)
  expect_setequal(race3$share, race1$share)

  expect_error(race %>% calc_shares(group = variable, denom = "total", moe = moe), "Must supply")
})

test_that("each name has 1 NA share", {
  edu1 <- edu %>%
    dplyr::group_by(name) %>%
    calc_shares(group = variable, denom = "age25plus", moe = moe)
  n_names <- length(unique(edu$name))

  expect_length(edu1$name[is.na(edu1$share)], n_names)
})

test_that("checks for denominator in grouping variable", {
  expect_silent(edu %>% calc_shares(name, group = variable, denom = "age25plus", moe = moe))
  expect_error(edu %>% calc_shares(name, group = variable, denom = "adults", moe = moe))
  expect_error(edu %>% calc_shares(name, group = variable, moe = moe))
})
