context("test-show_uniq")
library(dplyr)
library(camiller)
library(testthat)

test_that("unique values printed", {
  expect_output(race_pops %>%
                  filter(region == "Inner Ring") %>%
                  show_uniq(variable))
})

test_that("returns original data frame", {
  d <- race_pops %>%
    filter(region == "Inner Ring")
  expect_equal(d, race_pops %>%
                 filter(region == "Inner Ring") %>%
                 show_uniq(variable))
})
