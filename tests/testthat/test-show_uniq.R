context("test-show_uniq")
library(camiller)
library(testthat)

race <- read.csv("../gnh_race.csv")

test_that("unique values printed", {
  expect_output(race %>%
                  dplyr::filter(region == "Inner Ring") %>%
                  show_uniq(variable))
})

test_that("returns original data frame", {
  d <- race %>%
    dplyr::filter(region == "Inner Ring")
  expect_equal(d, race %>%
                 dplyr::filter(region == "Inner Ring") %>%
                 show_uniq(variable))
})
