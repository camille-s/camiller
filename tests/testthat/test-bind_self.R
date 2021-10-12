library(camiller)
library(testthat)

race <- read.csv("../gnh_race.csv")

test_that("bind_self returns proper number of rows", {
  race_ir <- race %>%
    dplyr::filter(region == "Inner Ring")
  expect_equal(nrow(bind_self(race_ir, region, "Inner Ring")), 2 * nrow(race_ir))
})

test_that("bind_self handles grouping", {
  race_ir <- race %>% dplyr::filter(region == "Inner Ring")
  race_ir_grp <- race_ir %>% dplyr::group_by(name)
  expect_true(dplyr::is_grouped_df(bind_self(race_ir_grp, name, "Inner Ring")))
  expect_false(dplyr::is_grouped_df(bind_self(race_ir, name, "Inner Ring")))
})

test_that("bind_self handles factors", {
  race_ir <- race %>% dplyr::filter(region == "Inner Ring")
  race_ir_fct <- race_ir %>% dplyr::mutate(name = as.factor(name))
  bound1 <- bind_self(race_ir, name, "Inner Ring")
  bound2 <- bind_self(race_ir_fct, name, "Inner Ring")
  expect_is(bound1$name, "character")
  expect_is(bound2$name, "factor")
})

