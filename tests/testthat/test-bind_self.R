context("test-bind_self")
library(dplyr)
library(camiller)
library(testthat)

test_that("returns proper number of rows", {
  race_ir <- race_pops %>% filter(region == "Inner Ring")
  expect_equal(nrow(bind_self(race_ir, region, "Inner Ring")), 2 * nrow(race_ir))
})

test_that("handles grouping", {
  race_ir <- race_pops %>% filter(region == "Inner Ring")
  race_ir_grp <- race_ir %>% group_by(name)
  expect_true(is_grouped_df(bind_self(race_ir_grp, name, "Inner Ring")))
  expect_false(is_grouped_df(bind_self(race_ir, name, "Inner Ring")))
})

test_that("handles factors", {
  race_ir <- race_pops %>% filter(region == "Inner Ring")
  race_ir_fct <- race_ir %>% mutate(name = as.factor(name))
  bound1 <- bind_self(race_ir, name, "Inner Ring")
  bound2 <- bind_self(race_ir_fct, name, "Inner Ring")
  expect_is(bound1$name, "character")
  expect_is(bound2$name, "factor")
})

