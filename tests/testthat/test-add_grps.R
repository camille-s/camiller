context("test-add_grps")
library(camiller)
library(testthat)

test_that("retains grouping", {
  edu_list <- list(total = 1, less_than_hs = 2:16, bach_plus = 22:25)

  edu1 <- edu_detail %>%
    dplyr::group_by(name) %>%
    add_grps(edu_list, group = variable)
  edu2 <- edu_detail %>%
    add_grps(edu_list, group = variable)

  expect_true(dplyr::is_grouped_df(edu1))
  expect_false(dplyr::is_grouped_df(edu2))
})

test_that("properly creates factor", {
  edu_list <- list(total = 1, less_than_hs = 2:16, bach_plus = 22:25)

  edu1 <- edu_detail %>%
    dplyr::group_by(name) %>%
    add_grps(edu_list, group = variable)

  expect_is(edu1$variable, "factor")
  expect_equal(levels(edu1$variable), names(edu_list))
})

test_that("retains or drops MOE", {
  edu_list <- list(total = 1, less_than_hs = 2:16, bach_plus = 22:25)

  edu_no_moe <- edu_detail %>%
    dplyr::group_by(name) %>%
    add_grps(edu_list, group = variable)
  edu_moe <- edu_detail %>%
    dplyr::group_by(name) %>%
    add_grps(edu_list, group = variable, moe = moe)

  expect_equal(ncol(edu_no_moe), 3)
  expect_equal(ncol(edu_moe), 4)
})
