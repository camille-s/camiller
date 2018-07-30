context("test-themed_label")
library(camiller)
library(testthat)

test_that("handles theme argument", {
  el1 <- themed_label("the title", element = "plot.title")
  el2 <- themed_label("the title", theme = ggplot2::theme_void(), element = "plot.title")

  expect_is(el1, "ggproto")
  expect_is(el2, "ggproto")
})

test_that("handles invalid element name", {
  expect_error(themed_label("caption", element = "source"), "valid ggplot theme")
})

test_that("handles theme as function", {
  el1 <- themed_label("the caption", theme = ggplot2::theme_classic, element = "plot.caption")
  el2 <- themed_label("the caption", theme = ggplot2::theme_classic(), element = "plot.caption")

  expect_is(el1, "ggproto")
  expect_is(el2, "ggproto")
})
