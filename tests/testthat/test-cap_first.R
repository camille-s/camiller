context("test-cap_first")
library(camiller)
library(testthat)

test_that("capitalizes correctly", {
  words <- c("camille", "new haven", "new Haven")
  expect_equal(cap_first(words), c("Camille", "New haven", "New haven"))
})
