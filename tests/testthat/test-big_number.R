library(camiller)
library(testthat)

test_that("big_number handles negatives", {
  n <- c(-2100, 2100)
  big <- big_number(n, digits = 2)
  expect_equal(big, c("-2.1k", "2.1k"))
})

test_that("big_number correctly orders - vs $", {
  n <- c(-5000)
  big <- big_money(n)
  expect_match(big, "^\\-\\$")
})

test_that("big_number warns beyond trillion", {
  expect_warning(big_number(1e16))
})
