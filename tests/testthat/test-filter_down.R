context("test-filter_down")
library(camiller)
library(testthat)

test_that("Rows properly excluded", {
  messy_summary <- tibble::tribble(
    ~x1,        ~x2,
    "A",        1,
    "B",        5,
    "C",        9,
    "Weights",  NA,
    "A",        0.2,
    "B",        0.5
  )
  u1 <- filter_until(messy_summary, x1 == "Weights")
  a1 <- filter_after(messy_summary, x1 == "Weights")
  expect_equal(nrow(u1), 3)
  expect_equal(nrow(a1), 2)
})

test_that("Rows properly included", {
  messy_summary <- tibble::tribble(
    ~x1,        ~x2,
    "A",        1,
    "B",        5,
    "C",        9,
    "Weights",  NA,
    "A",        0.2,
    "B",        0.5
  )
  a1 <- filter_after(messy_summary, x1 == "Weights")
  a2 <- filter_after(messy_summary, x1 == "Weights", .include_first = TRUE)
  expect_equal(nrow(a1), 2)
  expect_equal(nrow(a2), 3)
})

test_that("Handles multiple conditions", {
  messy_notes <- tibble::tribble(
    ~x1,       ~x2,        ~x3,
    "A",       "dog",      0,
    "B",       "cat",      1,
    "Source:", "xyz.com",  NA,
    "Date:",   "Jan",      2021
  )
  a1 <- filter_after(messy_notes, grepl("\\:", x1) & is.na(x3))
  expect_equal(nrow(a1), 1)
})

test_that("Throws error for commas", {
  messy_notes <- tibble::tribble(
    ~x1,       ~x2,        ~x3,
    "A",       "dog",      0,
    "B",       "cat",      1,
    "Source:", "xyz.com",  NA,
    "Date:",   "Jan",      2021
  )
  a1 <- filter_after(messy_notes, grepl("\\:", x1) & is.na(x3))
  expect_equal(nrow(a1), 1)
  expect_error(filter_after(messy_notes, grepl("\\:", x1), is.na(x3)))
})
