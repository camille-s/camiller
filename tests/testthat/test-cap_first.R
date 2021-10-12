library(camiller)
library(testthat)

test_that("cap_first capitalizes correctly", {
  words <- c("camille", "new haven", "new Haven")
  expect_equal(cap_first(words), c("Camille", "New haven", "New haven"))
})
