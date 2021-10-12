library(camiller)
library(testthat)

race <- read.csv("../gnh_race.csv")

test_that("show_uniq unique values printed", {
  expect_output(race %>%
                  dplyr::filter(region == "Inner Ring") %>%
                  show_uniq(variable))
})

test_that("show_uniq returns original data frame", {
  d <- race %>%
    dplyr::filter(region == "Inner Ring")
  expect_equal(d, race %>%
                 dplyr::filter(region == "Inner Ring") %>%
                 show_uniq(variable))
})

test_that("show_uniq handles long strings", {
  set.seed(123)
  d <- data.frame(label = replicate(5, sample(c(letters, " "), round(runif(5, 110, 120)), replace = TRUE)) %>%
                    purrr::map_chr(paste, collapse = ""))
  expect_error(d %>% show_uniq(label), NA)
})


