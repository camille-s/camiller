library(camiller)
library(testthat)

test_that("clean_titles handles multiple space chars", {
  t <- c("Male!CollegeGraduates", "Male CollegeGraduates", "male/college!graduates")
  space_vec <- clean_titles(t, cap_all = FALSE, split_case = TRUE, space = c("_", "!", "/"))
  space_pat <- clean_titles(t, cap_all = FALSE, split_case = TRUE, space = "[_!/]")
  expect_equal(space_vec, rep("Male college graduates", 3))
  expect_equal(space_pat, rep("Male college graduates", 3))
})

test_that("clean_titles converts all caps to title case", {
  t <- c("GREATER_NEW_HAVEN", "GREATER NEW HAVEN")
  title <- clean_titles(t, cap_all = TRUE, keep_running_caps = FALSE)
  expect_equal(title, rep("Greater New Haven", 2))
})

test_that("clean_titles converts all caps to only capped first", {
  t <- c("MALE_COLLEGE_GRADUATES", "MALE COLLEGE GRADUATES")
  first <- clean_titles(t, cap_all = FALSE, keep_running_caps = FALSE)
  expect_equal(first, rep("Male college graduates", 2))
})

test_that("clean_titles retains running capitals", {
  t <- c("Greater BPT Men", "GreaterBPT", "greaterBPT", "GreaterBPT_POC")
  abbr <- clean_titles(t, cap_all = FALSE, keep_running_caps = TRUE, split_case = TRUE)
  expect_equal(abbr, c("Greater BPT men", "Greater BPT", "Greater BPT", "Greater BPT POC"))
})
