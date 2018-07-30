context("test-color_prev")
library(camiller)
library(testthat)

test_that("returns ggplot with correct layers", {
  pal <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F")
  p_label <- color_prev(pal, labels = T)
  p_unlabel <- color_prev(pal, labels = F)
  # geom_rect, geom_text at top, geom_text if labels
  expect_equal(length(p_label$layers), 3)
  expect_equal(length(p_unlabel$layers), 2)
})
