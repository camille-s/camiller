library(camiller)
library(testthat)

test_that("color_prev returns ggplot with correct layers", {
  pal <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F")
  p_label <- color_prev(pal, labels = TRUE)
  p_unlabel <- color_prev(pal, labels = FALSE)
  # geom_rect, geom_text at top, geom_text if labels
  expect_length(p_label$layers, 3)
  expect_length(p_unlabel$layers, 2)
})

test_that("color_prev maps names to geom_text label", {
  pal1 <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F")
  pal2 <- setNames(pal1, c("blue", "orange", "green", "pink", "tan"))
  p_unnamed <- color_prev(pal1)
  p_named <- color_prev(pal2)
  expect_equal(rlang::as_label(p_unnamed$layers[[3]]$mapping$label), "lbl")
  expect_equal(ggplot2::layer_data(p_named, i = 3)$label,
               names(pal2))
  expect_equal(ggplot2::layer_data(p_unnamed, i = 3)$label,
               as.character(seq_along(pal1)))
})

test_that("color_prev handles R named colors", {
  pal <- paste0("turquoise", 1:4)
  p <- color_prev(pal)
  expect_equal(ggplot2::layer_data(p, i = 2)$label, pal)
})
