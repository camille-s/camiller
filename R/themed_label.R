#' Create `cowplot` label based on a ggplot theme
#'
#' This is a wrapper around `cowplot::draw_label()` that creates a `ggplot`-based label that inherits formatting from a given theme element. It's more or less been superceded by `ggplot`'s new `plot.title.position` theme argument.
#' @param label A string of text for label.
#' @param theme A ggplot theme; if `NULL` (the default), will get current theme with `ggplot2::theme_get()`.
#' @param element Name of a theme element; defaults to base text.
#' @param x x-position; defaults to 0.01
#' @param hjust Horizontal alignment; defaults 0
#' @param ... Any other arguments to pass to `cowplot::draw_label()`.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   town_pops <- race_pops %>%
#'     dplyr::filter(variable == "total") %>%
#'     dplyr::mutate(name = forcats::fct_reorder(as.factor(name), estimate))
#'
#'   library(ggplot2)
#'   p <- ggplot(town_pops, aes(x = name, y = estimate)) +
#'     geom_col() +
#'     coord_flip()
#'   # With long labels on the left, ggplot's default title placement
#'   # aligned to the panel can become awkward
#'   p + ggtitle("Total population by town, 2017")
#'   # Instead, make a label grob and arrange plot elements how you want
#'   title <- themed_label("Total population by town, 2017", element = "plot.title")
#'   cowplot::plot_grid(
#'     title,
#'     p,
#'     ncol = 1,
#'     rel_heights = c(1, 10)
#'   )
#' }
#' @export
themed_label <- function(label, theme = NULL, element = "text", x = 0.01, hjust = 0, ...) {
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }
  # if theme isn't put in as theme(), invoke it
  if (is.function(theme)) {
    theme <- rlang::exec(theme)
  }
  assertthat::assert_that(element %in% names(theme), msg = "Element must be a valid ggplot theme element name")

  elements <- ggplot2::calc_element(element, theme)

  lbl <- cowplot::draw_label(label,
                      fontfamily = elements$family,
                      fontface = elements$face,
                      colour = elements$color,
                      size = elements$size,
                      x = x,
                      hjust = hjust,
                      ...
  )
  cowplot::ggdraw() + lbl
}
