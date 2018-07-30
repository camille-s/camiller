#' Create `cowplot` label based on a ggplot theme
#'
#' This is a wrapper around `cowplot::draw_label()` that creates a `ggplot`-based label that inherits formatting from a given theme element.
#' @param label A string of text for label.
#' @param theme A ggplot theme; if `NULL` (the default), will get current theme with `ggplot2::theme_get()`.
#' @param element Name of a theme element; defaults to base text.
#' @param ... Any other arguments to pass to `cowplot::draw_label()`.
#' @return A `ggplot` object.
#' @examples
#' title <- cowplot::ggdraw() +
#'   themed_label("Plot title", ggplot2::theme_bw(), element = "plot.title", x = 0.05)
#' @export
themed_label <- function(label, theme = NULL, element = "text", ...) {
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }
  # if theme isn't put in as theme(), invoke it
  if (is.function(theme)) {
    theme <- rlang::invoke(theme)
  }
  assertthat::assert_that(element %in% names(theme), msg = "Element must be a valid ggplot theme element name")

  elements <- ggplot2::calc_element(element, theme)

  cowplot::draw_label(label,
                      fontfamily = elements$family,
                      fontface = elements$face,
                      colour = elements$color,
                      size = elements$size,
                      ...
  )
}
