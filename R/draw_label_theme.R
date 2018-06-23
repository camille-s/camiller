#' Create `cowplot` label based on a ggplot theme
#' @param label A string of text for label
#' @param theme A ggplot theme; if blank, will get current theme with `ggplot2::theme_get`
#' @param element Name of a theme element; defaults to base text
#' @param ... Any other arguments to pass to `cowplot::draw_label`
#' @return A `ggplot` object
#' @examples
#' title <- cowplot::ggdraw() +
#'   draw_label_theme("Plot title", ggplot2::theme_bw(), element = "plot.title", x = 0.05)
#' @export
draw_label_theme <- function(label, theme = NULL, element = "text", ...) {
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }
  if (!element %in% names(theme)) {
    stop("Element must be a valid ggplot theme element name")
  }

  elements <- ggplot2::calc_element(element, theme)

  cowplot::draw_label(label,
                      fontfamily = elements$family,
                      fontface = elements$face,
                      colour = elements$color,
                      size = elements$size,
                      ...
  )
}
