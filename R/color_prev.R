#' Preview color palette
#'
#' This makes a simple `ggplot` with boxes side by side of each color in a vector. Numbers along the top show the position of the color in the vector. Optional labels are shown with the name or hexcode of each color; if `ggfittext` is installed, the labels will be scaled to fit in the rectangles.
#'
#' @param colors Character vector of colors
#' @param labels Logical; whether to show color name/hexcode labels
#' @param label_color Color to use for optional labels
#' @param border Logical; whether to draw border around rects
#' @param border_color Color to use for optional borders
#' @param border_size Size of optional borders
#' @return `ggplot` object showing rects for each color
#' @examples
#' # Paul Tol's colorblind-friendly palette, from ggthemes
#' tol <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677")
#' color_prev(tol, border = TRUE, border_size = 1)
#' @export
color_prev <- function(colors, labels = T, label_color = "black", border = F, border_color = "white", border_size = 0.5) {
  p <- data.frame(x = 1:length(colors), y = 0, fill = colors) %>%
    ggplot2::ggplot(ggplot2::aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1, fill = fill)) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::theme_void()
  if (border) {
    p <- p + ggplot2::geom_rect(color = border_color, size = border_size)
  } else {
    p <- p + ggplot2::geom_rect()
  }
  if (labels) {
    if (requireNamespace("ggfittext", quietly = TRUE)) {
      p <- p + ggfittext::geom_fit_text(ggplot2::aes(label = fill), color = label_color)
    } else {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = fill, x = x + 0.5, y = y + 0.5), color = label_color, size = 3)
    }

  }
  p +
    ggplot2::geom_text(ggplot2::aes(label = x, x = x + 0.5, y = y + 1, color = fill),
                       fontface = "bold", vjust = 0.5, hjust = 0.5, nudge_y = 0.05)
}
