#' Theme Din
#'
#' @description A clean theme especially suited for labeled bar charts or Cleveland dot plots. Designed based on the DIN family of fonts, but now defaults to font "Roboto Condensed".
#' This defaults to `base_family = "roboto"`, which depends on a) using `showtext` and `sysfonts` to get a font called "roboto", or b) already having a font loaded with this name. Use `sysfonts::font_add_google` or `sysfonts::font_add` to set a different font.
#' @param base_size Base font size
#' @param base_family Base font family; defaults to "roboto", as set by `showtext`
#' @param xgrid A logical for turning x-grid on or off, or "dotted", for a light dotted grid
#' @param ygrid A logical for turning y-grid on or off, or "dotted", for a light dotted grid
#' @param fallback_google Logical: if `TRUE` and `base_family` not currently loaded, will load Roboto Condensed from Google. If `FALSE`, will load system sans font. Defaults `TRUE`.
#' @inheritParams ggplot2::theme_light
#' @seealso [sysfonts::font_add()], [sysfonts::font_add_google()]
#' @export
theme_din <- function(base_size = 14, base_family = "roboto", xgrid = FALSE, ygrid = TRUE, fallback_google = TRUE) {
  showtext::showtext_auto(enable = TRUE)
  loaded_fonts <- sysfonts::font_families()
  if (!base_family %in% loaded_fonts) {
    if (fallback_google) {
      message(sprintf("Font %s not found. ", base_family), "Substituting with font Roboto Condensed as family 'roboto'")
      sysfonts::font_add_google(name = "Roboto Condensed", family = "roboto")
      base_family <- "roboto"
    } else {
      message(sprintf("Font %s not found. ", base_family), "Substituting with system sans font.")
      base_family <- "sans"
    }
  }
  out <- ggplot2::theme_light(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(vjust = 1, size = ggplot2::rel(0.7), color = "gray30", margin = ggplot2::margin(12, 0, 0, 0)),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "gray85"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold", colour = "gray20", size = ggplot2::rel(0.8)),
      axis.text = ggplot2::element_text(color = "gray30", size = ggplot2::rel(0.8)),
      plot.title = ggplot2::element_text(face = "bold", colour = "gray10"),
      plot.subtitle = ggplot2::element_text(color = "gray20"),
      panel.background = ggplot2::element_rect(fill = "gray100"),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "gray95"),
      strip.text = ggplot2::element_text(color = "gray20"))
  if(is.logical(xgrid)) {
    if(!xgrid) {
      out <- out + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    }
  } else if(xgrid == "dotted") {
    out <- out + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "gray92", size = 1, linetype = "22"))
  }
  if(is.logical(ygrid)) {
    if(!ygrid) {
      out <- out + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    }
  } else if(ygrid == "dotted") {
    out <- out + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "gray92", size = 1, linetype = "22"))
  }
  return(out)
}
