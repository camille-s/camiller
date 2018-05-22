#' Theme Din
#'
#' @description A clean theme especially suited for labeled bar charts or Cleveland dot plots. Defaults to font "Din".
#' @param base_size Base font size
#' @param base_family Base font family; defaults to "din", as set by `showtext``
#' @param xgrid A logical for turning x-grid on or off, or "dotted", for a light dotted grid
#' @param ygrid A logical for turning y-grid on or off, or "dotted", for a light dotted grid
#' @export
theme_din <- function(base_size = 14, base_family = "din", xgrid = F, ygrid = T) {
  out <- theme_light(base_size = base_size, base_family = base_family) +
    theme(
      plot.caption = element_text(vjust = 1, size = rel(0.7), color = "gray30", margin = margin(12, 0, 0, 0)),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(colour = "gray85"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", colour = "gray20", size = rel(0.8)),
      axis.text = element_text(color = "gray30", size = rel(0.8)),
      plot.title = element_text(face = "bold", colour = "gray10"),
      plot.subtitle = element_text(color = "gray20"),
      panel.background = element_rect(fill = "gray100"),
      panel.border = element_blank(),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(color = "gray20"))
  if(is.logical(xgrid)) {
    if(!xgrid) {
      out <- out + theme(panel.grid.major.x = element_blank())
    }
  } else if(xgrid == "dotted") {
    out <- out + theme(panel.grid.major.x = element_line(color = "gray92", size = 1.5, linetype = "12"))
  }
  if(is.logical(ygrid)) {
    if(!ygrid) {
      out <- out + theme(panel.grid.major.y = element_blank())
    }
  } else if(ygrid == "dotted") {
    out <- out + theme(panel.grid.major.y = element_line(color = "gray92", size = 1.5, linetype = "12"))
  }
  return(out)
}
