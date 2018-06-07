#' Tufte's Maximal Data, Minimal Ink Theme
#'
#' Theme based on Chapter 6 'Data-Ink Maximization and Graphical
#' Design' of Tufte *The Visual Display of Quantitative Information*.
#' No border, no axis lines, no grids. This theme works
#' well in combination with \code{\link{geom_rug}} or
#' \code{\link{geom_rangeframe}}.
#'
#' @inheritParams ggplot2::theme_grey
#' @param ticks \code{logical} Show axis ticks?
#'
#' @references Tufte, Edward R. (2001) The Visual Display of
#' Quantitative Information, Chapter 6.
#'
#' @md
#' @family themes tufte
#' @example examples/ex-theme_tufte.R
#' @export
#' @importFrom ggplot2 theme element_blank element_text theme_bw
theme_tufte <- function(base_size = 11, ticks = TRUE, sans_title = FALSE) {
  font_roman <- get_etbembo()
  font_italic <- get_etbembo(italic = TRUE)
  font_sans <- get_gill_sans()

  title_font <- if (sans_title) {
    get_gill_sans()
  } else {
    font_italic
  }

  ret <- theme_bw(base_family = font_roman, base_size = base_size) +
    theme(
      text = element_text(family = font_roman),
      plot.title = element_text(family = title_font,
                                face = if (sans_title) "plain" else "italic"),
      plot.subtitle = element_text(family = title_font,
                                   face = if (sans_title) "plain" else "italic"),
      plot.caption = element_text(hjust = 0),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank()
    )
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}
