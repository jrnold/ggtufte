#' Tufte Maximal Data, Minimal Ink Theme
#'
#' Theme based on Chapter 6 'Data-Ink Maximization and Graphical
#' Design' of Edward Tufte *The Visual Display of Quantitative
#' Information*. No border, no axis lines, no grids. This theme works
#' best in combination with \code{\link{geom_rug}} or
#' \code{\link{geom_rangeframe}}.
#'
#' @note
#' The default font family is set to 'serif' as he uses serif fonts
#' for labels in 'The Visual Display of Quantitative Information'.
#' The serif font used by Tufte in his books is a variant of Bembo,
#' while the sans serif font is Gill Sans. If these fonts are
#' installed on your system, then you can use them with the package
#' \bold{extrafont}.
#'
#' @inheritParams ggplot2::theme_grey
#' @param ticks \code{logical} Show axis ticks?
#'
#' @references Tufte, Edward R. (2001) The Visual Display of
#' Quantitative Information, Chapter 6.
#'
#' @family themes tufte
#' @example examples/ex-theme_tufte.R
#' @export
#' @importFrom ggplot2 theme element_blank theme_bw
theme_tufte <- function(base_size = 11,
                        base_family = extrafont::choose_font(ggtufte::tufte_theme_fonts),
                        ticks = TRUE) {
  ret <- theme_bw(base_family = base_family, base_size = base_size) +
    theme(legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank())
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}

#' Font options to use with the Tufte Theme
#'
#' List of font or font-families to use with \code{theme_tufte}.
#' It starts with the preferred ETBembo font used in Tufte's book along with
#' fallback options.
#'
#' This list is derived from the list of fonts used in
#' [https://github.com/edwardtufte/tufte-css](tufte-css)
#'
#' @md
#' @format A character vector.
#' @export
#' @examples
#' tufte_theme_fonts
tufte_theme_fonts <-
c("ETBembo RomanLF", "Palatino", "Palatino Linotype", "Palatino LT STD",
  "Book Antiqua", "Georgia", "serif")