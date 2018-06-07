#' A box and whisker's plot (in the style of Tufte)
#'
#' Tufte \emph{The Visual Display of Quantitative Information} (Ch 6)
#' proposes several revisions of the box plot.
#' These variants compactly display the distribution of a continuous variable.
#' It visualises five summary statistics: the median, two hinges (25\% and 75\% quartiles)
#' and two whiskers (minimum, maximum).
#'
#' The statistics are different than \code{\link{geom_boxplot}}.
#'
#' @section Aesthetics:
#' \itemize{
#'   \item x (required) x coordinate of the line
#'   \item upper
#'   \item lower
#'   \item middle
#'   \item ymin
#'   \item ymax
#'   \item colour (required)
#'   \item size thickness of the lines
#'   \item linetype types of the lines
#'   \item width
#'   \item shape shape of the point(s)
#'   \item fill fill of the point(s)
#'   \item alpha transparency
#' }
#'
#' @references Tufte, Edward R. (2001) The Visual Display of
#'   Quantitative Information, Chapter 6.
#'
#' @seealso \code{\link{geom_boxplot}}
#' @inheritParams ggplot2::geom_point
#' @seealso \code{\link{geom_boxplot}} and \code{\link{geom_pointrange}}.
#' @family geom tufte
#' @export
#'
#' @example examples/ex-geom_quartileplot.R
geom_quartileplot <- function(mapping = NULL,
                             data = NULL,
                             stat = "fivenumber",
                             position = "dodge",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             middle.line = FALSE,
                             whisker.line = TRUE,
                             hoffset = 0.01,
                             fatten = 1,
                             gapsize = 0.01,
                             ...) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomQuartilePlot,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        middle.line = middle.line,
        whisker.line = whisker.line,
        na.rm = na.rm,
        hoffset = hoffset,
        fatten = fatten,
        gapsize = gapsize,
        ...
      )
    )
  }

#' @rdname geom_quartileplot
#' @usage NULL
#' @format NULL
#' @export
#' @importFrom ggplot2 ggproto_parent aes GeomPoint GeomSegment
#' @importFrom grid grobTree
GeomQuartilePlot <-
  ggplot2::ggproto("GeomQuartilePlot",
          ggplot2::GeomBoxplot,
          setup_data = function(self, data, params) {
            data <- ggproto_parent(GeomBoxplot, self)$setup_data(data, params)
            x_range <- diff(range(data$x))
            y_range <- max(data$ymax) - min(data$ymin)
            data$hoffset <- params$hoffset * x_range
            data$gapsize <- params$gapsize * y_range
            data
          },
          draw_group = function(data, panel_scales, coord,
                                middle.line = FALSE,
                                whisker.line = TRUE,
                                hoffset = 0.001,
                                gapsize = 0.01,
                                fatten = 1) {
            common <- data.frame(
              colour = data$colour,
              linetype = data$linetype,
              fill = alpha(data$fill, data$alpha),
              stroke = data$stroke,
              shape = data$shape,
              group = data$group,
              stringsAsFactors = FALSE
            )
            # whiskers
            if (whisker.line) {
              whiskers <- data.frame(
                x = data$x,
                xend = data$x,
                y = c(data$upper, data$lower),
                yend = c(data$ymax, data$ymin),
                size = data$size,
                alpha = data$alpha,
                common,
                stringsAsFactors = FALSE
              )
              whiskers_grob <- GeomSegment$draw_panel(whiskers, panel_scales,
                                                      coord)
            } else {
              whiskers <- data.frame(
                x = data$x,
                y = c(data$ymin, data$ymax),
                size = data$size,
                alpha = data$alpha,
                common,
                stringsAsFactors = FALSE
              )
              whiskers_grob <- GeomPoint$draw_panel(whiskers, panel_scales,
                                                    coord)
            }
            # middle point
            if (middle.line) {
              middata <- data.frame(
                y = c(data$upper, data$middle - 0.5 * data$gapsize),
                yend = c(data$middle + 0.5 * data$gapsize, data$lower),
                x = data$x + data$hoffset,
                xend = data$x + data$hoffset,
                size = data$size * fatten,
                alpha = data$alpha,
                common,
                stringsAsFactors = FALSE
              )
              middle_grob <- GeomSegment$draw_panel(middata, panel_scales,
                                                    coord)
            } else {
              middata <- data.frame(
                x = data$x,
                y = data$middle,
                size = data$size * fatten,
                alpha = data$alpha,
                common,
                stringsAsFactors = FALSE
              )
              middle_grob <- GeomPoint$draw_panel(middata, panel_scales, coord)             }
            ggname("geom_quartileplot",
                   grobTree(
                     whiskers_grob,
                     middle_grob
                   ))
          },
          draw_legend = ggplot2::draw_key_pointrange,
          default_aes = ggplot2::aes(weight = 1,
                                     colour = "black",
                                     fill = "grey20",
                                     size = 0.5,
                                     alpha = NA,
                                     shape = 19,
                                     stroke = 0.5,
                                     width = 1,
                                     linetype = "solid")
  )
