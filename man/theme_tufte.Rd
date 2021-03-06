% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tufte.R
\name{theme_tufte}
\alias{theme_tufte}
\title{Tufte's Maximal Data, Minimal Ink Theme}
\usage{
theme_tufte(base_size = 11, ticks = TRUE, sans_title = FALSE)
}
\arguments{
\item{base_size}{base font size}

\item{ticks}{\code{logical} Show axis ticks?}

\item{sans_title}{If \code{TRUE}, then use a sans serif font for
\code{plot.title} and \code{plot.subtitle}.
The default is to search for a Gill Sans font, followed by Lato (distributed with \pkg{ggtufte}),
followed by the default device "sans" font.
If \code{FALSE}, then use an italic serif font.
This will use ETBembo DisplayItalic
(distributed with \pkg{ggtufte}) if installed, otherwise it will
use default Palatino font.}
}
\description{
Theme based on Chapter 6 'Data-Ink Maximization and Graphical
Design' of Tufte \emph{The Visual Display of Quantitative Information}.
No border, no axis lines, no grids. This theme works
well in combination with \code{\link{geom_rug}} or
\code{\link{geom_rangeframe}}.
}
\examples{
library("ggplot2")
library("extrafont")
extrafont::loadfonts(quiet = TRUE)
extrafont::loadfonts("postscript", quiet = TRUE)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  scale_x_continuous(breaks = pretty_range_breaks(mtcars$wt, 5)) +
  scale_y_continuous(breaks = pretty_range_breaks(mtcars$mpg, 5)) +
  ggtitle("Cars")

p + geom_rangeframe() +
  theme_tufte()

p + geom_rug() +
 theme_tufte(ticks = FALSE)
}
\references{
Tufte, Edward R. (2001) The Visual Display of
Quantitative Information, Chapter 6.
}
