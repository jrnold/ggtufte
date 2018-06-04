# copied from ggplot2
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
