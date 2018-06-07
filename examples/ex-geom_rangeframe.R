library("ggplot2")
library("extrafont")
extrafont::loadfonts(quiet = TRUE)
extrafont::loadfonts("postscript", quiet = TRUE)

ggplot(mtcars, aes(wt, mpg)) +
 geom_point() +
 geom_rangeframe() +
 theme_tufte()
