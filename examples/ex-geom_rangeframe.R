library("ggplot2")
library("extrafont")
extrafont::loadfonts()

ggplot(mtcars, aes(wt, mpg)) +
 geom_point() +
 geom_rangeframe() +
 theme_tufte()
