library("ggplot2")
library("extrafont")
extrafont::loadfonts()

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  scale_x_continuous(breaks = pretty_range_breaks(mtcars$wt, 5)) +
  scale_y_continuous(breaks = pretty_range_breaks(mtcars$mpg, 5)) +
  ggtitle("Cars")

p + geom_rangeframe() +
  theme_tufte()

p + geom_rug() +
 theme_tufte(ticks = FALSE)
