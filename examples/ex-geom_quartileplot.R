library("ggplot2")
library("extrafont")
extrafont::loadfonts()

p <- ggplot(mtcars, aes(factor(cyl), mpg))
# with a point for the median and lines for whiskers
p + geom_quartileplot()
# with a line for the interquartile range and points for whiskers
p + geom_quartileplot(median.type = "line", whisker.type = "point", hoffset = 0)
# with a wide line for the interquartile range and lines for whiskers
p + geom_quartileplot(median.type = "line", hoffset = 0, width = 3)
# with an offset line for the interquartile range and lines for whiskers
p + geom_quartileplot(median.type = "line")
# combined with theme_tufte
p + geom_quartileplot() +
  theme_tufte() +
  theme(axis.ticks.x = element_blank())
