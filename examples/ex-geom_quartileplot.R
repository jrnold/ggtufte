library("ggplot2")
library("extrafont")
extrafont::loadfonts(quiet = TRUE)
extrafont::loadfonts("postscript", quiet = TRUE)

p <- ggplot(mtcars, aes(factor(cyl), mpg))
# with a point for the median and lines for whiskers
p + geom_quartileplot()
# with a line for the interquartile range and points for whiskers
p + geom_quartileplot(middle.line = TRUE, whisker.line = FALSE)
# with a wide line for the interquartile range and lines for whiskers
p + geom_quartileplot(middle.line = TRUE, hoffset = 0, fatten = 2)
# with an offset line for the interquartile range and lines for whiskers
p + geom_quartileplot(middle.line = TRUE, hoffset = 0.01, fatten = 1)
# combined with theme_tufte
p + geom_quartileplot() +
  theme_tufte() +
  theme(axis.ticks.x = element_blank())
