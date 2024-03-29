library(dplyr)
library(tidyr)

plotSPC <- function(subgroup, point, mean, sigma, k = 3,
                    ucl.show = TRUE, lcl.show = TRUE,
                    band.show = TRUE, rule.show = TRUE,
                    ucl.max = Inf, lcl.min = -Inf,
                    label.x = "Subgroup", label.y = "Value") {
  # Plots control chart with ggplot
  ##
  # Args:
  # subgroup: Subgroup definition (for x-axis)
  # point: Subgroup sample values (for y-axis)
  # mean: Process mean value (for center line)
  # sigma: Process variation value (for control limits)
  # k: Specification for k-sigma limits above and below center line, default is 3
  # ucl.show: Visible upper control limit? Default is true
  # lcl.show: Visible lower control limit? Default is true
  # band.show: Visible bands between 1-2 sigma limits? Default is true
  # rule.show: Highlight run rule indicators in orange? Default is true
  # ucl.max: Maximum feasible value for upper control limit
  # lcl.min: Minimum feasible value for lower control limit
  # label.x: Specify x-axis label
  # label.y: Specify y-axis label

  df = data.frame(subgroup, point)
  df$ucl = pmin(ucl.max, mean + k*sigma)
  df$lcl = pmax(lcl.min, mean - k*sigma)
  warn.points = function(rule, num, den) {
    sets = mapply(seq, 1:(length(subgroup) - (den - 1)),
                  den:length(subgroup))
    hits = apply(sets, 2, function(x) sum(rule[x])) >= num
    intersect(c(sets[,hits]), which(rule))
  }
  orange.sigma = numeric()

  p = ggplot(data = df, aes(x = subgroup)) +
    geom_hline(yintercept = mean, col = "gray", size = 1)
  if (ucl.show) {
    p = p + geom_line(aes(y = ucl), col = "gray", size = 1)
  }
  if (lcl.show) {
    p = p + geom_line(aes(y = lcl), col = "gray", size = 1)
  }
  if (band.show) {
    p = p +
      geom_ribbon(aes(ymin = mean + sigma,
                      ymax = mean + 2*sigma), alpha = 0.1) +
      geom_ribbon(aes(ymin = pmax(lcl.min, mean - 2*sigma),
                      ymax = mean - sigma), alpha = 0.1)
    orange.sigma = unique(c(
      warn.points(point > mean + sigma, 4, 5),
      warn.points(point < mean - sigma, 4, 5),
      warn.points(point > mean + 2*sigma, 2, 3),
      warn.points(point < mean - 2*sigma, 2, 3)
    ))
  }
  df$warn = "blue"
  if (rule.show) {
    shift.n = round(log(sum(point!=mean), 2) + 3)
    orange = unique(c(orange.sigma,
                      warn.points(point > mean - sigma & point < mean + sigma, 15, 15),
                      warn.points(point > mean, shift.n, shift.n),
                      warn.points(point < mean, shift.n, shift.n)))
    df$warn[orange] = "orange"
  }
  df$warn[point > df$ucl | point < df$lcl] = "red"


 p +
    geom_line(aes(y = point), col = "royalblue3") +
    geom_point(data = df, aes(x = subgroup, y = point, col = warn)) +
    scale_color_manual(values = c("blue" = "royalblue3", "orange" = "orangered", "red" = "red3"), guide = FALSE) +
    labs(x = label.x, y = label.y) +
    theme_bw()
}
