# Supplemental graphics functions that are not exported

#' Plot a circle
#' Plot a circle
#' @param x x-axis position of circle.
#' @param y y-axis position of circle.
#' @param r Radius of circle.
#' @param from Measurment in radians.
#' @param to Measurement in radians.
#' @param lines TBD
#' @param ... Additional arguments to \code{\link[graphics]{lines}}.
#'
#' @return Lines of a parametrized circle.
#'

circle <- function(x, y, r, from=0, to=2*pi, lines=FALSE, ...) {
  theta <- seq(from, to, length=100)
  if (lines)
    lines(x + r * cos(theta), y + r * sin(theta), ...)
  else polygon(x + r * cos(theta), y + r * sin(theta), ...)
}

#' Plot Fullcourt
#' Plot an NBA regulation full basketball court.
#'
#' @return Lines corresponding to a full basketball court.
#'
#' @export
#'

fullcourt_lines <- function() {
  rect(0, 0, 94, 50)
  points(c(5.25, 94 - 5.25), c(25, 25), cex = 2)
  # circle(94 - 5.25, 25, 9/12, ..., col = "#808080")
  segments(47, 0, 47, 50)
  circle(47, 25, 8)
  circle(47, 25, 2, col = "lightgray")
  theta1 <- acos((25 - 35 / 12) / 23.75)
  circle(5.25, 25, 23.75, -pi / 2 + theta1, pi / 2 - theta1, TRUE)
  circle(94 - 5.25, 25, 23.75, pi / 2 + theta1, 3 * pi / 2 - theta1, TRUE)
  segments(0, 35/12, 5.25 + 23.75 * sin(theta1), 35 / 12)
  segments(0, 50 - 35 / 12, 5.25 + 23.75 * sin(theta1), 50 - 35 / 12)
  segments(94, 35 / 12, 94 - 5.25 - 23.75 * sin(theta1), 35 / 12)
  segments(94, 50 - 35 / 12, 94 - 5.25 - 23.75 * sin(theta1), 50 - 35 / 12)
  circle(19, 25, 6, -pi/2, pi/2, TRUE)
  circle(19, 25, 6, pi/2, 3 * pi/2, TRUE, lty = 2)
  circle(94 - 19, 25, 6, pi/2, 3 * pi/2, TRUE)
  circle(94 - 19, 25, 6, -pi/2, pi/2, TRUE, lty = 2)
  circle(5.25, 25, 4, -pi/2, pi/2, TRUE)
  circle(94 - 5.25, 25, 4, pi/2, 3 * pi/2, TRUE)
  rect(0, 17, 19, 33, border = "gray")
  rect(94, 17, 94 - 19, 33, border = "gray")
}
