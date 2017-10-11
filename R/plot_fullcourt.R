#' Plot Full Court
#'
#' Plot the NBA regulation court. Wrapper for
#' \code{\link[NBAsportvu]{fullcourt_lines}}.
#'
#' @return A plot of the NBA regulation court.
#'
#' @examples
#' \dontrun{
#' plot_fullcourt()
#' }
#'
#' @export

plot_fullcourt <- function() {
  # court is (0,94) x (0,50)
  plot(0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       xlim = c(0,94), ylim = c(0,50))
  fullcourt_lines()
}
