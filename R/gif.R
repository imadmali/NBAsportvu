#' Plot an Event.
#' @param obj A subset of the data frame object returned from the
#'   \code{\link[NBAsportvu]{sportvu_df}} function.
#' @param loop Integer used to loop over each time point of \code{obj} when
#'   using the \code{\link[animation]{saveGIF}} function.
#' @param static Logical variable which defaults to \code{FALSE}. If looping
#'   over \code{obj} for use with \code{\link[animation]{saveGIF}} then this
#'   should be set to \code{TRUE}.
#'
#' @return A plot of a basketball event.
#'
#' @export

plot_shot <- function(obj, loop = 1, static = FALSE) {
  past <- 1
  if (static) {
    future <- nrow(obj)
  }
  else {
    future <- past + loop
  }
  # trajectory
  lines(obj$x[past:future], obj$y[past:future], col = "#ffa50020", lwd = 3)
  lines(obj$a1_x[past:future], obj$a1_y[past:future], col = "#99000020", lwd = 3)
  lines(obj$a2_x[past:future], obj$a2_y[past:future], col = "#99000020", lwd = 3)
  lines(obj$a3_x[past:future], obj$a3_y[past:future], col = "#99000020", lwd = 3)
  lines(obj$a4_x[past:future], obj$a4_y[past:future], col = "#99000020", lwd = 3)
  lines(obj$a5_x[past:future], obj$a5_y[past:future], col = "#99000020", lwd = 3)
  lines(obj$h1_x[past:future], obj$h1_y[past:future], col = "#00000020", lwd = 3)
  lines(obj$h2_x[past:future], obj$h2_y[past:future], col = "#00000020", lwd = 3)
  lines(obj$h3_x[past:future], obj$h3_y[past:future], col = "#00000020", lwd = 3)
  lines(obj$h4_x[past:future], obj$h4_y[past:future], col = "#00000020", lwd = 3)
  lines(obj$h5_x[past:future], obj$h5_y[past:future], col = "#00000020", lwd = 3)
  # ending position
  points(obj$a1_x[future], obj$a1_y[future], col = "#99000090", pch = 20, cex = 6)
  points(obj$a2_x[future], obj$a2_y[future], col = "#99000090", pch = 20, cex = 6)
  points(obj$a3_x[future], obj$a3_y[future], col = "#99000090", pch = 20, cex = 6)
  points(obj$a4_x[future], obj$a4_y[future], col = "#99000090", pch = 20, cex = 6)
  points(obj$a5_x[future], obj$a5_y[future], col = "#99000090", pch = 20, cex = 6)
  points(obj$h1_x[future], obj$h1_y[future], col = "#00000090", pch = 20, cex = 6)
  points(obj$h2_x[future], obj$h2_y[future], col = "#00000090", pch = 20, cex = 6)
  points(obj$h3_x[future], obj$h3_y[future], col = "#00000090", pch = 20, cex = 6)
  points(obj$h4_x[future], obj$h4_y[future], col = "#00000090", pch = 20, cex = 6)
  points(obj$h5_x[future], obj$h5_y[future], col = "#00000090", pch = 20, cex = 6)
  points(obj$x[future], obj$y[future], col = "#ffa500", cex = 3.5)
  circle(obj$x[future], obj$y[future], 9.55/2/12, col = "#ffa500", border = NA)
  # player identifier
  text(obj$a1_x[future], obj$a1_y[future], "1", col = "#ffffff", cex = 1, font = 2)
  text(obj$a2_x[future], obj$a2_y[future], "2", col = "#ffffff", cex = 1, font = 2)
  text(obj$a3_x[future], obj$a3_y[future], "3", col = "#ffffff", cex = 1, font = 2)
  text(obj$a4_x[future], obj$a4_y[future], "4", col = "#ffffff", cex = 1, font = 2)
  text(obj$a5_x[future], obj$a5_y[future], "5", col = "#ffffff", cex = 1, font = 2)
  text(obj$h1_x[future], obj$h1_y[future], "1", col = "#ffffff", cex = 1, font = 2)
  text(obj$h2_x[future], obj$h2_y[future], "2", col = "#ffffff", cex = 1, font = 2)
  text(obj$h3_x[future], obj$h3_y[future], "3", col = "#ffffff", cex = 1, font = 2)
  text(obj$h4_x[future], obj$h4_y[future], "4", col = "#ffffff", cex = 1, font = 2)
  text(obj$h5_x[future], obj$h5_y[future], "5", col = "#ffffff", cex = 1, font = 2)
}
