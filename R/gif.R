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
  obj_game <- obj$game
  home <- obj$players$home
  visitor <- obj$players$visitor
  home_abb <-  obj$players$home$abb[1]
  visitor_abb <- game$players$visitor$abb[1]
  # stuff for gif
  past <- 1
  if (static) {
    future <- nrow(obj_game)
  }
  else {
    future <- past + loop
  }
  # find team colors
  team_colors <- team_col_map()
  if (home_abb == "BKN") {
    home_col <- "#000000"
    visitor_col <- team_colors[[which(names(team_colors) == visitor_abb)]][1]
  }
  else if (visitor_abb == "BKN") {
    home_col <- team_colors[[which(names(team_colors) == home_abb)]][1]
    visitor_col <- "#000000"
  }
  else {  # Need to do greatest distance measure
    home_col <- team_colors[[which(names(team_colors) == home_abb)]][1]
    visitor_col_full <- team_colors[[which(names(team_colors) == visitor_abb)]]
    visitor_col <- color_dist(home_col, visitor_col_full,
                              omit = c("#FFFFFF"))
  }
  # transparent colors
  home_col <- paste0(home_col, "80")
  visitor_col <- paste0(visitor_col, "80")
  # trajectory
  lines(obj_game$x[past:future], obj_game$y[past:future], col = "#ffa50020", lwd = 3)
  lines(obj_game$a1_x[past:future], obj_game$a1_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a2_x[past:future], obj_game$a2_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a3_x[past:future], obj_game$a3_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a4_x[past:future], obj_game$a4_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a5_x[past:future], obj_game$a5_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$h1_x[past:future], obj_game$h1_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h2_x[past:future], obj_game$h2_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h3_x[past:future], obj_game$h3_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h4_x[past:future], obj_game$h4_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h5_x[past:future], obj_game$h5_y[past:future], col = home_col, lwd = 3)
  # ending position
  points(obj_game$a1_x[future], obj_game$a1_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a2_x[future], obj_game$a2_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a3_x[future], obj_game$a3_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a4_x[future], obj_game$a4_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a5_x[future], obj_game$a5_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$h1_x[future], obj_game$h1_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h2_x[future], obj_game$h2_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h3_x[future], obj_game$h3_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h4_x[future], obj_game$h4_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h5_x[future], obj_game$h5_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$x[future], obj_game$y[future], col = "#ffa500", cex = 3.5)
  circle(obj_game$x[future], obj_game$y[future], 9.55/2/12, col = "#ffa500", border = NA)
  # player identifier
  text(obj_game$a1_x[future], obj_game$a1_y[future], jersey(obj$game$a1_ent, visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a2_x[future], obj_game$a2_y[future], jersey(obj$game$a2_ent, visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a3_x[future], obj_game$a3_y[future], jersey(obj$game$a3_ent, visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a4_x[future], obj_game$a4_y[future], jersey(obj$game$a4_ent, visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a5_x[future], obj_game$a5_y[future], jersey(obj$game$a5_ent, visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h1_x[future], obj_game$h1_y[future], jersey(obj$game$h1_ent, home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h2_x[future], obj_game$h2_y[future], jersey(obj$game$h2_ent, home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h3_x[future], obj_game$h3_y[future], jersey(obj$game$h3_ent, home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h4_x[future], obj_game$h4_y[future], jersey(obj$game$h4_ent, home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h5_x[future], obj_game$h5_y[future], jersey(obj$game$h5_ent, home), col = "#ffffff", cex = 1, font = 2)
}
