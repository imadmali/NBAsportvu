#' Create a gif of an event
#' @param event An integer that identifies the event as a row in the data frame.
#' @param back An integer representing the number of time steps prior to the event.
#' @param N An integer representing the number of iterations (moving forward in time).
#' @param fname A string ending in \code{.gif}.
#'
#' @return A gif of the basketball play.
#'
#' @export

create_gif_frame <- function(event, back, loop_iter, static = FALSE, legend = TRUE) {
  if (legend)
    layout(rbind(c(1,1,1), c(2,3,4)), heights = c(2,1))
  par(mar = c(0.1, 2.1, 0.1, 2.1))
  plot(0, type  = "n", xlim = c(0,94), ylim = c(0,50), xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", bty = "n")
  draw.fullcourt()
  plot_shot(event, loop = loop_iter, back = back)
  if (legend)
    plot_legend(event, loop = loop_iter, back = back)
}

shot <- function(index) {
  col_select <- grep("_event", colnames(game))
  col_names <- colnames(game[,col_select])

  active_player <- game[index, col_select] %in% 1:4
  shooter <- gsub("_event", "", col_names[active_player])
  shooter_id <- game[index, paste0(shooter,"_ent")]

  shot_info <- game[index, c("x", "y", "z", paste0(shooter, c("_x", "_y", "_event")), "game_clock", "quarter")]
  player_info <- players[players$player_id == shooter_id, c("player_id", "firstname", "lastname")]

  return(cbind(player_info, shot_info))
}

id2name <- function(id) {
  sel <- sapply(id, function (x) {which(players$player_id == x)}, USE.NAMES = FALSE)
  return(paste(players$firstname[sel], players$lastname[sel]))
}

whose_playing <- function(index) {
  out <- list()
  a_sel <- colnames(game) %in% paste0("a",1:5,"_ent")
  h_sel <- colnames(game) %in% paste0("h",1:5,"_ent")
  out$MIA <- id2name(game[index, a_sel])
  out$BRK <- id2name(game[index, h_sel])
  return(out)
}

get_time <- function(index, loop) {
  sel <- index
  out <- list(quarter = game$quarter[sel],
              game_clock = game$game_clock[sel])
}

plot_legend <- function(index, loop, back) {
  sel <- index - back + loop
  oncourt <- whose_playing(sel)
  timeinfo <- get_time(sel, loop)
  event_type <- shot(index)
  event_type <- event_type[[grep("_event", colnames(event_type))]]
  if (event_type %in% c(1,3))
    event_str <- " (hit)"
  else if (event_type %in% c(2,4))
    event_str <- " (miss)"
  else
    stop("Event not recognized.")
  plot.new()
  text(0.5, seq(0.9,0.1,length.out = 6), c("BKN", paste0(1:5, ". ", oncourt$BRK)), col = "#000000", font = 2)
  plot.new()
  text(0.5, seq(0.9,0.1,length.out = 6), c("MIA", paste0(1:5, ". ", oncourt$MIA)), col = "#990000", font = 2)
  plot.new()
  text(0.1, 0.9, paste0("Quarter: ", timeinfo$quarter), pos = 4)
  text(0.1, 0.5, paste0("Game Clock: ", timeinfo$game_clock), pos = 4)
  text(0.1, 0.1, paste0("Event: ", event_type, event_str), pos = 4)
}

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
