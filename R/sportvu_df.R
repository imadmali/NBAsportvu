#' Convert SportVU json to Data Frame.
#'
#' Convert SportVU json file of NBA player tracking data to a data frame. Note,
#' due to the size of the player tracking data a single game can take a few
#' minutes to convert.
#'
#' @param path Path to SportVU json file as a string.
#' @return A data frame that includes motion tracking information for a game.
#'   Variables \code{quarter}, \code{game_clock}, and \code{shot_clock} are self
#'   explanatory. Variables \code{x} and \code{y} denotes the spatial location
#'   of the ball, and \code{z} denotes the height of the ball. For the remaining
#'   variables, the first letter in the variable name is either \code{a} for the
#'   away team or \code{h} for the home team. Variables \code{a*_team} and
#'   \code{a*_ent} are the team and player identifiers. The spatial coordinates
#'   of each player are denoted by \code{a*_x} and \code{a*_y}.
#'
#' @examples
#' \dontrun{
#' game <- sportvu_df("~/path_to_file/0021500411.json")
#' }
#'
#' @import dplyr
#' @export

sportvu_df <- function(path) {
  cat("... reading game data.\n")
  raw_game <- jsonlite::fromJSON(path)
  game_id <- raw_game$gameid
  cat("...... restructuring game info.\n")
  var1 <- NULL
  var2 <- NULL
  var3 <- NULL
  var4 <- NULL
  var5 <- NULL
  var6 <- NULL
  var7 <- NULL
  for (i in 1:length(raw_game$events$moments)) {
    var1 <- append(var1, sapply(raw_game$events$moments[[i]], "[[", 1))
    var2 <- append(var2, sapply(raw_game$events$moments[[i]], "[[", 2))
    var3 <- append(var3, sapply(raw_game$events$moments[[i]], "[[", 3))
    var4 <- append(var4, sapply(raw_game$events$moments[[i]], "[[", 4))
    var5 <- append(var5, sapply(raw_game$events$moments[[i]], "[[", 5))
    var6 <- append(var6, sapply(raw_game$events$moments[[i]], "[", 6))
    var7 <- append(var7, rep(raw_game$events$eventId[i], length(raw_game$events$moments[[i]])))
  }
  var4 <- lapply(var4, function(x) {ifelse(is.null(x), NA, x)})
  var5 <- lapply(var5, function(x) {ifelse(is.null(x), NA, x)})
  var1 <- unlist(var1)
  var2 <- unlist(var2)
  var3 <- unlist(var3)
  var4 <- unlist(var4)
  var5 <- unlist(var5)

  df <- data.frame(var1,var2,var3,var4,var5,var7,
                   stringsAsFactors = FALSE)

  cat("...... restructuring player info.\n")
  format_player_info <- function(player_moment_matrix, home_id, away_id) {
    b_sel <- which(player_moment_matrix[,1] == -1)
    h_sel <- which(player_moment_matrix[,1] == home_id)
    a_sel <- which(player_moment_matrix[,1] == away_id)
    b_out <- player_moment_matrix[b_sel,]
    h_out <- c(t(player_moment_matrix[h_sel,]))
    a_out <- c(t(player_moment_matrix[a_sel,]))
    if (length(b_out) < 5)
      b_out <- c(b_out, rep(NA, 5 - length(b_out)))
    if (length(h_out) < 25)
      h_out <- c(h_out, rep(NA, 25 - length(h_out)))
    if (length(a_out) < 25)
      a_out <- c(a_out, rep(NA, 25 - length(a_out)))
    out <- c(b_out, a_out, h_out)
    return(out)
  }

  home_id <- raw_game$events$home$teamid[1]
  away_id <- raw_game$events$visitor$teamid[1]

  players_df <- lapply(var6, format_player_info, home_id = home_id, away_id = away_id)
  players_df <- do.call("rbind", players_df)

  cat("...... combining game/player info.\n")

  ranges <- matrix(NA, nrow = 12, ncol = 2)
  ranges[1,] <- range(players_df[,1], na.rm = T) == c(-1,-1)
  ranges[2,] <- range(players_df[,2], na.rm = T) == c(-1,-1)
  for (i in 1:10)
    ranges[i+2,] <- range(players_df[,(10 + (i - 1) * 5)], na.rm = T) == c(0,0)

  if (all(ranges)) {
    col_nums <- c(1,2,seq(10,55,by=5))
    players_df <- players_df[,-col_nums]
  } else {
    stop("Problem when dropping columns out of 'players_df' matrix.")
  }
  header <- c("quarter", "unknown1", "game_clock", "shot_clock", "unknown2", "event_id",
              "x", "y", "z")
  for (i in 1:5)
    header <- append(header, c(paste0("a", i, "_team"), paste0("a", i, "_ent"), paste0("a", i, "_x"), paste0("a", i, "_y")))
  for (i in 1:5)
    header <- append(header, c(paste0("h", i, "_team"), paste0("h", i, "_ent"), paste0("h", i, "_x"), paste0("h", i, "_y")))
  full_df <- data.frame(df, players_df, stringsAsFactors = FALSE)
  colnames(full_df) <- header

  full_df$event_id <- as.numeric(full_df$event_id)
  return(full_df)
}
