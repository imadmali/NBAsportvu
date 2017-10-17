#' Jersey Number For internal use. Get jersey number from player ID.
#'
#' @param playerid Player ID number as a number.
#' @param player_table Table from SportVU indicating the player's name, jersey
#'   number, ID, position, and team affiliation.
#'
#' @return Jersey number that corresponds to player ID.
#'

jersey <- function(playerid, player_table) {
  return(player_table$jersey[which(player_table$playerid == playerid)])
}

