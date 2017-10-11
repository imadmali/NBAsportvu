#' Convert Game Clock Time in play-by-play Data
#'
#' Convert the \code{PCTIMESTRING} variable to seconds to match the
#' \code{game_clock} variable in the data frame returned by the
#' \code{\link[NBAsportvu]{sportvu_df}} function.
#'
#' @param pbp Play-by_play data frame (e.g. an object returned by
#'   \code{\link[NBAapi]{get_pbp}}). One of the columns must contain the
#'   \code{PCTIMESTRING} variable.
#' @return Returns the object given with an additional variable
#'   \code{game_clock} that represents \code{PCTIMESTRING} in seconds.
#'
#' @examples
#' \dontrun{
#' game <- sportvu_df("~/path_to_file/0021500411.json")
#' }
#'
#' @export

convert_time <- function(pbp) {
  pbp$game_clock <- as.numeric(lubridate::seconds(lubridate::ms(pbp$PCTIMESTRING)))
  return(pbp)
}
