#' Extract Shots from SportVU Data
#'
#' Extract all field goals from the SportVU data frame within a certain time margin of the field goal occuring.
#'
#' @param obj A data frame returned from \code{\link[NBAsportvu]{sportvu_df}}.
#' @param pbp Play-by_play data frame (e.g. an object returned by
#'   \code{\link[NBAapi]{get_pbp}}). One of the columns must contain the
#'   \code{PCTIMESTRING} variable.
#' @param margin Margin around the game clock for each (shot) event. A vector of
#'   length two with the first element indicating how far back to go and the
#'   second element indicating how far forward to go (in seconds).
#'
#' @export

extract_shots <- function(obj, pbp, margin = c(3,3)) {
  # filter field goals made/missed and free throws
  pbp <- filter(pbp, EVENTMSGTYPE %in% 1:3)
  # check if time has been converted
  if (!any(names(pbp2) == "game_clock"))
    pbp <- convert_time(pbp)
  # trim time around shot
  out <- list()
  for (i in 1:nrow(pbp)) {
    obj_filt <- dplyr::filter(obj, event_id == pbp$EVENTNUM[i])
    upr_time <- pbp$game_clock[i] + 3
    lwr_time <- pbp$game_clock[i] - 3
    indxs <- which(obj_filt$game_clock <= upr_time & obj_filt$game_clock >= lwr_time)
    out[[i]] <- obj_filt[indxs,]
    # out[[i]] <- obj_filt
  }
  out <- do.call("rbind", out)
  return(out)
}
