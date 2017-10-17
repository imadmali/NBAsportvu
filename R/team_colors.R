#' Team Color Map
#'
#' Mapping of official team colors.
#'
#' @return A list with length equal to the number of teams. Each team name
#'   element in the list corresponds to a vector of hex colors that represent
#'   the team's official colors.
#'

team_col_map <- function() {
  list(
  "ATL" = c("#E03A3E", "#C3D600", "#FFFFFF", "#000000"),
  "BKN" = c("#000000", "#FFFFFF"),
  "BOS" = c("#008348", "#000000", "#FFFFFF", "#FFD700", "#C0C0C0"),
  "CHA" = c("#1D1160", "#008CA8", "#FFFFFF", "#A1A1A4"),
  "CHI" = c("#CE1141", "#FFFFFF", "#000000"),
  "CLE" = c("#860038", "#FDBB30", "#002D62", "#FFFFFF"),
  "DAL" = c("#007DC5", "#C4CED3", "#20385B", "#000000", "#FFFFFF"),
  "DEN" = c("#4FA8FF", "#FFB20F", "#004770", "#FFFFFF"),
  "DET" = c("#006BB6", "#ED174C" , "#001F70", "#FFFFFF"),
  "GSW" = c("#006BB6", "#FFC72D"),
  "HOU" = c("#CE1141", "#C4CED3", "#FDB927", "#FFFFFF", "#000000"),
  "IND" = c("#00275D", "#FFC633", "#BEC0C2", "#FFFFFF"),
  "LAC" = c("#ED174C", "#006BB6", "#BEC0C2", "#FFFFFF", "#00285D"),
  "LAL" = c("#552582", "#FDB927", "#FFFFFF"),
  "MEM" = c("#23375B", "#6189B9", "#BBD1E4", "#FFD432"),
  "MIA" = c("#98002E", "#F9A01B", "#FFFFFF", "#000000"),
  "MIL" = c("#00471B", "#EEE1C6", "#0077C0", "#FFFFFF", "#000000"),
  "MIN" = c("#002B5C", "#C6CFD4", "#7AC143", "#005083", "#FFFFFF"),
  "NOP" = c("#002B5C", "#B4975A", "#E31937","#FFFFFF"),
  "NYK" = c("#006BB6", "#F58426", "#FFFFFF", "#BEC0C2"),
  "OKC" = c("#007DC3", "#F05133", "#FDBB30", "#002D62"),
  "ORL" = c("#007DC5", "#C4CED3", "#FFFFFF", "#000000"),
  "PHI" = c("#006BB6", "#ED174C", "#FFFFFF"),
  "PHX" = c("#E56020", "#1D1160", "#63717A", "#FFFFFF", "#000000"),
  "POR" = c("#F0163A", "#B6BFBF", "#FFFFFF", "#000000"),
  "SAC" = c("#724C9F", "#8E9090", "#FFFFFF", "#000000"),
  "SAS" = c("#B6BFBF", "#FFFFFF", "#000000"),
  "TOR" = c("#CE1141", "#C4CED3", "#FFFFFF", "#000000"),
  "UTA" = c("#002B5C", "#F9A01B", "#00471B", "#BEC0C2"),
  "WAS" = c("#002566", "#F5002F", "#C2CCCC", "#FFFFFF")
  )
}

#' Find Colors with Greatest Distance
#' Find the hex color in the color vector \code{b} that has the greatest (Euclidean) distance from the color \code{a}.
#' @param a Single color as a hex value.
#' @param b Color vector as hex values.
#' @param omit Color vector as hex values to omit from distance calculation.
#'
#' @return The hex color from \code{b} that has the largest distance from \code{a}
#'
color_dist <- function(a, b, omit = NULL) {
  if (!is.null(omit)) {
    indx <- !(b %in% omit)
    b <- b[indx]
  }
  b_dist <- sapply(b,
                   FUN = function(x, a){dist(rbind(t(col2rgb(a)), t(col2rgb(x))))}, a = a)
  b_max <- which(max(b_dist) == b_dist)
  return(names(b_max))
}
