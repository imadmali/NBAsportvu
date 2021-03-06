% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{convert_time}
\alias{convert_time}
\title{Convert Game Clock Time in play-by-play Data}
\usage{
convert_time(pbp)
}
\arguments{
\item{pbp}{Play-by_play data frame (e.g. an object returned by
\code{\link[NBAapi]{get_pbp}}). One of the columns must contain the
\code{PCTIMESTRING} variable.}
}
\value{
Returns the object given with an additional variable
  \code{game_clock} that represents \code{PCTIMESTRING} in seconds.
}
\description{
Convert the \code{PCTIMESTRING} variable to seconds to match the
\code{game_clock} variable in the data frame returned by the
\code{\link[NBAsportvu]{sportvu_df}} function.
}
\examples{
\dontrun{
game <- sportvu_df("~/path_to_file/0021500411.json")
}

}
