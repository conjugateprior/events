\name{filter_codes}
\alias{filter_codes}
\title{Discard all but relevant event codes}
\usage{
  filter_codes(edo, fun = function(x) {     return(TRUE) })
}
\arguments{
  \item{edo}{Event data}

  \item{fun}{Function that returns \code{TRUE} or event
  codes that should not be discarded}
}
\value{
  Event data containing only events that pass through
  \code{fun}
}
\description{
  Discards all but relevant event codes
}
\details{
  Applies the filter function to each event code to see
  whether to keep the observation.
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{filter_actors}}, \code{\link{filter_time}}
}

