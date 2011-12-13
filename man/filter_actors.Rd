\name{filter_actors}
\alias{filter_actors}
\title{Discard all but elevant actors}
\usage{
  filter_actors(edo, fun = function(x) {     return(TRUE)
    }, which = c("both", "target", "source"))
}
\arguments{
  \item{edo}{Event data}

  \item{fun}{Function that returns \code{TRUE} for actor
  codes that should not be discarded.}

  \item{which}{What actor roles should be filtered}
}
\value{
  Event data containing only actors that pass through
  \code{fun}
}
\description{
  Discards all but relevant actors
}
\details{
  The \code{which} parameter specifies whether the filter
  should be applied only to targets, only to sources, or to
  all actors in the event data.
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{filter_codes}}, \code{\link{filter_time}}
}

