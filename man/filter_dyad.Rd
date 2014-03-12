\name{filter_dyad}
\alias{filter_dyad}
\title{Discard all but relevant actors}
\usage{
filter_dyad(edo, source = function(x) {     return(TRUE) },
  target = function(x) {     return(TRUE) })
}
\arguments{
  \item{edo}{Event data}

  \item{source}{Function that returns \code{TRUE} for
  source actor codes, or actor name.}

  \item{target}{Function that returns \code{TRUE} for
  target actor codes, or actor name.}
}
\value{
All events in which one of the sources does something to
one of the targets
}
\description{
Extracts a directed dyad
}
\details{
The \code{source} parameter identifies sources and the
\code{target} parameter specifies the target names.
}
\author{
Will Lowe
}
\seealso{
\code{\link{filter_codes}}, \code{\link{filter_time}},
\code{\link{filter_actors}}
}

