\name{map_codes}
\alias{map_codes}
\title{Aggregate event codes}
\usage{
  map_codes(edo, fun = function(x) {     return(x) })
}
\arguments{
  \item{edo}{Event data}

  \item{fun}{Function or list specifying the aggregation
  mapping}
}
\value{
  Event data with new event codes
}
\description{
  Aggregates event codes
}
\details{
  This function relabels event codes according to
  \code{fun}, which may either be a function that returns
  the new name of an event when handed the old one, or a
  list with entries of the form: \code{lst[[newname]] =
  c(oldname1, oldname2)}.

  It can also be used as a renaming function, but it is
  most useful when multiple codes should be treated as
  equivalent.
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{map_actors}}
}

