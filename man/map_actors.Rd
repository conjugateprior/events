\name{map_actors}
\alias{map_actors}
\title{Aggregate actor codes}
\usage{
map_actors(edo, fun = function(x) {     return(x) })
}
\arguments{
  \item{edo}{Event data}

  \item{fun}{Function specifying the aggregation mapping}
}
\value{
Event data with new actor codes
}
\description{
Aggregates actor codes
}
\details{
The function relabels actor codes according to \code{fun}.
\code{mapper} provides an easy way to make a function
specifying what things should now be called.

This function can also be used as a renaming function, but
it is most useful when multiple codes should be treated as
equivalent.
}
\author{
Will Lowe
}
\seealso{
\code{\link{map_codes}}
}

