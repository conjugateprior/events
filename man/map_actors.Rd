\name{map_actors}
\alias{map_actors}
\title{Aggregate actor codes}
\usage{
  map_actors(edo, fun = function(x) {     return(x) })
}
\arguments{
  \item{edo}{Event data}

  \item{fun}{Function or list specifying the aggregation
  mapping}
}
\value{
  Event data with new actor codes
}
\description{
  Aggregates actor codes
}
\details{
  The function relabels actor codes according to the
  filter. The filter may either be a function that returns
  the new name of an event when handed the old one, or a
  list structured like \code{list(fruit=c('tomato',
  'orange'), veg=c('red pepper', 'carrot'))}.

  This function can also be used as a renaming function,
  but it is most useful when multiple codes should be
  treated as equivalent.
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{map_codes}}
}

