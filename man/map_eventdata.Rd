\name{map_eventdata}
\alias{map_eventdata}
\title{Aggregate field values}
\usage{
  map_eventdata(edo, fun, which)
}
\arguments{
  \item{edo}{Event data}

  \item{fun}{Function specifying the aggregation mapping}

  \item{which}{Name of the field to map}
}
\value{
  Event data with new fields
}
\description{
  Aggregates event data fields
}
\details{
  This function applies a mapping/aggregration function to
  event data. It is the workhorse function behind the
  \code{map_} functions. You should use these in ordinary
  use.
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{map_codes}}, \code{\link{map_actors}}
}

