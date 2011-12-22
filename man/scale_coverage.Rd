\name{scale_coverage}
\alias{scale_coverage}
\title{Check coverage of scale for event data}
\usage{
  scale_coverage(sc, edo)
}
\arguments{
  \item{sc}{An eventscale}

  \item{edo}{Event data}
}
\value{
  Array of unscaleable event codes
}
\description{
  Checks coverage of scale for event data
}
\details{
  Returns an array of event codes that occur in an event
  data set but are not assigned values by the scale.  These
  are the codes that will, in subsequent processing, be
  assigned the scale's default value.
}
\author{
  Will Lowe
}

