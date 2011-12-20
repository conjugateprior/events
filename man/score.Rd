\name{score}
\alias{score}
\title{Score event codes with an event scale}
\usage{
  score(eventscale, codes)
}
\arguments{
  \item{eventscale}{An event scale}

  \item{codes}{Event codes}
}
\value{
  Numerical values for each event codes from the scale
}
\description{
  Gets scale scores for event codes
}
\details{
  Returns an array of scores corresponding to the the
  second argument's scale values or the scale's default
  value if not recognized.

  You should use this function to avoid relying on the
  internal structure of event scales.  They are currently
  lists, but this may change.
}
\author{
  Will Lowe
}

