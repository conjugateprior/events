\name{spotter}
\alias{spotter}
\title{Make a spotting function}
\usage{
  spotter(...)
}
\arguments{
  \item{...}{The items for which the new function should
  return \code{TRUE}}
}
\value{
  A function
}
\description{
  Hands back a function to spot the items it was given in
  (\code{...})
}
\details{
  This is a convenience function for creates a function
  that returns true for exact matches to its arguments.
}
\author{
  Will Lowe
}

