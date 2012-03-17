\name{test_spotter}
\alias{test_spotter}
\title{Test spotter function coverage}
\usage{
  test_spotter(spotter, data, unspotted = TRUE)
}
\arguments{
  \item{spotter}{a spotter function}

  \item{data}{a vector of character test data}

  \item{unspotted}{whether to also list strings to which
  the spotter returns FALSE}
}
\value{
  the elements of \code{data} that the spotter spots, i.e.
  returns TRUE to, and also optionally the elements which
  the spotter ignores, i.e. returns FALSE to.
}
\description{
  Tests the coverage of a spotter function
}
\details{
  This is a test function to runs a spotter function over a
  set of strings and reports which strings are spotted and
  which are not.  It's mostly useful to ensure that any
  regular expressions given to \code{spotter} (i.e. when
  re=TRUE) have the right coverage before using the spotter
  function to select events from your data.
}
\author{
  Will Lowe
}

