\name{spotter}
\alias{spotter}
\title{Make a spotter function}
\usage{
  spotter(..., re = TRUE)
}
\arguments{
  \item{...}{Patterns, matches to which the returned
  function should return \code{TRUE}}
}
\value{
  A spotter function
}
\description{
  Creates spotter function
}
\details{
  This is a convenience function that creates and returns a
  function which returns TRUE when any of its arguments
  match one of the patterns given here. When \code{re} is
  TRUE, arguments to this function are matched as regular
  expressions in the returned function using the base R
  function \code{grepl} with its default parameters. These
  currently imply no PERL-style regexps, case sensitivity,
  and character rather than byte-based matching. When
  \code{re} is FALSE any exact match is sufficient for the
  returned function to return TRUE.
}
\author{
  Will Lowe
}

