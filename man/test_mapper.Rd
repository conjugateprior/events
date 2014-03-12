\name{test_mapper}
\alias{test_mapper}
\title{Test mapper function behaviour}
\usage{
test_mapper(mapper, data)
}
\arguments{
  \item{mapper}{a mapper function}

  \item{data}{a vector of character test data}
}
\value{
What the elements of \code{data} are mapped to, if anything
}
\description{
Tests the behaviour of a mapper function
}
\details{
This is a test function to runs a mapper function over a
set of strings and reports which strings are mapped and
what they are mapped to.  It's mostly useful to ensure that
any regular expressions given to \code{mapper} (i.e. when
re=TRUE) have the right behaviour before using the mapper
function to aggregate things from your data.
}
\author{
Will Lowe
}

