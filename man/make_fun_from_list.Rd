\name{make_fun_from_list}
\alias{make_fun_from_list}
\title{Create a mapping function from list}
\usage{
  make_fun_from_list(lst)
}
\arguments{
  \item{lst}{A list}
}
\value{
  A function that inverts the mapping specified by
  \code{lst}
}
\description{
  Creates a mapping function from list
}
\details{
  Turns a list of the form \code{list(a=c(1,2), b=3)} into
  a function that returns 'a' when given 1 or 2 as
  argument, 'b' when given 3 and otherwise gives back its
  argument unchanged.

  This is a convenience function to make it possible to
  specify onto mappings using lists.  The \code{map_*}
  functions use it internally, but you might find a a use
  for it.
}
\author{
  Will Lowe
}

