\name{make_fun_from_list}
\alias{make_fun_from_list}
\title{Create a mapping function from list}
\usage{
  make_fun_from_list(lst, re = FALSE)
}
\arguments{
  \item{lst}{A list}

  \item{re}{Whether to interpret the name values as regular
  expressions}
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
  argument unchanged.  Name values are treated as regular
  expressions if \code{re}=TRUE.

  This is a convenience function to make it possible to
  specify onto mappings using lists.  The \code{mapper}
  uses it internally, but you might find a use for it too.
}
\author{
  Will Lowe
}

