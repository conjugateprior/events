\name{mapper}
\alias{mapper}
\title{Make a mapper function}
\usage{
  mapper(lst, re = FALSE)
}
\arguments{
  \item{lst}{A list of string to pattern associations}
}
\value{
  A mapper function
}
\description{
  Creates mapper function
}
\details{
  This function creates and returns a function which
  renames arguments or returns them unchanged if no mapping
  is provided. The function is initialised using a list
  describing the reverse mapping.  For example, given
  \code{list(a=c(1,2), b=3)} a function is returned that
  turns 1 and 2 in 'a', 3 into 'b' and leaves all other
  arguments unchanged. When \code{re} is TRUE, name values
  are treated as regular expressions.  In the returned
  function the base R function \code{grepl} is used to find
  the first matching argument.  It has its default
  parameters: no PERL-style regexps, case sensitivity, and
  character rather than byte-based matching. When \code{re}
  is FALSE only exact matches trigger the mapping.
}
\author{
  Will Lowe
}

