\name{one_a_day}
\alias{one_a_day}
\title{Apply the one-a-day filter}
\usage{
  one_a_day(edo)
}
\arguments{
  \item{edo}{Event data object}
}
\value{
  New event data object with duplicate events removed
}
\description{
  Tries to remove duplicate events
}
\details{
  This function removes duplicates of any event that occurs
  to the same source and target with the same event code,
  on the assumption that these are in fact the same event
  reported twice.

  This function can also be applied as part of
  \code{\link{read_keds}}
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{read_keds}}
}

