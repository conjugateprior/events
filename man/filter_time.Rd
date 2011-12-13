\name{filter_time}
\alias{filter_time}
\title{Restrict events to a time period}
\usage{
  filter_time(edo, start = min(edo$date),
    end = max(edo$date))
}
\arguments{
  \item{edo}{Event data}

  \item{start}{Something convertable to a \code{Date}
  object}

  \item{end}{Something convertable to a \code{Date} object}
}
\value{
  Event data restricted to a time period
}
\description{
  Restricts events to a time period
}
\details{
  Restricts events on or after \code{start} and before or
  on \code{end}.
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{filter_codes}}, \code{\link{filter_actors}}
}

