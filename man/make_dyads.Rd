\name{make_dyads}
\alias{make_dyads}
\title{Aggregate events to a regular time interval}
\usage{
  make_dyads(edo, scale = NULL,
    unit = c("week", "day", "month", "quarter", "year"),
    monday = TRUE, fun = mean, missing.data = NA)
}
\arguments{
  \item{edo}{Event data}

  \item{scale}{Name of an eventscale or \code{NULL} to
  create counts}

  \item{unit}{Temporal aggregation unit}

  \item{monday}{Whether weeks start on Monday. If
  \code{FALSE}, they start on Sunday}

  \item{fun}{Aggregation function.  Should take a vector
  and return a scalar}

  \item{missing.data}{What weeks with no data are assigned}
}
\value{
  A list of named dyadic aggregated time series
}
\description{
  Aggregates events to a regular time interval
}
\details{
  In an event data set S, assume that
  \eqn{A}=\code{length(actors(S))} actors
  \eqn{K}=\code{length(codes(S))} event codes occur.  This
  function creates \eqn{A^2} data streams labelled by the
  combination of source and target actors.  If \code{scale}
  is \code{NULL} these are \eqn{K}-dimensional time series
  of event counts. If \code{scale} names a scale that has
  been added to the event data \code{fun} is used to
  aggregate the events falling into each temporal interval.
  This creates a univariate interval valued time series for
  each directed dyad.
}
\author{
  Will Lowe
}

