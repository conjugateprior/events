\name{filter_eventdata}
\alias{filter_eventdata}
\title{Filter events data}
\usage{
  filter_eventdata(edo, fun, which)
}
\arguments{
  \item{edo}{Events data object}

  \item{fun}{Function that shoudl be applied}

  \item{which}{Which field should be filtered}
}
\value{
  Event data
}
\description{
  Applies a generic field filter to event data
}
\details{
  This function applies a filter function to event data. It
  is the workhorse function behind the \code{filter_}
  functions. You should use these in ordinary use.
}
\author{
  Will Lowe
}

