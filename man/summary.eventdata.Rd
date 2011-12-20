\name{summary.eventdata}
\alias{summary.eventdata}
\title{Summarise event data}
\usage{
  \method{summary}{eventdata} (object, ...)
}
\arguments{
  \item{object}{Event data object}

  \item{...}{Not used}
}
\value{
  A short description of the event data
}
\description{
  Summarises a set of event data
}
\details{
  This is a compact summary of an event data object.  For
  more detail consult the object itself.  Currently it is
  simply a data.frame with conventionally named column
  names, but that almost certainly will change to deal with
  larger datasets in later package versions. If your code
  uses the package's accessor functions then you won't feel
  a thing when this happens.
}
\author{
  Will Lowe
}

