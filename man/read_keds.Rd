\name{read_keds}
\alias{read_keds}
\title{Read KEDS events files}
\usage{
  read_keds(d, keep.quote = FALSE, keep.label = TRUE,
    one.a.day = TRUE, scrub.keds = TRUE,
    date.format = "\%y\%m\%d")
}
\arguments{
  \item{d}{Names of files of KEDS/TABARI output}

  \item{keep.quote}{Whether the exact noun phrase be
  retained}

  \item{keep.label}{Whether the label for the event code
  should be retained}

  \item{one.a.day}{Whether to apply the duplicate event
  remover}

  \item{scrub.keds}{Whether to apply the data cleaner}

  \item{date.format}{How dates are represented in the first
  column}
}
\value{
  An event data set
}
\description{
  Reads KEDS event data output files
}
\details{
  Reads KEDS output and optionally applies the
  \code{\link{scrub_keds}} cleaning function and the
  \code{\link{one_a_day}} duplicate removal filter.  This
  function is thin wrapper around \code{read.csv}.

  This function assumes that \code{d} are a vector of
  KEDS/TABARI output files. These are assumed to be tab
  separated text files wherein the first field is a date in
  \code{yymmdd} format or as specified by
  \code{date.format}, the second and third fields are actor
  codes, the fourth field is an event code, and the fifth
  field is a text label for the event type, and the sixth
  field is a quote - some kind of text from which the event
  code was inferred.  Label and quote are optional and can
  be discarded when reading in.
}
\author{
  Will Lowe
}

