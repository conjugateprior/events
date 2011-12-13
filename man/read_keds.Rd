\name{read_keds}
\alias{read_keds}
\title{Read KEDS events file}
\usage{
  read_keds(d, keep.quote = FALSE, keep.desc = TRUE,
    one.a.day = TRUE, scrub.keds = TRUE)
}
\arguments{
  \item{d}{Filename}

  \item{keep.quote}{Whether the exact noun phrase be
  retained}

  \item{keep.desc}{Whether the label for the event code
  should be retained}

  \item{one.a.day}{Whether to apply the duplicate event
  remover}

  \item{scrub.keds}{Whether to apply the data cleaner}
}
\value{
  Event data
}
\description{
  Reads a KEDS event data output file
}
\details{
  Reads KEDS output and optionally applies the
  \code{\link{scrub_keds}} cleaning function and the
  \code{\link{one_a_day}} duplicate removal filter.  This
  function is thin wrapper around \code{read.csv}.

  This function assumes that output data is a tab separated
  text file wherein the first field is a date in
  \code{yymmdd} format, the second and third fields are
  actor codes, the fourth field is an string event code,
  and the fifth field is a matched noun phrase.  The last
  two fields are optional and can be discarded when reading
  in.
}
\author{
  Will Lowe
}

