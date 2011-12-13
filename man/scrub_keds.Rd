\name{scrub_keds}
\alias{scrub_keds}
\title{Remove well-known noise from KEDS event data file}
\usage{
  scrub_keds(edo)
}
\arguments{
  \item{edo}{An event data object}
}
\value{
  Event data
}
\description{
  Removes well-known noise from KEDS output files
}
\details{
  This function applies the regular expression based
  cleaning routine from the KEDS website.  This is a direct
  translation from the original PERL which replaces capital
  'O's and small 'l's with 0 and 1 respectively and removes
  the event code '---]', on the assumption that these are
  all output noise.
}
\author{
  Will Lowe
}
\seealso{
  \code{\link{read_keds}}
}

