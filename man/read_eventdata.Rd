\name{read_eventdata}
\alias{read_eventdata}
\title{Read event data files}
\usage{
  read_eventdata(d, col.format = "D.STC", one.a.day = TRUE,
    scrub.keds = TRUE, date.format = "\%y\%m\%d",
    sep = "\t", head = FALSE)
}
\arguments{
  \item{d}{Names of event data files}

  \item{col.format}{Format for columns in d (see details)}

  \item{one.a.day}{Whether to apply the duplicate event
  remover}

  \item{scrub.keds}{Whether to apply the data cleaner}

  \item{date.format}{How dates are represented in the
  orginal file}

  \item{sep}{File separator}

  \item{head}{Whether there is a header row in d}
}
\value{
  An event data set
}
\description{
  Reads event data output files in free format
}
\details{
  Reads event data output and optionally applies the
  \code{\link{scrub_keds}} cleaning function and the
  \code{\link{one_a_day}} duplicate removal filter.

  This function assumes that \code{d} is a vector of output
  files. These are assumed to be \code{sep}-separated text
  files.  The column ordering is given by the
  \code{col.format} parameter: \itemize{ \item D the date
  field \item S the source actor field \item T the target
  actor field \item C the event code field \item L the
  event code label field (optional) \item Q the quote field
  (optional) \item . (or anything not shown above) an
  ignorable column } e.g. the defaul "D.STC" format means
  that column 1 is the date, column 2 should be ignored,
  column 3 is the source, column 4 is the target, and
  column 5 is the event code.  The optional quote and label
  column are not searched for.

  The code plucks out just these columns, formats them
  appropriately and ignores everything else in the file.
  Only D, S, T, C, and C are required.

  The format of the date field is given by
  \code{format.date}
}
\author{
  Will Lowe
}

