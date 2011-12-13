\name{make_scale}
\alias{make_scale}
\title{Make an event scale}
\usage{
  make_scale(name, types = NULL, values = NULL,
    file = NULL, desc = "", default = NA, sep = ",")
}
\arguments{
  \item{name}{Name of scale}

  \item{types}{Array of event codes}

  \item{values}{Array of event code values}

  \item{file}{Input file defining event codes and their
  values}

  \item{desc}{Optional description of the scale}

  \item{default}{What to assign event codes that have no
  mapping in the scale. Defaults to \code{NA}.}

  \item{sep}{Separator in \code{file}}
}
\value{
  An event scale object
}
\description{
  Makes an event scale
}
\details{
  Makes an event scale from a specification found in a file
  or using the \code{types} and \code{variables}
  parameters.  If a file is specified it is assumed to be
  headerless and to contain event codes in the first column
  and numerical values in the second column.

  Scales must be assigned a name and may also be assigned a
  description.  If you wish to assign codes without a
  specified value to some particular value, set
  \code{default} to something other than \code{NA}.
}
\author{
  Will Lowe
}

