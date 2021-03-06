% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/events.R
\name{filter_dyad}
\alias{filter_dyad}
\title{Discard all but relevant actors}
\usage{
filter_dyad(edo, source = function(x) {     TRUE }, target = function(x) {  
    TRUE })
}
\arguments{
\item{edo}{Event data}

\item{source}{Function that returns \code{TRUE} for source actor codes, or actor name, or vector of names.}

\item{target}{Function that returns \code{TRUE} for target actor codes, or actor name, or vector of names.}
}
\value{
All events in involving the specified source and target
}
\description{
Extracts a directed dyad
}
\details{
The \code{source} parameter identifies sources and the \code{target} parameter specifies 
the target names.
}
\seealso{
\code{\link{filter_codes}}, \code{\link{filter_time}}, \code{\link{filter_actors}}
}
\author{
Will Lowe
}
