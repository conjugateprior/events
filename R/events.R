##' Creates mapper function
##'
##' This function creates and returns a function which renames
##' arguments or returns them unchanged if no mapping is provided.  
##' The function is initialised using a list describing the reverse mapping.  For example, 
##' given \code{list(a=c(1,2), b=3)} a function is returned that turns 1 and 2 in 'a', 
##' 3 into 'b' and leaves all other arguments unchanged.  
##' When \code{re} is TRUE,
##' name values are treated as regular expressions.  In the returned 
##' function the base R function \code{grepl} is used to find the first
##' matching argument.  It has its default parameters: no PERL-style regexps,
##' case sensitivity, and character rather than byte-based matching.
##' When \code{re} is FALSE only exact matches trigger the mapping.
##'
##' @title Make a mapper function
##' @param lst A list of string to pattern associations
##' @param re whether to treat the patterns as regular expressions
##' @return A mapper function 
##' @export
##' @author Will Lowe
mapper <- function(lst, re=FALSE){
  f <- make_fun_from_list(lst, re=re)
  return(f)
}

##' Creates a mapping function from list
##'
##' Turns a list of the form \code{list(a=c(1,2), b=3)} into a function
##' that returns 'a' when given 1 or 2 as argument, 'b' when given 3
##' and otherwise gives back its argument unchanged.  Name values are
##' treated as regular expressions if \code{re}=TRUE.
##'
##' This is a convenience function to make it possible to specify onto mappings
##' using lists.  The \code{mapper} uses it internally, 
##' but you might find a use for it too.
##' 
##' @title Create a mapping function from list
##' @param lst A list
##' @param re Whether to interpret the name values as regular expressions
##' @return A function that inverts the mapping specified by \code{lst}
##' @export
##' @author Will Lowe
make_fun_from_list <- function(lst, re=FALSE){
  revf <- list() ## reverse this list to do look ups the other way
  for (newname in names(lst)){
    newname.lst <- lst[[newname]]
    for (n in newname.lst)
      revf[[n]] <- newname
  }
  print(revf)
  
  if (!re){
     f <- function(x){ 
       find_in_list <- function(z){ 
         if (z %in% names(revf)) 
           revf[[z]]
         else
           z
       } 
       unlist(lapply(x, find_in_list))
     }
  } else {
     f <- function(x){
       find_in_list_re <- function(z){
         for (n in names(revf))
           if (grepl(n, z))
             return(revf[[ n ]])
         return(z)
       } 
       unlist(lapply(x, find_in_list_re))
     }
  }	
  f
}


##' Tests the behaviour of a mapper function
##'
##' This is a test function to runs a mapper function over a set of strings
##' and reports which strings are mapped and what they are mapped to.  It's mostly useful
##' to ensure that any regular expressions given to \code{mapper} (i.e. when
##' re=TRUE) have the 
##' right behaviour before using the mapper function to aggregate things from your data.
##' 
##' @title Test mapper function behaviour
##' @param mapper a mapper function
##' @param data a vector of character test data
##' @return What the elements of \code{data} are mapped to, if anything
##' @export
##' @author Will Lowe
test_mapper <- function(mapper, data){
  sp <- sapply(data, mapper)
  unmapped <- sp==data
  mapped <- sp!=data

  mp <- list()
  nms <- unique(sp[mapped])
  cat("Mapped:\n")
  for (n in nms){
    middle <- paste(as.vector(data[mapped][sp[mapped]==n]), collapse=", ")
    cat(paste("\t", middle, " --> ", n, "\n", sep=""))
  }

  cat(paste("Unmapped:\n\t", paste(data[unmapped], collapse=", "), sep="")) 
}


##' Creates spotter function
##'
##' This is a convenience function that creates and returns a function which returns
##' TRUE when any of its arguments match one of the patterns given here. 
##' When \code{re} is TRUE,
##' arguments to this function are matched as regular expressions in the returned 
##' function using the base R function \code{grepl} with its default parameters.  
##' These currently imply no PERL-style regexps,
##' case sensitivity, and character rather than byte-based matching.
##' When \code{re} is FALSE any exact match is sufficient for the returned function
##' to return TRUE.  
##'
##' @title Make a spotter function
##' @param ... Patterns, matches to which the returned function should return \code{TRUE}
##' @param re whether to treat the patterns as regular expressions
##' @return A spotter function 
##' @export
##' @author Will Lowe
spotter <- function(..., re=FALSE){
  lst <- unlist(list(...))
  if (re){
    f <- function(x){ 
      find_in_list_re <- function(z){ any(unlist(lapply(lst, grepl, x=z))) } 
      unlist(lapply(x, find_in_list_re))
    }
  } else {
    f <- function(x){ 
      find_in_list <- function(z){ z %in% lst } 
      unlist(lapply(x, find_in_list)) 
    }
  }
  return(f)
}


##' Tests the coverage of a spotter function
##'
##' This is a test function to runs a spotter function over a set of strings
##' and reports which strings are spotted and which are not.  It's mostly useful
##' to ensure that any regular expressions given to \code{spotter} (i.e. when
##' re=TRUE) have the 
##' right coverage before using the spotter function to select events from your data.
##' 
##' @title Test spotter function coverage
##' @param spotter a spotter function
##' @param data a vector of character test data
##' @param unspotted whether to also list strings to which the spotter returns FALSE
##' @return the elements of \code{data} that the spotter spots, i.e. returns TRUE to, and
##'         also optionally the elements which the spotter ignores, i.e. returns FALSE to.
##' @export
##' @author Will Lowe
test_spotter <- function(spotter, data, unspotted=TRUE){
  sp <- sapply(data, spotter)
  spotted <- data[sp]
  notspotted <- data[!sp]

  cat("Spots:   ", paste(spotted[order(spotted)]), "\n")
  if (unspotted)
    cat("Ignores: ", paste(notspotted[order(notspotted)]), "\n") 
}

##' Removes well-known noise from KEDS output files
##'
##' This function applies the regular expression based cleaning routine 
##' from the KEDS website.  This is a direct translation from the original 
##' PERL which replaces capital 'O's and small 'l's with 0 and 1
##' respectively and removes the event code '---]', on the 
##' assumption that these are all output noise.
##' 
##' @title Remove well-known noise from KEDS event data file
##' @param edo An event data object  
##' @return Event data
##' @export
##' @seealso \code{\link{read_keds}}
##' @author Will Lowe
scrub_keds <- function(edo){
  edo$code <- sub('O(\\d\\d)$', '0\\1', edo$code, perl=TRUE)
  edo$code <- sub('l(\\d\\d)$', '1\\1', edo$code, perl=TRUE)
  good <- grep('^.*---].*$', edo$code, invert=TRUE)
  edo <- edo[good,]
  ## edo$code <- factor(edo$code)
  return(edo)
}

##' Reads event data output files in free format
##'
##' Reads event data output and optionally applies the \code{\link{scrub_keds}} cleaning function
##' and the \code{\link{one_a_day}} duplicate removal filter.
##'
##' This function assumes that \code{d} is a vector of output files.
##' These are assumed to be \code{sep}-separated text files.  The column
##' ordering is given by the \code{col.format} parameter:
##' \itemize{
##' \item D the date field
##' \item S the source actor field
##' \item T the target actor field
##' \item C the event code field
##' \item L the event code label field (optional)
##' \item Q the quote field (optional)
##' \item . (or anything not shown above) an ignorable column
##' }
##' e.g. the defaul "D.STC" format means that column 1 is the date, column 2 should be 
##' ignored, column 3 is the source, column 4 is the target, and column 5 is the event
##' code.  The optional quote and label column are not searched for.
##'
##' The code plucks out just these columns, formats them appropriately and ignores 
##' everything else in the file.  Only D, S, T, C, and C are required.
##'
##' The format of the date field is given by \code{format.date} 
##' 
##' @title Read event data files 
##' @param d Names of event data files
##' @param col.format Format for columns in d (see details) 
##' @param one.a.day Whether to apply the duplicate event remover
##' @param scrub.keds Whether to apply the data cleaner
##' @param date.format How dates are represented in the orginal file
##' @param sep File separator
##' @param header Whether there is a header row in d
##' @return An event data set
##' @export
##' @author Will Lowe
read_eventdata <- function(d, col.format="D.STC", one.a.day=TRUE, scrub.keds=TRUE, date.format="%y%m%d", sep='\t', header=FALSE){

  which.letter <- function(l, s){
     res <- grep(l, unlist(strsplit(s, '')))
     out <- ifelse(length(res)>0, res, -1)     
     return(out)
  }
  
  trim_whitespace <- function(x){
    as.vector(sapply(x, gsub, pattern="^\\s+|\\s+$", replacement=''))
  }

  date.col <- which.letter('D', col.format)
  source.col <- which.letter('S', col.format)
  target.col <- which.letter('T', col.format)
  code.col <- which.letter('C', col.format)
  if (sum(c(date.col=-1, source.col=-1, target.col=-1, code.col=-1))>0)
    stop("Could not find all of D, T, S, and C in col.format")
  ## optionals
  label.col <- which.letter('L', col.format)
  quote.col <- which.letter('Q', col.format)

  read_ed_file <- function(d){
    vv <- read.csv(d, sep=sep, header=header, strip.white=TRUE, colClasses='character')
    dd <- data.frame(date=vv[,date.col], 
                     source=vv[,source.col], 
                     target=vv[,target.col],
                     code=vv[,code.col])
    if (label.col != -1)
      dd$label <- vv[,label.col]
    if (quote.col != -1)
      dd$quote <- vv[,quote.col]
    dd
  }

  ff <- read_ed_file(d[1])
  if (length(d)>1)
    for (i in 2:length(d))
      ff <- rbind(ff, read_ed_file(d[i]))

  ## make dates dates
  ff$date <- as.Date(as.character(ff$date), date.format)

  if (scrub.keds)
  	ff <- scrub_keds(ff)
  ## else
    ##ff$code <- factor(ff$code) ## a side effect of scrub_keds

  ## assert temporal order
  ff <- ff[order(ff$date),]

  if (one.a.day)
  	ff <- one_a_day(ff)
  
  ff$source <- as.character(ff$source)
  ff$target <- as.character(ff$target)
  ff$code <- as.character(ff$code)
  
  class(ff) <- c("eventdata", class(ff))
  return(ff)   
}  

##' Reads KEDS event data output files
##'
##' Reads KEDS output and optionally applies the \code{\link{scrub_keds}} cleaning function
##' and the \code{\link{one_a_day}} duplicate removal filter.  This function is thin wrapper
##' around \code{read.csv}.
##'
##' This function assumes that \code{d} are a vector of KEDS/TABARI output files.
##' These are assumed to be tab separated text files wherein the
##' first field is a date in \code{yymmdd} format or as specified by \code{date.format}, 
##' the second and third fields are actor
##' codes, the fourth field is an event code, and the fifth field is a
##' text label for the event type, and the sixth field is a quote - some kind of
##' text from which the event code was inferred.  Label and quote are optional and can 
##' be discarded when reading in.
##' 
##' @title Read KEDS events files 
##' @param d Names of files of KEDS/TABARI output
##' @param keep.quote Whether the exact noun phrase be retained
##' @param keep.label Whether the label for the event code should be retained
##' @param one.a.day Whether to apply the duplicate event remover
##' @param scrub.keds Whether to apply the data cleaner
##' @param date.format How dates are represented in the first column
##' @return An event data set
##' @export
##' @author Will Lowe
read_keds <- function(d, keep.quote=FALSE, keep.label=TRUE, one.a.day=TRUE, scrub.keds=TRUE, date.format="%y%m%d"){
	form <- paste("DSTC", ifelse(keep.label, "L", ""), ifelse(keep.quote, "Q", ""), sep='')
	read_eventdata(d, col.format=form, one.a.day=one.a.day, scrub.keds=scrub.keds, 
	  date.format=date.format)	
}

##' Tries to remove duplicate events
##'
##' This function removes duplicates of any event that occurs to the same source
##' and target with the same event code, on the assumption that these are
##' in fact the same event reported twice.
##'
##' This function can also be applied as part of \code{\link{read_keds}} 
##' 
##' @title Apply the one-a-day filter
##' @param edo Event data object
##' @return New event data object with duplicate events removed
##' @seealso \code{\link{read_keds}}
##' @export
##' @author Will Lowe
one_a_day <- function(edo){
	edo[!duplicated(edo[,1:4]),]
}

##' Summarises a set of event data
##'
##' This is a compact summary of an event data object.  For more detail
##' consult the object itself.  Currently it is simply a data.frame with 
##' conventionally named column names, but that almost certainly will change to
##' deal with larger datasets in later package versions.  
##' If your code uses the package's accessor functions then
##' you won't feel a thing when this happens.
##' 
##' @title Summarise event data
##' @param object Event data object 
##' @param ... Not used
##' @return A short description of the event data
##' @author Will Lowe
##' @export 
##' @method summary eventdata
summary.eventdata <- function(object, ...){
  src <- sum(table(object$source)>0)
  trg <- sum(table(object$target)>0)
  start <- format(min(object$date), "%a %b %d, %Y")
  end <- format(max(object$date), format="%a %b %d, %Y")
  all <- nrow(object)
  cat(paste(all, "events", "involving", src, "sources and",
            trg, "targets", "\n"))
  cat(paste("from", start, "to", end, "\n"))
}

##' Discards all but relevant actors
##'
##' The \code{which} parameter specifies whether the filter should be applied
##' only to targets, only to sources, or to all actors in the event data.
##' 
##' @title Discard all but relevant actors 
##' @param edo Event data
##' @param fun Function that returns \code{TRUE} for actor codes or a single actor code, that should not be discarded.
##' @param which What actor roles should be filtered
##' @return Event data containing only actors that pass through \code{fun}
##' @seealso \code{\link{filter_codes}}, \code{\link{filter_time}}
##' @export
##' @author Will Lowe
filter_actors <- function(edo, fun=function(x){return(TRUE)}, which=c('both','target','source')){
  wh <- match.arg(which)
  if (is.character(fun)) ## if they just passed in a raw name
    FUN <- function(x){ x==fun }
  else
    FUN <- fun
  
  if (wh=='both')
    filter(edo, FUN(target) | FUN(source))
  else if (wh=='target')
    filter(edo, FUN(target))
  else if (wh=='source')
    filter(edo, FUN(source))  
}


##' Extracts a directed dyad
##'
##' The \code{source} parameter identifies sources and the \code{target} parameter specifies 
##' the target names.
##' 
##' @title Discard all but relevant actors 
##' @param edo Event data
##' @param source Function that returns \code{TRUE} for source actor codes, or actor name.
##' @param target Function that returns \code{TRUE} for target actor codes, or actor name.
##' @return All events in which one of the sources does something to one of the targets
##' @seealso \code{\link{filter_codes}}, \code{\link{filter_time}, \code{\link{filter_actors}}
##' @export
##' @author Will Lowe
filter_dyad <- function(edo, source=function(x){return(TRUE)}, target=function(x){return(TRUE)}){
  if (is.character(source))
    sourceFUN <- function(x){ x==source }
  else
    sourceFUN <- source
  if (is.character(target))
    targetFUN <- function(x){ x==target }
  else
    targetFUN <- target

  filter(edo, sourceFUN(source), targetFUN(target))
}

##' Discards all but relevant event codes
##'
##' Applies the filter function to each event code to see whether
##' to keep the observation.  
##' 
##' @title Discard all but relevant event codes
##' @param edo Event data
##' @param fun Function that returns \code{TRUE} for event codes, or single event code, that should not be discarded
##' @return Event data containing only events that pass through \code{fun}
##' @seealso \code{\link{filter_actors}}, \code{\link{filter_time}}
##' @export
##' @author Will Lowe
filter_codes <- function(edo, fun=function(x){return(TRUE)}){
  if (is.character(fun))
    codeFUN <- function(x){ x==fun }
  else
    codeFUN <- fun
  
  filter(edo, codeFUN(code))
}

##' Restricts events to a time period
##'
##' Restricts events on or after \code{start} and before or on \code{end}.
##' @title Restrict events to a time period
##' @param edo Event data
##' @param start Something convertable to a \code{Date} object
##' @param end Something convertable to a \code{Date} object
##' @return Event data restricted to a time period
##' @seealso \code{\link{filter_codes}}, \code{\link{filter_actors}}
##' @export
##' @author Will Lowe
filter_time <- function(edo, start=min(edo$date), end=max(edo$date)){
  st <- as.Date(start)
  en <- as.Date(end)
  filter(edo, date >= st & date <= en)
}

##' Lists actor codes
##'
##' Lists all the actor codes that occur in the event data in alphabetical order.
##' @title List actor codes
##' @param edo Event data
##' @return Array of actor codes
##' @seealso \code{\link{sources}}, \code{\link{targets}}, \code{\link{codes}}
##' @export
##' @author Will Lowe
actors <- function(edo){
  sort(unique(c(as.character(edo$target), as.character(edo$source))))
}

##' Lists target actor codes
##'
##' Lists all the actor codes that appear as a target in the event data
##' in alphabetical order.
##' @title Lists target actor codes
##' @param edo Event data
##' @return Array of actor codes
##' @seealso \code{\link{sources}}, \code{\link{actors}}, \code{\link{codes}}
##' @export
##' @author Will Lowe
targets <- function(edo){
  sort(unique(as.character(edo$target)))
}

##' Lists source actor codes
##'
##' Lists all the actor codes that appear as a source in the event data
##' in alphabetical order.
##' @title List source actor codes
##' @param edo Event data
##' @return Array of actor codes
##' @seealso \code{\link{actors}}, \code{\link{targets}}, \code{\link{codes}}
##' @export
##' @author Will Lowe
sources <- function(edo){
  sort(unique(as.character(edo$source)))
}

##' Lists event codes
##'
##' Lists all the event codes that appear in the event data
##' @title List event codes
##' @param edo Event data
##' @return Array of event codes
##' @seealso \code{\link{sources}}, \code{\link{targets}}, \code{\link{actors}}
##' @export
##' @author Will Lowe
codes <- function(edo){
  sort(unique(as.character(edo$code)))
}

##' Aggregates event data fields
##'
##' This function applies a mapping/aggregration function to event data.
##' It is the workhorse function behind the \code{map_} functions.
##' You should use these in ordinary use.
##'
##' @title Aggregate field values 
##' @param edo Event data
##' @param fun Function specifying the aggregation mapping, or a list 
##' @param which Name of the field to map
##' @return Event data with new fields
##' @seealso \code{\link{map_codes}}, \code{\link{map_actors}}
##' @export
##' @author Will Lowe
map_eventdata <- function(edo, fun, which){
  if (!is.function(fun))
    fun <- mapper(fun)
  edo[[which]] <- sapply(edo[[which]], fun)
  return(edo)
}

##' Aggregates event codes
##'
##' This function relabels event codes according to \code{fun},
##' The \code{mapper} function is an easy way to make such a function.
##'
##' This can also be used as a renaming function, but it is most
##' useful when multiple codes should be treated as equivalent.
##' 
##' @title Aggregate event codes 
##' @param edo Event data
##' @param fun Function specifying the aggregation mapping 
##' @return Event data with new event codes
##' @seealso \code{\link{map_actors}}
##' @export
##' @author Will Lowe
map_codes <- function(edo, fun=function(x){return(x)}){
  map_eventdata(edo, fun, 'code')
}

##' Aggregates actor codes
##'
##' The function relabels actor codes according to \code{fun}.
##' \code{mapper} provides an easy way to make a function specifying
##' what things should now be called.
##'
##' This function can also be used as a renaming function, but it is most
##' useful when multiple codes should be treated as equivalent.
##' 
##' @title Aggregate actor codes 
##' @param edo Event data
##' @param fun Function specifying the aggregation mapping
##' @return Event data with new actor codes
##' @seealso \code{\link{map_codes}}
##' @export
##' @author Will Lowe
map_actors <- function(edo, fun=function(x){return(x)}){
  map_eventdata(map_eventdata(edo, fun, 'source'), fun, 'target')
}

##' Aggregates events to a regular time interval
##'
##' In an event data set S, assume that \eqn{A}=\code{length(actors(S))} actors 
##' \eqn{K}=\code{length(codes(S))} event codes occur.  This function
##' creates \eqn{A^2} data streams labelled by the combination of source and target
##' actors.  If \code{scale} is \code{NULL} these are \eqn{K}-dimensional time series of event counts.
##' If \code{scale} names a scale that has been
##' added to the event data \code{fun} is used to aggregate the events falling into
##' each temporal interval. This creates a univariate interval valued
##' time series for each directed dyad.
##' 
##' @title Aggregate events to a regular time interval 
##' @param edo Event data
##' @param scale Name of an eventscale or \code{NULL} to create counts
##' @param unit Temporal aggregation unit
##' @param monday Whether weeks start on Monday. If \code{FALSE}, they start on Sunday
##' @param fun Aggregation function.  Should take a vector and return a scalar
##' @param missing.data What weeks with no data are assigned
##' @return A list of named dyadic aggregated time series
##' @export
##' @author Will Lowe
make_dyads <- function(edo, scale=NULL, unit=c('week','day','month','quarter','year'), monday=TRUE, fun=mean, missing.data=NA) {
  
  unit <- match.arg(unit)
  segs <- cut(edo$date, breaks=unit, start.on.monday=monday)
  segsd <- as.Date(levels(segs))
  ## add the segs to edo before aggregating
  edo$sEGs <- segs
  evs <- codes(edo)
  
  ff <- function(a, b){
    edo.subset <- edo[edo$source==a & edo$target==b, ]
    if (nrow(edo.subset) == 0)
      return(NULL)
    
    if (is.null(scale)){
      ## this process 
      rr <- as.matrix(with(edo.subset, table(sEGs, code)))
      ret <- data.frame(date=segsd)
      ## sometimes dyads just do not use some codes
      for (e in evs){
        if (e %in% colnames(rr))
          ret[,e] <- rr[,e]
        else
          ret[,e] <- rep(0, nrow(rr))
      }
      
    } else {
      agg <- aggregate(edo.subset[[scale]], by=list(unit=edo.subset$sEGs), FUN=fun)
      hh <- data.frame(date=as.Date(agg$unit))
      hh[[scale]] <- agg$x
      aggn <- aggregate(edo.subset[[scale]], by=list(unit=edo.subset$sEGs), FUN=length) ## to get N
      hh$n <- aggn$x
      ## fold into a proper timeline
      ret <- data.frame(date=segsd)
      ret <- merge(ret, hh, by='date', all.x=TRUE)
      ret$n[is.na(ret$n)] <- 0
      ## pad with zeros or something, if you must...
      if (!is.na(missing.data))
      	ret[[scale]][ret$n==0] <- missing.data
    }
    return(ret)
  }

  res <- list()
  act <- actors(edo)
  for (a in act){
    for (b in act){
      val <- ff(a, b)
      if (!is.null(scale))
      	class(val) <- c('dyadic.aggregated.scaled.eventdata', class(val))
      else
      	class(val) <- c('dyadic.aggregated.count.eventdata', class(val))
      res[[paste(a,b,sep='.')]] <- val
    }
  }    
  return(res)  
}

##' Plots scaled directed dyad
##'
##' A convenience function to plot the named scale within a directed dyad against time.  
##' @title Plot scaled directed dyad
##' @param dyad One directed dyadic time series from the \code{make_dyads} function 
##' @param ... Extra arguments to plot
##' @return Nothing, used for side effect
##' @export
##' @author Will Lowe
plot_dyad <- function(dyad, ...){
  stopifnot(is(dyad, 'dyadic.aggregated.scaled.eventdata'))
  
  scalename <- names(dyad)[which(!(names(dyad) %in% c("n", "date")))]
  with(dyad, plot(dyad$date, dyad[[scalename]], ...))
}

##' Makes an event scale
##'
##' Makes an event scale from a specification found in a file or 
##' using the \code{types} and \code{variables}
##' parameters.  If a file is specified it is assumed to be headerless and to
##' contain event codes in the first column and numerical values in the second
##' column.
##'
##' Scales must be assigned a name and may also be assigned a
##' description.  If you wish to assign codes without a specified value to
##' some particular value, set \code{default} to something other than \code{NA}. 
##' 
##' @title Make an event scale
##' @param name Name of scale
##' @param types Array of event codes
##' @param values Array of event code values
##' @param file Input file defining event codes and their values
##' @param desc Optional description of the scale
##' @param default What to assign event codes that have no mapping in the scale. Defaults to \code{NA}.
##' @param sep Separator in \code{file} 
##' @return An event scale object
##' @export
##' @author Will Lowe
make_scale <- function(name, types=NULL, values=NULL, file=NULL, desc="", default=NA, sep=","){
  def <- default
  v <- list()
  if (!is.null(file)){
    vvf <- read.csv(file, header=FALSE, 
	    colClasses=c("character", "numeric"),
    	col.names=c("code", name), sep=sep)
	for (i in 1:nrow(vvf))
  		v[[vvf[i,1]]] <- vvf[i,2]
  } else {
    for (i in 1:length(types))
  		v[[ types[i] ]] <- values[i]
  }
  
  attr(v, 'desc') <- desc
  attr(v, 'name') <- name
  attr(v, 'default') <- def
  class(v) <- c('eventscale', class(v))
  return(v)
}

##' Gets scale scores for event codes
##'
##' Returns an array of scores corresponding to the the second
##' argument's scale values or the scale's default value if
##' not recognized.
##' 
##' You should use this function to avoid
##' relying on the internal structure of event scales.  They 
##' are currently lists, but this may change.
##' 
##' @title Score event codes with an event scale
##' @param eventscale An event scale
##' @param codes Event codes
##' @return Numerical values for each event codes from the scale
##' @export
##' @author Will Lowe
score <- function(eventscale, codes){
  ##if (is.factor(codes))
  ##  codes <- as.character(codes) 

  def <- attr(eventscale, 'default')
  ## seem to need to do this so NAs are not dropped
  dd <- sapply(codes, function(x){ 
      ff <- eventscale[[x]] 
      ifelse(is.null(ff), def, ff) 
    })
  return(as.numeric(dd))
}

##' Checks coverage of scale for event data
##'
##' Returns an array of event codes that occur in an event data set but are not
##' assigned values by the scale.  These are the codes that will, in subsequent processing,
##' be assigned the scale's default value.
##' 
##' @title Check coverage of scale for event data
##' @param sc An eventscale
##' @param edo Event data
##' @return Array of unscaleable event codes
##' @export
##' @author Will Lowe
scale_coverage <- function(sc, edo){
  evts <- codes(edo)
  mis <- which(!(evts %in% scale_codes(sc)))
  nme <- attr(sc, 'name')
  if (length(mis)==0){
    cat(paste("Scale", nme, "covers all codes in the data\n"))
    return(c())
  } else {
    cat(paste("Scale", nme, "does not cover codes:\n"))
    return(evts[mis])
  }
}

##' Shows which events codes are covered by a scale
##'
##' Returns an array of event codes to which an eventscale assigns a value.
##' 
##' @title Show which events are scaleable
##' @param es Eventscale
##' @return Array of scaleable event codes
##' @export
##' @author Will Lowe
scale_codes <- function(es){
  names(es)
}

##' Summarise an eventscale
##'
##' Print summary statistics for an eventscale.
##'
##' @title Summarise an eventscale
##' @param object Scale
##' @param ... Not used
##' @return Nothing, used for side effect
##' @author Will Lowe
##' @export 
##' @method summary eventscale
summary.eventscale <- function(object, ...){
  nme <- attr(object, 'name')
  des <- attr(object, 'desc')
  def <- attr(object, 'default')
  len.es <- length(object)
  max.es <- max(unlist(object))
  min.es <- min(unlist(object))
  cat(paste("Scale name:", nme, "\n"))
  if ((!is.null(des)) & (des != ""))
    cat(paste(des, "\n"))
  cat(paste("Unrecognized event codes:", def, "\n"))
  cat(paste(len.es, "event codes assigned scores between", min.es, 
            "and", max.es, "\n"))
}

##' Applies an eventscale to event data
##'
##' Applies an eventscale to event data.  This adds a new field in the event data
##' with the same name as the eventscale.  Add as many as you want to keep 
##' around.
##' @title Apply eventscale to event data
##' @param edo Event data
##' @param sc scale
##' @return Event data with a scaling
##' @export
##' @author Will Lowe
add_eventscale <- function(edo, sc){
  scname <- attr(sc, 'name') 
  def <- attr(sc, 'default')
  edo[[scname]] <- score(sc, edo$code)
  class(edo) <- c('scaled.eventdata', class(edo))
  return(edo)
}

## assumes [-1,1] ranged data, so squash scales first
fisher.transform <- function(x){
  0.5 * log((1+x)/(1-x))
}

inv.fisher.transform <- function(x){
  (exp(2*x)-1) / (exp(2*x)+1)	
}

############ Data from here on ###############

#' Balkans conflict events in WEIS encoding
#'
#' Event data on the conflict during the collapse of Yugoslavia.  Events are
#' coded according to an extended WEIS scheme by the KEDS Project.  The event
#' stream contains 72953 events occurring between 2 April 1989 and 31 July 2003
#' involving 325 actors.
#'
#' @name balkans.weis
#' @docType data
#' @author KEDS Project
#' @references \url{http://web.ku.edu/~keds/data.dir/balk.html}
#' @keywords data
NULL

#' WEIS codes to Goldstein conflict-cooperation scale
#'
#' A mapping of WEIS event codes to [-10,10] representing a scale
#' of conflict and cooperation, developed by Joshua Goldstein and
#' slightly extended for the KEDS project.  
#' Note: This mapping does not cover all the event codes in \code{balkans.weis}.
#' Taken from the KEDS Project's documentation.
#'
#' @name weis.goldstein.scale
#' @docType data
#' @author KEDS Project
#' @references \url{http://web.ku.edu/~keds/}
#' @keywords data
NULL

#' CAMEO codes to conflict-cooperation scale
#'
#' A mapping of CAMEO event codes to [-10,10] representing a scale
#' of conflict and cooperation, developed by the KEDS project.
#' Taken from the documentation of the KEDS_Count software.
#'
#' The version of CAMEO used here is 0.9B5 [08.04.15].
#'
#' @name cameo.scale
#' @docType data
#' @author KEDS Project
#' @references \url{http://web.ku.edu/~keds/}
#' @keywords data
NULL

################### Package blah ################################

#' Stores and manipulates event data
#'
#' Stores, manipulates, scales, aggregates and creates directed dyadic time series
#' from event data generated by KEDS, TABARI, or any other extraction tool 
#' with similarly structured output.
#'
#' Events offers simple methods for aggregating and renaming actors and event codes, 
#' applying event scales, and constructing regular time series at a choice of
#' temporal scales and measurement levels. 
#'
#' @author Will Lowe \email{will.lowe@@uni-mannheim.de}
#' @docType package
#' @name events
#' @aliases events package-events
NULL
