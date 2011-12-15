# The `events` Package

The aim of `events` is to make life a bit easier for people who
analyse event data: the kind of thing that KEDS/TABARI generates as
output.  There's nothing fancy in the package, just a logically
structured interface to all the data massaging we do to event data
before any actual analysis.

The philosophy of the package is that you will want to read in raw
event data and then apply a sequence of filters and aggregations of
actors and event types, each of which result in an new event data set,
and finish by applying `make_dyads` to make a set of named temporally
regular directed dyad time series suitable for time series analysis.
All rather unix-ish, if you care for that sort of thing.

There's a fairly complete walkthrough in the package vignette.  You
should probably read that.

## Precursors

The package is ultimately intended to unify the existing software,
e.g.  the packages currently linked from the [PSU event
data](http://eventdata.psu.edu/software.dir/utilities.html) pages.

The unification is certainly not complete, but we're getting there.
In particular, extremely large data sets are probably going to be
rather unwieldy in the current version of `events`.

The sections below provide a quick compare and contrast to the PSU
software that works with event data output (not Factiva stuff or actor
dictionaries).

### High-Volume Processing Suite

Nope, all this stuff is out of reach, but `events` has plans for
accessing databases as sources of event data that will probably cover
this sort of thing.

### scrubkeds

This functionality is implemented as a standalone function
`scrub_keds` and as an option in the event data reading function
`read_keds`.

### One-A-Day_Filter

This is also a standalone function `one_a_day` and available as an
option in the event data reading function `read_keds`.

### Event_Filter

Since I can't figure out what this program does, there are probably no
functions in `events` to replicate it.

### KEDS_Count

Most, but not all, of the functionality of KEDS_Count is available in
`events`.  Some differences are noted below:

 * Aggregation is possible by day, week, month, quarter and year, but
   not biweekly.  

 * There is no choice of date format: `read_keds` just assumes that it's
   going to be yyMMdd.  This may be relaxed in later versions.

 * Multiple input files are not automatically dealt with in `events`,
   although this is planned.  Concatenating the files before reading them
   in is a simple option for all but Windows users.

 * Wildcarding for actors nand event codes is not implemented
   directly, so you have to use R's facilities.  An example using
   `grep` is provided in the vignette.  Also, using "***" to mean 'all
   targets' is done by using `filter_actors` with a suitable second
   parameter.

 * KEDS_Count aggregates up to a temporal unit, e.g. a week,
   differently to `events`: It tosses events that occur before the
   beginning of the first full unit whereas `make_dyads` does not.
   There is no warning about this behaviour, but then it doesn't eat
   your data either.

 * KEDS_Count only offers summed scores and provides the per time unit
   N to construct means.  `events` will use any aggregating function
   you give it, e.g. `sd` or `median` and also automatically generate
   N.
   
### Aggregator

Unlike Aggregator, `events` does not have a customizable aggregation
period, but it is a lot more flexible about what the aggregation
function is (see above).

## Planned

 * Multiple input file support
 * More flexible time format reading
 * Database connection for larger data sources
 * Tests.  Mmmm, tests...