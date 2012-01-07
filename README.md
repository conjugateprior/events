# The events Package

The aim of events is to make life a bit easier for people who
analyse event data: the kind of thing that KEDS/TABARI generates as
output (see e.g. [here](http://eventdata.psu.edu)).  
There's nothing fancy in the package, just a hopefully logical
interface to all the data massaging we do to event data
before any actual analysis.

The philosophy of the package is that you will want to read in raw
event data and then apply a sequence of filters and aggregators for
actors and event types, each of which will result in an new event data set.
The final step is an application of `make_dyads` to make a set of named 
temporally regular directed dyad time series suitable for time series analysis.
All rather unix-ish, if you care for that sort of thing.

There's a fairly complete walkthrough in the package vignette.  You
should probably read that.

## Precursors

The package is ultimately intended to unify the existing software,
e.g.  the packages currently linked from the [PSU event
data](http://eventdata.psu.edu/software.dir/utilities.html) pages.

The unification is certainly not complete, but we're getting there.
In particular, extremely large data sets are probably going to be
rather unwieldy in the current version.

The sections below provide a quick compare and contrast to the 
software available from PSU that works with event data output 
(not Factiva stuff or actor dictionaries).

### scrubkeds

This functionality is implemented as a standalone function
`scrub_keds` and as an option in the event data reading function
`read_keds`.

### One-A-Day_Filter

This is also a standalone function `one_a_day` and available as an
option in the event data reading function `read_keds`.

### KEDS_Count

Most, but not all, of the functionality of KEDS_Count is implemented.  
Some differences are noted below:

 * Aggregation is possible by day, week, month, quarter and year, but
   not biweekly.

 * `read_keds` assumes that two digit year specifications 69 to 99
   indicate years between 1969 to 1999, and 00 to 68 indicate 2000 to 2068.  
   This is R and the POSIX standard interpretation for this needlessly
   ambiguous date formulation.  In contrast KEDS_Count treats 28 to 99 as 
   in the twentieth century and 00 to 27 as in the twenty first century.

 * Wildcarding for actors and event codes is not implemented
   directly, so you have to use R's facilities.  An example using
   `grep` is provided in the vignette.  Also, using "***" to mean 'all
   targets' is done by using `filter_actors` with a suitable second
   parameter.

 * KEDS_Count aggregates up to a temporal unit, e.g. a week,
   differently to `events`:  It tosses events that occur before the
   beginning of the first full unit.  `make_dyads` does not.
   There is no warning about this behaviour, but then it doesn't eat
   your data either.

 * KEDS_Count only offers summed scores and provides the per time unit
   N to construct means.  This package will use any aggregating function
   you give it, e.g. `sd` or `median` and also automatically generate
   N.
   
### Aggregator

Unlike Aggregator, events does not have a customizable aggregation
period, but it is a lot more flexible about what the aggregation
function is (see above).

### Event_Filter

Since I can't figure out what this program does, there are probably no
functions to replicate it.

### High-Volume Processing Suite

All this stuff is currently out of reach, but there are plans for
accessing databases as sources of event data.  That'll be how we scale.


## TODO 

 * More test coverage.  `make_dyads` coverage is still lacking
 * Database connection for larger data sources
