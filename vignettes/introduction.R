## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(events)

## ----read.data----------------------------------------------------------------
data("balkans.weis")
summary(balkans.weis)

## ----show.data----------------------------------------------------------------
head(balkans.weis)

## ----one.a.day----------------------------------------------------------------
dd1 <- one_a_day(balkans.weis)

## ----actors-------------------------------------------------------------------
head(actors(dd1))

## ----filter.actors------------------------------------------------------------
dd2 <- filter_actors(dd1, fun = spotter("SER", "SERMIL", "BOS", "BOSMIL"))

## ----aggregate.actors---------------------------------------------------------
actor.agg <- list(ser = c("SER", "SERMIL"), bos = c("BOS", "BOSMIL"))
dd3 <- map_actors(dd2, fun = actor.agg)

## ----filter.time--------------------------------------------------------------
dd4 <- filter_time(dd3, start = "1991-01-01", end = "1995-12-30")

## ----summary------------------------------------------------------------------
summary(dd4)

## ----intro.scaling------------------------------------------------------------
data("weis.goldstein.scale")
summary(weis.goldstein.scale)

## ----add.scale----------------------------------------------------------------
dd5 <- add_eventscale(dd4, weis.goldstein.scale)
head(dd5)

## ----aggregate.scores---------------------------------------------------------
dyads <- make_dyads(dd5, scale = "goldstein", unit = "week", monday = TRUE, 
		fun = sum, missing.data = 0)

## ----dyads--------------------------------------------------------------------
tail(dyads$ser.bos)

## ----serbos, fig.width = 6, fig.height = 5------------------------------------
with(dyads$ser.bos, 
     plot(goldstein ~ date, type = "l", lwd = 2))

## ----gaps---------------------------------------------------------------------
scale_coverage(weis.goldstein.scale, dd5)

## ----event.aggregation--------------------------------------------------------
evts <- codes(dd4)
event.agg <- list(
    coop.verb = grep("02.|03.|04.|05.|08.|09.|10.", evts, value = TRUE),
    coop.mat = grep("01.|06.|07.", evts, value = TRUE),
    conf.verb = grep("11.|12.|13.|14.|15.|16.|17.", evts, value = TRUE),
    conf.mat = grep("18.|19.|20.|21.|22.", evts, value = TRUE)
)
dc1 <- map_codes(dd4, fun = event.agg)

## ----make.dyads---------------------------------------------------------------
dyad.counts <- make_dyads(dc1, scale = NULL, unit = "week", monday = TRUE, 
		fun = sum, missing.data = 0)
tail(dyad.counts$ser.bos)

