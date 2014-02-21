library(dplyr)
load('data//balkans.weis.RData')
load('data//weis.goldstein.scale.RData')
bws <- balkans.weis 
bws$sEGs <- cut(bws$date, 'week', start.on.monday=TRUE)

func <- function(x){ 
  res <- weis.goldstein.scale[[ x ]]; 
  if (is.null(res)) NA else res 
}
## this is just the score function actually