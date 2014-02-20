load('data//balkans.weis.RData')
load('data//weis.goldstein.scale.RData')
bws <- balkans.weis 
bws$sEGs <- cut(bws$date, 'week', start.on.monday=TRUE)

bws$gold <- apply(bws$code, function(x){ res <- weis.goldstein.scale[[ x ]]; if (is.null(res)) NA else res })

## how do we get the special name into the mean function?  should we just demand that the scale is called 'scale' or something?
arrange(summarise(group_by(bws, sEGs), m=mean(gold, na.rm=TRUE), v=var(gold, na.rm=TRUE), n=n()), sEGs)
