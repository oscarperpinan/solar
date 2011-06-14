xscale <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

myTheme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
myTheme$strip.background$col='transparent'##'lightgray'
myTheme$strip.shingle$col='transparent'
myTheme$strip.border$col='transparent'

mySPplot <- function(x,
                     names.attr=names(x),
                     par.settings=myTheme,
                     between=list(x=0.5, y=0.2),
                     as.table=TRUE,
                     xscale.components=xscale,
                     yscale.components=yscale,
                     ...){
  p <-spplot(x, names.attr=names.attr,
             par.settings=par.settings,
             scales=list(draw=TRUE),
             between=between,
             as.table=as.table,
             xscale.components=xscale.components,
             yscale.components=yscale.components)
  p
}
