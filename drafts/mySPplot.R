xscale <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

solaR.theme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
solaR.theme$strip.background$col='transparent'##'lightgray'
solaR.theme$strip.shingle$col='transparent'
solaR.theme$strip.border$col='transparent'

mySPplot <- function(x,
                     names.attr=names(x),
                     par.settings=solaR.theme,
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

setMethod('spplot',
          'Raster',
          function(obj, size=1e+05, extent=NULL, ...){
            smp<- sampleRegular(obj, size=size, extent=extent, asRaster=TRUE)
            SP <- as(smp, 'SpatialGridDataFrame')
            nms <- sub('values.', '', names(SP))
            mySPplot(SP, names.attr=nms,...)
            }
            )

