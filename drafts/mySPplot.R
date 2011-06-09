xscale <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

myTheme.theme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
myTheme.theme$strip.background$col='transparent'##'lightgray'
myTheme.theme$strip.shingle$col='transparent'
myTheme.theme$strip.border$col='transparent'

mySPplot <- function(x,
                     names.attr=names(x),
                     par.settings=myTheme.theme,
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


##Toy example
r <- raster(system.file("external/test.grd", package="raster"))
pts1 <- sampleRandom(r, size=20, sp=TRUE)
pts2 <- sampleRandom(r, size=20, sp=TRUE)

spplot(r) +
  layer(sp.points(pts1, pch=19, col='black')) +
  layer(sp.points(pts2, pch=17, col='green'))
