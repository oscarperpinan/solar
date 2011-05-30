library(raster)


setOldClass('POSIXct')
setOldClass('yearmon')
setOldClass('yearqtr')
setClassUnion('timeExtended', c('POSIXct', 'yearmon', 'yearqtr'))

setClass(
         Class='RasterTime',
         contains='RasterBrick',
         representation(index='timeExtended'),         
         validity=function(object){
           if (length(object@index)==nlayers(object)) TRUE
           else 'Length of index is different from number of layers.'
         }
         )

setMethod("initialize",
          "RasterTime",
          function(.Object, ...){
         .Object <- callNextMethod()
            layerNames(.Object) <- as.character(.Object@index)
            .Object
          })

setGeneric('index')
setMethod('index', 'RasterTime', function(x,...)x@index)

setGeneric('aggregate')
setMethod('aggregate',
          signature=(object='RasterTime'),
          definition=function(x, by, FUN, ...){
            ##from aggregate.zoo
            my.unique <- function(x) x[MATCH(x, x) == seq_len(length(x))]
            my.sort <- function(x) x[order(x)]
            if (is.function(by)) by <- by(x@index)
            ##stopifnot(length(time(x)) == length(by))
            b <- stackApply(s, as.numeric(factor(by)), match.fun(FUN))
            ##sa <- stack(b)##stackApply gives a brick
            indexAgg <- my.sort(my.unique(by))
            ##and what about?
            ##indexAgg <- sort(unique(by))
            res <- as(b, 'RasterTime')
            res@index <- indexAgg
            layerNames(res) <- as.character(indexAgg)
            res
          }
          )

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


## stag <- aggregate(st, by=as.yearqtr, mean)
## stag2 <- aggregate(st, by=as.yearmon, mean)

###


  ## G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;

  ## nr=10
  ## nc=10
  ## num2raster <- function(x, nrows=nr, ncols=nc, ...){
  ##   r <- raster(nrows=nrows, ncols=ncols)
  ##   r[] <- x
  ##   r
  ##   }

  ## G0dm2 <- lapply(G0dm, function(x)x+100*rnorm(nc*nr))
  ## s <- brick(lapply(G0dm2, num2raster))
  ## layerNames(s) <- month.abb

  ## index <- seq(as.POSIXct('2010-01-15'), as.POSIXct('2010-12-15'), 'month')
  ## by <- as.yearqtr
