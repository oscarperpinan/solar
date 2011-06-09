library(raster)
library(zoo)

setOldClass('Date')
setOldClass('POSIXct')
setOldClass('yearmon')
setOldClass('yearqtr')
setClassUnion('timeExtended', c('Date','POSIXct', 'yearmon', 'yearqtr'))

setClass(
         Class='RasterTime',
         contains='RasterBrick',
         representation(index='timeExtended'),         
         validity=function(object){
           if (length(object@index)==nlayers(object)) TRUE
           else 'Length of index is different from number of layers.'
         }
         )

##añadir comprobación de que index está ordenado
setMethod("initialize",
          "RasterTime",
          function(.Object, ...){
         .Object <- callNextMethod()
            layerNames(.Object) <- as.character(.Object@index)
            .Object
          })

setGeneric('index')
setMethod('index', 'RasterTime', function(x,...)x@index)

setMethod('show',
          'RasterTime',
          function(object){
            callNextMethod()
            cat('Time index of the object:\n')
            print(summary(index(object)))
            })


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


##a toy example
##12 values of irradiation, 1 for each month
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;

nr=10
nc=10
num2raster <- function(x, nrows=nr, ncols=nc, ...){
  r <- raster(nrows=nrows, ncols=ncols)
  r[] <- x
  r
}

##some noise
G0dm2 <- lapply(G0dm, function(x)x+100*rnorm(nc*nr))
##a brick with 12 layers
s <- brick(lapply(G0dm2, num2raster))

##the time index
idx <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')

st <- new('RasterTime', s, index=idx)
st
##mean for each quarter
stAgg <- aggregate(st, by=as.yearqtr, FUN=mean)
stAgg

