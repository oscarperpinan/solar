library(raster)
library(zoo)
library(lattice)
library(latticeExtra)

myTheme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
myTheme$strip.background$col='lightgray'
myTheme$strip.shingle$col='transparent'

direction <- function(object, dirXY=y){
  ## dirLayer=switch(direction,
  ## y={ ##Latitude
  latLayer <- raster(object, layer=1)
  ## layerNames(latLayer) <- 'Latitude'
  latLayer[] <- yFromCell(object, 1:ncell(object))
  ##   latLayer
  ## },
  ## x={ ##Longitude
  lonLayer <- raster(object, layer=1)
  ## layerNames(lonLayer) <- 'Longitude'
  lonLayer[] <- xFromCell(object, 1:ncell(object))
  ##   lonLayer
  ## }
  x=lonLayer
  y=latLayer
  
  isLanguage <- try(is.language(dirXY), silent=TRUE)
  if (class(isLanguage)=='try-error' || !isLanguage) dirXY <- substitute(dirXY)

  dirLayer <- eval(dirXY)
}

## if (missing(xlab)) xlab=switch(direction, x='Longitude', y='Latitude')

setGeneric('hovmoller', function(object, ...){standardGeneric('hovmoller')})

setMethod('hovmoller', signature='RasterTime',
          definition=function(object, dirXY=y, xlab='Direction', ylab='Time', digits=2, add.contour=TRUE, ...){
            idx=index(object)
            dirLayer <- direction(object, dirXY=substitute(dirXY))
            z <- zonal(object, dirLayer, mean, digits=digits)
            colnames(z) <- c(xlab, as.character(idx))
            if (add.contour){
              contour=TRUE
              labels=list(cex=0.7)
              pretty=TRUE
            } else {
              contour=FALSE
              labels=FALSE
              pretty=FALSE
              }
            p <- levelplot(z[,-1],
                           row.values=z[,1], xlab=xlab,
                           ylab=ylab,
                           contour=contour, labels=labels, pretty=pretty,
                           par.settings=myTheme)
            p
          }
          )

setGeneric('horizonplot')

setMethod('horizonplot', signature='RasterTime',
          definition=function(x, data=NULL, dirXY=y, xlab='Time', ylab='Direction', digits=0, ...){
            idx=index(x)
            dirLayer <- direction(x, dirXY=substitute(dirXY))
            z <- zonal(x, dirLayer, mean, digits=digits)
            nRows <- nrow(z)
            zz <- as.data.frame(t(z[,-1]), row.names='')
            names(zz) <- z[,1]
            zz <- zoo(zz, order.by=idx)
            p <- horizonplot(zz, xlab=xlab, ylab=ylab, layout=c(1, nRows), 
                 colorkey=TRUE, colorkey.digits=1, origin=mean(zz),
                 scales=list(y=list(relation="same")))
            p
          }
          )

setGeneric('xyplot')

setMethod('xyplot', signature='RasterTime',
          definition=function(x, data=NULL, dirXY=y, xlab='Time', ylab='', digits=0, ...){
            idx=index(x)
            dirLayer <- direction(x, dirXY=substitute(dirXY))
            z <- zonal(x, dirLayer, mean, digits=digits)
            nRows <- nrow(z)
            zz <- as.data.frame(t(z[,-1]), row.names='')
            names(zz) <- z[,1]
            zz <- zoo(zz, order.by=idx)
            p <- xyplot(zz, xlab=xlab, ylab=ylab, superpose=TRUE, auto.key=FALSE)
            p + glayer(panel.text(x[1], y[1], group.value, cex=0.7))
          }
          )

##Ejemplo con Medias Mensuales
##Datos de 2008 de CMSAF
##compongo un objeto stack de la librería raster
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
listFich <- dir(pattern='2008')
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##para pasar de W/m2 (irradiancia media) a Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
idx <- as.yearmon(idx)

SISmm <- new('RasterTime', stackSIS, index=idx)

hovmoller(SISmm)
horizonplot(SISmm)



##Ejemplo con valores diarios
##Datos de 2005 de CMSAF
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
listFich <- dir(pattern='SISdm2005')
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC[1:10])
stackSIS <- calc(stackSIS, fun=function(x)x*24, filename='SISdm') ##para pasar de W/m2 (irradiancia media) a Wh/m2
setwd(old)

idx <- seq(as.Date('2005-01-01'), as.Date('2005-12-31'), 'day')[1:10]

SISdm <- new('RasterTime', stackSIS, index=idx)
