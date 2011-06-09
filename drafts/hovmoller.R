library(raster)
library(zoo)
library(lattice)
library(latticeExtra)

solaR.theme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
solaR.theme$strip.background$col='lightgray'
solaR.theme$strip.shingle$col='transparent'

setGeneric('hovmoller', function(object, ...){standardGeneric('hovmoller')})

setMethod('hovmoller', signature='RasterTime',
          definition=function(object, direction='y', xlab, ylab='Time', digits=2, ...){
            idx=index(object)
            dirLayer=switch(direction,
              y={##Latitude
                latLayer <- raster(object, layer=1)
                layerNames(latLayer) <- 'Latitude'
                latLayer[] <- yFromCell(object, 1:ncell(object))
                latLayer
              },
              x={##Longitude
                lonLayer <- raster(object, layer=1)
                layerNames(lonLayer) <- 'Longitude'
                lonLayer[] <- xFromCell(object, 1:ncell(object))
                lonLayer
              }
              )
            if (missing(xlab)) xlab=switch(direction, x='Longitude', y='Latitude')
            
            z <- zonal(object, dirLayer, mean, digits=digits)
            colnames(z) <- c(xlab, as.character(idx))
            p <- levelplot(z[,-1],
                           row.values=z[,1], xlab=xlab,
                           ylab=ylab,
                           contour=TRUE, labels=list(cex=0.7), pretty=TRUE,
                           par.settings=solaR.theme)
            p
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
