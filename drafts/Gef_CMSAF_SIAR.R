library(sp)
library(maptools)
library(raster)
library(gstat)
library(lattice)
library(latticeExtra)
library(solaR)##instalar version 0.24

##########       CMSAF     #################

##Datos de 2008 de CMSAF
##compongo un objeto stack de la librería raster
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
listFich <- dir(pattern='2008')
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##para pasar de W/m2 (irradiancia media) a Wh/m2
setwd(old)

idx <- fBTd('prom', year=2008)

SISmm <- setZ(stackSIS, idx)
layerNames(SISmm) <- as.character(idx)

##Calculation of yearly effective irradiation
##A latitude layer for calculations with solaR::calcGef
latLayer <- init(SISmm, v='y')

##The function to be applied to each cell of the 13 layers
foo <- function(x, ...){
  gefFixed <- calcGef(lat=x[1], dataRad=list(G0dm=x[2:13]), modeTrk='fixed')
  gef2x <- calcGef(lat=x[1], dataRad=list(G0dm=x[2:13]), modeTrk='two')
  gefHoriz <- calcGef(lat=x[1], dataRad=list(G0dm=x[2:13]), modeTrk='horiz')
  resultFixed <- as.numeric(as.data.frameY(gefFixed)$Gefd)
  result2x <- as.numeric(as.data.frameY(gef2x)$Gefd)
  resultHoriz <- as.numeric(as.data.frameY(gefHoriz)$Gefd)
  result <- c(resultFixed, result2x, resultHoriz)
  result
}

##calc applies the function to the stack of latitude and irradiation
gefS <- calc(stack(latLayer, SISmm), foo,
             filename='/home/oscar/Datos/CMSAF/gefCMSAFTracking',##change to your folder
             overwrite=TRUE)
layerNames(gefS)=c('Fixed', '2x', 'Horiz')

gefS <- stack('/home/oscar/Datos/CMSAF/gefCMSAFTracking')
layerNames(gefS)=c('Fixed', '2x', 'Horiz')

