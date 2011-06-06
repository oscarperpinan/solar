library(sp)
library(maptools)
library(raster)
library(gstat)

##Latitud y longitud de (algunas) estaciones de SIAR
load('/home/oscar/Investigacion/solar/drafts/redGN.RData')


foo <- function(x){
  prov=as.numeric(x[3])
  est=as.numeric(x[4])
  tryError <- try(meteo <- readSIAR(prov=prov, est=est,start='01/01/2008', end='31/12/2008'))
  g0 <- getG0(meteo)
  G0dm <- aggregate(g0, by=as.yearmon, FUN=function(x, ...)mean(x, na.rm=1))
  idx<- seq(from=as.POSIXct('2008-01-01'), to=as.POSIXct('2008-12-31'), by='month')
  idx <- as.yearmon(idx)
  G0dm
}

smp <- redGN[1:25,]
dataG0dm <- apply(smp, 1, foo)
dataG0dm <- t(dataG0dm)
colnames(dataG0dm) <- month.abb
smp <- cbind(smp, dataG0dm)

proj <- CRS('+proj=latlon +ellps=WGS84')
spSMP <- SpatialPointsDataFrame(coords=smp[c('lng', 'lat')],
                                   data=smp[,7:18],
                                   proj4string=proj)

box <- t(bbox(spSMP))

grd <- expand.grid(lng=seq(box[1,1], box[2,1], .1), lat=seq(box[1,2], box[2,2], .1))
coordinates(grd) <- ~lng+lat
gridded(grd) <- TRUE
proj4string(grd) <- proj
krigeG0dm <- krige(Mar~1, spSMP, grd)
spplot(krigeG0dm)+layer(sp.points(spSMP, pch=19, col='black', cex=0.7))

##Datos de 2008 de CMSAF
listFich <- dir('/home/oscar/Datos/CMSAF/', pattern='2008')
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
setwd(old)

