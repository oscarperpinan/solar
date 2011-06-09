library(sp)
library(maptools)
library(raster)
library(gstat)

source('/home/oscar/Investigacion/solar/drafts/mySPplot.R')

##proyección de todos los datos
proj <- CRS('+proj=latlon +ellps=WGS84')

##Descargo y descomprimo un zip de http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip
##Contiene un Shapefile con información de las fronteras entre provincias de españa.
old <- getwd()
setwd('/home/oscar/Datos/ESP_adm')##Cambiar!!!
##Leo el contenido:
mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string=proj)
setwd(old)

##Latitud y longitud de (algunas) estaciones de SIAR
##construido en drafts/redEstaciones.R

load('/home/oscar/Investigacion/solar/drafts/redGN.RData')

##Objecto SpatialPointsDataFrame
spRedGN <- SpatialPointsDataFrame(coords=redGN[c('lng', 'lat')],
                                   data=redGN[c('NomProv', 'NomEst')],
                                   proj4string=proj)


###Obtengo todos los datos de estas estaciones desde el 2004 al 2010
##En la llamada a APPLY debo eliminar las dos últimas columnas (caracteres)
##para que no convierta todo a character

##No ejecutar (los resultados están en spainMeteo.RData, ver a continuación)
spainMeteo <- apply(redGN[, 1:4], 1,
                    function(x){
                      readSIAR(prov=x[3], est=x[4],
                               start='01/01/2004', end='31/12/2010',
                               lat=x[1])
                    }
                    )
##el resultado es una lista de objetos Meteo,
##cada uno el resultado de una estación con datos
##desde el 2004 hasta el 2010.

##save(file='spainMeteo', spainMeteo)
old <- getwd()
setwd('/home/oscar/Datos/MAPA_SIAR/')##CAMBIAR!!
load('spainMeteo.RData')
setwd(old)
                    
####Medias de sumas anuales
meanYearlySums <- function(x)mean(aggregate(getG0(x), year, sum, na.rm=1))

##aplico la función a cada una de las estaciones que componen la lista spainMeteo
##divido entre 1000 para pasar a kWh
spainG0y <- sapply(spainMeteo, meanYearlySums)/1000
##añado el resultado como una capa más al SpatialPointsDataFrame
spRedGN$G0y=spainG0y

##dejo fuera canarias para que el kriging funcione bien...lo siento :-)
spRedGN <- spRedGN[coordinates(spRedGN)[,2]>30,]

##Datos de 2008 de CMSAFç
##compongo un objeto stack de la librería raster
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
listFich <- dir(pattern='2008')
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##para pasar de W/m2 (irradiancia media) a Wh/m2
setwd(old)

##Para calcular el valor anual de cada celda debo multiplicar cada capa
##por el número de días de cada mes
DiasMes <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
##y sumar todas las capas, nuevamente dividiendo entre mil
G0yCMSAF <- calc(stackSIS*DiasMes, sum)/1000

##Ahora incorporo información sobre elevación
elevES <- raster('/home/oscar/Datos/ESP_alt/ESP_alt.grd')
projection(elevES) <- "+proj=longlat +datum=WGS84"

G0yCMSAF <- crop(G0yCMSAF, elevES)##igualo la extensión de ambos rasters
elevES <- resample(elevES, G0yCMSAF, 'bilinear')##remuestro el raster de elevación para igualarlo a CMSAF
##G0yCMSAF <- mask(G0yCMSAF, elevES)##finalmente pongo a "NA" todo lo que está a nivel del mar

spplot(elevES)
spplot(G0yCMSAF)

##Ahora incorporo al SpatialPointsDataFrame una capa más
##con los valores de CMSAF en las posiciones de las estaciones
##con la función extract de raster
spRedGN$CMSAF <- extract(G0yCMSAF, spRedGN)
##y también calculo la diferencia entre ambas
spRedGN$dif <-spRedGN$G0y-spRedGN$CMSAF
##Lo mismo con datos de elevación
spRedGN$elev <- extract(elevES, spRedGN)


####Empieza el análisis estadístico

##Ajuste del variograma
vgmG0y <- variogram(G0y~1, data=spRedGN)
plot(vgmG0y)
##por inspección parece que el modelo esférico ajusta bien
##con los valores 25000 y 1
##y con un nugget de 10000
fitvgmG0y <- fit.variogram(vgmG0y, vgm(25000, 'Sph', 1, 10000))
plot(vgmG0y, fitvgmG0y)

##Creo una rejilla para la interpolación
##necesito que contenga la latitud y longitud
##para el universal kriging
latLayer <- raster(G0yCMSAF)
latLayer[] <- yFromCell(G0yCMSAF, 1:ncell(G0yCMSAF))
lonLayer <- raster(G0yCMSAF)
lonLayer[] <- xFromCell(G0yCMSAF, 1:ncell(G0yCMSAF))

grd <- as(stack(lonLayer, latLayer, G0yCMSAF, elevES), 'SpatialGridDataFrame')
names(grd) <- c('lng', 'lat', 'CMSAF', 'elev')
proj4string(grd) <- proj

##Empieza la interpolación##

##En primer lugar, el método IDW
idwG0y <- krige(G0y~1, spRedGN, grd)

mySPplot(idwG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))

##En segundo lugar un ajuste de superficie
surfG0y <- krige(G0y~1, spRedGN, grd, degree=2)

mySPplot(surfG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))

##En tercer lugar, un ordinary kriging usando el variograma
okrigG0y <- krige(G0y~1, spRedGN, grd, model=fitvgmG0y)

mySPplot(okrigG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))

##En cuarto lugar, universal kriging usando la latitud y longitud
LLkrigG0y <- krige(G0y~lat+lng, spRedGN, grd, model=fitvgmG0y)

mySPplot(LLkrigG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))

##Diagnostico
surf <- krige.cv(G0y~1, spRedGN, degree=2)
LL <- krige.cv(G0y~lat+lng, spRedGN, model=fitvgmG0y)
ord <- krige.cv(G0y~1, spRedGN, model=fitvgmG0y)

surfPlot <- bubble(surf, 'residual')
llPlot <- bubble(LL, 'residual')
ordPlot <- bubble(ord, 'residual')


##En quinto lugar, universal kriging usando datos de CMSAF
##Primero hay que ajustar nuevamente el variograma
vgmCMSAF <- variogram(G0y~CMSAF, spRedGN)
fitvgmCMSAF <- fit.variogram(vgmCMSAF, vgm(20000, 'Sph', 1, 10000))
plot(vgmCMSAF, fitvgmCMSAF)

CMSAFkrigG0y <- krige(G0y~CMSAF, spRedGN, grd, model=fitvgmCMSAF)

mySPplot(CMSAFkrigG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))


vgmelev <- variogram(G0y~elev, spRedGN[!is.na(spRedGN$elev),])
fitvgmelev <- fit.variogram(vgmelev, vgm(20000, 'Sph', 1, 10000))
plot(vgmelev, fitvgmelev)

elevkrigG0y <- krige(G0y~elev, spRedGN[!is.na(spRedGN$elev),], grd, model=fitvgmelev)

mySPplot(elevkrigG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))






####MEDIAS MENSUALES, en elaboración
## foo <- function(x){
##   prov=as.numeric(x[3])
##   est=as.numeric(x[4])
##   tryError <- try(meteo <- readSIAR(prov=prov, est=est,start='01/01/2008', end='31/12/2008'))
##   g0 <- getG0(meteo)
##   G0dm <- aggregate(g0, by=as.yearmon, FUN=function(x, ...)mean(x, na.rm=1))
##   idx<- seq(from=as.POSIXct('2008-01-01'), to=as.POSIXct('2008-12-31'), by='month')
##   idx <- as.yearmon(idx)
##   G0dm
## }

## smp <- redGN[1:25,]
## dataG0dm <- apply(smp, 1, foo)
## dataG0dm <- t(dataG0dm)
## colnames(dataG0dm) <- month.abb
## smp <- cbind(smp, dataG0dm)

## proj <- CRS('+proj=latlon +ellps=WGS84')
## spSMP <- SpatialPointsDataFrame(coords=smp[c('lng', 'lat')],
##                                    data=smp[,7:18],
##                                    proj4string=proj)

## box <- t(bbox(spSMP))

## grd <- expand.grid(lng=seq(box[1,1], box[2,1], .1), lat=seq(box[1,2], box[2,2], .1))
## coordinates(grd) <- ~lng+lat
## gridded(grd) <- TRUE
## proj4string(grd) <- proj
## krigeG0dm <- krige(Mar~1, spSMP, grd)
## mySPplot(krigeG0dm)+layer(sp.points(spSMP, pch=19, col='black', cex=0.7))

