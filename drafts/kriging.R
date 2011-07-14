library(sp)
library(maptools)
library(raster)
library(rasterVis)
library(gstat)
library(lattice)
library(latticeExtra)
library(solaR)##instalar version 0.24

##Función para hacer kriging con Raster
##Devuelve un RasterStack
##La primera capa es la predicción y la segunda es la varianza
krigeRaster <- function(formula, data, raster, ...){
  latLayer <- init(raster, v='y')
  lonLayer <- init(raster, v='x')

  grd <- as(stack(lonLayer, latLayer, raster), 'SpatialGridDataFrame')
  names(grd) <- c('lon', 'lat', deparse(substitute(raster)))
  proj4string(grd) <- proj

  resSP <- krige(formula, data, grd, ...)
  res <- as(resSP, 'RasterStack')
  layerNames(res) <- c('pred', 'var')
  res
}


######  RECUPERAMOS DATOS  ######################################

###Obtenidos en UTMLonLat.R
##Contiene una lista de objetos Meteo (spainMeteo) y el índice de los
##que sirven (idxMeteo). De esta forma, trabajamos con
##spainMeteoOK<-spainMeteo[idxMeteo]
load('spainMeteo20110701.RData')

##Obtenidos en Gef_CMSAF_SIAR.R
##Contiene los datos de la red SIAR en formato data.frame y en
##SpatialPointsDataFrame junto con los resultados de cálculo de
##radiación efectiva anual
load('gefSIAR.RData')

##proyección de todos los datos
proj <- CRS(proj4string(spSIARGef))


#####  RADIACION HORIZONTAL  ###########

##Datos de 2008 de CMSAF
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

levelplot(G0yCMSAF)

## #Creo una rejilla para la interpolación
## ##necesito que contenga la latitud y longitud
## ##para el universal kriging
## latLayer <- init(G0yCMSAF, v='y')
## lonLayer <- init(G0yCMSAF, v='x')

## grd <- as(stack(lonLayer, latLayer, G0yCMSAF), 'SpatialGridDataFrame')
## names(grd) <- c('lon', 'lat', 'G0yCMSAF')
## proj4string(grd) <- proj

##Descargo y descomprimo un zip de http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip
##Contiene un Shapefile con información de las fronteras entre provincias de españa.
old <- getwd()
setwd('/home/oscar/Datos/ESP_adm')##Cambiar!!!
##Leo el contenido:
mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string=proj)
setwd(old)

###Trabajaremos sólo con aquelas estaciones con al menos dos años
###completos y 1000 kWh/m2 anuales de radiación
spGef <- spSIARGef[spSIARGef$ndays>=730 & spSIARGef$G0y>=1000,]
idxNms <- which(names(spGef) %in% c('G0y', 'Fixed', 'Two', 'Horiz'))
names(spGef)[idxNms] <- paste(names(spGef)[idxNms], 'SIAR', sep='')
##Ahora incorporo al SpatialPointsDataFrame una capa más
##con los valores de CMSAF en las posiciones de las estaciones
##con la función extract de raster
spGef$G0yCMSAF <- extract(G0yCMSAF, spGef)
##y también calculo la diferencia entre ambas
spGef$difG0y <-spGef$G0ySIAR-spGef$G0yCMSAF

###Análisis de valores (sin considerar caracter espacial)
datGef <- as.data.frame(spGef)
##Ejemplos...
##diferencia frente a la latitud para grupos de G0ySIAR
xyplot(difG0y~lat, groups=cut(G0ySIAR, 5), data=datGef, auto.key=list(space='right'))

##y 5 grupos de CMSAF
xyplot(difG0y~lat, groups=cut(G0yCMSAF, 5), data=datGef, auto.key=list(space='right'))
##G0y frente a CMSAF para cinco grupos de latitud
xyplot(G0ySIAR~G0yCMSAF, groups=cut(lat, 5), data=datGef)

bwplot(difG0y~cut(lat, 10), data=datGef)
###etc, etc,

####Empieza el análisis estadístico

##Ajuste del variograma
vgmG0y <- variogram(G0ySIAR~1, data=spGef)
plot(vgmG0y)
##por inspección parece que el modelo esférico ajusta bien
modelG0y <- vgm(psill=17000, model='Sph', range=200, nugget=5000)
fitvgmG0y <- fit.variogram(vgmG0y, modelG0y)
plot(vgmG0y, fitvgmG0y)

##Tipos de Interpolación:

##En primer lugar, el método IDW
idwG0y <- krigeRaster(G0ySIAR~1, spGef, G0yCMSAF)

levelplot(idwG0y, layer='pred') +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

##En segundo lugar un ajuste de superficie
surfG0y <- krigeRaster(G0ySIAR~1, spGef, G0yCMSAF, degree=2)

levelplot(surfG0y, layer='pred') +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

levelplot(surfG0y, layer='var')+
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

##En tercer lugar, un ordinary kriging usando el variograma
okrigG0y <- krigeRaster(G0ySIAR~1, spGef, G0yCMSAF, model=fitvgmG0y)

levelplot(okrigG0y, layer='pred') +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

levelplot(okrigG0y, layer='var') +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

##En cuarto lugar, universal kriging usando la latitud y longitud
LLkrigG0y <- krigeRaster(G0ySIAR~lat+lon, spGef, G0yCMSAF, model=fitvgmG0y)

levelplot(LLkrigG0y, layer='pred') +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

levelplot(LLkrigG0y, layer='var') +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

##Diagnostico
surf <- krige.cv(G0ySIAR~1, spGef, degree=2)
LL <- krige.cv(G0ySIAR~lat+lon, spGef, model=fitvgmG0y)
ord <- krige.cv(G0ySIAR~1, spGef, model=fitvgmG0y)

surfPlot <- bubble(surf, 'residual')
llPlot <- bubble(LL, 'residual')
ordPlot <- bubble(ord, 'residual')


##En quinto lugar, universal kriging usando datos de CMSAF
##Primero hay que ajustar nuevamente el variograma
vgmCMSAF <- variogram(G0ySIAR~G0yCMSAF, spGef)
plot(vgmCMSAF)
fitvgmCMSAF <- fit.variogram(vgmCMSAF, vgm(psill=12000, model='Sph', range=100, nugget=5000))
plot(vgmCMSAF, fitvgmCMSAF)

CMSAFkrigG0y <- krigeRaster(G0ySIAR~G0yCMSAF, spGef, G0yCMSAF, model=fitvgmCMSAF)

levelplot(CMSAFkrigG0y, layer='pred') +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))

levelplot(CMSAFkrigG0y, layer='var', par.settings=BTCTheme) +
  layer(sp.points(spGef, pch=19, cex=0.3, col='black')) +
  layer(sp.lines(mapaSHP))
##Ejemplos de representación de resultados
##más ejemplos en http://rastervis.r-forge.r-project.org/
histogram(CMSAFkrigG0y)
splom(CMSAFkrigG0y)
xyplot(var~pred|cut(y, 6), CMSAFkrigG0y)
xyplot(pred~y, CMSAFkrigG0y)
xyplot(var~y, CMSAFkrigG0y)


##### RADIACION EFECTIVA #######

##Resultados de 2008 de CMSAF
##compongo un objeto stack de la librería raster
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
gefCMSAF <- stack('gefCMSAFTracking')
layerNames(gefCMSAF) <- c('Fixed', 'Two', 'Horiz')
setwd(old)

gefExtract <- as.data.frame(extract(gefCMSAF, spGef))
names(gefExtract) <- paste(layerNames(gefCMSAF), 'CMSAF', sep='')

spGef <- spCbind(spGef, gefExtract)
datGef <- as.data.frame(spGef)

## Sistemas Estáticos
##Se trata de sustituir G0y por Fixed utilizando
##el código anterior para radiación horizontal
FixedCMSAF <- raster(gefCMSAF, layer='Fixed')

vgmFixed <- variogram(FixedSIAR~1, data=spGef)
plot(vgmFixed)
##COMPROBAR QUE LOS VALORES SIGUEN SIENDO ADECUADOS
modelFixed <- vgm(psill=17000, model='Sph', range=200, nugget=5000)
fitvgmFixed <- fit.variogram(vgmFixed, modelFixed)
plot(vgmFixed, fitvgmFixed)


##En primer lugar, el método IDW
idwFixed <- krigeRaster(FixedSIAR~1, spGef, FixedCMSAF)

###etc etc
