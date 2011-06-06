library(sp)
library(maptools)
library(raster)
library(gstat)

##Defino una función para dibujar con todos los parámetros que me gustan
solaR.theme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
solaR.theme$strip.background$col='transparent'##'lightgray'
solaR.theme$strip.shingle$col='transparent'
solaR.theme$strip.border$col='transparent'

xscale <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

myplot <- function(x, par.settings=solaR.theme,
                   between=list(x=0.5, y=0.2),
                   as.table=TRUE,
                   xscale.components=xscale,
                   yscale.components=yscale,
                   ...){
  p <-spplot(x, par.settings=par.settings,
             scales=list(draw=TRUE),
             between=between,
             as.table=as.table,
             xscale.components=xscale.components,
             yscale.components=yscale.components)
  p
}

##Descargo y descomprimo un zip de http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip
##Contiene un Shapefile con información de las fronteras entre provincias de españa.
old <- getwd()
setwd('/home/oscar/Datos')##Cambiar!!!
##Leo el contenido:
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)
setwd(old)

##Latitud y longitud de (algunas) estaciones de SIAR
##construido en drafts/redEstaciones.R

load('/home/oscar/Investigacion/solar/drafts/redGN.RData')

proj <- CRS('+proj=latlon +ellps=WGS84')
##Objecto SpatialPointsDataFrame
spRedGN <- SpatialPointsDataFrame(coords=redGN[c('lng', 'lat')],
                                   data=redGN[c('NomProv', 'NomEst')],
                                   proj4string=proj)


##En la llamada a APPLY debo eliminar las dos últimas columnas (caracteres)
##para que no convierta todo a character

spainMeteo <- apply(redGN[, 1:4], 1,
                    function(x){
                      readMAPA(prov=x[3], est=x[4],
                               start='01/01/2004', end='31/12/2010',
                               lat=x[1])
                    }
                    )
##el resultado es una lista de objetos Meteo,
##cada uno el resultado de una estación con datos
##desde el 2004 hasta el 2010.

##save(file='spainMeteo', spainMeteo)
load('/home/oscar/Datos/MAPA_SIAR/spainMeteo.RData')
                    
####Medias de sumas anuales
meanYearlySums <- function(x)mean(aggregate(getG0(x), year, sum, na.rm=1))

spainG0y <- sapply(spainMeteo, meanYearlySums)/1000

spRedGN$G0y=spainG0y

##dejo fuera canarias para que el kriging funcione bien
spRedGN <- spRedGN[coordinates(spRedGN)[,2]>30,]

## ncuts=15
## paleta=colorRampPalette(brewer.pal(9, 'Reds'))(ncuts)
## spplot(spRedGN['G0y'], sp.layout=mapaES, col.regions=paleta,
##        scales=list(draw=TRUE), ylim=c(35, 45), xlim=c(-10, 5),
##        cuts=ncuts, key.space='right', cex=0.8)


##Datos de 2008 de CMSAF
listFich <- dir('/home/oscar/Datos/CMSAF/', pattern='2008')
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##para pasar de W/m2 (irradiancia media) a Wh/m2
setwd(old)

DiasMes <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
G0yCMSAF <- calc(stackSIS*DiasMes, sum)/1000

spRedGN$CMSAF <- extract(G0yCMSAF, spRedGN)
spRedGN$dif <-spRedGN$G0y-spRedGN$CMSAF

##Datos de elevación
## elevES <- raster('/home/oscar/Datos/ESP_alt/ESP1_alt.grd')##ESP1 es España peninsular
## spRedGN$elev <- extract(elevES, spRedGN)

##Ajuste del variograma
vgmG0y <- variogram(G0y~1, data=spRedGN)
plot(vgmG0y)
##por inspección parece que el modelo esférico ajusta bien
##con los valores 25000 y 1
##y con un nugget de 10000
fitvgmG0y <- fit.variogram(vgmG0y, vgm(25000, 'Sph', 1, 10000))
plot(vgmG0y, fitvgmG0y)

##Creo una rejilla para la interpolación
## box <- t(bbox(spRedGN))
## grd <- expand.grid(lng=seq(box[1,1], box[2,1], .03),
##                    lat=seq(box[1,2], box[2,2], .03))
## coordinates(grd) <- ~lng+lat
## gridded(grd) <- TRUE
## proj4string(grd) <- proj

grd <- as(G0yCMSAF, 'SpatialGridDataFrame')
names(grd) <- 'CMSAF'
proj4string(grd) <- proj
##Empieza la interpolación##

##En primer lugar, el método IDW
idwG0y <- krige(G0y~1, spRedGN, grd)

myplot(idwG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))

##En segundo lugar un ajuste de superficie
surfG0y <- krige(G0y~1, spRedGN, grd, degree=2)

myplot(surfG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))

##En tercer lugar, un ordinary kriging usando el variograma
okrigG0y <- krige(G0y~1, spRedGN, grd, model=fitvgmG0y)

myplot(okrigG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))

##En cuarto lugar, universal kriging usando la latitud y longitud
LLkrigG0y <- krige(G0y~lat+lng, spRedGN, grd, model=fitvgmG0y)

myplot(LLkrigG0y['var1.pred']) +
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

myplot(CMSAFkrigG0y['var1.pred']) +
  layer(sp.points(spRedGN, pch=19, cex=0.7, col='black')) +
  layer(sp.lines(mapaSHP))


####MEDIAS MENSUALES
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

