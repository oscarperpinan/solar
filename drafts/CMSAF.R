## ##extraido de plot.TimeSeries.R de CMSAF_examples
####Ahora con la libreria RASTER
library(raster)

library(hexbin)
paleta=colorRampPalette(rev(brewer.pal('YlOrRd', n=9)))
paleta2=colorRampPalette((brewer.pal('RdBu', n=11)))
paleta3=heat.ob
paletaTierra=terrain.colors

# Open the netcdf-file
nc <- raster('~/Datos/CMSAF/SISmm199401010000001170030701MH.nc')
nc@zvalue##time

nccrop <- crop(nc, extent(-10, 5, 35, 45),
               filename='~/Datos/CMSAF/crop.nc')
nccrop
plot(nccrop)

ncsample <- sampleRegular(nccrop, 1e5, asRaster=TRUE)
ncsp <- as(ncsample, 'SpatialGridDataFrame')
spplot(ncsp, contour=TRUE, scales=list(draw=TRUE))

load('/home/oscar/Investigacion/solar/drafts/redGN.RData')
proj <- CRS('+proj=latlon +ellps=WGS84')
redGN$datos <- 10
spRedGN <- SpatialPointsDataFrame(coords=redGN[c('lng', 'lat')],
                                   data=redGN['datos'],
                                   proj4string=proj)

redGNraster <- rasterize(spRedGN, nccrop)

#¿Sirve z-value para almacenar el index temporal?
zvalue <- function(...){
  dots <- list(...)
  zzz <- lapply(dots, function(x)paste(unlist(strsplit(x@zvalue, '-')), collapse=''))
  res <- do.call(c, zzz)
  res
  }

l8 <- raster('~/Datos/CMSAF/SISmm198805010000001170030501MH.nc')
l9 <- raster('~/Datos/CMSAF/SISmm198806010000001170030501MH.nc')
l10 <- raster('~/Datos/CMSAF/SISmm198807010000001170030501MH.nc')
b <- stack(l8, l9, l10)
b@zvalue <- zvalue(l8, l9, l10)

bcrop <- crop(b, extent(-10, 5, 35, 45), filename='~/Datos/CMSAF/SIScrop.nc')

bsp <- as(bcrop*24/1000, 'SpatialGridDataFrame')
names(bsp) <- paste('T', zvalue(l8, l9, l10), sep='')


##Descargo y descomprimo un zip de http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip
##Contiene un Shapefile con información de las fronteras entre provincias de españa.
proj <- CRS('+proj=latlon +ellps=WGS84')
old <- getwd()
setwd('/home/oscar/Datos')##Cambiar!!!
##Leo el contenido:
library(maptools)
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)
setwd(old)
##y lo transformo en un objeto SpatialLines, con grosr de linea definido por lwd
mapaES <- list('sp.lines', mapaSHP, lwd=0.5)
####

spplot(bsp, sp.layout=mapaES)

##más experimentos con raster
library(raster)
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;

nr=10
nc=10
num2raster <- function(x, nrows=nr, ncols=nc, ...){
  r <- raster(nrows=nrows, ncols=ncols)
  r[] <- x
  r
  }

G0dm2 <- lapply(G0dm, function(x)x+100*rnorm(nc*nr))
s <- stack(lapply(G0dm2, num2raster))
layerNames(s) <- month.abb
spplot(as(s, 'SpatialGridDataFrame'))

foo <- function(x, ...){
  gef <- calcGef(lat=x[1], prom=list(G0dm=x[2:13]))
  result <- as.data.frameY(gef)[c('Gefd', 'Befd', 'Defd')]
  as.numeric(result)
}

latLayer <- raster(s, layer=1)
layerNames(latLayer) <- 'Latitude'
latLayer[] <- yFromCell(s, 1:ncell(s))


gefS <- calc(stack(latLayer, s), foo)
layerNames(gefS)=c('Gefd', 'Befd', 'Defd')
gefSP <- (as(gefS, 'SpatialGridDataFrame'))
spplot(gefSP['values.Gefd'])

x <- 1:12
monSeq <- paste(ifelse(x>=10, '', '0'), x, sep='')
listaFich <- paste('/home/oscar/Datos/CMSAF/SISmm1996',monSeq, '010000001170030801MH.nc', sep='')
listaRasters <- lapply(listaFich, raster)
S <- stack(listaRasters)


Scrop <- crop(S, extent(-10, 5, 35, 45), filename='~/Datos/CMSAF/1996mm', overwrite=TRUE)
##Scrop <- brick('~/Datos/CMSAF/1987mm')
Scrop <- Scrop*24/1000##a kWh/m2
Ssample <- sampleRegular(Scrop, size=1e+4, asRaster=TRUE)
layerNames(Ssample) <- month.abb
Ssp <- as(Ssample, 'SpatialGridDataFrame')

ncuts <- 7
colContour <- 'brown'

spplot(Ssp, sp.layout=mapaES,
       col.regions=paleta3, colorkey=list(space='bottom'),
       contour=TRUE, cuts=ncuts, col=colContour, lwd=0.5,
       scales=list(draw=TRUE)
       )


###capas de latitud y longitud
latLayer <- raster(Ssample, layer=1)
layerNames(latLayer) <- 'Latitude'
latLayer[] <- yFromCell(Ssample, 1:ncell(Ssample))

zLat <- zonal(Ssample, latLayer, mean, digits=1)
nms <- paste(names(zLat)[2:13], collapse='+')
formula <- as.formula(paste(nms, '~zone', sep=''))
p <- xyplot(formula, data=zLat, type='l')
p+glayer(panel.text(x[10], y[10], group.number))

lonLayer <- raster(Ssample, layer=1)
layerNames(lonLayer) <- 'Longitude'
lonLayer[] <- xFromCell(Ssample, 1:ncell(Ssample))

zLon <- zonal(Ssample, lonLayer, mean, digits=1)
nms <- paste(names(zLon)[2:13], collapse='+')
formula <- as.formula(paste(nms, '~zone', sep=''))
p <- xyplot(formula, data=zLon, type='l')
p+glayer(panel.text(x[10], y[10], group.number))


###Datos de elevación disponibles en http://diva-gis.org/data/
##por alguna razón getData no es capaz de descargar adecuadamente la información
elevES <- raster('/home/oscar/Datos/ESP_alt/ESP_alt.grd')##ESP1 es España peninsular
projection(elevES) <- "+proj=longlat +datum=WGS84"
elevESsp <- as(sampleRegular(elevES, 5e5, asRaster=TRUE), 'SpatialGridDataFrame')

ncuts <- 7
colContour <- 'brown'

 spplot(elevESsp, sp.layout=mapaES,
       col.regions=paletaTierra, colorkey=list(space='bottom'),
       contour=TRUE, cuts=ncuts, col=colContour, lwd=0.5,
       scales=list(draw=TRUE)
       )

ScropElevES <- crop(S, elevES)
elevESrs <- resample(elevES, ScropElevES, 'bilinear')
ScropElevES <- mask(ScropElevES, elevESrs)
ScropElevES <- ScropElevES*24/1000##a kWh/m2

SsmpElevES <- sampleRegular(ScropElevES, size=ncell(ScropElevES), asRaster=TRUE)
layerNames(SsmpElevES) <- month.abb
Ssp <- as(SsmpElevES, 'SpatialGridDataFrame')

ncuts <- 7
colContour <- 'brown'

spplot(Ssp, ##sp.layout=mapaES,
       col.regions=paleta3, colorkey=list(space='bottom'),
       ##contour=TRUE, cuts=ncuts, col=colContour, lwd=0.5,
       scales=list(draw=TRUE)
       )
###slopeAspect
norteSpain <- crop(elevES, extent(-5, 5, 40, 45))
slas <- slopeAspect(norteSpain)
south <- sign(slas[[2]]*180/pi-180)
slopeSign <- slas[[1]]*south
slopeSign.sp <- as(slopeSign, 'SpatialGridDataFrame')
spplot(slopeSign.sp)

hora <- as.POSIXct('2010-06-01 11:00:00', tz='CET')

longNorteSpain <- xFromCol(norteSpain, 1:ncol(norteSpain))
horaLong <- local2Solar(hora, longNorteSpain)

foo <- function(lat, hora=horaLong){
  sol <- calcSol(lat=lat, BTi=hora)
  angles <- as.data.frameI(sol)[c('AzS', 'AlS')]
  rownames(angles) <- NULL
  r2d(angles)
  }

solRaster <- brick(stack(norteSpain, norteSpain), values=FALSE)
solRaster <- writeStart(solRaster, filename='norteSpain', overwrite=TRUE)
for (i in seq_len(nrow(norteSpain))){
  tmp <- foo(lat=yFromRow(norteSpain, i), hora=horaLong)
  solRaster <- writeValues(solRaster, as.matrix(tmp), i)
  }
layerNames(solRaster) <- c('AzS', 'AlS')
solRaster <- writeStop(solRaster)
plot(mask(solRaster, norteSpain), col=heat.colors(255))
## latNorteSpain <- yFromRow(norteSpain, 1:nrow(norteSpain))
## angLatLong <- lapply(latNorteSpain, foo, horaLong)
## angLatLong <- do.call('rbind', angLatLong)

## azsRaster <- raster(norteSpain)
## azsRaster[] <- angLatLong$AzS

## alsRaster <- raster(norteSpain)
## alsRaster[] <- angLatLong$AlS

## solRaster <- stack(azsRaster, alsRaster)
## layerNames(solRaster) <- c('AzS', 'AlS')
solSP <- as(solRaster, 'SpatialGridDataFrame')
spplot(solSP['values.AlS'], contour=TRUE)
spplot(solSP['values.AzS'])

shd <- hillShade(slas[[1]], slas[[2]], solRaster[[2]], solRaster[[1]])
plot(shd, col=grey(0:100/100))
plot(norteSpain, add=TRUE)



x <- mask(solRaster, norteSpain)
plot3D(x[[2]])
xsp <- as(sampleRegular(x, 1e5, asRaster=TRUE), 'SpatialGridDataFrame')
spplot(xsp['values.AlS'], contour=TRUE, col.regions=heat.colors)

####2011-05-26
library(sp)
library(maptools)
library(raster)

load('/home/oscar/Investigacion/solar/drafts/redGN.RData')
proj <- CRS('+proj=latlon +ellps=WGS84')
spRedGN <- SpatialPoints(coords=redGN[c('lng', 'lat')],
                        ##           data=redGN['datos'],
                                   proj4string=proj)


old <- getwd()
setwd('/home/oscar/Datos')##Cambiar!!!
##Leo el contenido:
library(maptools)
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)
setwd(old)

library(latticeExtra)
layerBound <- layer(sp.lines(mapaSHP, lwd=0.6))
layerPoints <- layer(sp.points(spRedGN, col='black', pch=19, cex=0.3))

listFich <- dir('/home/oscar/Datos/CMSAF/', pattern='2008')
old <- getwd()
setwd('/home/oscar/Datos/CMSAF')##Cambiar!!!
library(raster)
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
setwd(old)

smp<- sampleRegular(stackSIS, 1e5, asRaster=TRUE)
SP <- as(smp, 'SpatialGridDataFrame')
names(SP) <- month.abb

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

myplot(SP)+layerBound+layerPoints



Jan <- subset(stackSIS, 1)
spJan <- as(sampleRegular(Jan, 1e4, asRaster=TRUE), 'SpatialGridDataFrame')
x <- sampleRandom(Jan, size=500, sp=TRUE)
projection(x) <- projection(spJan)

library(gstat)

vg <- variogram(Data1~1, x)
plot(vg)
kr <- idw(Data1~1, x, spJan)

####Calculo GEF a partir de datos de CMSAF
##Solar irradiation data from CMSAF
##Data available from http://www.box.net/shared/rl51y1t9sldxk54ogd44

old <- getwd()
##change to your folder...
setwd('/home/oscar/Datos/CMSAF')
listFich <- dir(pattern='2008')
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- fBTd('prom', year=2008)

SISmm <- setZ(stackSIS, idx)
layerNames(SISmm) <- as.character(idx)

spplot(SISmm, names.attr=layerNames(SISmm))

##Improvements to spplot
xscale.raster <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale.raster <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

myTheme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
myTheme$strip.background$col='transparent'
myTheme$strip.shingle$col='transparent'
myTheme$strip.border$col='transparent'

p <- spplot(SISmm,
            names.attr=layerNames(SISmm),
            as.table=TRUE,
            par.settings=myTheme,
            between=list(x=0.5, y=0.2),
            scales=list(draw=TRUE),
            xscale.components=xscale.raster,
            yscale.components=yscale.raster)

##Administrative borders for Spain
##http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip
##It is also available with raster::getData although it does not work for me...
library(maptools)
proj <- CRS('+proj=latlon +ellps=WGS84')
old <- getwd()
setwd('/home/oscar/Datos/ESP_adm')##Cambiar!!!
mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string=proj)
setwd(old)

trellis.device(jpeg, file='CMSAF_G0dm.jpg', width=1280, height=960, quality=100)
p + layer(sp.lines(mapaSHP))
dev.off()

##Calculation of yearly effective irradiation
##A latitude layer for calculations with solaR::calcGef
latLayer <- init(SISmm, v='y')

##The function to be applied to each cell of the 13 layers
foo <- function(x, ...){
  gef <- calcGef(lat=x[1], dataRad=list(G0dm=x[2:13]))
  result <- as.data.frameY(gef)[c('Gefd', 'Befd', 'Defd')]##the results are yearly values
  as.numeric(result)
}
##calc applies the function to the stack of latitude and irradiation
gefS <- calc(stack(latLayer, SISmm), foo,
             filename='/home/oscar/Datos/CMSAF/gefCMSAF',##change to your folder
             overwrite=TRUE)
layerNames(gefS)=c('Gefd', 'Befd', 'Defd')##Three layers

##The result is available here:
##http://www.box.net/shared/is4gdf5ltkhdqlvf5aeb
gefS <- stack('/home/oscar/Datos/CMSAF/gefCMSAF.grd')
layerNames(gefS)=c('Gefd', 'Befd', 'Defd')

library(rasterVis)
trellis.device(pdf, file='CMSAF_Gef.pdf')
levelplot(gefS, layers='Gefd') + layer(sp.lines(mapaSHP, lwd=0.6))
dev.off()

## trellis.device(jpeg, file='CMSAF_Gef.jpg', width=1280, height=960, quality=100)
## spplot(subset(gefS, 'Gefd'), par.settings=myTheme, scales=list(draw=TRUE)) +  layer(sp.lines(mapaSHP))
## dev.off()
