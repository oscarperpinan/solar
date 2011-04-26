##extraido de plot.TimeSeries.R de CMSAF_examples
library(ncdf)
library(RNetCDF)

# Open the netcdf-file
nc <- open.ncdf('~/temp/SIS_METCH_hrv_cor_moe_110_regular_200401.nc')

# Retrieve the data from the first variable in the netcdf-file 
varname <- nc$var[[1]]$name
field <- get.var.ncdf(nc, varname)
unit <- att.get.ncdf(nc, varname,"units")$value
missval <- att.get.ncdf(nc,varname,"_FillValue")$value
# Close the file
close.ncdf(nc)

# Set the missing data to NA, considering scale.factor and add.offset
scale.factor <- 1.
add.offset <- 0.
#Derive the scale factor and the offset
has.scale <- nc$var[[1]]$hasScaleFact
if (has.scale) scale.factor <- nc$var[[1]]$scaleFact
has.offset <- nc$var[[1]]$hasAddOffset
if (has.offset) add.offset <- nc$var[[1]]$addOffset
# Set the missing values to NA
na.ind <- which(field == missval*scale.factor + add.offset)
field[na.ind] <- NA

#--------------------------------------------------#

# determine the location 
londim <- nc$dim[["lon"]]
lon <- londim$vals
latdim <- nc$dim[["lat"]]
lat <- latdim$vals

#--------------------------------------------------#

# retrieve the time variable
timedim <- nc$dim[["time"]]
nt <- timedim$len
time.unit <- timedim$units
time <- timedim$vals

# Create a R-date-object 
##cambiar
date.time <- as.Date(utcal.nc(time.unit,time,type="s"))

library(spacetime)
proj <- CRS('+proj=latlon +ellps=WGS84')
coords <- expand.grid(lon=signif(lon, 4), lat=signif(lat, 5))
sp <- SpatialPixels(SpatialPoints(coords, proj4string=proj))
data <- data.frame(G0=c(field))
stData <- STFDF(sp, time=xts(seq_along(time), as.Date(time)), data=data)

stplot(stData, scales=list(draw=TRUE))

####Ahora con la libreria RASTER
library(raster)
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

#####
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
proj <- CRS('+proj=latlon +ellps=WGS84')
##Descargo y descomprimo un zip de http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip
##Contiene un Shapefile con información de las fronteras entre provincias de españa.
old <- getwd()
setwd('/home/oscar/Datos')##Cambiar!!!
##Leo el contenido:
library(maptools)
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)
setwd(old)
##y lo transformo en un objeto SpatialLines, con grosr de linea definido por lwd
mapaES <- list('sp.lines', mapaSHP, lwd=0.5)

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
Scrop <- Scrop*24/1000##a kWh/m2
Ssample <- sampleRegular(Scrop, size=1e+4, asRaster=TRUE)
layerNames(Ssample) <- month.abb
Ssp <- as(Ssample, 'SpatialGridDataFrame')
spplot(Ssp, contour=TRUE)##, sp.layout=mapaES)

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
