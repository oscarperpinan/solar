library(latticeExtra)
library(sp)
library(rgdal)
library(maps)
library(mapdata)
library(maptools)
library(mapproj)
gpclibPermit()
library(gstat)
library(animation)
## Los datos y por lo tanto las coordenadas (X,Y) se encuentran en la proyección Lambert Conformal Conic (WGS84)
## Los parámetros de la proyeccion son:
## CENTRAL LONGITUDE: -3.5  (WEST)
## CENTRAL LATITUDE: 40.0 (NORT)
## NUMERO DE CELDILLAS: 67 (en la dirección del eje de las X) por 64 (en la dirección del eje de las Y)
## RESOLUCIÓN: 27 KM.

data <- read.table('/home/oscar/temp/29112010.SWRAD.txt', header=TRUE)
proj <- CRS('+proj=lcc +ellps=WGS84 +lat_0=40 +lon_0=-3.5')
##proj <- CRS('+proj=lcc +lat_1=40 +lat_0=40 +lon_0=-3.5')# +k_0=0.9988085293')# +a=6378298.3 +b=6356657.142669561 +units=m')
coords=data[,1:2]
datos=data[,3:26]
datosSP <- SpatialPixelsDataFrame(points=coords, data=datos, proj4string=proj)


##http://gadm.org/
##http://gadm.org/data/rda/ESP_adm1.RData

## con <- url('http://gadm.org/data/rda/ESP_adm1.RData')
## load(con)
## close(con)
## ##obtengo un objeto llamado gadm
## spain <- gadm
## ##x <- pruneMap(spain)
## xT <- spTransform(spain, proj)


spain <- map("worldHires", plot=FALSE, xlim=c(-15, 10), ylim=c(30, 50))

llCRS <- CRS("+proj=latlong +ellps=WGS84")
spain_sp <- map2SpatialLines(spain, proj4string=llCRS)
spain_proj <- spTransform(spain_sp, proj)

paleta=colorRampPalette(rev(brewer.pal('YlOrRd', n=9)))
mapa <- list('sp.lines', spain_proj)

spplot(datosSP, col.regions=paleta, layout=c(6,4), as.table=TRUE, sp.layout=mapa)
spplot(datosSP["H12"], col.regions=paleta, sp.layout=mapa)

saveMovie(print(spplot(datosSP, layout=c(1,1), col.regions=paleta, sp.layout=mapa, interpolate=TRUE)),
          interval=0.5, loop=1,
          moviename='mapa.gif')
  
v <- variogram(H12~1, datosSP, map=TRUE, cutoff=85000, width=200)


######AEMET

temperatura <- readGDAL('/home/oscar/temp/20101219000000_sfc_fc30')
