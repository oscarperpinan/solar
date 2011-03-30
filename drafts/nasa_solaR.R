library(lattice)
library(latticeExtra)
library(solaR)
library(sp)
#library(rgdal)
library(maps)
library(mapdata)
library(maptools)
#library(mapproj)
#gpclibPermit()
library(gstat)
#library(animation)

load('nasa_solaR.RData')

nasafile <- 'http://eosweb.larc.nasa.gov/sse/global/text/global_radiation'
nasa <- read.table(file=nasafile, skip=13, header=TRUE)

proj <- CRS('+proj=latlon +ellps=WGS84')
coords=nasa[,2:1]
datos=nasa[,3:15]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=datos, proj4string=proj)

## world <- map("worldHires", plot=FALSE)
world <- map("world", plot=FALSE)

llCRS <- CRS("+proj=latlong +ellps=WGS84")
world_sp <- map2SpatialLines(world, proj4string=llCRS)
mapa <- list('sp.lines', world_sp, lwd=0.5)

paleta=colorRampPalette(rev(brewer.pal('YlOrRd', n=9)))
paleta2=colorRampPalette((brewer.pal('RdBu', n=11)))
library(hexbin)
paleta3=heat.ob

#spplot(nasaSP[,1:12], col.regions=paleta, layout=c(3,4), as.table=TRUE, sp.layout=mapa)
trellis.device(file='NasaG0y.pdf', pdf)
spplot(nasaSP["Ann"], col.regions=paleta3, sp.layout=mapa)
dev.off()

###
N=nrow(nasa)
gefNasa <- matrix(nrow=N, ncol=3)
for (i in 1:N){
  prom=list(G0dm=as.numeric(nasa[i,3:14]*1000))
  lat=nasa[i,1]
  gefFixed <- calcGef(lat=lat, prom=prom)
  gef2x <- calcGef(lat=lat, modeRad='prev', prev=gefFixed, modeTrk='two')
  gefHoriz <- calcGef(lat=lat, modeRad='prev', prev=gefFixed, modeTrk='horiz')
  gefNasa[i, ] <- sapply(list(gefFixed, gef2x, gefHoriz), function(x)as.data.frameY(x)$Gefd)
  print(i)
  }

##load('nasa_solaR.RData'
gefNasaDF <- as.data.frame(gefNasa)
names(gefNasaDF) <- c('Fixed', 'Two', 'Horiz')
gefNasaSP <- SpatialPixelsDataFrame(points=coords, data=gefNasaDF, proj4string=proj)

spplot(gefNasaSP, col.regions=paleta3, as.table=TRUE, layout=c(1,3), sp.layout=mapa)

trellis.device(file='NasaGefFixed.pdf', pdf)
spplot(gefNasaSP['Fixed'], col.regions=paleta3, sp.layout=mapa)
dev.off()

trellis.device(file='NasaGefTwo.pdf', pdf)
spplot(gefNasaSP['Two'], col.regions=paleta3, sp.layout=mapa)
dev.off()

trellis.device(file='NasaGefHoriz.pdf', pdf)
spplot(gefNasaSP['Horiz'], col.regions=paleta3, sp.layout=mapa)
dev.off()

gefNasaSP$HorizFixed <- gefNasaSP$Horiz/gefNasaSP$Fixed
gefNasaSP$TwoHoriz <- gefNasaSP$Two/gefNasaSP$Horiz
gefNasaSP$TwoFixed <- gefNasaSP$Two/gefNasaSP$Fixed

spplot(gefNasaSP[c('TwoHoriz', 'TwoFixed', 'HorizFixed')],
       strip=FALSE, strip.left=TRUE, layout=c(1, 3),
       col.regions=paleta, contour=TRUE)# sp.layout=mapa)



