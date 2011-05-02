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
library(hexbin)

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
paleta3=heat.ob

ncuts <- 7
colContour <- paleta3(ncuts)[2]

#spplot(nasaSP[,1:12], col.regions=paleta, layout=c(3,4), as.table=TRUE, sp.layout=mapa)
trellis.device(file='NasaG0y.pdf', pdf)
spplot(nasaSP['Ann'], sp.layout=mapa,
       col.regions=paleta3, colorkey=list(space='bottom'),
       contour=TRUE, cuts=ncuts, col=colContour, lwd=0.5,
       scales=list(draw=TRUE)
       )
dev.off()

##Otra forma con latticeExtra y layer()
p <- spplot(nasaSP['Ann'])
p+layer(sp.lines(world_sp, lwd=0.5))

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

spplot(gefNasaSP, col.regions=paleta3, as.table=TRUE, layout=c(1,3))##, sp.layout=mapa)

## trellis.device(file='NasaGefFixed.pdf', pdf)
## spplot(gefNasaSP['Fixed'], col.regions=paleta3)##, sp.layout=mapa)
## dev.off()

ncuts <- 7
colContour <- paleta3(ncuts)[2]

trellis.device(file='NasaGefTwo.pdf', pdf)
spplot(gefNasaSP['Two'], sp.layout=mapa,
       col.regions=paleta3, colorkey=list(space='bottom'),
       contour=TRUE, cuts=ncuts, col=colContour, lwd=0.5,
       scales=list(draw=TRUE)
       )   
dev.off()

trellis.device(file='NasaGefHoriz.pdf', pdf)
spplot(gefNasaSP['Horiz'], sp.layout=mapa,
       col.regions=paleta3, colorkey=list(space='bottom'),
       contour=TRUE, cuts=ncuts, col=colContour, lwd=0.5,
       scales=list(draw=TRUE)
       )
dev.off()

trellis.device(file='NasaGefFixed.pdf', pdf)
spplot(gefNasaSP['Fixed'], sp.layout=mapa,
       col.regions=paleta3, colorkey=list(space='bottom'),
       contour=TRUE, cuts=ncuts, col=colContour, lwd=0.5,
       scales=list(draw=TRUE)
       )
dev.off()


gefNasaSP$HorizFixed <- gefNasaSP$Horiz/gefNasaSP$Fixed
gefNasaSP$TwoHoriz <- gefNasaSP$Two/gefNasaSP$Horiz
gefNasaSP$TwoFixed <- gefNasaSP$Two/gefNasaSP$Fixed

trellis.device(file='NasaHorizFixed.pdf', pdf)

bwplot(HorizFixed~cut(Lat, pretty(Lat, 40)),
       xlab='Latitude', ylab=expression(G[ef]^{horiz}/G[ef]^{fixed}),
       data=as.data.frame(gefNasaSP),
       horizontal=FALSE,
       panel = function(..., box.ratio) {
         panel.violin(..., col = "lightblue",
                      varwidth = FALSE, box.ratio = box.ratio)
         panel.bwplot(..., col='black',
                      cex=0.8, pch='|', fill='gray', box.ratio = .1)
       },
       par.settings = list(box.rectangle=list(col='black'),
         plot.symbol = list(pch='.', cex = 0.1)),
       scales=list(x=list(rot=45, cex=0.6)),
       subset=(abs(Lat)<60))

dev.off()

trellis.device(file='NasaG0Year.pdf', pdf)
bwplot(Ann~cut(Lat, pretty(Lat, 40)),
       data=nasa, subset=(abs(Lat)<60),
       xlab='Latitude', ylab='G(0) (kWh/m²)',
       horizontal=FALSE,
       panel = function(..., box.ratio) {
         panel.violin(..., col = "lightblue",
                      varwidth = FALSE, box.ratio = box.ratio)
         panel.bwplot(..., col='black',
                      cex=0.8, pch='|', fill='gray', box.ratio = .1)
       },
       par.settings = list(box.rectangle=list(col='black'),
         plot.symbol = list(pch='.', cex = 0.1)),
       scales=list(x=list(rot=45, cex=0.5))
       )
dev.off()

x <- paste(names(nasa)[3:14], collapse='+')
formula <- as.formula(paste(x, '~cut(Lat, pretty(Lat, 20))', sep=''))

trellis.device(file='NasaG0Month.pdf', pdf)
bwplot(formula, data=nasa, subset=(abs(Lat)<60),
       xlab='Latitude', ylab='G(0) (kWh/m²)',
       outer=TRUE, as.table=TRUE, horizontal=FALSE,
       col='lightblue',
       panel=panel.violin,
       scales=list(x=list(rot=70, cex=0.5))
       )
dev.off()

