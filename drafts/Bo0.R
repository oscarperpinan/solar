###Extra-terrestial irradiance (only geometric computations)
library(solaR)
library(maps)
library(sp)
library(mapdata)
library(maptools)
library(geonames)

hh <- as.POSIXct('2011-05-01 11:00:00', tz='CET')

longitude <- seq(-180, 180, 1)
horaLong <- local2Solar(hh, longitude)
latitude <- seq(70, -70, -1)

solList <- lapply(latitude, calcSol, BTi=horaLong)
Bo0List <- lapply(solList, function(x)as.data.frameI(x)$Bo0)
Bo0 <- do.call('c', Bo0List)
Bo0[is.na(Bo0)] <- 0

Bo0DF=expand.grid(lon=longitude, lat=latitude)
Bo0DF$Bo0 <- c(Bo0)

proj <- CRS('+proj=latlon +ellps=WGS84')
Bo0SP <- SpatialPixelsDataFrame(points=Bo0DF[,1:2], data=Bo0DF["Bo0"], proj4string=proj)

paleta=colorRampPalette(rev(brewer.pal('Greys', n=9)))
p <- spplot(Bo0SP, scales=list(draw=TRUE), col.regions=paleta, cuts=50)

world <- map("world", plot=FALSE)
world_sp <- map2SpatialLines(world, proj4string=proj)
p2 <- p+layer(sp.lines(world_sp, lwd=0.5))


idxMax <- which.max(Bo0)
lonlatMax <- Bo0DF[idxMax,1:2]
place <- GNfindNearby(lat=lonlatMax[2], lng=lonlatMax[1])
place <- place[[1]][[1]]$countryName
p3 <- p2 + layer(panel.text(lonlatMax[1], lonlatMax[2], place, cex=0.5))

trellis.device(file='Bo0.pdf', pdf)
p3
dev.off()
