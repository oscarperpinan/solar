library(sp)
library(rgdal)
library(maptools)


##Leo fronteras administrativas
old <- getwd()
setwd('/home/oscar/Datos/ESP_adm')##Cambiar!!!
proj <- CRS('+proj=latlon +ellps=WGS84')
mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string=proj)
setwd(old)

datos <- read.csv2('/home/oscar/Investigacion/solar/drafts/UTM_latlon_2.csv',
                   colClasses=c('factor', 'factor', 'factor', 'factor', 
                                   'numeric', 'numeric', 'numeric',
                                   'integer', 'character', 'character',
                                   'character')
                   )

datos <- subset(datos, subset=!(is.na(Huso) & Latitud=='' & Longitud==''))
datosUTM29 <- subset(datos, Huso==29, select=-c(Latitud, Longitud, SignoLongitud))
datosUTM30 <- subset(datos, Huso==30, select=-c(Latitud, Longitud, SignoLongitud))
datosUTM31 <- subset(datos, Huso==31, select=-c(Latitud, Longitud, SignoLongitud))

datosLonLat <- subset(datos, is.na(Huso), select=-c(UTMX, UTMY, Huso))

datosLonLat$Latitud <- sub("''", "\"", datosLonLat$Latitud)
datosLonLat$Latitud <- sub("°", "º", datosLonLat$Latitud)
datosLonLat$Latitud <- gsub(" ", "", datosLonLat$Latitud)

datosLonLat$Longitud <- sub("''", "\"", datosLonLat$Longitud)
datosLonLat$Longitud <- sub("°", "º", datosLonLat$Longitud)
datosLonLat$Longitud <- gsub(" ", "", datosLonLat$Longitud)

datosLonLat$Latitud <- with(datosLonLat, as.numeric(char2dms(Latitud, chd="º", chm="'", chs="\"")))
datosLonLat$Longitud <- with(datosLonLat, as.numeric(char2dms(Longitud, chd="º", chm="'", chs="\"")))
datosLonLat$Longitud <- ifelse(datosLonLat$SignoLongitud=='-', -datosLonLat$Longitud, datosLonLat$Longitud)
datosLonLat$SignoLongitud <- NULL

##datos[is.na(datos)] <- 0


projUTM30 <- CRS('+proj=utm +zone=30')
projUTM31 <- CRS('+proj=utm +zone=31')
projUTM29<- CRS('+proj=utm +zone=29')

SPUTM29 <- SpatialPointsDataFrame(coords=datosUTM29[c("UTMX", "UTMY")],
                             data=datosUTM29[c('Altitud', 'Estacion', 'Provincia', 'Comunidad', 'Numero')],
                    proj4string=projUTM29)
SPUTM30 <- SpatialPointsDataFrame(coords=datosUTM30[c("UTMX", "UTMY")],
                             data=datosUTM30[c('Altitud', 'Estacion', 'Provincia', 'Comunidad', 'Numero')],
                    proj4string=projUTM30)
SPUTM31 <- SpatialPointsDataFrame(coords=datosUTM31[c("UTMX", "UTMY")],
                             data=datosUTM31[c('Altitud', 'Estacion', 'Provincia', 'Comunidad', 'Numero')],
                    proj4string=projUTM31)

##Transformo de UTM a long-lat
SPlonlat29 <- spTransform(SPUTM29, CRS("+proj=longlat"))
SPlonlat30 <- spTransform(SPUTM30, CRS("+proj=longlat"))
SPlonlat31 <- spTransform(SPUTM31, CRS("+proj=longlat"))

SPlonlatNoUTM <- SpatialPointsDataFrame(coords=datosLonLat[c("Longitud", "Latitud")],
                             data=datosLonLat[c('Altitud', 'Estacion','Provincia', 'Comunidad', 'Numero')],
                    proj4string=CRS("+proj=longlat"))

SPlonlat <- spRbind(SPlonlat29, SPlonlat30)
SPlonlat <- spRbind(SPlonlat, SPlonlat31)
SPlonlat <- spRbind(SPlonlat, SPlonlatNoUTM)
##Primera representación.
## spplot(SPlonlat['Comunidad'], key.space='right') + layer(sp.lines(mapaSHP)) 
## ##Elimino estas estaciones erróneas y vuelvo a representar
## ##Compruebo que algunas estaciones se salen de su comunidad
## idxError1 <- which(coordinates(SPlonlat)[,2]<30)
## SPlonlat[idxError1,]

## SPlonlat2<- SPlonlat[-idxError1,]
trellis.device(pdf, file='RedEstaciones20110630.pdf')
spplot(SPlonlat['Comunidad'], col.regions=brewer.pal(n=12, 'Paired'),
       key.space='right', scales=list(draw=TRUE),
       type=c('p','g')) + layer(sp.lines(mapaSHP))
dev.off()
##Estaciones con longitud superior a 0.9
## idxError2 <- which(coordinates(SPlonlat)[,1]>0.5)
## SPlonlat[idxError2,]

##Hay dos de Extremadura fuera de sitio
## extr <- SPlonlat[SPlonlat$Comunidad=='Extremadura',]
## idxExtr <- rev(order(coordinates(extr)[,2]))[1:2]
## extr[idxExtr,]

## ##Y también una de Andalucia
## andal <- SPlonlat[SPlonlat$Comunidad=='Andalucia',]
## idxAndal <- rev(order(coordinates(andal)[,2]))
## andal[idxAndal,]

