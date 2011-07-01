library(sp)
library(rgdal)
library(maptools)


##Leo fronteras administrativas
old <- getwd()
setwd('/home/oscar/Datos/ESP_adm')##Cambiar!!!
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string=proj)
setwd(old)

datos <- read.csv2('/home/oscar/Investigacion/solar/drafts/UTM_latlon_3.csv',
                   colClasses=c('factor', 'factor',
                     'integer', 'integer', 'character',
                     'numeric', 'numeric', 'numeric',
                     'integer', 'character', 'character', 'character')
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
datosLonLat$Longitud <- ifelse(datosLonLat$SignoLongitud=='-0', -datosLonLat$Longitud, datosLonLat$Longitud)
datosLonLat$SignoLongitud <- NULL

##datos[is.na(datos)] <- 0


projUTM30 <- CRS('+proj=utm +zone=30')
projUTM31 <- CRS('+proj=utm +zone=31')
projUTM29<- CRS('+proj=utm +zone=29')

SPUTM29 <- SpatialPointsDataFrame(coords=datosUTM29[c("UTMX", "UTMY")],
                             data=datosUTM29[c('Altitud', 'N_Estacion', 'Estacion',
                               'N_Provincia', 'Provincia', 'Comunidad')],
                    proj4string=projUTM29)

SPUTM30 <- SpatialPointsDataFrame(coords=datosUTM30[c("UTMX", "UTMY")],
                             data=datosUTM30[c('Altitud', 'N_Estacion', 'Estacion',
                               'N_Provincia', 'Provincia', 'Comunidad')],
                    proj4string=projUTM30)

SPUTM31 <- SpatialPointsDataFrame(coords=datosUTM31[c("UTMX", "UTMY")],
                             data=datosUTM31[c('Altitud', 'N_Estacion', 'Estacion',
                               'N_Provincia', 'Provincia', 'Comunidad')],
                    proj4string=projUTM31)

##Transformo de UTM a long-lat
SPlonlat29 <- spTransform(SPUTM29, proj)
SPlonlat30 <- spTransform(SPUTM30, proj)
SPlonlat31 <- spTransform(SPUTM31, proj)

SPlonlatNoUTM <- SpatialPointsDataFrame(coords=datosLonLat[c("Longitud", "Latitud")],
                             data=datosLonLat[c('Altitud', 'N_Estacion', 'Estacion',
                               'N_Provincia', 'Provincia', 'Comunidad')],
                    proj4string=CRS("+proj=longlat"))

SPlonlat <- spRbind(SPlonlat29, SPlonlat30)
SPlonlat <- spRbind(SPlonlat, SPlonlat31)
SPlonlat <- spRbind(SPlonlat, SPlonlatNoUTM)
coordnames(SPlonlat) <- c('lon', 'lat')
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

redSIAR <- as.data.frame(SPlonlat)

##En la llamada a APPLY hay que eliminar las columnas con caracteres
##para que no convierta todo a character
###NO EJECUTAR, recuperar con load()
spainMeteo <- apply(redSIAR[, c(8, 4, 2)], 1,
                    function(x){
                      try(readSIAR(prov=x[2], est=x[3],
                               start='01/01/2008', end='31/12/2010',
                               lat=x[1]))
                    }
                    )


idxMeteo <- sapply(spainMeteo, function(x)class(x)=='Meteo')
spainMeteoOK <- spainMeteo[idxMeteo]

##Días registrados
fooDays <- function(x)as.numeric(diff(range(indexD(x))))
ndays <- sapply(spainMeteoOK, fooDays)

####Medias de sumas anuales
meanYearlySums <- function(x)mean(aggregate(getG0(x), year, sum, na.rm=1))

##aplico la función a cada una de las estaciones que componen la lista spainMeteo
##divido entre 1000 para pasar a kWh
spainG0y <- sapply(spainMeteoOK, meanYearlySums)/1000

redSIAROK <- redSIAR[idxMeteo,]
SPlonlatOK <- SPlonlat[idxMeteo,]

##añado el resultado como una capa más al SpatialPointsDataFrame
redSIAROK$G0y=spainG0y
redSIAROK$ndays=ndays
SPlonlatOK$G0y=spainG0y
SPlonlatOK$ndays=ndays

save(redSIAR, redSIAROK, SPlonlat, SPlonlatOK, file='redSIAR.RData')
save(spainMeteo, idxMeteo, file='spainMeteo20110701.RData')
####FIN DE CALCULOS###########################################

