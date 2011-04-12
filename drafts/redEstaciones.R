library(geonames)
library(sp)
library(maptools)
library(solaR)

data(RedEstaciones)

##elimina el guión del nombre de algunas provincias
deleteLine <- function(x)
             sapply(strsplit(as.character(x), '_'), paste, collapse=" ")

RedEstaciones$NomProv <- factor(deleteLine(RedEstaciones$NomProv))

##pone en mayúsculas la inicial y el resto en minúsculas
capital <- function(x)paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep='')
RedEstaciones$NomEst <- capital(RedEstaciones$NomEst)
##¿cuantas estaciones tenemos?
nrow(RedEstaciones)

##Función para buscar una estación con nombre de provincia en geonames.org (usa GNsearch de geonames)
buscaGN <- function(x){
  cadena <- paste(unlist(x[4:3]), collapse=', ')
  res <- GNsearch(q=cadena, country='ES', maxRows=1)
  if (length(res)==0){##si no encuentra nada asigna NA (missing) a latitud y longitud
    res <- data.frame(lat=NA, lng=NA)
  } else {
    res <- res[c('lat', 'lng')]
  }
  res
}

##busco en geonames para todas las estaciones contenidas en RedEstaciones
redGN <- apply(RedEstaciones, 1, buscaGN)
redGN <- do.call('rbind', redGN)
##añado las variables de RedEstaciones
redGN <- cbind(redGN, RedEstaciones)
##Elimino aquellos registros para los que no encuentro resultados
redGN <- redGN[!is.na(redGN$lat),]
##save(file='redGN.RData', redGN)
##¿cuantas estaciones hemos localizado?
nrow(redGN)
###Construyo un objeto Spatial usando las funcionalidades de sp, maptools, etc.
##Proyección de los datos
proj <- CRS('+proj=latlon +ellps=WGS84')
##Objecto SpatialPointsDataFrame con los resultados obtenidos antes
spRedGN <- SpatialPointsDataFrame(coords=redGN[c('lng', 'lat')],
                                   data=redGN[c('NomProv', 'NomEst')],
                                   proj4string=proj)

##Descargo y descomprimo un zip de http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip
##Contiene un Shapefile con información de las fronteras entre provincias de españa.
old <- getwd()
setwd('/home/oscar/temp')##Cambiar!!!
##Leo el contenido:
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)
setwd(old)
##y lo transformo en un objeto SpatialLines, con grosr de linea definido por lwd
mapaES <- list('sp.lines', mapaSHP, lwd=0.5)
##Ya puedo representar los resultados:
spplot(spRedGN['NomProv'], sp.layout=mapaES, scales=list(draw=TRUE), key.space='right', cex=0.5)


##En la llamada a APPLYdebo eliminar las dos últimas columnas (caracteres) para que no convierta todo a character
spainMeteo <- apply(redGN[, 1:4], 1,
                      function(x)readMAPA(prov=x[3], est=x[4], start='01/01/2004', end='31/12/2010', lat=x[1])
                      )
##save(file='spainMeteo', spainMeteo)

meanYearlySums <- function(x)mean(aggregate(getG0(x), year, sum, na.rm=1))

spainG0y <- sapply(spainMeteo, meanYearlySums)/1000

redGN$G0y=spainG0y
spRedGN$G0y=spainG0y

ncuts=15
paleta=colorRampPalette(brewer.pal(9, 'Reds'))(ncuts)
spplot(spRedGN['G0y'], sp.layout=mapaES, col.regions=paleta,
       scales=list(draw=TRUE), ylim=c(35, 45), xlim=c(-10, 5),
       cuts=ncuts, key.space='right', cex=0.8)

vgmG0y <- variogram(G0y~1, data=spRedGN)
plot(vgmG0y)
fitvgmG0y <- fit.variogram(vgmG0y, vgm(20000, 'Sph', 10))

krigeG0y <- krige(G0y~1, spRedGN, spRedGN, model=fitvgmG0y)

################################################################################
###Otra alternativa. En lugar de buscar en geonames, uso la información del IGN
###http://wiki.openstreetmap.org/wiki/ES:NGBE
###Hay que descargar el NGBE.zip, que contiene una base de datos MDB.
###Exportamos el contenido de su tabla principal a un fichero de texto
###separando los campos con tabuladores y los decimales con coma
## y lo leemos con read.delim2:
NGBE <- read.delim2('NGBE/NGBE.txt')
##ahora hacemos un "merge" de los dos contenedores de datos
redNGBE <- merge(RedEstaciones, NGBE, by.x='NomEst', by.y='Texto')
##¿cuantas estaciones hemos localizado?...cuidado, muchas están repetidas...
nrow(redNGBE)
##y repetimos las operaciones anteriores para representar los resultados
spNGBE <- SpatialPointsDataFrame(coords=redNGBE[c('Longitud', 'Latitud')],
                                   data=redNGBE[c('NomProv', 'NomEst')],
                                   proj4string=proj)
spplot(spNGBE['NomProv'], sp.layout=mapaES, scales=list(draw=TRUE), key.space='right')
