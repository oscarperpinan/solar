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
##añado las variables NomProv y NomEst
redGN$NomProv <- RedEstaciones$NomProv
redGN$NomEst <- RedEstaciones$NomEst
##Elimino aquellos registros para los que no encuentro resultados
redGN <- redGN[!is.na(redGN$lat),]
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
##Leo el contenido:
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)
##y lo transformo en un objeto SpatialLines, con grosr de linea definido por lwd
mapaES <- list('sp.lines', mapaSHP, lwd=0.5)
##Ya puedo representar los resultados:
spplot(spRedGN['NomProv'], sp.layout=mapaES, scales=list(draw=TRUE), key.space='right')

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
