\name{E_SIAR}
\alias{SIAR}
\docType{data}
\title{
  Meteorological stations of marm.es/siar
}
\description{
  Meteorological stations available at \url{www.marm.es/siar}
}
\usage{data(SIAR)}
\format{A data frame with 8 columns.
  \describe{
    \item{\code{N_Estacion}}{Code of the station}
    \item{\code{Estacion}}{Name of the station}
    \item{\code{N_Provincia}}{Code of the Province}
    \item{\code{Provincia}}{Name of the Province}
    \item{\code{Comunidad}}{Name of the Community (first-level political
      division of Spain).}
    \item{\code{lon}}{Longitude (degrees)}
    \item{\code{lat}}{Latitude (degrees)}
    \item{\code{Altitud}}{Altitude (meters)}
}}
\details{
The number codes of the stations and provinces have been obtained with a
"trial-and-error" procedure. Therefore, this information could include
mistakes.

The latitude, longitude and altitude information have been obtained in
the context of a Master Project at the EOI
(\url{https://sites.google.com/a/learning.eoi.es/merme2010-estudioradiacionsolar/home})
by Fernando Antoñanzas Torres, Federico Cañizares Jover, Rafael Morales Cabrera and Manuel Ojeda Fernández.

This dataset is an improvement of the previous \code{RedEstaciones}.
}
\source{
  \url{www.marm.es/siar}
}
\seealso{\code{\link{readSIAR}}}

\examples{
data(SIAR)
\dontrun{
## Plot the stations in a map
library(sp)
library(maptools)

proj <- CRS('+proj=longlat +ellps=WGS84')
spSIAR <- SpatialPointsDataFrame(SIAR[, c(6, 7)], SIAR[, -c(6, 7)],
                                 proj4str=proj)


###download a shapefile with the administrative borders of Spain
old <- setwd(tempdir())
download.file('http://www.gadm.org/data/shp/ESP_adm.zip', 'ESP_adm.zip')
unzip('ESP_adm.zip')
mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string=proj)
setwd(old)

p <- spplot(spSIAR['Comunidad'],
       col.regions=brewer.pal(n=12, 'Paired'),
       key.space='right', scales=list(draw=TRUE),
       type=c('p','g'))

p  + layer(sp.lines(mapaSHP))


}
}
\keyword{datasets}
