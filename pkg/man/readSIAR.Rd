\name{A8_readSIAR}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{readSIAR}

\title{
  Meteorological data from the SIAR network.
}

\description{ IMPORTANT: The SIAR webpage has changed again and the data
  cannot be accessed with a direct URL but using javascript
  code. Therefore, the function \code{readSIAR} no longer works. This
  help page is still here as a reference. The SIAR webpage is now
  \url{http://eportal.magrama.gob.es/websiar}.
  

  % Download and transform meteorological data from
  % the SIAR network for a station of a province (Spain) in a time
  % period.
}

\usage{
readSIAR(prov, est, start, end, lat=0,
  format='\%d/\%m/\%Y')
}
\arguments{
  \item{prov}{numeric, number of the spanish province according to the
    information of \url{www.marm.es/siar} (see details below).}
  \item{est}{numeric, number of the station according to the
    information of \url{www.marm.es/siar} (see details below).}
  \item{start}{character, first day of the time period.}
  \item{end}{character, last day of the time period.}
  \item{lat}{numeric, latitude (degrees) of the station.}
  \item{format}{character string with the format of the \code{start}
    and \code{end} dates. (Default: '\%d/\%m/\%Y')}
}
\value{\code{Meteo} object,  with the information available at
  \url{www.marm.es/siar}.

  It should be noted that SIAR uses MJ/m² units
  for the irradiation data and \code{readSIAR} converts the values to
  Wh/m².}

\details{

The number codes of the stations and provinces are available at
\url{http:/solar.r-forge.r-project.org/data/SIAR.csv'}, a data frame with 8 columns:
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
}

Part of this information has been obtained with a
"trial-and-error" procedure so could include mistakes.

The latitude, longitude and altitude information have been obtained in
the context of a Master Project at the EOI
(\url{https://sites.google.com/a/learning.eoi.es/merme2010-estudioradiacionsolar/home})
by Fernando Antoñanzas Torres, Federico Cañizares Jover, Rafael Morales
Cabrera and Manuel Ojeda Fernández. This dataset is an improvement of
the previous \code{RedEstaciones} (not available in this version).  }



\references{\url{http://eportal.magrama.gob.es/websiar/Inicio.aspx}}

\author{
  Oscar Perpiñán Lamigueiro.
}
\seealso{
  \code{\link{readG0dm}},
  \code{\link{readBD}},
  \code{\link{fTemp}},
  \code{\link{read.zoo}}
}
\examples{
\dontrun{
#Aranjuez, Madrid
BD<-readSIAR(28,3,'01/01/2008','31/12/2008')

xyplot(TempMedia~G0|equal.count(VelViento),data=BD)
}

\dontrun{
## Plot the stations in a map
library(sp)
library(maptools)

SIAR <- read.csv('http://solar.r-forge.r-project.org/data/SIAR.csv')
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

\keyword{utilities}
\keyword{constructors}
