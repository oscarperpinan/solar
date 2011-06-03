\name{A8_readSIAR}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{readSIAR}
\alias{readMAPA}

\title{
  Meteorological data from www.marm.es/siar
}
\description{
  Download and transform meteorological data from \url{www.marm.es/siar} for a station of a province (Spain) in a time period.
}
\usage{
readSIAR(prov, est, start, end, lat=0, format='\%d/\%m/\%Y')
readMAPA(prov, est, start, end, lat=0, format='\%d/\%m/\%Y')

}
\arguments{
  \item{prov}{numeric, number of the spanish province according to the information of RedEstaciones.}
  \item{est}{numeric, number of the station according to the
    information of RedEstaciones.}
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

\note{The webpage of SIAR has changed. The meteorological data is no
  longer available at \url{www.mapa.es/siar}. Therefore, the name
  \code{readMAPA} has been deprecated and the function \code{readSIAR}
  should be used instead.}

\references{\url{www.marm.es/siar}}

\author{
  Oscar Perpiñán Lamigueiro.
}
\seealso{
  \code{\link{readG0dm}},
  \code{\link{readBD}},
  \code{\link{fTemp}},
  \code{\link{RedEstaciones}}.
}
\examples{
\dontrun{
#Aranjuez, Madrid
BD<-readSIAR(28,3,'01/01/2008','31/12/2008')

xyplot(TempMedia~G0|equal.count(VelViento),data=BD)
}
}

\keyword{utilities}
\keyword{constructors}
