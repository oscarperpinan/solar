\name{E_RedEstaciones}
\alias{RedEstaciones}
\docType{data}
\title{
  Meteorological stations of mapa.es/siar
}
\description{
  Meteorological stations available at \url{www.mapa.es/siar}
}
\usage{data(RedEstaciones)}
\format{A data frame with 4 columns.
  \describe{
    \item{\code{Provincia}}{Code of the province}
    \item{\code{Estacion}}{Code of the station}
    \item{\code{NomProv}}{Name of the province}
    \item{\code{NomEst}}{Name of the station}
}}
\details{
  In this version the information about latitude and longitude of the stations is not available. It may be obtained through \url{http://www.geonames.org/} or with the package \code{{geonames}}.
}
\source{
  \url{www.mapa.es/siar}
}
\seealso{\code{\link{readMAPA}}}

\keyword{datasets}
