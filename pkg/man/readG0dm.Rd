\name{A8_readG0dm}
\Rdversion{1.0}
\encoding{UTF-8}
\alias{readG0dm}

\title{Monthly mean values of global horizontal irradiation.}
\description{Constructor for the class \code{Meteo} with 12 values of
  monthly means of irradiation.}
\usage{
readG0dm(G0dm, Ta=25, lat=0,
    year= as.POSIXlt(Sys.Date())$year+1900,
    promDays=c(17,14,15,15,15,10,18,18,18,19,18,13),
    source='')
}
\arguments{
  \item{G0dm}{numeric, 12 values of monthly means of daily global horizontal
    irradiation (Wh/m²).}
  \item{Ta}{numeric, 12 values of monthly means of ambient temperature
    (degrees Celsius).}
  \item{lat}{numeric, latitude (degrees) of the location.}
  \item{year}{numeric (Default: current year).}
  \item{promDays}{numeric, set of the average days for each month.}
  \item{source}{character string with information about the source of
    the values.}
}
\value{\code{Meteo} object}


\author{
  Oscar Perpiñán Lamigueiro.
}
\seealso{
  \code{\link{readMAPA}},
  \code{\link{readBD}}.
}
\examples{
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
BD<-readG0dm(G0dm=G0dm, Ta=Ta, lat=37.2)
print(BD)
getData(BD)
xyplot(BD)
}
\keyword{utilities}
\keyword{constructors}
