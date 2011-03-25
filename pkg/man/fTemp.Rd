\name{C_fTemp}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fTemp}
\title{
  Intradaily evolution of ambient temperature
}
\description{
  From the maximum and minimum daily values of ambient temperature, its evolution its calculated through a combination of cosine functions (ESRA method)
}
\usage{fTemp(sol, BD)}

\arguments{
  \item{sol}{A \code{\linkS4class{Sol}} object. It may be the result of the
    \code{\link{calcSol}} function.}
  \item{BD}{A \code{\linkS4class{Meteo}} object, as provided by the
    \code{\link{readMAPA}} or \code{\link{readBD}} functions. It must
    include information about \code{TempMax} and \code{TempMin}.}
}
\details{
  The ESRA method  estimates the dependence of the temperature on the time of the day (given as the local solar time) from only two inputs: minimum and maximum daily
  temperatures. It assumes that the temperature daily profile can be
  described using three piecewise cosine functions, dividing the day
  into three periods: from midnight to sunrise, from sunrise to the time
  of peak temperature (3 hours after midday), and to midnight.
}

\value{A \code{zoo} object with the profile of the ambient temperature.}

\references{\itemize{
    \item Huld, T. , Suri, M., Dunlop, E. D., and Micale F., Estimating
      average daytime and daily temperature profiles within Europe,
      Environmental Modelling & Software 21 (2006) 1650-1661.

    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
  }
}

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\link{calcSol}},
  \code{\link{readMAPA}},
  \code{\link{readBD}}.
}

\examples{
\dontrun{
#Aranjuez, Madrid
BD<-readMAPA(28,3,'01/01/2008','31/12/2008')
lat=41;
sol=calcSol(lat, BTd=indexD(BD), sample='hour')
Temp<-fTemp(sol,BD)

###Temperature of March
library(latticeExtra)
wTemp=window(Temp, start=as.POSIXct('2008-03-01'), end=as.POSIXct('2008-03-31'))
xyplot(wTemp)+layer_(panel.xblocks(x, DoY, col=c('lightgray', 'white')))
}
}

\keyword{utilities}

