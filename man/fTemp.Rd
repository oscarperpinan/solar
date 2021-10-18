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
  \code{\link{readBD}} function. It must
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

      \item Perpiñán, O, Energía Solar Fotovoltaica, 2015.
    (\url{https://oscarperpinan.github.io/esf/})

    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \doi{10.18637/jss.v050.i09}
  }
}

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\link{calcSol}},
  \code{\link{readBD}}.
}

\keyword{utilities}

