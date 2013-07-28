\name{A1_calcSol}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{calcSol}

\title{
  Apparent movement of the Sun from the Earth
}
\description{
  Compute the apparent movement of the Sun from the Earth with the
  functions \code{\link{fSolD}} and \code{\link{fSolI}}.
}
\usage{
calcSol(lat, BTd, sample='hour', BTi, EoT=TRUE, keep.night=TRUE, method='michalsky')
}

\arguments{
  \item{lat}{Latitude (degrees) of the point of the Earth where
    calculations are needed. 
    It is positive for locations above the Equator.}

  \item{BTd}{Daily time base, a \code{POSIXct} object which  may be
    the result of \code{\link{fBTd}}. It is not considered if \code{BTi}
    is provided.}

  \item{sample}{Increment of the intradaily sequence. It is a character
    string, containing one of ‘"sec"’, ‘"min"’, ‘"hour"’.
    This can optionally be preceded by a (positive or
    negative) integer and a space, or followed by ‘"s"’. It is
    used by \code{\link{seq.POSIXt}}.

    It is not considered if \code{BTi} is provided.}

  \item{BTi}{Intradaily time base, a \code{POSIXct} object to be used by
    \code{\link{fSolI}}. It could be the index of the \code{G0I} argument
    to \code{\link{calcG0}}.}

  \item{EoT}{logical, if \code{TRUE} the Equation of Time is
  used. Default is TRUE.}

  \item{keep.night}{logical, if \code{TRUE} (default) the night is
    included in the time series.}
  \item{method}{\code{character}, method for the sun geometry
    calculations to be chosen from 'cooper', 'spencer', 'michalsky' and
    'strous'. See references for details.}
}

\value{A \code{\link{Sol-class}} object.}

\references{\itemize{ \item{Cooper, P.I., Solar Energy, 12, 3
    (1969). "The Absorption of Solar Radiation in Solar Stills"}
    \item{Spencer, Search 2 (5), 172,
    \url{http://www.mail-archive.com/sundial@uni-koeln.de/msg01050.html}}
    \item{Strous:
    \url{http://www.astro.uu.nl/~strous/AA/en/reken/zonpositie.html}}
    \item{Michalsky, J., 1988: The Astronomical Almanac's algorithm for
    approximate solar position (1950-2050), Solar Energy 40, 227-235,
    \url{ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f}}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2012.
      \url{http://procomun.wordpress.com/documentos/libroesf/} }
    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
    Systems with R", Journal of Statistical Software, 50(9), 1-32,
    \url{http://www.jstatsoft.org/v50/i09/} } }

\author{Oscar Perpiñán Lamigueiro.}

\examples{
BTd=fBTd(mode='serie')

lat=37.2
sol=calcSol(lat, BTd[100])
print(as.zooD(sol))

library(lattice)
xyplot(as.zooI(sol))

solStrous=calcSol(lat, BTd[100], method='strous')
print(as.zooD(solStrous))

solSpencer=calcSol(lat, BTd[100], method='spencer')
print(as.zooD(solSpencer))

solCooper=calcSol(lat, BTd[100], method='cooper')
print(as.zooD(solCooper))
}

\keyword{utilities}
\keyword{constructors}

