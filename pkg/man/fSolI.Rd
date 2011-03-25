\name{C_fSolI}
\Rdversion{1.3}
\encoding{UTF-8}
\alias{fSolI}
\title{
  Instantaneous apparent movement of the Sun from the Earth
}
\description{
  Compute the angles which describe the intradaily apparent movement of the Sun from the Earth.
}
\usage{
fSolI(solD, sample='hour', BTi, EoT=FALSE, keep.night=TRUE)
}
\arguments{
  \item{solD}{A \code{zoo} object with the result of \code{fSolD}}

  \item{sample}{Increment of the intradaily sequence. It is a character
    string, containing one of ‘"sec"’, ‘"min"’,
    ‘"hour"’.  This can optionally be preceded by a (positive or
    negative) integer and a space, or followed by ‘"s"’. It is
    used by \code{\link{seq.POSIXt}}.

    It is not considered when \code{BTi} is provided.}

  \item{BTi}{Intradaily time base, a \code{POSIXct} object. It could be
    the index of the \code{G0I} argument to
    \code{\link{calcG0}}. \code{fSolI} will produce results only for those
    days contained both in \code{solD} and in \code{BTi}.}

  \item{EoT}{logical, if \code{TRUE} the Equation of Time is used.}
  \item{keep.night}{logical, if \code{TRUE} (default) the night is
    included in the time series.}
}

\value{
  A \code{zoo} object is returned with these components:
  \item{w}{numeric, solar hour angle (radians)}
  \item{aman}{logical, \code{TRUE} when Sun is above the horizon }
  \item{cosThzS}{numeric, cosine of the solar zenith angle}
  \item{AzS}{numeric, solar acimuth angle (radians)}
  \item{AlS}{numeric, solar elevation angle (radians)}
  \item{Bo0}{numeric, extra-atmospheric irradiance (W/m2)}
  \item{rd, rg}{numeric, relation between irradiance and irradiation
    of diffuse and global values, respectively, following the
    correlations proposed by Collares-Pereira and Rabl}

  The latitude is stored as the attribute \code{lat} of this object.
}
\references{ \itemize{
    \item Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
      diffuse and hemispherical and between daily and hourly insolation values. Solar Energy, 22:155–164, 1979.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}}
\author{
  Oscar Perpiñán Lamigueiro.
}
\seealso{
  \code{\link{fSolD}}
}
\examples{
###Angles for one day
BTd=fBTd(mode='serie')

#North hemisphere
lat=37.2
solD<-fSolD(lat,BTd[100])
solI<-fSolI(solD, sample='hour')
print(solI)

#South hemisphere
lat=-37.2;
solDs<-fSolD(lat,BTd[283])
solIs<-fSolI(solDs, sample='hour')
print(solIs)

 ###Angles for the 12 average days
lat=37.2;
solD<-fSolD(lat,BTd=fBTd(mode='prom'))
solI<-fSolI(solD, sample='10 min', keep.night=FALSE)

library(lattice)
library(latticeExtra)

###Solar elevation angle vs. azimuth.
#This kind of graphics is useful for shadows calculations
mon=month.abb
p<-xyplot(r2d(AlS)~r2d(AzS),
    groups=month,
    data=solI, type='l', col='black',
    xlab=expression(psi[s]),ylab=expression(gamma[s]))
plab=p+glayer(panel.text(0, y[x==0], mon[group.value], pos=4, cex=0.8))
print(plab)
}

\keyword{utilities}
