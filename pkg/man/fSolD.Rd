\name{C_fSolD}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fSolD}

\title{
  Daily apparent movement of the Sun from the Earth
}
\description{
  Compute the daily apparent movement of the Sun from the Earth. This movement is mainly described (for the simulation of photovoltaic systems) by the declination angle, the sunset angle and the daily extra-atmospheric irradiation.
}
\usage{
fSolD(lat, BTd)
}

\arguments{
  \item{lat}{Latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}
  \item{BTd}{Daily temporal base, a \code{POSIXct} object which  may be the result of \code{\link{fBTd}}.}

}
\value{A \code{zoo} object with these components:
  \item{decl}{Declination angle (radians) for each day of year in \code{dn} or \code{BTd}}
  \item{eo}{Factor of correction due the eccentricity of orbit of the Earth around the Sun.}
  \item{ws}{Sunset angle (in radians) for each day of year. Due to the convention which considers that the solar hour angle is negative before midday, this angle is negative.}
  \item{Bo0d}{Extra-atmospheric daily irradiation (watt-hour per
    squared meter) incident on a horizontal surface}
  \item{EoT}{Equation of Time.}


  The latitude is stored as the attribute \code{lat} of the result,
  and thus it is accessible with \code{attr(object, 'lat')}.

}
\references{
  Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}
\author{
  Oscar Perpiñán Lamigueiro.
}
\examples{
BTd=fBTd(mode='serie')

lat=37.2
fSolD(lat,BTd[100])

lat=-37.2
fSolD(lat,BTd[283])

#Solar angles along the year
SolD<-fSolD(lat,BTd=fBTd())

library(lattice)
xyplot(SolD)

#Calculation of the daylength for several latitudes
library(latticeExtra)

Lats=c(-60,-40,-20,0,20,40,60)
NomLats=ifelse(Lats>0, paste(Lats,'N', sep=''), paste(abs(Lats), 'S',
sep=''))
NomLats[Lats==0]='0'

mat=matrix(nrow=365, ncol=length(Lats))
colnames(mat)=NomLats
WsZ=zoo(mat, fBTd(mode='serie'))

for (i in seq_along(Lats)){
    SolDaux<-fSolD(lat=Lats[i],BTd=fBTd(mode='serie'));
    WsZ[,i]<-r2h(2*abs(SolDaux$ws))}

p=xyplot(WsZ, superpose=TRUE,
        ylab=expression(omega[s] (h)), auto.key=FALSE)
plab<-p+glayer(panel.text(x[1], y[1], NomLats[group.number], pos=2))
print(plab)
}

\keyword{utilities}

