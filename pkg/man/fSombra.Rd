\name{C_fSombra}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fSombra}
\alias{fSombra2X}
\alias{fSombra6}
\alias{fSombraHoriz}
\alias{fSombraEst}

\title{Shadows on PV systems}
\description{
  Compute the shadows factor for two-axis and horizontal N-S axis trackers and fixed surfaces.
}
\usage{
fSombra(angGen, distances, struct, modeTrk='fixed',prom=TRUE)

fSombra6(angGen,distances,struct,prom=TRUE)

fSombra2X(angGen,distances,struct)

fSombraHoriz(angGen, distances,struct)

fSombraEst(angGen, distances,struct)

}

\arguments{
  \item{angGen}{A \code{zoo} object, including at least variables named
    \code{Beta}, \code{Alfa}, \code{AzS}, \code{AlS} and \code{cosTheta}.}

  \item{distances}{\code{data.frame}, with a component named \code{Lew}, being the distance (meters) between horizontal NS and two-axis trackers along the East-West direction, a component named \code{Lns} for two-axis trackers or a component named \code{D} for static surfaces. An additional component named \code{H} can be included with the relative height (meters) between surfaces. When \code{modeTrk='two'} (or when \code{fSombra6} is used) this data.frame may have five rows. Each of these rows defines the distances of a tracker in a set of six ones.}

  \item{struct}{\code{list}. When \code{modeTrk='fixed'} or \code{modeTrk='horiz'} only a component named \code{L}, which is the height (meters) of the tracker, is needed. For two-axis trackers (\code{modeTrk='two'}), an additional component named \code{W}, the width of the tracker, is required. Moreover, two components named \code{Nrow} and \code{Ncol} are included under this list. These components define, respectively, the number of rows and columns of the whole set of trackers in the PV plant.}

  \item{modeTrk}{character, to be chosen from \code{'fixed'}, \code{'two'} or \code{'horiz'}. When \code{modeTrk='fixed'} the surface is fixed (inclination and azimuth angles are constant). The performance of a two-axis tracker is calculated with \code{modeTrk='two'}, and \code{modeTrk='horiz'} is the option for an horizontal N-S tracker. Its default value is \code{modeTrk='fixed'}}

  \item{prom}{logical, only needed for two-axis tracker mode. If \code{TRUE} the shadows are averaged between the set of trackers defined by \code{struct$Nrow} and \code{struct$Ncol}}

}
\details{
  \code{\link{fSombra}} is only a wrapper for \code{\link{fSombra6}} (two-axis trackers), \code{\link{fSombraEst}} (fixed systems) and \code{\link{fSombraHoriz}} (horizontal N-S axis trackers). Depending on the value of \code{modeTrk} the corresponding function is selected.
  \code{\link{fSombra6}} calculates the shadows factor in a set of six two-axis trackers. If \code{distances} has only one row, this function constructs a symmetric grid around a tracker located at (0,0,0). These five trackers are located at (-Lew, Lns, H), (0, Lns, H), (Lew, Lns, H), (-Lew, 0, H) and (Lns, 0, H). It is possible to define a irregular grid around (0,0,0) including five rows in \code{distances}. When \code{prom=TRUE} the shadows factor for each of the six trackers is calculated. Then, according to the distribution of trackers in the plant defined by \code{struct$Nrow} and \code{struct$Ncol}, a weighted average of the shadows factors is the result.
  It is important to note that the distances are defined between axis for trackers and between similar points of the structure for fixed surfaces.
}
\value{\code{data.frame} including \code{angGen} and a variable named \code{FS}, which is the shadows factor. This factor is the ratio between the area of the generator affected by shadows and the total area. Therefore its value is 1 when the PV generator is completely shadowed.}

\references{\itemize{
    \item Perpiñán, O.: Grandes Centrales Fotovoltaicas: producción, seguimiento y ciclo de vida. PhD Thesis, UNED, 2008. \url{http://e-spacio.uned.es/fez/view.php?pid=bibliuned:20080}.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
  }
}
\author{
  Oscar Perpiñán Lamigueiro.
}

\seealso{
  \code{\link{calcShd}}, \code{\link{optimShd}}, \code{\link{fTheta}}, \code{\link{calcSol}}
}
\examples{
lat=37.2;
sol<-calcSol(lat, fBTd(mode='prom'), sample='10 min', keep.night=FALSE)
angGen<-fTheta(sol, beta=35);
Angles=CBIND(as.zooI(sol), angGen)

###Two-axis tracker
#Symmetric grid
distances=data.frame(Lew=40,Lns=30,H=0)
struct=list(W=23.11, L=9.8, Nrow=2, Ncol=8)

ShdFactor<-fSombra6(Angles, distances, struct, prom=FALSE)

Angles$FS=ShdFactor
xyplot(FS~w, groups=month, data=Angles,
    type='l',auto.key=list(space='right', lines=TRUE, points=FALSE))

#Symmetric grid defined with a five rows data.frame
distances=data.frame(Lew=c(-40,0,40,-40,40),Lns=c(30,30,30,0,0),H=0)
ShdFactor2<-fSombra6(Angles, distances, struct,prom=FALSE)

#of course, with the same result
identical(coredata(ShdFactor), coredata(ShdFactor2))
}

\keyword{utilities}

