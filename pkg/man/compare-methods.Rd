\name{D_compare-methods}
\docType{methods}
\alias{compare}
\alias{compare-methods}
\alias{compare,G0-method}
\alias{compare,Gef-method}
\alias{compare,ProdGCPV-method}

\title{Compare G0, Gef and ProdGCPV objects}
\description{Compare and plot the yearly values of several objects.}
\usage{
\S4method{compare}{G0}(...)}
\arguments{
  \item{...}{A list of objects to be compared.}
  }
\section{Methods}{The class of the first element of \code{...} is used to determine the
    suitable method. The result is plotted with \code{\link{dotplot}}:\describe{
    \item{\code{signature(... = "G0")}}{yearly values of \code{G0d}, 
      \code{B0d} and \code{D0d}.}
    \item{\code{signature(... = "Gef")}}{yearly values of \code{Gefd}, \code{Befd} and \code{Defd}.}
    \item{\code{signature(... = "ProdGCPV")}}{yearly values of \code{Yf}, \code{Gefd} and \code{G0d}.}
}}
\examples{
lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Comparison of different tracker methods
ProdFixed<-prodGCPV(lat=lat,prom=prom, keep.night=FALSE)
Prod2x<-prodGCPV(lat=lat, prom=prom, modeTrk='two', keep.night=FALSE)
ProdHoriz<-prodGCPV(lat=lat,prom=prom, modeTrk='horiz', keep.night=FALSE)

compare(ProdFixed, Prod2x, ProdHoriz)

##The first element rules the method
GefFixed=as(ProdFixed, 'Gef')
compare(GefFixed, Prod2x, ProdHoriz)

\dontrun{
###compare and do.call
EstMadrid <- subset(RedEstaciones, NomProv=='Madrid')
nEstMadrid <- nrow(EstMadrid)
namesMadrid <- EstMadrid$NomEst

prodMadrid <- lapply(1:nEstMadrid,
                     function(x){try(prodGCPV(lat=41, modeRad='mapa',
                                                  mapa=list(prov=28, est=x,
                                                    start='01/01/2009', end='31/12/2010'))
                                     )})
names(prodMadrid) <- namesMadrid
okMadrid <- lapply(prodMadrid, class)!='try-error'
prodMadrid <- prodMadrid[okMadrid]

do.call(compare, prodMadrid)
}
}

\author{Oscar Perpiñán Lamigueiro}

\seealso{
  \code{\link{dotplot}}
}

\keyword{methods}
