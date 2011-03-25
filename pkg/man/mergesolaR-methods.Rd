\name{D_mergesolaR-methods}
\docType{methods}
\alias{mergesolaR}
\alias{mergesolaR-methods}
\alias{mergesolaR,G0-method}
\alias{mergesolaR,Gef-method}
\alias{mergesolaR,Meteo-method}
\alias{mergesolaR,ProdGCPV-method}
\alias{mergesolaR,ProdPVPS-method}
\title{Merge solaR objects}
\description{Merge the daily time series of solaR objects}
\usage{mergesolaR(...)}
\arguments{
  \item{...}{A list of objects to be merged.}
  }

\section{Methods}{The class of the first element of \code{...} is used to
    determine the suitable method. Only the most important daily variable is
    merged, depending on the class of the objects:\describe{
    \item{\code{signature(... = "Meteo")}}{\code{G0}}
    \item{\code{signature(... = "G0")}}{\code{G0d}}
    \item{\code{signature(... = "Gef")}}{\code{Gefd}}
    \item{\code{signature(... = "ProdGCPV")}}{\code{Yf}}
    \item{\code{signature(... = "ProdPVPS")}}{\code{Yf}}
}}
\keyword{methods}

\examples{
lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Different tracker methods
ProdFixed<-prodGCPV(lat=lat,prom=prom, keep.night=FALSE)
Prod2x<-prodGCPV(lat=lat, prom=prom, modeTrk='two', keep.night=FALSE)
ProdHoriz<-prodGCPV(lat=lat,prom=prom, modeTrk='horiz', keep.night=FALSE)

prod <- mergesolaR(ProdFixed, Prod2x, ProdHoriz)
head(prod)

\dontrun{
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

YfMadrid <- do.call(mergesolaR, prodMadrid)

horizonplot(YfMadrid-rowMeans(YfMadrid),
            origin=0,
            scales=list(y=list(relation='same')),
            colorkey=TRUE)

TargetDiagram(YfMadrid, end=as.POSIXct('2010-12-31'), ndays=c(10, 20,
30, 40, 50, 60), cex=0.6)
}
}