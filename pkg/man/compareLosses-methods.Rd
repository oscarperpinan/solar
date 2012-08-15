\name{D_Losses-methods}
\docType{methods}
\alias{compareLosses}
\alias{compareLosses-methods}
\alias{compareLosses,ProdGCPV-method}
\alias{losses}
\alias{losses-methods}
\alias{losses,Gef-method}
\alias{losses,ProdGCPV-method}

\title{Losses of a GCPV system}
\description{The function \code{losses} calculates the yearly losses
  from a \code{Gef} or a \code{ProdGCPV} object. The function
  \code{compareLosses} compares the losses from several \code{ProdGCPV}
  objects and plots the result with \code{\link{dotplot}}.
}
\usage{
compareLosses(...)
losses(object)
}
\arguments{
  \item{...}{A list of \code{ProdGCPV} objects to be compared.}
  \item{object}{An object of \code{Gef} or \code{ProdGCPV} class..}
}

\section{Methods}{
  \describe{
    \item{\code{signature(... = "Gef")}}{shadows and angle of
      incidence (\code{AoI}) losses.}
    \item{\code{signature(... = "ProdGCPV")}}{shadows, \code{AoI},
      generator (mainly temperature), DC and AC system (as detailed in
      \code{effSys} of \code{\link{fProd}}) and inverter losses.}
}}
\examples{
lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Comparison of different tracker methods
ProdFixed<-prodGCPV(lat=lat,dataRad=prom, keep.night=FALSE)
Prod2x<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two', keep.night=FALSE)
ProdHoriz<-prodGCPV(lat=lat,dataRad=prom, modeTrk='horiz', keep.night=FALSE)

losses(ProdFixed)
losses(as(ProdFixed, 'Gef'))

compareLosses(ProdFixed, Prod2x, ProdHoriz)
}

\references{\itemize{

    \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
    (\url{http://procomun.wordpress.com/documentos/libroesf/})

    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
    Systems with R", Journal of Statistical Software, 50(9), 1-32,
    \url{http://www.jstatsoft.org/v50/i09/} }}

\author{Oscar Perpiñán Lamigueiro}

\seealso{
  \code{\link{fInclin}},
  \code{\link{fProd}}
  }

\keyword{methods}

