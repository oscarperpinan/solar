\name{D_window-methods}
\docType{methods}
\alias{window}
\alias{window-methods}
\alias{window,Meteo-method}
\alias{window,Sol-method}
\alias{window,G0-method}
\alias{window,Gef-method}
\alias{window,ProdGCPV-method}
\alias{window,ProdPVPS-method}


\title{Methods for Function window}
\description{Method for extracting the subset of a \code{solaR} object
  whose daily time index (\code{\link{indexD}}) is comprised between the
  times \code{start} and \code{end}.
}

\usage{
\S4method{window}{Meteo}(x, start=NULL, end=NULL, ...)
\S4method{window}{Sol}(x, start=NULL, end=NULL, ...)
\S4method{window}{G0}(x, start=NULL, end=NULL, ...)
\S4method{window}{Gef}(x, start=NULL, end=NULL, ...)
\S4method{window}{ProdGCPV}(x, start=NULL, end=NULL, ...)
\S4method{window}{ProdPVPS}(x, start=NULL, end=NULL, ...)
}

\arguments{
  \item{x}{A \code{Meteo}, \code{Sol}, etc. object.}
  \item{start}{an index/time value (\code{Date} or \code{POSIXct}
    classes)}
  \item{end}{an index/time value (\code{Date} or \code{POSIXct}
    classes)}
  \item{...}{Additional arguments for \code{window.zoo}}
}
\seealso{
  \code{\link{window.zoo}}
\code{\link{indexD}}
}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
