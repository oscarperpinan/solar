\name{D_getLat-methods}
\docType{methods}
\alias{getLat}
\alias{getLat-methods}
\alias{getLat,G0-method}
\alias{getLat,Meteo-method}
\alias{getLat,Sol-method}
\title{Methods for Function getLat}
\description{
  Latitude angle of \code{solaR} objects.
}
\usage{getLat(object, units='rad')}
\arguments{
  \item{object}{A \code{Sol} or \code{Meteo} object (or extended.)}
  \item{units}{A character, 'rad' or 'deg'.}
}
\section{Methods}{This function returns the latitude angle in radians
  (\code{units='rad'}, default) or degrees (\code{units='deg'}).

  \describe{
    \item{\code{signature(object = "Meteo")}}{Value of the \code{latData}
      slot, which is defined by the argument \code{lat} of the
      \code{\link{readMAPA}}, \code{\link{readG0dm}} and
      \code{\link{readBD}} functions, or by the \code{lat} component of the
      \code{prom}, \code{mapa} and \code{bd} lists passed to \code{calcG0}
      (or equivalent) . It is the latitude of the
      meteorological station (or equivalent) which provided the irradiation
      source data. It may be different from the value used for the
      calculation procedure.}

    \item{\code{signature(object = "Sol")}}{Value of the \code{lat} slot,
      which is defined by the argument \code{lat} of the \code{calcSol}
      function. It is the value used through the calculation procedure.}

    \item{\code{signature(object = "G0")}}{same as for the \code{Sol} class.}

}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
