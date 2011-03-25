\name{B1_Meteo-class}
\Rdversion{1.2}
\docType{class}
\alias{Meteo-class}
\alias{show,Meteo-method}

\title{Class "Meteo"}
\description{
  A class for meteorological data.
}
\section{Objects from the Class}{
  Objects can be created by the family of \code{\link{readBD}} functions.
}
\section{Slots}{
  \describe{
    \item{\code{latData}:}{Latitude (degrees) of the meteorological station or
      source of the data.}
    \item{\code{data}:}{A \code{zoo} object with the time series of
      daily irradiation (\code{G0}), the ambient temperature (\code{Ta})
      or the maximum and minimum ambient
      temperature (\code{TempMax} and \code{TempMin}).}
    \item{\code{source}:}{A character with a short description of the
      source of the data.}
    \item{\code{type}:}{A character, \code{prom}, \code{bd}, \code{bdI} or
      \code{mapa}, depending on the constructor.}

  }
}
\section{Methods}{
  \describe{
    \item{getData}{\code{signature(object = "Meteo")}: extracts the
      \code{data} slot as a \code{zoo} object.}
    \item{getG0}{\code{signature(object = "Meteo")}: extracts the
      irradiation time series as a \code{zoo} object.}
    \item{getLat}{\code{signature(object = "Meteo")}: extracts the
      latitude value.}
    \item{indexD}{\code{signature(object = "Meteo")}: extracts the index
      of the \code{data} slot.}
    \item{xyplot}{\code{signature(x = "formula", data = "Meteo")}: plot
      the content of the object according to the \code{formula} argument.}
    \item{xyplot}{\code{signature(x = "Meteo", data = "missing")}: plot
      the \code{data} slot using the \code{xyplot} method for \code{zoo}
      objects.}
  }
}

\author{Oscar Perpiñán Lamigueiro.}


\seealso{
  \code{\link{readBD}},
  \code{\link{readBDi}},
  \code{\link{zoo2Meteo}},
  \code{\link{df2Meteo}},
    \code{\link{dfI2Meteo}},
  \code{\link{readG0dm}},
  \code{\link{readMAPA}}.
}

\keyword{classes}
