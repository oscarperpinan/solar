\name{D_levelplot-methods}
\docType{methods}
\alias{levelplot-methods}
\alias{levelplot,formula,zoo-method}
\alias{levelplot,formula,Sol-method}
\alias{levelplot,formula,Meteo-method}
\alias{levelplot,formula,G0-method}
\title{Methods for function levelplot.}
\description{
  Methods for function \code{levelplot} and \code{zoo} and \code{solaR} objects.
}
\section{Methods}{
  \describe{

    \item{\code{signature(x = "formula", data = "zoo")}: }{
      The \code{zoo} object is converted into a \code{data.frame} object and
      additional columns are added (\code{day}, \code{month} and
      \code{year}, and \code{w} with the solar hour in radians).
      This \code{data.frame} is the \code{data} argument for a
      call to \code{levelplot}, using the S3 method for class \code{formula}.}

    \item{\code{signature(x = "formula", data = "Meteo")}: }{
      The \code{Meteo} object is converted into a \code{zoo} object, and
      the previous method is used.}

    \item{\code{signature(x = "formula", data = "Sol")}: }{idem}

    \item{\code{signature(x = "formula", data = "G0")}: }{idem}

  }
}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}

