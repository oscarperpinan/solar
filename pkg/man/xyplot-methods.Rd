\name{D_xyplot-methods}
\docType{methods}
\alias{xyplot-methods}
\alias{xyplot,formula,Shade-method}
\alias{xyplot,formula,Meteo-method}
\alias{xyplot,formula,Sol-method}
\alias{xyplot,formula,G0-method}
\alias{xyplot,formula,zoo-method}
\alias{xyplot,Meteo,missing-method}
\alias{xyplot,G0,missing-method}
\alias{xyplot,ProdPVPS,missing-method}
\alias{xyplot,ProdGCPV,missing-method}

\title{Methods for function xyplot in Package `solaR'}
\description{Methods for function \code{xyplot} in Package `solaR'}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "formula", data = "zoo")}: }{
      The \code{zoo} object is converted into a \code{data.frame} object and
      additional columns are added (\code{day}, \code{month} and
      \code{year}, and \code{w} with the solar hour in radians).
      This \code{data.frame} is the \code{data} argument for a
      call to \code{xyplot}, using the S3 method for class \code{formula}.
    }

    \item{\code{signature(x = "formula", data = "Meteo")}: }{
      The \code{Meteo} object is converted into a \code{zoo} object
      with \code{getData(data)}. This \code{zoo} is the \code{data} argument for a
      call to \code{xyplot}, using the S4 method for \code{signature(x = "formula", data = "zoo")}.

    }
    \item{\code{signature(x = "formula", data = "Sol")}: }{
      The \code{Sol} object is converted into a \code{zoo} object
      with \code{as.zooI(data, complete=TRUE, day=TRUE)} (therefore,
      the \code{zoo} includes the whole content of the object).
      This \code{zoo} is the \code{data} argument for a
      call to \code{xyplot}, using the S4 method for \code{signature(x =
        "formula", data = "zoo")}.
    }

    \item{\code{signature(x = "formula", data = "G0")}: }{
      The \code{G0} object is converted into a \code{zoo} object
      with \code{as.zooI(data, complete=TRUE, day=TRUE)} (therefore,
      the \code{zoo} includes the whole content of the object).
      This \code{zoo} is the \code{data} argument for a
      call to \code{xyplot}, using the S4 method for \code{signature(x =
        "formula", data = "zoo")}.
    }

    \item{\code{signature(x = "Meteo", data = "missing")}: }{
      The \code{Meteo} object is converted into a \code{zoo} object with
      \code{getData(x)} and displayed with the method for \code{zoo}.}

    \item{\code{signature(x = "G0", data = "missing")}: }{
      The \code{x} object is converted into a \code{zoo} object with
      \code{as.zooD(x, complete=FALSE)}. Therefore, the content of the
      \code{G0D} slot (a \code{zoo} object) is displayed with the method for \code{zoo}.}

    \item{\code{signature(x = "ProdGCPV", data = "missing")}: }{
      Idem, but the variables are not superposed.}

    \item{\code{signature(x = "ProdPVPS", data = "missing")}: }{
      Idem.}

    \item{\code{signature(x = "formula", data = "Shade")}: }{The \code{Shade}
      object is converted into a \code{data.frame} and passed as the
      \code{data} argument to the \code{xyplot} function. Once again, the S3
      method for class \code{formula} is used.
    }

}}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}

