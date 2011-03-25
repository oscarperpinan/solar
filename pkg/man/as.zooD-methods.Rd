\name{D_as.zooD-methods}
\docType{methods}
\alias{as.zooD}
\alias{as.zooD-methods}
\alias{as.zooD,G0-method}
\alias{as.zooD,Gef-method}
\alias{as.zooD,ProdGCPV-method}
\alias{as.zooD,ProdPVPS-method}
\alias{as.zooD,Sol-method}

\title{Methods for Function as.zooD}
\description{Convert a \code{Sol}, \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{zoo} object with daily values.}

\usage{
as.zooD(object, complete=FALSE)
}

\arguments{

  \item{object}{A \code{Sol} object (or extended.)}
  \item{complete}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "Sol")}}{Conversion to a \code{zoo}
  object with the content of the \code{solD} slot.
}

\item{\code{signature(object = "G0")}}{If \code{complete=FALSE} (default) the result includes only
  the columns of \code{G0d}, \code{D0d} and \code{B0d} from the
  \code{G0D} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD}
  and \code{G0D}.
}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE} (default) the result includes only
  the columns of \code{Gefd}, \code{Defd} and \code{Befd} from the
  \code{GefD} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD},
  \code{G0D} and \code{GefD}
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE}
  (default) the result includes only the columns of \code{Eac},
  \code{Edc} and \code{Yf} from the \code{prodD} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD},
  \code{G0D}, \code{GefD} and \code{prodD}.
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE}
  (default) the result includes only the columns of \code{Eac},
  \code{Qd} and \code{Yf} from the \code{prodD} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{solD},
  \code{G0D}, \code{GefD} and \code{prodD}.
}
}}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}

