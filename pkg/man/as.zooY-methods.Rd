\name{D_as.zooY-methods}
\docType{methods}
\alias{as.zooY}
\alias{as.zooY-methods}
\alias{as.zooY,G0-method}
\alias{as.zooY,Gef-method}
\alias{as.zooY,ProdGCPV-method}
\alias{as.zooY,ProdPVPS-method}

\title{Methods for Function as.zooY}
\description{Convert a \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{zoo} object with yearly values.}

\usage{
\S4method{as.zooY}{G0}(object, complete=FALSE)
}

\arguments{

  \item{object}{A \code{G0} object (or extended.)}
  \item{complete}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "G0")}}{The result is the \code{G0y} slot.}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE}
  (default) the result is the slot \code{Gefy}. If
  \code{complete=TRUE} it returns the slot \code{G0y}.
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE}
  (default) the result is the \code{prody} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0y} and \code{Gefy}.
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE}
  (default) the result is the \code{prody} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0y} and \code{Gefy}.
}

}}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}

