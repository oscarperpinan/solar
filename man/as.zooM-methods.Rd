\name{D_as.zooM-methods}
\docType{methods}
\alias{as.zooM}
\alias{as.zooM-methods}
\alias{as.zooM,G0-method}
\alias{as.zooM,Gef-method}
\alias{as.zooM,ProdGCPV-method}
\alias{as.zooM,ProdPVPS-method}

\title{Methods for Function as.zooM}
\description{Convert a \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{zoo} object with monthly average of daily values.}

\usage{
\S4method{as.zooM}{G0}(object, complete=FALSE)
}

\arguments{

  \item{object}{A \code{G0} object (or extended.)}
  \item{complete}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "G0")}}{The result is the \code{G0dm} slot.}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE}
  (default) the result is the slot \code{Gefdm}. If
  \code{complete=TRUE} it returns the slot \code{G0dm}.
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE}
  (default) the result is the \code{prodDm} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0dm} and \code{Gefdm}.
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE}
  (default) the result is the \code{prodDm} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0dm} and \code{Gefdm}.
}

}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}

