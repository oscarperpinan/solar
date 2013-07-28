\name{D_as.zooI-methods}
\encoding{UTF-8}
\docType{methods}
\alias{as.zooI-methods}
\alias{as.zooI}

\alias{as.zooI,G0-method}
\alias{as.zooI,Gef-method}
\alias{as.zooI,ProdGCPV-method}
\alias{as.zooI,ProdPVPS-method}
\alias{as.zooI,Sol-method}
\title{Methods for Function as.zooI}
\description{Convert a \code{Sol}, \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{zoo} object with intradaily values and (optionally) daily values.}

\usage{
\S4method{as.zooI}{Sol}(object, complete=FALSE, day=FALSE)}

\arguments{

  \item{object}{A \code{Sol} object (or extended).}
  \item{complete}{A logical.}
  \item{day}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "Sol")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only the content of the
  \code{solI} slot. It \code{day=TRUE} the contents of the \code{solD}
  slot are included.
}

\item{\code{signature(object = "G0")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only   the columns of
  \code{G0}, \code{D0} and \code{B0} of the \code{G0I} slot.
  If  \code{complete=TRUE} it returns the contents of the slots \code{G0I}
  and \code{solI}. If \code{day=TRUE} the daily values (slots \code{G0D}
  and \code{solD}) are also included.)
}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only  the columns of
  \code{Gef}, \code{Def} and \code{Bef} of the \code{GefI} slot. If  \code{complete=TRUE} it
  returns the contents of the slots \code{GefI}, \code{G0I} and
  \code{solI}. If \code{day=TRUE} the daily values (slots \code{GefD}, \code{G0D}
  and \code{solD}) are also included.)
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only  the columns of
  \code{Pac} and \code{Pdc} of the \code{prodI} slot. If  \code{complete=TRUE} it
  returns the contents of the slots \code{prodI}, \code{GefI}, \code{G0I} and
  \code{solI}. If \code{day=TRUE} the daily values (slots \code{prodD}, \code{GefD}, \code{G0D}
  and \code{solD}) are also included.)
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only  the columns of
  \code{Pac} and \code{Q} of the \code{prodI} slot. If  \code{complete=TRUE} it
  returns the contents of the slots \code{prodI}, \code{GefI}, \code{G0I} and
  \code{solI}. If \code{day=TRUE} the daily values (slots \code{prodD}, \code{GefD}, \code{G0D}
  and \code{solD}) are also included.)
}

}}

\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}

