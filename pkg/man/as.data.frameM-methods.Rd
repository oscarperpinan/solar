\name{D_as.data.frameM-methods}
\docType{methods}
\alias{as.data.frameM}
\alias{as.data.frameM-methods}
\alias{as.data.frameM,G0-method}
\title{Methods for Function as.data.frameM}
\description{Convert a \code{G0} object (or a extended class) into a \code{data.frame} with
  monthly values.}
\usage{as.data.frameM(object, complete=FALSE)}
\arguments{
  \item{object}{A \code{G0} object (or extended.)}
  \item{complete}{A logical.}
  }

\section{Methods}{
\describe{
\item{\code{signature(object = "G0")}}{This function converts the
  object into a \code{zoo} container with the \code{as.zooM}
  function and then into a \code{data.frame} with
  \code{as.data.frame}. Besides, it includes two   additional columns
  named \code{month}  and \code{year}.
}
}
See \code{\link{as.zooM-methods}} for a description of the argument
\code{complete}.}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}

