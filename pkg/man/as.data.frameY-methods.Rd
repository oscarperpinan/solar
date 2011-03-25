\name{D_as.data.frameY-methods}
\docType{methods}
\alias{as.data.frameY}
\alias{as.data.frameY-methods}
\alias{as.data.frameY,G0-method}
\title{Methods for Function as.data.frameY}
\description{Convert a \code{G0} object (or a extended class) into a \code{data.frame} with
  yearly values.}
\usage{as.data.frameY(object, complete=FALSE)}
\arguments{
  \item{object}{A \code{G0} object (or extended.)}
  \item{complete}{A logical.}
  }

\section{Methods}{
\describe{
\item{\code{signature(object = "G0")}}{This function converts the
  object into a \code{zoo} container with the \code{as.zooY}
  function and then into a \code{data.frame} with
  \code{as.data.frame}. Besides, it includes an additional column
  named \code{year}.
}
}
See \code{\link{as.zooY-methods}} for a description of the argument
\code{complete}.}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}

