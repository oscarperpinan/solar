\name{D_as.data.frameI-methods}
\docType{methods}
\alias{as.data.frameI}
\alias{as.data.frameI-methods}
\alias{as.data.frameI,Sol-method}
\title{Methods for Function as.data.frameI}
\description{Convert a \code{Sol} object (or a extended class) into a data.frame with
  intradaily values.}
\usage{as.data.frameI(object, complete=FALSE, day=FALSE)}
\arguments{
  \item{object}{A \code{Sol} object (or extended.)}
  \item{complete}{A logical.}
  \item{day}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "Sol")}}{This function converts the
  object into a \code{zoo} container with the \code{as.zooI}
  function and then into a \code{data.frame} with
  \code{as.data.frame}. Besides, it includes three additional columns
  named \code{month}, \code{day} (day of year) and \code{year}.
}
}
See \code{\link{as.zooI-methods}} for a description of the arguments
\code{complete} and \code{day}.}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}

