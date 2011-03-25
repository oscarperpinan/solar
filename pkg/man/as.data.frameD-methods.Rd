\name{D_as.data.frameD-methods}
\docType{methods}
\alias{as.data.frameD}
\alias{as.data.frameD-methods}
\alias{as.data.frameD,Sol-method}
\title{Methods for Function as.data.frameD}
\description{Convert a \code{Sol} object (or a extended class) into a \code{data.frame} with
  daily values.}
\usage{as.data.frameD(object, complete=FALSE)}
\arguments{
  \item{object}{A \code{Sol} object (or extended.)}
  \item{complete}{A logical.}
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "Sol")}}{This function converts the
      object into a \code{zoo} container with the \code{as.zooD}
      function and then into a \code{data.frame} with
      \code{as.data.frame}. Besides, it includes three additional columns
      named \code{month}, \code{day} (day of year) and \code{year}.
    }
  }
  See \code{\link{as.zooD-methods}} for a description of the argument
  \code{complete}.}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}

