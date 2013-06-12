\name{D_writeSolar-methods}
\docType{methods}
\alias{writeSolar}
\alias{writeSolar-methods}
\alias{writeSolar,Sol-method}
\title{Exporter of solaR results}
\description{Exports the results of the \code{solaR} functions as text
  files using \code{\link{read.zoo}}}

\usage{\S4method{writeSolar}{Sol}(object, file, complete=FALSE,
    day=FALSE, timeScales=c('i', 'd', 'm', 'y'), sep=',', ...)}

\arguments{
  \item{object}{A \code{Sol} object (or extended.)}
  \item{file}{A character with the name of the file.}
  \item{complete}{A logical. Should all the variables be exported?}
  \item{day}{A logical. Should be daily values included in the
  intradaily file?}
  \item{timeScales}{A character. Use 'i' to export intradaily values,
  'd' for daily values, 'm' for monthly values and 'y' for yearly
  values. A different file will be created for each choice.}
\item{sep}{The field separator character.}
\item{...}{Additional arguments for \code{write.zoo}}
  }
\section{Methods}{

  \describe{

  \item{\code{signature(object = "Sol")}}{This function exports the
  slots with results using \code{\link{write.zoo}}. If
  \code{complete=FALSE} and \code{day=FALSE} (default) the result
  includes only the content of the \code{solI} slot. It \code{day=TRUE}
  the contents of the \code{solD} slot are included.}

\item{\code{signature(object = "G0")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only the columns of
  \code{G0}, \code{D0} and \code{B0} of the \code{G0I} slot.  If
  \code{complete=TRUE} it returns the contents of the slots \code{G0I}
  and \code{solI}. If \code{day=TRUE} the daily values (slots \code{G0D}
  and \code{solD}) are also included.  }

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only the columns of
  \code{Gef}, \code{Def} and \code{Bef} of the \code{GefI} slot. If
  \code{complete=TRUE} it returns the contents of the slots \code{GefI},
  \code{G0I} and \code{solI}. If \code{day=TRUE} the daily values (slots
  \code{GefD}, \code{G0D} and \code{solD}) are also included.  }

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE}
  and \code{day=FALSE} (default) the result includes only the columns of
  \code{Pac} and \code{Pdc} of the \code{prodI} slot. If
  \code{complete=TRUE} it returns the contents of the slots
  \code{prodI}, \code{GefI}, \code{G0I} and \code{solI}. If
  \code{day=TRUE} the daily values (slots \code{prodD}, \code{GefD},
  \code{G0D} and \code{solD}) are also included.  }

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE}
  and \code{day=FALSE} (default) the result includes only the columns of
  \code{Pac} and \code{Q} of the \code{prodI} slot. If
  \code{complete=TRUE} it returns the contents of the slots
  \code{prodI}, \code{GefI}, \code{G0I} and \code{solI}. If
  \code{day=TRUE} the daily values (slots \code{prodD}, \code{GefD},
  \code{G0D} and \code{solD}) are also included.  }

}}

\seealso{
  \code{\link{write.zoo}},
  \code{\link{read.zoo}},
  \code{\link{as.zooI}},
  \code{\link{as.zooD}},
  \code{\link{as.zooM}},
  \code{\link{as.zooY}}
}

\examples{

lat <- 37.2;
G0dm <- c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814, 2179)
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom <- list(G0dm=G0dm, Ta=Ta)

prodFixed <- prodGCPV(lat=lat, dataRad=prom, modeRad='aguiar', keep.night=FALSE)

old <- setwd(tempdir())

writeSolar(prodFixed, 'prodFixed.csv')

dir()

zI <- read.zoo("prodFixed.csv",
               header = TRUE, sep = ",",
               FUN = as.POSIXct)

zD<- read.zoo("prodFixed.D.csv",
               header = TRUE, sep = ",")

zD<- read.zoo("prodFixed.D.csv",
               header = TRUE, sep = ",",
               FUN = as.yearmon)

setwd(old)
}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
