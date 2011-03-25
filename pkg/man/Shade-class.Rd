\name{B7_Shade-class}
\encoding{UTF-8}
\Rdversion{1.1}
\docType{class}
\alias{Shade-class}
\alias{as.data.frame,Shade-method}
\alias{show,Shade-method}

\title{Class "Shade": shadows in a PV system.}
\description{A class for the optimization of shadows in a PV system.}
\section{Objects from the Class}{Objects can be created by \code{\link{optimShd}}.}
\section{Slots}{
  \describe{
    \item{\code{FS}:}{numeric, shadows factor values for each
      combination of distances.}
    \item{\code{GRR}:}{numeric, Ground Requirement Ratio for each combination.}
    \item{\code{Yf}:}{numeric, final productivity for each combination.}
    \item{\code{FS.loess}:}{A local fitting of \code{FS} with \code{loess}.}
    \item{\code{Yf.loess}:}{A local fitting of \code{Yf} with \code{loess}.}
    \item{\code{modeShd}:}{character, mode of shadows.}
    \item{\code{struct}:}{A list with the dimensions of the structure. }
    \item{\code{distances}:}{A data.frame with the distances between structures.}
    \item{\code{res}}{numeric, difference (meters) between the different steps of the calculation.}
  }
  Besides, as a reference, this class includes a \code{\linkS4class{ProdGCPV}} object
  with the performance of a PV systems without shadows.
}
\section{Extends}{
  Class \code{"\linkS4class{ProdGCPV}"}, directly.
  Class \code{"\linkS4class{Gef}"}, by class "ProdGCPV", distance 2.
  Class \code{"\linkS4class{G0}"}, by class "ProdGCPV", distance 3.
  Class \code{"\linkS4class{Meteo}"}, by class "ProdGCPV", distance 4.
  Class \code{"\linkS4class{Sol}"}, by class "ProdGCPV", distance 4.
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "Shade")}: conversion to a
      data.frame including columns for distances (\code{Lew},
      \code{Lns}, and \code{D}) and results (\code{FS}, \code{GRR} and \code{Yf}).}
    \item{shadeplot}{\code{signature(x = "Shade")}: display the results of
      the iteration with a level plot for the two-axis tracking, or with
      conventional plot for horizontal tracking and fixed systems.}
    \item{xyplot}{\code{signature(x = "formula", data = "Shade")}:
      display the content of the \code{Shade} object with the
      \code{xyplot} method for formulas.}
  }
}


\references{\itemize{
    \item Perpiñán, O.: Grandes Centrales Fotovoltaicas: producción, seguimiento y ciclo de vida. PhD Thesis, UNED, 2008. \url{http://e-spacio.uned.es/fez/view.php?pid=bibliuned:20080}.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
  }
}


\author{
  Oscar Perpiñán Lamigueiro.
}

\seealso{
  \code{\linkS4class{Gef}},
  \code{\linkS4class{ProdGCPV}}.
}
\keyword{classes}
