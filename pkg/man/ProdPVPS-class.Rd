\name{B6_ProdPVPS-class}
\Rdversion{1.1}
\encoding{UTF-8}
\docType{class}
\alias{ProdPVPS-class}
\alias{show,ProdPVPS-method}

\title{Class "ProdPVPS": performance of a PV pumping system.}
\description{Performance of a PV pumping system with a centrifugal pump
  and a variable frequency converter.}
\section{Objects from the Class}{
  Objects can be created by \code{\link{prodPVPS}}.
}
\section{Slots}{
  \describe{
    \item{\code{prodI}:}{Object of class \code{"zoo"} with these components:
      \describe{
        \item{Q: }{Flow rate, (m³/h)}
        \item{Pb, Ph: }{Pump shaft power and hydraulical power (W), respectively.}
        \item{etam, etab: }{Motor and pump efficiency, respectively.}
        \item{f: }{Frequency (Hz)}

    }}
    \item{\code{prodD}:}{A \code{zoo} object with daily values of AC
      energy (Wh), flow (m³) and productivity of the system.}
    \item{\code{prodDm}:}{A \code{zoo} object with monthly means of
      daily values of AC energy (kWh), flow (m³) and
      productivity of the system.}
    \item{\code{prody}:}{A \code{zoo} object with yearly sums of AC
      energy (kWh), flow (m³) and productivity of the system.}

    \item{\code{pump}}{A \code{list} extracted from \code{\link{pumpCoef}}}
    \item{\code{H}}{Total manometric head (m)}
    \item{\code{Pg}}{Nominal power of the PV generator (Wp)}
    \item{\code{converter}}{\code{list} containing the nominal power of the
      frequency converter, \code{Pnom}, and \code{Ki}, vector of three
      values, coefficients of the efficiency curve.}
    \item{\code{effSys}}{list of numeric values with information about the system
      losses}
  }
  Besides, this class contains the slots from the \code{\linkS4class{Gef}} class.
}
\section{Extends}{
  Class \code{"\linkS4class{Gef}"}, directly.
  Class \code{"\linkS4class{G0}"}, by class "Gef", distance 2.
  Class \code{"\linkS4class{Meteo}"}, by class "Gef", distance 3.
  Class \code{"\linkS4class{Sol}"}, by class "Gef", distance 3.
}
\section{Methods}{
  \describe{
\item{as.zooD}{\code{signature(object = "ProdPVPS")}: conversion to a
      \code{zoo} object with daily values.}
    \item{as.zooI}{\code{signature(object = "ProdPVPS")}: conversion to a
      \code{zoo} object with intradaily values.}
    \item{as.zooM}{\code{signature(object = "ProdPVPS")}: conversion to a
      \code{zoo} object with monthly values.}
    \item{as.zooY}{\code{signature(object = "ProdPVPS")}: conversion to a
      \code{zoo} object with yearly values.}
    \item{as.data.frameD}{\code{signature(object = "ProdPVPS")}: conversion
      to a data.frame with daily values.}
    \item{as.data.frameI}{\code{signature(object = "ProdPVPS")}: conversion
      to a data.frame with intradaily values.}
    \item{as.data.frameM}{\code{signature(object = "ProdPVPS")}: conversion
      to a data.frame with monthly values.}
    \item{as.data.frameY}{\code{signature(object = "ProdPVPS")}: conversion
      to a data.frame with yearly values.}
    \item{indexD}{\code{signature(object = "ProdPVPS")}: index of the
      \code{solD} slot. }
    \item{indexI}{\code{signature(object = "ProdPVPS")}: index of the
      \code{solI} object. }
    \item{indexRep}{\code{signature(object = "ProdPVPS")}: accesor for the
      \code{match} slot.}
    \item{getLat}{\code{signature(object = "ProdPVPS")}: latitude of the inherited
      \code{\linkS4class{Sol}} object. }
    \item{xyplot}{\code{signature(x = "ProdPVPS", data = "missing")}: display
      the time series of daily values.}
    \item{xyplot}{\code{signature(x = "formula", data = "ProdPVPS")}:
      displays the contents of a \code{ProdPVPS} object with the
      \code{xyplot} method for formulas.}
  }
}
\references{\itemize{
    \item Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping systems based on standard frequency converters. Progress in Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN 1099-159X.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}}

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\link{prodPVPS}},
  \code{\link{fPump}}.
}

\keyword{classes}
