\name{B5_ProdGCPV-class}
\Rdversion{1.1}
\encoding{UTF-8}
\docType{class}
\alias{ProdGCPV-class}
\alias{show,ProdGCPV-method}


\title{Class "ProdGCPV": performance of a grid connected PV system.}
\description{A class containing values of the performance of a grid
  connected PV system.}
\section{Objects from the Class}{
  Objects can be created by \code{\link{prodGCPV}}.
}
\section{Slots}{
  \describe{
    \item{\code{prodI}:}{Object of class \code{"zoo"} created by
      \code{\link{fProd}}. It includes these components:
      \describe{
        \item{Tc: }{cell temperature, \eqn{^{\circ}{\rm C}}.}
        \item{Voc, Isc, Vmpp, Impp: }{open circuit voltage, short circuit current, MPP voltage and current, respectively.}
        \item{Vdc, Idc: }{voltage and current at the input of the inverter.}
        \item{Pdc: }{power at the input of the inverter, W}
        \item{Pac: }{power at the output of the inverter, W}
        \item{EffI: }{efficiency of the inverter}
    }}
    \item{\code{prodD}:}{A \code{zoo} object with daily values of AC (\code{Eac}) and
      DC (\code{Edc}) energy (Wh), and
      productivity (\code{Yf}, Wh/Wp) of the system.}
    \item{\code{prodDm}:}{A \code{zoo} object with monthly means of
      daily values of AC and DC energy (kWh), and
      productivity of the system.}
    \item{\code{prody}:}{A \code{zoo} object with yearly sums of AC and
      DC energy (kWh), and
      productivity of the system.}

    \item{\code{module}:}{A list with the characteristics of the module.}
    \item{\code{generator}:}{A list with the characteristics of the PV generator.}
    \item{\code{inverter}:}{A list with the characteristics of the inverter.}
    \item{\code{effSys}:}{A list with the efficiency values of the system.}
  }
  Besides, this class contains the slots from the
  \code{"\linkS4class{Meteo}"}, \code{"\linkS4class{Sol}"},
  \code{"\linkS4class{G0}"} and \code{"\linkS4class{Gef}"} classes.
}
\section{Extends}{
  Class \code{"\linkS4class{Gef}"}, directly.
  Class \code{"\linkS4class{G0}"}, by class "Gef", distance 2.
  Class \code{"\linkS4class{Meteo}"}, by class "Gef", distance 3.
  Class \code{"\linkS4class{Sol}"}, by class "Gef", distance 3.
}
\section{Methods}{
  \describe{
    \item{as.zooD}{\code{signature(object = "ProdGCPV")}: conversion to a
      \code{zoo} object with daily values.}
    \item{as.zooI}{\code{signature(object = "ProdGCPV")}: conversion to a
      \code{zoo} object with intradaily values.}
    \item{as.zooM}{\code{signature(object = "ProdGCPV")}: conversion to a
      \code{zoo} object with monthly values.}
    \item{as.zooY}{\code{signature(object = "ProdGCPV")}: conversion to a
      \code{zoo} object with yearly values.}
    \item{as.data.frameD}{\code{signature(object = "ProdGCPV")}: conversion
      to a data.frame with daily values.}
    \item{as.data.frameI}{\code{signature(object = "ProdGCPV")}: conversion
      to a data.frame with intradaily values.}
    \item{as.data.frameM}{\code{signature(object = "ProdGCPV")}: conversion
      to a data.frame with monthly values.}
    \item{as.data.frameY}{\code{signature(object = "ProdGCPV")}: conversion
      to a data.frame with yearly values.}
    \item{indexD}{\code{signature(object = "ProdGCPV")}: index of the
      \code{solD} slot. }
    \item{indexI}{\code{signature(object = "ProdGCPV")}: index of the
      \code{solI} object. }
    \item{indexRep}{\code{signature(object = "ProdGCPV")}: accesor for the
      \code{match} slot.}
    \item{getLat}{\code{signature(object = "ProdGCPV")}: latitude of the inherited
      \code{\linkS4class{Sol}} object. }
    \item{xyplot}{\code{signature(x = "ProdGCPV", data = "missing")}: display
      the time series of daily values.}
    \item{xyplot}{\code{signature(x = "formula", data = "ProdGCPV")}:
      displays the contents of a \code{ProdGCPV} object with the
      \code{xyplot} method for formulas.}
    \item{as.zooD}{\code{signature(object = "ProdGCPV")}: conversion to a
      \code{zoo} object with daily values.}
    \item{as.zooI}{\code{signature(object = "ProdGCPV")}: conversion to a
      \code{zoo} object with intradaily values.}
  }
}

\references{\itemize{
  \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
  (\url{http://procomun.wordpress.com/documentos/libroesf/})
  \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \url{http://www.jstatsoft.org/v50/i09/}
}}

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\linkS4class{Sol}},
  \code{\linkS4class{G0}},
  \code{\linkS4class{Gef}},
  \code{\linkS4class{Shade}}.

}
\keyword{classes}
