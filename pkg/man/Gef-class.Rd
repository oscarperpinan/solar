\name{B4_Gef-class}
\Rdversion{1.1}
\encoding{UTF-8}
\docType{class}
\alias{Gef-class}
\alias{show,Gef-method}

\title{Class "Gef": irradiation and irradiance on the generator plane.}
\description{This class contains the global, diffuse and direct
  irradiation and irradiance on the horizontal plane, and ambient
  temperature. }

\section{Objects from the Class}{
  Objects can be created by the function \code{\link{calcGef}}.
}
\section{Slots}{
  \describe{
    \item{\code{GefI}:}{Object of class \code{"zoo"} created by
      \code{\link{fInclin}}. It contains these components:
      \describe{
        \item{Bo: }{Extra-atmospheric irradiance on the inclined surface
	  (W/m²)}
	\item{Bn: }{Direct normal irradiance (W/m²)}
        \item{G, B, D, Di, Dc, R: }{Global, direct, diffuse (total, isotropic
          and anisotropic) and albedo irradiance incident on an inclined
          surface (W/m²)}
        \item{Gef, Bef, Def, Dief, Dcef, Ref: }{Effective global, direct,
          diffuse (total, isotropic and anisotropic) and albedo irradiance
          incident on an inclined surface (W/m²)}
        \item{FTb, FTd, FTr: }{Factor of angular losses for the direct,
          diffuse and albedo components}}}

    \item{\code{GefD}:}{Object of class \code{"zoo"} with daily values
      of global, diffuse and direct irradiation.}

    \item{\code{Gefdm}:}{Object of class \code{"zoo"} with monthly means
      of daily global, diffuse and direct irradiation.}

    \item{\code{Gefy}:}{Object of class \code{"zoo"} with yearly sums of
      global, diffuse and direct irradiation.}

    \item{\code{Theta}:}{Object of class \code{"zoo"} created by
      \code{\link{fTheta}}. It contains these components:
      \describe{
        \item{\code{Beta}: }{numeric, inclination angle of the surface
          (radians). When \code{modeTrk='fixed'} it is the value of the
          argument \code{beta} converted from degreesto radians.}
        \item{\code{Alfa}: }{numeric, azimuth angle of the surface
          (radians). When \code{modeTrk='fixed'} it is the value of the
          argument \code{alfa} converted from degreesto radians.}
        \item{\code{cosTheta}: }{numeric, cosine of the incidence angle of the solar
          irradiance on the surface}
    }}
    \item{\code{iS}:}{numeric, degree of dirtiness.}
    \item{\code{alb}:}{numeric, albedo reflection coefficient.}
    \item{\code{modeTrk}:}{character, mode of tracking.}
    \item{\code{modeShd}:}{character, mode of shadows.}
    \item{\code{angGen}:}{A list with the values of \code{alfa},
      \code{beta} and \code{betaLim}.}
    \item{\code{struct}:}{A list with the dimensions of the structure. }
    \item{\code{distances}:}{A data.frame with the distances between structures.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{G0}"}, directly.
  Class \code{"\linkS4class{Meteo}"}, by class "G0", distance 2.
  Class \code{"\linkS4class{Sol}"}, by class "G0", distance 2.
}
\section{Methods}{
  \describe{
    \item{as.zooD}{\code{signature(object = "Gef")}: conversion to a
      \code{zoo} object with daily values.}
    \item{as.zooI}{\code{signature(object = "Gef")}: conversion to a
      \code{zoo} object with intradaily values.}
    \item{as.zooM}{\code{signature(object = "Gef")}: conversion to a
      \code{zoo} object with monthly values.}
    \item{as.zooY}{\code{signature(object = "Gef")}: conversion to a
      \code{zoo} object with yearly values.}
    \item{as.data.frameD}{\code{signature(object = "Gef")}: conversion
      to a data.frame with daily values.}
    \item{as.data.frameI}{\code{signature(object = "Gef")}: conversion
      to a data.frame with intradaily values.}
    \item{as.data.frameM}{\code{signature(object = "Gef")}: conversion
      to a data.frame with monthly values.}
    \item{as.data.frameY}{\code{signature(object = "Gef")}: conversion
      to a data.frame with yearly values.}
    \item{indexD}{\code{signature(object = "Gef")}: index of the
      \code{solD} slot. }
    \item{indexI}{\code{signature(object = "Gef")}: index of the
      \code{solI} object. }
    \item{indexRep}{\code{signature(object = "Gef")}: accesor for the
      \code{match} slot.}
    \item{getLat}{\code{signature(object = "Gef")}: latitude of the inherited
      \code{\linkS4class{Sol}} object. }
    \item{xyplot}{\code{signature(x = "Gef", data = "missing")}: display
      the time series of daily values of irradiation.}
    \item{xyplot}{\code{signature(x = "formula", data = "Gef")}:
      displays the contents of a \code{Gef} object with the
      \code{xyplot} method for formulas.}
  }
}

\references{\itemize{
  \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
  (\url{http://procomun.wordpress.com/documentos/libroesf/})
  \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \url{http://www.jstatsoft.org/v50/i09/}
  }}

\author{
  Oscar Perpiñán Lamigueiro.
}

\seealso{
  \code{\linkS4class{Sol}},
  \code{\linkS4class{G0}}.
}
\keyword{classes}
