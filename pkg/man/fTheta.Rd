\name{C_fTheta}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fTheta}

\title{
  Angle of incidence of solar irradiation on a inclined surface
}
\description{
  The orientation, azimuth and incidence angle are calculated from the
  results of \code{fSolI} or \code{calcSol}and from the information supplied by the
  arguments \code{beta} and \code{alfa} when the surface is fixed
  (\code{modeTrk='fixed')} or the movement equations when a tracking
  surface is chosen (\code{modeTrk='horiz'} or \code{modeTrk='two')}.
  Besides, the modified movement of a horizontal NS tracker due to the
  backtracking strategy is calculated if \code{BT=TRUE} with information
  about the tracker and the distance between the trackers included in the
  system.

  This function is used by the \code{\link{calcGef}} function.
}
\usage{
fTheta(sol, beta, alfa = 0, modeTrk = "fixed", betaLim = 90,
    BT = FALSE, struct, dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sol}{\code{Sol} object as provided by \code{\link{calcSol}}.}
  \item{beta}{numeric, inclination angle of the surface (degrees). It is only needed when \code{modeTrk='fixed'}.}
  \item{alfa}{numeric, azimuth angle of the surface (degrees). It is positive to the West. It is only needed when \code{modeTrk='fixed'}. Its default value is \code{alfa=0}}
  \item{modeTrk}{character, to be chosen from \code{'fixed'}, \code{'two'} or \code{'horiz'}. When \code{modeTrk='fixed'} the surface is fixed (inclination and azimuth angles are constant). The performance of a two-axis tracker is calculated with \code{modeTrk='two'}, and \code{modeTrk='horiz'} is the option for an horizontal N-S tracker. Its default value is \code{modeTrk='fixed'}}
  \item{betaLim}{numeric, maximum value of the inclination angle for a tracking surface. Its default value is 90 (no limitation))}
  \item{BT}{logical, \code{TRUE} when the bactracking technique is to be used with a horizontal NS tracker, as described by Panico et al. (see References). The default value is \code{FALSE}. In future versions of this package this technique will be available for two-axis trackers.}
  \item{struct}{Only needed when \code{BT=TRUE}. A \code{list}, with a component named \code{L}, which is the height (meters) of the tracker. In future versions the backtracking technique will be used in conjuction with two-axis trackers, and a additional component named \code{W} will be needed.}
  \item{dist}{Only needed when \code{BT=TRUE}. A \code{data.frame}, with a component named \code{Lew}, being the distance between the horizontal NS trackers along the East-West direction. In future versions an additional component named \code{Lns} will be needed for two-axis trackers with backtracking.}
}

\value{A \code{zoo} object with these components:
  \item{Beta}{numeric, inclination angle of the surface (radians). When \code{modeTrk='fixed'} it is the value of the argument \code{beta} converted from degrees to radians.}
  \item{Alfa}{numeric, azimuth angle of the surface (radians). When \code{modeTrk='fixed'} it is the value of the argument \code{alfa} converted from degrees to radians.}
  \item{cosTheta}{numeric, cosine of the incidence angle of the solar irradiance on the surface}

}
\references{
  \itemize{
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
    \item Panico, D., Garvison, P., Wenger, H. J., Shugar, D., Backtracking: a novel strategy for tracking PV systems, Photovoltaic Specialists Conference, 668-673, 1991
}}
\author{
  Oscar Perpiñán Lamigueiro.
}


\seealso{
  \code{\link{fInclin}},
  \code{\link{fSombra}},
  \code{\link{calcGef}}.
}
\keyword{utilities}

