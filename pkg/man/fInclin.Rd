\name{C_fInclin}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fInclin}

\title{
  Solar irradiance on an inclined surface
}
\description{
  The solar irradiance incident on an inclined surface is calculated from
  the direct and  diffuse irradiance on a horizontal surface, and from the
  evolution of the angles of the Sun and the surface. Moreover, the effect
  of the angle of incidence and dust on the PV module is included to
  obtain the effective irradiance.

  This function is used by the \code{\link{calcGef}} function.
}
\usage{
fInclin(compI, angGen, iS = 2, alb = 0.2, horizBright=FALSE)
}

\arguments{
  \item{compI}{A \code{G0} object. It may be the result of \code{\link{calcG0}}.}
  \item{angGen}{A \code{zoo} object, including at least three variables
  named \code{Beta}, \code{Alfa} and \code{cosTheta}. It may be the result of \code{\link{fTheta}}.}
  \item{iS}{integer, degree of dirtiness. Its value must be included in the set (1,2,3,4). \code{iS=1} corresponds to a clean surface while \code{iS=4} is the choice for a dirty surface. Its default value is 2}
  \item{alb}{numeric, albedo reflection coefficient. Its default value
    is 0.2}
  \item{horizBright}{logical, with default value FALSE. If TRUE, the
    horizon brightness correction proposed by Reind et al. is used.}
}

\details{
  The solar irradiance incident on an inclined surface can be calculated from the direct and  diffuse irradiance on a horizontal surface, and from the evolution of the angles of the Sun and the surface. The transformation of the direct radiation is straightforward since only geometric considerations are needed. However, the treatment of the diffuse irradiance is more complex since it involves the modelling of the atmosphere. There are several models for the estimation of diffuse irradiance on an inclined surface. The one which combines simplicity and acceptable results is the proposal of Hay and McKay. This model divides the diffuse component in isotropic and anisotropic whose values depends on a anisotropy index.
  On the other hand, the effective irradiance, the fraction of the incident irradiance that reaches the cells inside a PV module, is calculated with the losses due to the angle of incidence and dirtiness. This behaviour can be simulated with a model proposed by Martin and Ruiz requiring information about the angles of the surface and the level of dirtiness (\code{iS}) .

}
\value{A \code{zoo} object with these components:
  \item{Bo}{Extra-atmospheric irradiance on the inclined surface (W/m²)}
  \item{Bn}{Direct normal irradiance (W/m²)}
  \item{G, B, D, Di, Dc, R}{Global, direct, diffuse (total, isotropic and anisotropic) and albedo irradiance incident on an inclined surface (W/m²)}
  \item{Gef, Bef, Def, Dief, Dcef, Ref}{Effective global, direct, diffuse (total, isotropic and anisotropic) and albedo irradiance incident on an inclined surface (W/m²)}
  \item{FTb, FTd, FTr}{Factor of angular losses for the direct, diffuse and albedo components}
}
\references{\itemize{
    \item Hay, J. E. and McKay, D. C.: Estimating Solar Irradiance on Inclined Surfaces: A Review and Assessment of Methodologies. Int. J. Solar Energy, (3):pp. 203, 1985.
    \item Martin, N. and Ruiz, J.M.: Calculation of the PV modules angular losses under field conditions by means of an analytical model. Solar Energy Materials & Solar Cells, 70:25–38, 2001.
    \item D. T. Reindl and W. A. Beckman and J. A. Duffie: Evaluation of
      hourly tilted surface radiation models, Solar Energy, 45:9-17, 1990.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}}
\author{
  Oscar Perpiñán Lamigueiro.
}

\seealso{
  \code{\link{fTheta}},
  \code{\link{fCompI}},
  \code{\link{calcGef}}.
}

\keyword{utilities}

