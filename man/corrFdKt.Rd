\name{C_corrFdKt}
\alias{corrFdKt}
\alias{FdKtPage}
\alias{FdKtLJ}
\alias{FdKtCPR}
\alias{FdKtEKDd}
\alias{FdKtCLIMEDd}
\alias{FdKtEKDh}
\alias{FdKtCLIMEDh}
\alias{FdKtBRL}
\Rdversion{1.0}
\encoding{UTF-8}

\title{Correlations between the fraction of diffuse irradiation and the
  clearness index.}
\description{A set of correlations between the fraction of diffuse irradiation and the
  clearness index used by \code{\link{fCompD}} and \code{\link{fCompI}}.
}
\usage{
## Monthly means of daily values
FdKtPage(Ktd)
FdKtLJ(Ktd)

## Daily values
FdKtCPR(Ktd)
FdKtEKDd(Ktd, sol)
FdKtCLIMEDd(Ktd)

## Intradaily values
FdKtEKDh(kt)
FdKtCLIMEDh(kt)
FdKtBRL(kt, sol)
}
\arguments{
  \item{Ktd}{A numeric, the daily clearness index.}

  \item{kt}{A numeric, the intradaily clearness index.}

  \item{sol}{A \code{Sol} object provided by \code{\link{calcSol}} or a
    \code{zoo} object provided by \code{\link{fSolD}} or \code{\link{fSolI}}.}
}

\value{A numeric, the diffuse fraction.}


\references{\itemize{
    \item Page, J. K., The calculation of monthly mean solar radiation for horizontal and inclined surfaces from
      sunshine records for latitudes 40N-40S. En U.N. Conference on New Sources of Energy, vol. 4, págs.
      378–390, 1961.
    \item Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
      diffuse and hemispherical and between daily and hourly insolation
      values. Solar Energy, 22:155–164, 1979.
    \item Erbs, D.G, Klein, S.A. and Duffie, J.A., Estimation of the diffuse
      radiation fraction for hourly, daily and monthly-average global
      radiation. Solar Energy, 28:293:302, 1982.
    \item De Miguel, A. et al., Diffuse solar irradiation model evaluation
      in the north mediterranean belt area, Solar Energy, 70:143-153, 2001.
    \item Ridley, B., Boland, J. and Lauret, P., Modelling of diffuse solar
      fraction with multiple predictors, Renewable Energy, 35:478-482, 2010.
  }
}
\seealso{
  \code{\link{fCompD}},
  \code{\link{fCompI}}
}
\examples{
Ktd=seq(0, 1, .01)
Monthly=data.frame(Ktd=Ktd)
Monthly$Page=FdKtPage(Ktd)
Monthly$LJ=FdKtLJ(Ktd)

xyplot(Page+LJ~Ktd, data=Monthly,
       type=c('l', 'g'), auto.key=list(space='right'))

Ktd=seq(0, 1, .01)
Daily=data.frame(Ktd=Ktd)
Daily$CPR=FdKtCPR(Ktd)
Daily$CLIMEDd=FdKtCLIMEDd(Ktd)

xyplot(CPR+CLIMEDd~Ktd, data=Daily,
       type=c('l', 'g'), auto.key=list(space='right'))

}
\author{Oscar Perpiñán Lamigueiro; The BRL model was suggested by Kevin Ummel.}
\keyword{utilities}
