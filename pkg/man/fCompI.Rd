\name{C_fCompI}
\Rdversion{1.3}
\encoding{UTF-8}
\alias{fCompI}

\title{
  Calculation of solar irradiance on a horizontal surface}

\description{
  From the daily global, diffuse and direct irradiation values supplied by \code{fCompD}, the profile of the global, diffuse and direct irradiance is calculated with the \code{rd} and \code{rg} components of \code{fSolI}.
}
\usage{fCompI(sol, compD, G0I, corr='none', f)}

\arguments{
  \item{sol}{A \code{Sol} object as provided by \code{\link{calcSol}} or
    a \code{zoo} object as provided by \code{\link{fSolI}}.}

  \item{compD}{A \code{zoo} object as provided by fCompD. It is not
    considered if \code{G0I} is provided.}


  \item{G0I}{A \code{Meteo} object from \code{\link{readBDi}},
    \code{\link{dfI2Meteo}} or \code{\link{zoo2Meteo}}, or a \code{zoo}
    object containing \emph{intradaily} global irradiation (Wh/m²) on a
    horizontal surface.

    See below for \code{corr='none'}.}

  \item{corr}{A character, the correlation between the the fraction of
    intradaily diffuse irradiation and the
    clearness index to be used.

    With this version several options are available, as described in
    \code{\link{corrFdKt}}. You should choose one of \emph{intradaily}
    proposals. For example, the \code{\link{FdKtCLIMEDh}} is
    selected with \code{corr='CLIMEDh'}.

    If \code{corr='user'} the use of a correlation
    defined by a function \code{f} is possible.

    If \code{corr='none'} the \code{G0I} object should include information
    about global, diffuse and direct intradaily irradiation with columns named
    \code{G0}, \code{D0} and \code{B0}, respectively. }


  \item{f}{A function defininig a correlation between the fraction of
    diffuse irradiation and the clearness index. It is only neccessary when \code{corr='user'}}
}

\value{A \code{zoo} with these components:

  \item{kt}{numeric, clearness index.}
  \item{fd}{numeric, diffuse fraction.}
  \item{G0}{numeric, global irradiance on a horizontal surface, (W/m²)}
  \item{D0}{numeric, diffuse irradiance on a horizontal surface, (W/m²)}
  \item{B0}{numeric, direct irradiance on a horizontal surface, (W/m²)}
}

\references{ \itemize{
    \item Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
      diffuse and hemispherical and between daily and hourly insolation values. Solar Energy, 22:155–164, 1979.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
  }
}
\author{
  Oscar Perpiñán Lamigueiro.
}
\seealso{
  \code{\link{fCompD}},
  \code{\link{fSolI}},
  \code{\link{calcSol}},
  \code{\link{corrFdKt}}.
}
\examples{

lat=37.2;
BTd=fBTd(mode='serie')

solD<-fSolD(lat, BTd[100])
solI<-fSolI(solD, sample='hour')
G0d=zoo(5000, index(solD))
compD<-fCompD(solD, G0d, corr = "Page")
fCompI(solI, compD)

lat=37.2;
sol<-calcSol(lat, fBTd(mode='prom'), sample='hour', keep.night=FALSE)

G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
BD<-readG0dm(G0dm=G0dm, Ta=Ta, lat=37.2)
compD<-fCompD(sol, BD, corr = 'Page')
compI<-fCompI(sol, compD)
head(compI)
}

\keyword{utilities}

