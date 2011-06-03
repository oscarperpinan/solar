\name{C_fCompD}
\Rdversion{1.3}
\encoding{UTF-8}
\alias{fCompD}

\title{Components of daily global solar irradiation on a horizontal surface}
\description{
  Extract the diffuse and direct components from the daily global irradiation on a horizontal surface by means of regressions between the clearness index and the diffuse fraction parameters.
}
\usage{
fCompD(sol, G0d, corr = "CPR",f)
}
\arguments{

  \item{sol}{A \code{Sol} object from \code{\link{calcSol}} or a \code{zoo} object
    from \code{\link{fSolD}}. Both of them include a component named
    \code{Bo0d}, which stands for the extra-atmospheric daily
    irradiation incident on a horizontalsurface}

  \item{G0d}{A \code{Meteo} object from \code{\link{readG0dm}}, \code{\link{readBD}},
    \code{\link{readSIAR}}, or a \code{zoo} object containing daily
    global irradiation (Wh/m²) on a horizontal surface. See below for \code{corr='none'}.}

  \item{corr}{A character, the correlation between the the fraction of diffuse irradiation and the
    clearness index to be used.

    With this version several options are available, as described in
    \code{\link{corrFdKt}}. For example, the \code{\link{FdKtPage}} is
    selected with \code{corr='Page'} and the \code{\link{FdKtCPR}} with
    \code{corr='CPR'}.

    If \code{corr='user'} the use of a correlation
    defined by a function \code{f} is possible.

    If \code{corr='none'} the \code{G0d} object should include information
    about global, diffuse and direct daily irradiation with columns named
    \code{G0d}, \code{D0d} and \code{B0d}, respectively. }

  \item{f}{A function defininig a correlation between the fraction of
    diffuse irradiation and the clearness index. It is only neccessary when \code{corr='user'}}
}

\value{A \code{zoo} object  which includes:
  \item{Fd}{numeric, the diffuse fraction}
  \item{Ktd}{numeric, the clearness index}
  \item{G0d}{numeric, the global irradiation on a horizontal surface (Wh/m²)}
  \item{D0d}{numeric, the diffuse irradiation on a horizontal surface (Wh/m²)}
  \item{B0d}{numeric, the direct irradiation on a horizontal surface (Wh/m²)}
}

\references{ Perpiñán, O, Energía Solar Fotovoltaica, 2011.
  (\url{http://procomun.wordpress.com/documentos/libroesf/})}

\author{
  Oscar Perpiñán Lamigueiro
}



\seealso{
  \code{\link{fCompI}}
}
\examples{
lat=37.2;
BTd=fBTd(mode='serie')

SolD<-fSolD(lat, BTd[100])

G0d=zoo(5000, index(SolD))
fCompD(SolD, G0d, corr = "Page")
fCompD(SolD, G0d, corr = "CPR")

#define a function fKtd with the correlation of CPR
fKTd=function(x){(0.99*(x<=0.17))+
                 (x>0.17)*(1.188-2.272*x+9.473*x^2-21.856*x^3+14.648*x^4)}
#The same as with corr="CPR"
fCompD(SolD,G0d, corr="user",f=fKTd)

lat=-37.2;
SolDs<-fSolD(lat, BTd[283])
G0d=zoo(5000, index(SolDs))
fCompD(SolDs, G0d, corr = "CPR")

lat=37.2;
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Rad=readG0dm(G0dm, lat=lat)
solD<-fSolD(lat,fBTd(mode='prom'))
fCompD(solD, Rad, corr = 'Page')
}

\keyword{utilities}

