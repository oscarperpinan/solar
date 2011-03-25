\name{C_fPump}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fPump}

\title{Performance of a centrifugal pump}
\description{
  Compute the performance of the different parts of a centrifugal pump fed by a frequency converter following the affinity laws.}
\usage{
fPump(pump, H)
}
\arguments{
  \item{pump}{\code{list} containing the parameters of the pump to be simulated. It may be a row of \code{\link{pumpCoef}}.}
  \item{H}{Total manometric head (m).}
}

\value{
  \item{lim}{Range of values of electrical power input}
  \item{fQ}{Function constructed with \code{splinefun} relating flow and electrical power}
  \item{fPb}{Function constructed with \code{splinefun} relating pump shaft power and electrical power of the motor}
  \item{fPh}{Function constructed with \code{splinefun} relating hydraulical power and electrical power of the motor}
  \item{fFreq}{Function constructed with \code{splinefun} relating frequency and electrical power of the motor}
}
\references{\itemize{
    \item Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping systems based on standard frequency converters. Progress in Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN 1099-159X.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}}
\author{
  Oscar Perpiñán Lamigueiro.
}

\seealso{\code{\link{NmgPVPS}}, \code{\link{prodPVPS}}, \code{\link{pumpCoef}}, \code{splinefun}.}

\examples{
library(latticeExtra)

data(pumpCoef)
CoefSP8A44<-subset(pumpCoef, Qn==8&stages==44)

fSP8A44<-fPump(pump=CoefSP8A44,H=40)
SP8A44=with(fSP8A44,{
                Pac=seq(lim[1],lim[2],by=100)
                Pb=fPb(Pac)
                etam=Pb/Pac
                Ph=fPh(Pac)
                etab=Ph/Pb
                f=fFreq(Pac)
                Q=fQ(Pac)
                result=data.frame(Q,Pac,Pb,Ph,etam,etab,f)})

#Efficiency of the motor, pump and the motor-pump
SP8A44$etamb=with(SP8A44,etab*etam)
lab=c(expression(eta[motor]), expression(eta[pump]), expression(eta[mp]))
p<-xyplot(etam+etab+etamb~Pac,data=SP8A44,type='l', ylab='Efficiency')
p+glayer(panel.text(x[1], y[1], lab[group.number], pos=3))

#Mechanical, hydraulic and electrical power
lab=c(expression(P[pump]), expression(P[hyd]))
p<-xyplot(Pb+Ph~Pac,data=SP8A44,type='l', ylab='Power (W)', xlab='AC Power (W)')
p+glayer(panel.text(x[length(x)], y[length(x)], lab[group.number], pos=3))

#Flow and electrical power
xyplot(Q~Pac,data=SP8A44,type='l')
}
\keyword{utilities}

