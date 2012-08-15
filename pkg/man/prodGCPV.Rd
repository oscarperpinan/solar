\name{A4_prodGCPV}
\Rdversion{1.3}
\encoding{UTF-8}
\alias{prodGCPV}

\title{Performance of a grid connected PV system.}

\description{Compute every step from solar angles to effective irradiance to calculate the performance of a grid connected PV system.}

\usage{
prodGCPV(lat,
         modeTrk='fixed',
         modeRad='prom',
         dataRad,
         prev, prom, mapa, bd,bdI,
         sample='hour',
         keep.night=TRUE,
         sunGeometry='michalsky',
         corr, f,
         betaLim=90, beta=abs(lat)-10, alfa=0,
         iS=2, alb=0.2, horizBright=TRUE, HCPV=FALSE,
         module=list(),
         generator=list(),
         inverter=list(),
         effSys=list(),
         modeShd='',
         struct=list(),
         distances=data.frame()
         )
}
\arguments{
  \item{lat}{numeric, latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}

  \item{modeTrk}{A character string, describing the tracking method
    of the generator. See \code{\link{calcGef}} for details.}

  \item{modeRad, dataRad, prom, mapa, bd, bdI}{Information about the source data of the
    global irradiation. See \code{\link{calcG0}} for details.}

  \item{prev}{Deprecated, \code{dataRad} should be used instead. A
    \code{Gef} or \code{G0} object (or something that can be coerced to
    a \code{Gef} object). It is only used when
    \code{modeRad='prev'}. \code{prodGCPV} will call
    \code{\link{calcGef}} for the effective irradiance and irradiation
    procedure only when \code{prev} is a \code{G0} object.}

  % \item{prom}{A list as defined by \code{\link{readG0dm}}. This list
  %   is only required when \code{modeRad='prom'}.}

  %   \item{mapa}{A list as defined by \code{\link{readMAPA}}. This
  %     list is only required  when \code{modeRad='mapa'}.}

  %    \item{bd}{\code{list} as defined by \code{\link{readBD}}. This list
  %    is only required when \code{modeRad='bd'}.}

  \item{sample, keep.night}{See \code{\link{calcSol}} for details.}

  \item{sunGeometry}{\code{character}, method for the sun geometry
    calculations. See \code{\link{calcSol}}, \code{\link{fSolD}} and \code{\link{fSolI}}.}

  \item{corr, f}{See \code{\link{calcG0}} for details.}
  % \code{character}, containing one of ‘"sec"’, ‘"min"’,
  %        ‘"hour"’.  This can optionally be preceded by a (positive or
  %        negative) integer and a space, or followed by ‘"s"’ (used by
  %        \code{\link{seq.POSIXt}})}

  %     \item{keep.night}{\code{logical} When it is TRUE (default) the
  %        time series includes the night.}


  \item{betaLim, beta, alfa, iS, alb, horizBright, HCPV}{See \code{\link{calcGef}} for details.}
  %     \item{betaLim}{numeric, maximum value of the inclination angle
  %     for a tracking surface. Its default value is 90 (no
  %     limitation))}

  %       \item{beta}{numeric, inclination angle of the surface
  %       (degrees). It is only needed when \code{modeTrk='fixed'}.}

  % \item{alfa}{numeric, azimuth angle of the surface (degrees). It is
  % positive to the West. It is only needed when
  % \code{modeTrk='fixed'}. Its default value is \code{alfa=0}}

  % \item{iS}{integer, degree of dirtiness. Its value must be included in
  % the set (1,2,3,4). \code{iS=1} corresponds to a clean surface while
  % \code{iS=4} is the selection for a dirty surface. Its default value is
  % 2}

  % \item{alb}{numeric, albedo reflection coefficient. Its default value is 0.2}

  \item{module}{list of numeric values with information about the PV module, \describe{
      \item{\code{Vocn}}{open-circuit voltage of the module at Standard
      Test Conditions (default value 57.6 volts.)}
      \item{\code{Iscn}}{short circuit current of the module at Standard
      Test Conditions (default value 4.7 amperes.)}
      \item{\code{Vmn}}{maximum power point voltage of the module at
      Standard Test Conditions (default value 46.08 amperes.)}
      \item{\code{Imn}}{Maximum power current of the module at Standard
      Test Conditions (default value 4.35 amperes.)}
      \item{\code{Ncs}}{number of cells in series inside the module
      (default value 96)}
      \item{\code{Ncp}}{number of cells in parallel inside the module (default value 1)}
      \item{\code{CoefVT}}{coefficient of decrement of voltage of each
      cell with the temperature (default value 0.0023 volts per celsius degree)}
      \item{\code{TONC}}{nominal operational  cell temperature, celsius
      degree (default value 47).}
  }}
  \item{generator}{list of numeric values with information about the generator, \describe{
      \item{\code{Nms}}{number of modules in series (default value 12)}
      \item{\code{Nmp}}{number of modules in parallel (default value 11)}
  }}
  \item{inverter}{list of numeric values with information about the DC/AC inverter, \describe{
      \item{\code{Ki}}{vector of three values, coefficients of the
      efficiency curve of the inverter (default c(0.01, 0.025, 0.05)),
      or a matrix of nine values (3x3) if there is dependence with the
      voltage (see references).}
      \item{\code{Pinv}}{nominal inverter power (W) (default value 25000
      watts.)}
      \item{\code{Vmin, Vmax}}{ minimum and maximum voltages of the MPP
      range of the inverter (default values 420 and 750 volts)}
      \item{\code{Gumb}}{ minimum irradiance for the inverter to start
      (W/m²) (default value 20 W/m²)}
  }}
  \item{effSys}{list of numeric values with information about the system losses, \describe{
      \item{\code{ModQual}}{average tolerance of the set of modules (\%), default value is 3}
      \item{\code{ModDisp}}{ module parameter disperssion losses (\%), default value is 2}
      \item{\code{OhmDC}}{ Joule losses due to the DC wiring (\%), default value is 1.5}
      \item{\code{OhmAC}}{Joule losses due to the AC wiring (\%), default value is 1.5}
      \item{\code{MPP}}{ average error of the MPP algorithm of the inverter (\%), default value is 1}
      \item{\code{TrafoMT}}{losses due to the MT transformer (\%), default value is 1}
      \item{\code{Disp}}{ losses due to stops of the system (\%), default value is 0.5}
  }}
  \item{modeShd, struct, distances}{See \code{\link{calcShd}} for details.}
  %   \item{modeShd}{character, defines the type of shadow calculation. In
  %     this version of the package the effect of the shadow is calculated
  %     as a proportional reduction of the circumsolar diffuse and direct
  %     irradiances. This type of approach is selected with
  %     \code{modeShd='area'}. In future versions other approaches which
  %     relate the geometric shadow and the electrical connections of the PV
  %     generator will be available. If \code{modeTrk='horiz'} it is
  %     possible to calculate the effect of backtracking with
  %     \code{modeShd='bt'}. If \code{modeShd=c('area','bt')} the
  %     backtracking method will be carried out and therefore no shadows
  %     will appear. Finally, for two-axis trackers it is possible to select
  %     \code{modeShd='prom'} in order to calculate the effect of shadows on
  %     an average tracker (see \code{\link{fSombra6}}).
  % The result will include three variables (Gef0, Def0 and Bef0) with the
  %     irradiance/irradiation without shadows as a reference.}

  %  \item{struct}{\code{list}. When \code{modeTrk='fixed'} or \code{modeTrk='horiz'} only a component named \code{L}, which is the height (meters) of the tracker, is needed. For two-axis trackers (\code{modeTrk='two'}), an additional component named \code{W}, the width of the tracker, is required. Moreover, two components named \code{Nrow} and \code{Ncol} are included under this list. These components define, respectively, the number of rows and columns of the whole set of trackers in the PV plant.}

  %  \item{distances}{\code{data.frame}, with a component named \code{Lew},
  %    being the distance between horizontal NS and two-axis trackers along
  %    the East-West direction, a component named \code{Lns} for two-axis
  %    trackers or a component named \code{D} for the distance between fixed
  %    surfaces. An additional component named \code{H} can be included with
  %    the relative height (meters) between surfaces.}

}

\value{A \code{ProdGCPV} object.}

\details{The calculation of the irradiance on the horizontal plane is
  carried out with the function \code{\link{calcG0}}. The transformation
  to the inclined surface makes use of the \code{\link{fTheta}} and
  \code{\link{fInclin}} functions inside the \code{\link{calcGef}} function. The shadows are computed with  \code{\link{calcShd}} while the performance of the PV system is simulated with \code{\link{fProd}}.}

\references{\itemize{
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
    (\url{http://procomun.wordpress.com/documentos/libroesf/})
  \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \url{http://www.jstatsoft.org/v50/i09/}
}}

\author{Oscar Perpiñán Lamigueiro}


\seealso{
  \code{\link{fProd}},
  \code{\link{calcGef}},
  \code{\link{calcShd}},
  \code{\link{calcG0}},
  \code{\link{compare}},
  \code{\link{compareLosses}},
  \code{\link{mergesolaR}}
}
\examples{
library(lattice)
library(latticeExtra)

lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Comparison of different tracker methods
prodFixed<-prodGCPV(lat=lat,dataRad=prom, keep.night=FALSE)
prod2x<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two', keep.night=FALSE)
prodHoriz<-prodGCPV(lat=lat,dataRad=prom, modeTrk='horiz', keep.night=FALSE)

##Comparison of yearly productivities
compare(prodFixed, prod2x, prodHoriz)
compareLosses(prodFixed, prod2x, prodHoriz)

##Comparison of power time series
ComparePac<-CBIND(two=as.zooI(prod2x)$Pac,
     horiz=as.zooI(prodHoriz)$Pac,
     fixed=as.zooI(prodFixed)$Pac)
AngSol=as.zooI(as(prodFixed, 'Sol'))
ComparePac=CBIND(AngSol, ComparePac)
mon=month(index(ComparePac))

xyplot(two+horiz+fixed~AzS|mon, data=ComparePac,
     type='l', auto.key=list(space='right', lines=TRUE, points=FALSE),ylab='Pac')

\dontrun{
###Use of modeRad='siar' and modeRad='prev'
prodSIARFixed<-prodGCPV(lat=41,
             modeRad='siar',
             dataRad=list(prov=28,est=3,
               start='01/01/2009', end='31/12/2009'),
             keep.night=FALSE)

##We want to compare systems with different effective irradiance
##so we have to convert prodSIARFixed to a 'G0' object.
G0SIAR=as(prodSIARFixed, 'G0')

prodSIAR2x<-prodGCPV(lat=41,modeTrk='two',modeRad='prev', dataRad=G0SIAR)
prodSIARHoriz<-prodGCPV(lat=41, modeTrk='horiz',modeRad='prev',
dataRad=G0SIAR)

##Comparison of yearly values
compare(prodSIARFixed, prodSIAR2x, prodSIARHoriz)
compareLosses(prodSIARFixed, prodSIAR2x, prodSIARHoriz)

##Compare of daily productivities of each tracking system
compareYf <- mergesolaR(prodSIARFixed, prodSIAR2x, prodSIARHoriz)
xyplot(compareYf, superpose=TRUE,
ylab='kWh/kWp', main='Daily productivity', auto.key=list(space='right'))
}

###Shadows
#Two-axis trackers
struct2x=list(W=23.11, L=9.8, Nrow=2, Ncol=8)
dist2x=data.frame(Lew=40, Lns=30, H=0)
prod2xShd<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two',
    modeShd='area', struct=struct2x, distances=dist2x)
print(prod2xShd)

#Horizontal N-S tracker
structHoriz=list(L=4.83);
distHoriz=data.frame(Lew=structHoriz$L*4);

#Without Backtracking
prodHorizShd<-prodGCPV(lat=lat, dataRad=prom, sample='10 min',
    modeTrk='horiz',
    modeShd='area', betaLim=60,
    distances=distHoriz,
    struct=structHoriz)
print(prodHorizShd)

xyplot(r2d(Beta)~r2d(w),
     data=prodHorizShd,
     type='l',
     main='Inclination angle of a horizontal axis tracker',
     xlab=expression(omega (degrees)),
     ylab=expression(beta (degrees)))

#With Backtracking
prodHorizBT<-prodGCPV(lat=lat, dataRad=prom, sample='10 min',
    modeTrk='horiz',
    modeShd='bt', betaLim=60,
    distances=distHoriz,
    struct=structHoriz)

print(prodHorizBT)

xyplot(r2d(Beta)~r2d(w),
     data=prodHorizBT,
     type='l',
     main='Inclination angle of a horizontal axis tracker\n with backtracking',
     xlab=expression(omega (degrees)),
     ylab=expression(beta (degrees)))

\dontrun{
compare(prodFixed, prod2x, prodHoriz, prod2xShd,
       prodHorizShd, prodHorizBT)

compareLosses(prodFixed, prod2x, prodHoriz, prod2xShd,
       prodHorizShd, prodHorizBT)

compareYf2 <- mergesolaR(prodFixed, prod2x, prodHoriz, prod2xShd,
       prodHorizShd, prodHorizBT)
xyplot(compareYf2, superpose=TRUE,
ylab='kWh/kWp', main='Daily productivity', auto.key=list(space='right'))
}
}

\keyword{utilities}
\keyword{constructors}
