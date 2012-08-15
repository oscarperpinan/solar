\name{A5_prodPVPS}
\encoding{UTF-8}
\Rdversion{1.3}
\alias{prodPVPS}

\title{
  Performance of a PV pumping system
}
\description{
  Compute every step from solar angles to effective irradiance to calculate the performance of a PV pumping system.  }

\usage{
prodPVPS(lat,
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
         pump , H,
         Pg, converter= list(),
         effSys=list()
         )
}

\arguments{
  \item{lat}{numeric, latitude (degrees) of the point of the Earth where
    calculations are needed. It is positive for locations above the
    Equator.}
  \item{modeTrk}{A character string, describing the tracking method
    of the generator. See \code{\link{calcGef}} for details.}

  \item{modeRad, dataRad, prom, mapa, bd, bdI}{Information about the source data of the
    global irradiation. See \code{\link{calcG0}} for details.}

  \item{prev}{Deprecated, \code{dataRad} should be used instead. A
    \code{G0} object (or something that can be coerced to a \code{G0}
    object). It is used when \code{modeRad='prev'}.}

  % \item{prom}{A list as defined by \code{\link{readG0dm}}. This list
  %   is only required when \code{modeRad='prom'}.}

  %   \item{mapa}{A list as defined by \code{\link{readMAPA}}. This
  %     list is only required  when \code{modeRad='mapa'}.}

  %    \item{bd}{\code{list} as defined by \code{\link{readBD}}. This list
  %    is only required when \code{modeRad='bd'}.}

  \item{sample, keep.night}{See \code{\link{calcSol}} for
    details.}

  \item{sunGeometry}{\code{character}, method for the sun geometry
    calculations. See \code{\link{calcSol}}, \code{\link{fSolD}} and \code{\link{fSolI}}.}

    \item{corr, f}{See \code{\link{calcG0}} for details.}
  % \item{sample}{\code{character}, containing one of ‘"sec"’, ‘"min"’,
  %       ‘"hour"’.  This can optionally be preceded by a (positive or
  %       negative) integer and a space, or followed by ‘"s"’ (used by
  %       \code{\link{seq.POSIXt}})}

  %     \item{keep.night}{\code{logical} When it is TRUE (default) the
  %       time series includes the night.}
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

  % \item{alb}{numeric, albedo reflection coefficient. Its default value
  % is0.2}

  \item{pump}{A \code{list} extracted from \code{\link{pumpCoef}}}
  \item{H}{Total manometric head (m)}
  \item{Pg}{Nominal power of the PV generator (Wp)}
  \item{converter}{\code{list} containing the nominal power of the
    frequency converter, \code{Pnom}, and \code{Ki}, vector of three
    values, coefficients of the efficiency curve.}

  \item{effSys}{list of numeric values with information about the system
    losses,
    \describe{
      \item{\code{ModQual}}{average tolerance of the set of modules
        (\%), default value is 3}
      \item{\code{ModDisp}}{ module parameter disperssion losses (\%),
        default value is 2}
      \item{\code{OhmDC}}{ Joule losses due to the DC wiring (\%),
        default value is 1.5}
      \item{\code{OhmAC}}{Joule losses due to the AC wiring (\%),
        default value is 1.5}
  }}
}

\value{A \code{\linkS4class{ProdPVPS}} object.}


\details{The calculation of the irradiance on the generator is carried
  out with the function \code{\link{calcGef}}. The  performance of the PV system is simulated with \code{\link{fPump}}.}

\references{\itemize{
    \item Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping systems based on standard frequency converters. Progress in Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN 1099-159X.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
    (\url{http://procomun.wordpress.com/documentos/libroesf/})
    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \url{http://www.jstatsoft.org/v50/i09/}
}}
\author{Oscar Perpiñán Lamigueiro.}

\seealso{\code{\link{NmgPVPS}}, \code{\link{fPump}}, \code{\link{pumpCoef}}}

\examples{
library(lattice)

data(pumpCoef)

CoefSP8A44<-subset(pumpCoef, Qn==8&stages==44)

\dontrun{
prodSP8A44<-prodPVPS(lat=41,
             modeRad='siar',
             dataRad=list(prov=28,est=3,
               start='01/01/2009', end='31/12/2009'),
             pump=CoefSP8A44, Pg=6000, H=140)
print(prodSP8A44)

xyplot(prodSP8A44)

xyplot(Q~Gef|month, data=prodSP8A44, cex=0.5)
}
}

\keyword{utilities}
\keyword{constructors}
