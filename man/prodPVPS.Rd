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
         modeTrk = 'fixed',
         modeRad = 'prom',
         dataRad,
         sample = 'hour',
         keep.night = TRUE,
         sunGeometry = 'michalsky',
         corr, f,
         betaLim = 90, beta = abs(lat)-10, alfa = 0,
         iS = 2, alb = 0.2, horizBright = TRUE, HCPV = FALSE,
         pump , H,
         Pg, converter= list(),
         effSys = list(),
         ...)
}

\arguments{
  \item{lat}{numeric, latitude (degrees) of the point of the Earth where
    calculations are needed. It is positive for locations above the
    Equator.}
  \item{modeTrk}{A character string, describing the tracking method
    of the generator. See \code{\link{calcGef}} for details.}

  \item{modeRad, dataRad}{Information about the source data of the
    global irradiation. See \code{\link{calcG0}} for details.}

  \item{sample, keep.night}{See \code{\link{calcSol}} for
    details.}

  \item{sunGeometry}{\code{character}, method for the sun geometry
    calculations. See \code{\link{calcSol}}, \code{\link{fSolD}} and \code{\link{fSolI}}.}

    \item{corr, f}{See \code{\link{calcG0}} for details.}

  \item{betaLim, beta, alfa, iS, alb, horizBright, HCPV}{See \code{\link{calcGef}} for details.}

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
  \item{...}{Additional arguments for \code{\link{calcSol}},
    \code{\link{calcG0}} and \code{\link{calcGef}}.}
}

\value{A \code{\linkS4class{ProdPVPS}} object.}


\details{The calculation of the irradiance on the generator is carried
  out with the function \code{\link{calcGef}}. The  performance of the PV system is simulated with \code{\link{fPump}}.}

\references{\itemize{
    \item Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping
    systems based on standard frequency converters. Progress in
    Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN
  1099-159X.
      \item Perpiñán, O, Energía Solar Fotovoltaica, 2015.
    (\url{https://oscarperpinan.github.io/esf/})

    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \doi{10.18637/jss.v050.i09}
}}
\author{Oscar Perpiñán Lamigueiro.}

\seealso{\code{\link{NmgPVPS}}, \code{\link{fPump}}, \code{\link{pumpCoef}}}

\keyword{utilities}
\keyword{constructors}
