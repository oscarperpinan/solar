\name{A3_calcGef}
\Rdversion{1.3}
\encoding{UTF-8}
\alias{calcGef}

\title{Irradiation and irradiance on the generator plane.}

\description{
  This function obtains the global, diffuse and direct irradiation and
  irradiance on the generator plane from the values of \emph{daily} or \emph{intradaily} global
  irradiation on the horizontal plane. It makes use of the functions
  \code{\link{calcG0}}, \code{\link{fTheta}},
  \code{\link{fInclin}}. Besides, it can calculate the shadows effect with
  the \code{\link{calcShd}} function.
}
\usage{
calcGef(lat,
        modeTrk = 'fixed',
        modeRad = 'prom',
        dataRad,
        sample = 'hour',
        keep.night = TRUE,
        sunGeometry = 'michalsky',
        corr, f,
        betaLim = 90, beta = abs(lat)-10, alfa = 0,
        iS = 2, alb = 0.2, horizBright = TRUE, HCPV = FALSE,
        modeShd = '',
        struct = list(),
        distances = data.frame(),
        ...)
}
\arguments{
  \item{lat}{numeric, latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}

  \item{modeTrk}{character, to be chosen from \code{'fixed'}, \code{'two'} or \code{'horiz'}. When \code{modeTrk = 'fixed'} the surface is fixed (inclination and azimuth angles are constant). The performance of a two-axis tracker is calculated with \code{modeTrk = 'two'}, and \code{modeTrk = 'horiz'} is the option for an horizontal N-S tracker. Its default value is \code{modeTrk = 'fixed'}}

  \item{modeRad, dataRad}{Information about the source data of the
    global irradiation. See \code{\link{calcG0}} for details.}

  \item{sample, keep.night}{See \code{\link{calcSol}} for details.}

  \item{sunGeometry}{\code{character}, method for the sun geometry
    calculations. See \code{\link{calcSol}}, \code{\link{fSolD}} and \code{\link{fSolI}}.}

  \item{corr, f}{See \code{\link{calcG0}} for details.}
  
  \item{beta}{numeric, inclination angle of the surface
    (degrees). It is only needed when \code{modeTrk = 'fixed'}.}

  \item{betaLim}{numeric, maximum value of the inclination angle
    for a tracking surface. Its default value is 90 (no
    limitation))}

  \item{alfa}{numeric, azimuth angle of the surface (degrees). It is
    measured from the south (\code{alfa = 0}), and it is negative to the
    east and positive to the west. It is only needed when
    \code{modeTrk = 'fixed'}. Its default value is \code{alfa = 0}}

  \item{iS}{integer, degree of dirtiness. Its value must be included in
    the set (1,2,3,4). \code{iS = 1} corresponds to a clean surface while
    \code{iS = 4} is the selection for a dirty surface. Its default value is
    2.}

  \item{alb}{numeric, albedo reflection coefficient. Its default value is 0.2}

  \item{modeShd, struct, distances}{See \code{\link{calcShd}} for
    details.}
  \item{horizBright}{logical, if TRUE, the horizon brightness correction proposed by Reind et al. is used.}
  \item{HCPV}{logical, if TRUE the diffuse and albedo components of the
  \emph{effective} irradiance are set to zero. HCPV is the acronym of
  High Concentration PV system.}

\item{...}{Additional arguments for \code{\link{calcSol}} and
  \code{\link{calcG0}}}

}

\value{A \code{Gef} object.}

\references{\itemize{
    \item Hay, J. E. and McKay, D. C.: Estimating Solar Irradiance on Inclined Surfaces: A Review and Assessment of Methodologies. Int. J. Solar Energy, (3):pp. 203, 1985.
    \item Martin, N. and Ruiz, J.M.: Calculation of the PV modules angular losses under field conditions by means of an analytical model. Solar Energy Materials & Solar Cells, 70:25–38, 2001.
    \item D. T. Reindl and W. A. Beckman and J. A. Duffie: Evaluation of
      hourly tilted surface radiation models, Solar Energy, 45:9-17,
      1990.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2015.
    (\url{http://oscarperpinan.github.io/esf/})
    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \url{http://www.jstatsoft.org/v50/i09/}
}}

% \note{The version 0.22 included a bug such that when horizBright = FALSE (default)
%   the isotropic diffuse component was set to zero. From version
%   0.23 the bug has been corrected and the default value of horizBright
%   is TRUE.}

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\link{calcG0}},
  \code{\link{fTheta}},
  \code{\link{fInclin}},
  \code{\link{calcShd}}.
}
\examples{
lat <- 37.2

###12 Average days.

G0dm = c(2.766, 3.491, 4.494, 5.912, 6.989, 7.742, 7.919, 7.027, 5.369,
         3.562, 2.814, 2.179)*1000;
Ta = c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2,
       15.2)

##Fixed surface, default values of inclination and azimuth.

gef <- calcGef(lat = lat, modeRad = 'prom', dataRad = list(G0dm = G0dm, Ta = Ta))
print(gef)
xyplot(gef)

##Two-axis surface, no limitation angle.

gef2 <- calcGef(lat = lat, modeRad = 'prom',
                dataRad = list(G0dm = G0dm, Ta = Ta),
                modeTrk = 'two')
print(gef2)
xyplot(gef2)

struct = list(W = 23.11, L = 9.8, Nrow = 2, Ncol = 8)
distances = data.frame(Lew = 40, Lns = 30, H = 0)

gefShd <- calcGef(lat = lat, modeRad = 'prom',
                  dataRad = list(G0dm = G0dm, Ta = Ta),
                  modeTrk = 'two',
                  modeShd = c('area', 'prom'), 
                  struct = struct, distances = distances)
print(gefShd)

\dontrun{
##Fixed surface using Aguiar method
gefAguiar <- calcGef(lat = lat, modeRad = 'aguiar', dataRad = G0dm)

##Two-axis tracker, using the previous result.
##'gefAguiar' is internally coerced to a 'G0' object.

gefAguiar2 <- calcGef(lat = lat, modeRad = 'prev', dataRad = gefAguiar, modeTrk = 'two')
print(gefAguiar2)
xyplot(gefAguiar2)

###Shadows between two-axis trackers, again using the gefAguiar result.

struct = list(W = 23.11, L = 9.8, Nrow = 2, Ncol = 8)
distances = data.frame(Lew = 40, Lns = 30, H = 0)

gefShdAguiar <- calcGef(lat = lat, modeRad = 'prev', 
                        dataRad = gefAguiar, modeTrk = 'two', 
                        modeShd = c('area', 'prom'), 
                        struct = struct, distances = distances)
print(gefShdAguiar)
}
}

\keyword{utilities}
\keyword{constructors}
