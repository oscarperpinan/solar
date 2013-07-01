\name{A2_calcG0}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{calcG0}

\title{Irradiation and irradiance on the horizontal plane.}

\description{
  This function obtains the global, diffuse and direct irradiation and
  irradiance on the horizontal plane from the values of \emph{daily} and
  \emph{intradaily} global irradiation on the horizontal plane.
  It makes use of the functions \code{\link{calcSol}},
  \code{\link{fCompD}}, \code{\link{fCompI}}, \code{\link{fBTd}}
  and \code{\link{readBD}} (or equivalent).

  Besides, if information about maximum and minimum temperatures values are available it obtains a series of temperature values with \code{\link{fTemp}}.
}

\usage{

calcG0(lat, modeRad='prom', dataRad, prom, mapa, bd,
bdI, sample='hour', keep.night=TRUE, sunGeometry='michalsky',
corr, f) }

\arguments{
  \item{lat}{numeric, latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}

  \item{modeRad}{A character string, describes the kind of source data of the
    global irradiation and ambient temperature.

    It can be \code{modeRad='prom'} for monthly mean calculations. With
    this option, a set of 12 values inside \code{dataRad} must be
    provided, as defined in \code{\link{readG0dm}}.

    \code{modeRad='aguiar'} uses a set of 12 monthly average values
    (provided with \code{dataRad}) and produces a synthetic daily
    irradiation time series following the procedure by Aguiar etal. (see
    reference below).

    \code{modeRad='siar'} is currently disabled. In previous versions,
    with \code{modeRad='siar'} the source data was downloaded from
    \url{www.marm.es/siar} with the information provided in
    \code{dataRad}.
    
    If \code{modeRad='bd'} the information of \emph{daily} irradiation is read from a
    file, a \code{data.frame} defined by \code{dataRad}, a \code{zoo} or a
    \code{Meteo} object. (See \code{\link{readBD}}, \code{\link{df2Meteo}}
    and \code{\link{zoo2Meteo}} for details).

    If \code{modeRad='bdI'} the information of \emph{intradaily} irradiation is read from a
    file, a \code{data.frame} defined by \code{dataRad}, a \code{zoo} or a
    \code{Meteo} object. (See \code{\link{readBDi}},
    \code{\link{dfI2Meteo}} and \code{\link{zoo2Meteo}} for details).
  }

  \item{dataRad}{\itemize{
      
      \item{If \code{modeRad='prom'} or \code{modeRad='aguiar'}, a
	numeric with 12 values or a named list whose components will be
	processed with \code{\link{readG0dm}}.}
      
      %% \item{If \code{modeRad='siar'}, a list as defined by
      %% 	\code{\link{readSIAR}}.}

      \item{If \code{modeRad='bd'} a character (name of the file to be
    read with \code{readBD}), a \code{data.frame} (to be processed with
    \code{df2Meteo}), a \code{zoo} (to be processed with
    \code{zoo2Meteo}), a \code{Meteo} object, or a \code{list} as
    defined by \code{\link{readBD}}, \code{\link{df2Meteo}} or
    \code{\link{zoo2Meteo}}. The resulting object will include a column
    named \code{Ta}, with information about ambient temperature.}

        \item{If \code{modeRad='bdI'} a character (name of the file to
    be read with \code{readBDi}), a \code{data.frame} (to be processed
    with \code{dfI2Meteo}), a \code{zoo} (to be processed with
    \code{zoo2Meteo}), a \code{Meteo} object, or a \code{list} as
    defined by \code{\link{readBDi}}, \code{\link{dfI2Meteo}} or
    \code{\link{zoo2Meteo}}. The resulting object will include a column
    named \code{Ta}, with information about ambient temperature.}
  }}

  \item{prom, mapa, bd, bdI}{Deprecated. \code{dataRad} should be used
    instead.}

  % \item{prom}{A list as defined by \code{\link{readG0dm}}. This list
  %   is only required when \code{modeRad='prom'}.}

  % \item{mapa}{A list as defined by \code{\link{readMAPA}}. This
  %   list is only required  when \code{modeRad='mapa'}.}

  % \item{bd}{A \code{list} as defined by \code{\link{readBD}},
  %   \code{\link{df2Meteo}} or \code{\link{zoo2Meteo}}, or a
  %   \code{Meteo} object. It is only required when
  %   \code{modeRad='bd'}.
    
  %   The resulting object will include a column named \code{Ta},
  %   with information about ambient temperature.}

  % \item{bdI}{A \code{list} as defined by \code{\link{readBDi}},
  %   \code{\link{dfI2Meteo}} or \code{\link{zoo2Meteo}}, or a
  %   \code{Meteo} object. It is only required when \code{modeRad='bdI'}.

  %   The resulting object will include a column named \code{Ta},
  %   with information about ambient temperature.}

  \item{sample}{\code{character}, containing one of ‘"sec"’, ‘"min"’,
    ‘"hour"’.  This can optionally be preceded by a (positive or
    negative) integer and a space, or followed by ‘"s"’ (used by
    \code{\link{seq.POSIXt}}). It is not used when \code{modeRad="bdI"}.}

  \item{keep.night}{\code{logical}. When it is TRUE (default) the
    time series includes the night.}

  \item{sunGeometry}{\code{character}, method for the sun geometry
    calculations. See \code{\link{calcSol}}, \code{\link{fSolD}} and \code{\link{fSolI}}.}

  \item{corr}{A character, the correlation between the the fraction of diffuse irradiation and the
    clearness index to be used.

    With this version several options are available, as described in
    \code{\link{corrFdKt}}. For example, the \code{\link{FdKtPage}} is
    selected with \code{corr='Page'} while the \code{\link{FdKtCPR}} with
    \code{corr='CPR'}.

    If \code{corr='user'} the use of a correlation
    defined by a function \code{f} is possible.

    If \code{corr='none'} the object defined by \code{dataRad} should include
    information about global, diffuse and direct daily irradiation with columns named 
    \code{G0d}, \code{D0d} and \code{B0d}, respectively
    (or \code{G0}, \code{D0} and \code{B0} if \code{modeRad='bdI'}).
    If \code{corr} is missing, then it is internally set to \code{CPR}
    when \code{modeRad='siar'} or \code{modeRad='bd'}, to \code{Page}
    when \code{modeRad='prom'} and to \code{BRL} when \code{modeRad='bdI'}.}

  \item{f}{A function defininig a correlation between the fraction of
    diffuse irradiation and the clearness index. It is only neccessary when \code{corr='user'}}
}

\value{A \code{G0} object.}

\references{\itemize{
    
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
    (\url{http://procomun.wordpress.com/documentos/libroesf/})

    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
    Systems with R", Journal of Statistical Software, 50(9), 1-32,
    \url{http://www.jstatsoft.org/v50/i09/}

    \item Aguiar, Collares-Pereira and Conde, "Simple procedure for
  generating sequences of daily radiation values using a library of
  Markov transition matrices", Solar Energy, Volume 40, Issue 3, 1988, Pages 269–279}
  }

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\link{calcSol}},
  \code{\link{fCompD}},
  \code{\link{fCompI}},
  \code{\link{readG0dm}},
  \code{\link{readBD}},
  \code{\link{readBDi}},
  \code{\link{readSIAR}},
  \code{\link{corrFdKt}}.
}
\examples{
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2,
  15.2)

g0 <- calcG0(lat=37.2, modeRad='prom', dataRad=list(G0dm=G0dm, Ta=Ta))
print(g0)
xyplot(g0)

## Aguiar et al.

g0 <- calcG0(lat=37.2, modeRad='aguiar', dataRad=G0dm)
print(g0)
xyplot(g0)

##Now the G0I component of g0 is used as
##the bdI argument to calcG0 in order to
##test the intradaily correlations of fd-kt

BDi=as.zooI(g0)
BDi$Ta=25 ##Information about temperature must be contained in BDi

g02 <- calcG0(lat=37.2,
            modeRad='bdI',
            dataRad=list(lat=37.2, file=BDi),
            corr='none')

print(g02)

g03 <- calcG0(lat=37.2,
            modeRad='bdI',
            dataRad=list(lat=37.2, file=BDi),
            corr='BRL')
print(g03)

xyplot(fd~kt, data=g03, pch=19, alpha=0.3)

\dontrun{
##NREL-MIDC
##La Ola, Lanai
##Latitude: 20.76685o North
##Longitude: 156.92291o West
##Elevation: 381 meters AMSL
##Time Zone: -10.0

NRELurl <- 'http://goo.gl/fFEBN'

dat <- read.table(NRELurl, header=TRUE, sep=',')
names(dat) <- c('date', 'hour', 'G0', 'B', 'D0', 'Ta')

##B is direct normal. We need direct horizontal.
dat$B0 <- dat$G0-dat$D0

##http://www.nrel.gov/midc/la_ola_lanai/instruments.html:
##The datalogger program runs using Greenwich Mean Time (GMT),
##data is converted to Hawaiin Standard Time (HST) after data collection
idxLocal <- with(dat, as.POSIXct(paste(date, hour), format='\%m/\%d/\%Y \%H:\%M', tz='HST'))
idx <- local2Solar(idxLocal, lon=-156.9339)

NRELMeteo <- zoo(dat[,c('G0', 'D0', 'B0', 'Ta')], idx)

lat=20.77

g0 <- calcG0(lat=lat, modeRad='bdI', dataRad=NRELMeteo, corr='none')
xyplot(g0)
xyplot(as.zooI(g0), superpose=TRUE)

g02 <- calcG0(lat=lat, modeRad='bdI', dataRad=NRELMeteo, corr='BRL')
xyplot(g02)
xyplot(as.zooI(g02), superpose=TRUE)
xyplot(fd~kt, data=g02, pch=19, cex=0.5, alpha=0.5)

g03 <- calcG0(lat=lat, modeRad='bdI', dataRad=NRELMeteo, corr='CLIMEDh')
xyplot(g03)
xyplot(as.zooI(g03), superpose=TRUE)
xyplot(fd~kt, data=g03, pch=19, cex=0.5, alpha=0.5)
}
}

\keyword{utilities}
\keyword{constructors}

