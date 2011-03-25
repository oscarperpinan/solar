\name{A8_readBD}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{readBD}
\alias{readBDi}
\alias{df2Meteo}
\alias{dfI2Meteo}
\alias{zoo2Meteo}

\title{Daily or intradaily values of global horizontal irradiation and ambient
  temperature from a local file or a data.frame.}
\description{Constructor for the class \code{Meteo} with values of
  \emph{daily} or \emph{intradaily} values of global horizontal irradiation and ambient temperature
  from a local file or a data.frame.}
\usage{
readBD(file,  lat,
       format='\%d/\%m/\%Y',
       header=TRUE, fill=TRUE, dec='.', sep=';',
       dates.col='date',source=file)

readBDi(file,  lat,
       format='\%d/\%m/\%Y \%H:\%M:\%S',
       header=TRUE, fill=TRUE, dec='.', sep=';',
       time.col='time',
       source=file)

df2Meteo(file,  lat,
         format='\%d/\%m/\%Y',
         dates.col='date',
         source='')

dfI2Meteo(file,  lat,
         format='\%d/\%m/\%Y \%H:\%M:\%S',
         time.col='time',
         source='')

zoo2Meteo(file, lat, source='')
}
\arguments{

  \item{file}{The name of the file (\code{readBD} and \code{readBDi}) or \code{data.frame}
    (\code{df2Meteo} \code{dfI2Meteo}) which the data are to be read from.
    It must contain a column \code{G0} with
    \emph{daily} (\code{readBD} and \code{df2Meteo}) or
    \emph{intradaily} (\code{readBDi} and \code{dfI2Meteo})  values of
    global horizontal irradiation (Wh/m²). It should also include a column
    named \code{Ta} with values of ambient temperature.

    If the \code{Meteo} object is to be used with \code{\link{calcG0}}
    (or \code{fCompD}, \code{fCompI}) and the option
    \code{corr='none'}, the file/data.frame \bold{must} include three
    columns named \code{G0}, \code{B0} and \code{D0} with values of
    global, direct and diffuse irradiation on the horizontal plane.

    Only for daily data: if the ambient temperature is not available,
    the file should include two columns named \code{TempMax} and \code{TempMin} with daily values
    of maximum and minimum ambient temperature, respectively (see
    \code{\link{fTemp}} for details).}

  \item{header, fill, dec, sep}{See \code{\link{read.table}}}

  \item{format}{character string with the format of the dates or time
    index.
    (Default for daily time bases:\code{\%d/\%m/\%Y}).
    (Default for intradaily time bases: \code{\%d/\%m/\%Y  \%H:\%M:\%S})}

  \item{lat}{numeric, latitude (degrees) of the location.}

  \item{dates.col}{character string with the name of the column wich
    contains the dates of the time series.}

  \item{time.col}{character string with the name of the column wich
    contains the time index of the series.}

  \item{source}{character string with information about the source of
    the values. (Default: the name of the file).}
}

\value{A \code{Meteo} object.}


\author{
  Oscar Perpiñán Lamigueiro.
}
\seealso{
  \code{\link{read.table}},
  \code{\link{readMAPA}},
  \code{\link{readG0dm}}.
}
\examples{
data(helios)
names(helios)=c('date', 'G0', 'TempMax', 'TempMin')

bd=df2Meteo(helios, dates.col='date', lat=41, source='helios-IES', format='\%Y/\%m/\%d')

summary(getData(bd))

xyplot(bd)
}
\keyword{utilities}
\keyword{constructors}
