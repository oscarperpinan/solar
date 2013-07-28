\name{C_fBTd}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fBTd}

\title{Daily time base}
\description{Construction of a daily time base for solar irradiation calculation}

\usage{
fBTd(mode = "prom",
    year=as.POSIXlt(Sys.Date())$year+1900,
    start=paste('01-01-',year,sep=''),
    end=paste('31-12-',year,sep=''),
                   format='\%d-\%m-\%Y')
}

\arguments{
  \item{mode}{character, controls the type of time base to be
    created. With  \code{mode='serie'} the result is a daily time series from \code{start} to \code{end}. With \code{mode='prom'} only twelve days, one for each month, are included. During these 'average days' the declination angle is equal to the monthly mean of this angle.}
  \item{year}{which year is to be used for the time base when \code{mode='prom'}. Its default value is the current year.}
  \item{start}{first day of the time base for \code{mode='serie'}. Its default value is the first of January of the current year. }
  \item{end}{last day of the time base for \code{mode='serie'}. Its
    default value is the last day of December of the current year.}

  \item{format}{format of \code{start} and \code{end}.}
}
\details{This function is commonly used inside \code{fSolD}.}
\value{This function returns a \code{POSIXct} object.}

\author{Oscar Perpiñán Lamigueiro}


\references{\itemize{
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
    (\url{http://procomun.wordpress.com/documentos/libroesf/})
    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \url{http://www.jstatsoft.org/v50/i09/}
  }}

\seealso{
  \code{\link{fSolD}},
  \code{\link{as.POSIXct}},
  \code{\link{seq.POSIXt}}.
}

\examples{
#Average days
fBTd(mode='prom')

#The day #100 of the year 2008
BTd=fBTd(mode='serie', year=2008)
BTd[100]
}

\keyword{utilities}
