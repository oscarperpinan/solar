\name{C_utils-time}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{hour}
\alias{minute}
\alias{second}
\alias{hms}
\alias{doy}
\alias{dom}
\alias{month}
\alias{year}
\alias{DoY}
\alias{DoM}
\alias{Month}
\alias{Year}
\alias{dst}
\alias{truncDay}

\title{Utilities for time indexes.}

\description{Several small functions to extract information from \code{POSIXct}
  indexes.
}

\usage{
hour(x)
minute(x)
second(x)
hms(x)
doy(x)
dom(x)
month(x)
year(x)
DoY(x)
DoM(x)
Month(x)
Year(x)
dst(x)
truncDay(x)
}
\arguments{
  \item{x}{A \code{POSIXct} vector.}
}
\value{The functions \code{year}, \code{month},
  \code{day}, \code{hour}, \code{minute}, \code{second} give the numeric
  value corresponding to their names.

  \code{doy} and \code{dom} provide the (numeric) day of year and day of month,
  respectively.

  \code{Month}, \code{Year}, \code{DoY} and \code{DoM} give
  the same result as \code{month}, \code{year}, \code{doy} and \code{dom}
  in a character string format.

  \code{hms} gives the numeric value
  \code{hour(x)+minute(x)/60+second(x)/3600}

  \code{dst} is +1 if the Daylight Savings Time flag is in force,
  zero if not, -1 if unknown (\code{\link{DateTimeClasses}}).

  \code{truncDay} truncates the \code{POSIXct} object towards the day.

}


\author{
  Oscar Perpiñán Lamigueiro.
}
\seealso{\code{as.POSIXct}
}

\keyword{utilities}

