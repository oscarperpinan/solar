\name{E_helios}
\alias{helios}
\docType{data}
\title{Daily irradiation and ambient temperature from the Helios-IES database}
\description{A year of irradiation, maximum and minimum ambient
  temperature from the HELIOS-IES database.}

\usage{data(helios)}
\format{
  A data frame with 355 observations on the following 4 variables:
  \describe{
    \item{\code{yyyy.mm.dd}}{a factor: year, month and day.}
    \item{\code{G.0.}}{a numeric vector, daily global horizontal irradiation.}
    \item{\code{TambMax}}{a numeric vector, maximum ambient temperature.}
    \item{\code{TambMin}}{a numeric vector, minimum ambient temperature.}
  }
}

\source{http://helios.ies-def.upm.es/consulta.aspx}


\keyword{datasets}
