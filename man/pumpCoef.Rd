\name{E_pumpCoef}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{pumpCoef}
\docType{data}
\title{Coefficients of centrifugal pumps.}
\description{Coefficients of centrifugal pumps}

\usage{data(pumpCoef)}

\format{
  A data frame with 13 columns:
  \describe{
    \item{Qn}{rated flux}
    \item{stages}{number of stages}
    \item{Qmax}{maximum flux}
    \item{Pmn}{rated motor power}
    \item{a, b, c}{Coefficients of the equation \eqn{H=a \cdot f^2+b \cdot f \cdot Q+c \cdot Q^2}.}
    \item{g, h, i}{Coefficients of the efficiency curve of the motor (50 Hz): \eqn{\eta_{m}=g \cdot (\%P_{mn})^2+h \cdot (\%P{mn})+i}.}
    \item{j, k, l}{Coefficients of the efficiency curve of the pump (50 Hz): \eqn{\eta_{b}=j \cdot Q^2+k \cdot Q+l}.}

  }
}
\details{With this version only pumps from the manufacturer Grundfos are included.}

\source{\url{http://net.grundfos.com/Appl/WebCAPS/custom?}}

\references{\itemize{
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2012.
    (\url{http://procomun.wordpress.com/documentos/libroesf/})
  \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \url{http://www.jstatsoft.org/v50/i09/}
}}

\keyword{datasets}
