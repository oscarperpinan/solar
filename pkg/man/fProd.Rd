\name{C_fProd}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fProd}
\title{
  Performance of a PV system
}
\description{
  Simulate the behaviour of a grid connected PV system under different
  conditions of irradiance and temperature. This function is used by the
  \code{\link{prodGCPV}} function.
}
\usage{fProd(inclin, module, generator, inverter, effSys)}

\arguments{
  \item{inclin}{A \code{\linkS4class{Gef}} object, a \code{zoo} object or a \code{data.frame}. In
    case of being \code{zoo} or \code{data.frame} it must include a component named \code{Gef} (effective irradiance, W/m²) and another named \code{Ta} (ambient temperature, \eqn{^{\circ}{\rm C}}).}

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
}
\value{If \code{inclin} is \code{zoo} or \code{Gef} object, the result
  is a \code{zoo} object with these components (if \code{inclin}
  is a \code{data.frame} the result is also a \code{data.frame} with these same components):
  \item{Tc}{cell temperature, \eqn{^{\circ}{\rm C}}.}
  \item{Voc, Isc, Vmpp, Impp}{open circuit voltage, short circuit current, MPP voltage and current, respectively, in the conditions of irradiance and temperature provided by \code{Inclin}}
  \item{Vdc, Idc}{voltage and current at the input of the inverter. If no voltage limitation occurs (according to the values of \code{inverter$Vmax} and \code{inverter$Vmin}), their values are identical to \code{Vmpp} and \code{Impp}. If the limit values are reached a warning is produced}
  \item{Pdc}{power at the input of the inverter, W}
  \item{Pac}{power at the output of the inverter, W}
  \item{EffI}{efficiency of the inverter}


}

\references{\itemize{
    \item Jantsch, M., Schmidt, H. y Schmid, J.: Results on the
    concerted action on power conditioning and control. 11th European
    photovoltaic Solar Energy Conference, 1992.
    \item Baumgartner, F. P., Schmidt, H., Burger, B., Bründlinger,
	R., Haeberlin, H. and Zehner, M.: Status and Relevance of the DC
    Voltage Dependency of the Inverter Efficiency. 22nd European
    Photovoltaic Solar Energy Conference, 2007.
    \item Alonso Garcia, M. C.: Caracterización y modelado de asociaciones de dispositivos fotovoltaicos. PhD Thesis, CIEMAT, 2005.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}}
\author{
  Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{fInclin}},
  \code{\link{prodGCPV}},
  \code{\link{fTemp}}.
}
\examples{
inclin=data.frame(Gef=c(200,400,600,800,1000),Ta=25)

#using default values
fProd(inclin)

#Using a matrix for Ki (voltage dependence)
inv1=list(Ki=rbind(c(-0.00019917, 7.513e-06, -5.4183e-09),
c(0.00806, -4.161e-06, 2.859e-08),
c(0.02118, 3.4002e-05, -4.8967e-08)))

fProd(inclin, inverter=inv1)

#Voltage limits of the inverter
inclin=data.frame(Gef=800,Ta=30)
gen1 = list(Nms = 10, Nmp = 11)

prod=fProd(inclin,generator=gen1)
print(prod)

with(prod,Vdc*Idc/(Vmpp*Impp))
}

\keyword{utilities}

