\name{solaR-package}
\alias{solaR-package}
\alias{solaR}
\docType{package}
\title{
Solar Radiation and Photovoltaic Systems with R
}
\description{
The \code{solaR} package allows for reproducible research both for
photovoltaics (PV) systems performance and solar radiation. It
includes a set of classes, methods and functions to calculate the sun
geometry and the solar radiation incident on a photovoltaic generator
and to simulate the performance of several applications of the
photovoltaic energy. This package performs the whole calculation
procedure from both daily and intradaily global horizontal irradiation
to the final productivity of grid-connected PV systems and water
pumping PV systems.
}

\details{

\code{solaR} is designed using a set of \code{S4} classes whose core
is a group of slots with multivariate time series. The classes share a
variety of methods to access the information and several visualization
methods. In addition, the package provides a tool for the visual
statistical analysis of the performance of a large PV plant composed of
several systems.

Although \code{solaR} is primarily designed for time series associated
to a location defined by its latitude/longitude values and the
temperature and irradiation conditions, it can be easily combined with
spatial packages for space-time analysis.

The best place to learn how to use the package is the companion paper
published by the Journal of Statistical Software:
http://www.jstatsoft.org/v50/i09/

\bold{Please note that this package needs to set the timezone to
  \code{UTC}}.

You can check it after loading \code{solaR} with:

\code{Sys.getenv('TZ')}

If you need to change it, use:

\code{Sys.setenv(TZ = 'YourTimeZone')}


Index of functions and classes:
\preformatted{
G0-class                Class "G0": irradiation and irradiance on the
                        horizontal plane.
Gef-class               Class "Gef": irradiation and irradiance on the
                        generator plane.
HQCurve                 H-Q curves of a centrifugal pump
Meteo-class             Class "Meteo"
NmgPVPS                 Nomogram of a photovoltaic pumping system
ProdGCPV-class          Class "ProdGCPV": performance of a grid
                        connected PV system.
ProdPVPS-class          Class "ProdPVPS": performance of a PV pumping
                        system.
Shade-class             Class "Shade": shadows in a PV system.
Sol-class               Class "Sol": Apparent movement of the Sun from
                        the Earth
aguiar                  Markov Transition Matrices for the Aguiar etal.
                        procedure
as.data.frameD          Methods for Function as.data.frameD
as.data.frameI          Methods for Function as.data.frameI
as.data.frameM          Methods for Function as.data.frameM
as.data.frameY          Methods for Function as.data.frameY
as.zooD                 Methods for Function as.zooD
as.zooI-methods         Methods for Function as.zooI
as.zooM                 Methods for Function as.zooM
as.zooY                 Methods for Function as.zooY
calcG0                  Irradiation and irradiance on the horizontal
                        plane.
calcGef                 Irradiation and irradiance on the generator
                        plane.
calcShd                 Shadows on PV systems.
calcSol                 Apparent movement of the Sun from the Earth
compare                 Compare G0, Gef and ProdGCPV objects
compareLosses           Losses of a GCPV system
corrFdKt                Correlations between the fraction of diffuse
                        irradiation and the clearness index.
d2r                     Conversion between angle units.
diff2Hours              Small utilities for difftime objects.
fBTd                    Daily time base
fCompD                  Components of daily global solar irradiation on
                        a horizontal surface
fCompI                  Calculation of solar irradiance on a horizontal
                        surface
fInclin                 Solar irradiance on an inclined surface
fProd                   Performance of a PV system
fPump                   Performance of a centrifugal pump
fSolD                   Daily apparent movement of the Sun from the
                        Earth
fSolI                   Instantaneous apparent movement of the Sun from
                        the Earth
fSombra                 Shadows on PV systems
fTemp                   Intradaily evolution of ambient temperature
fTheta                  Angle of incidence of solar irradiation on a
                        inclined surface
getData                 Methods for function getData
getG0                   Methods for function getG0
getLat                  Methods for Function getLat
helios                  Daily irradiation and ambient temperature from
                        the Helios-IES database
hour                    Utilities for time indexes.
indexD                  Methods for Function indexD
indexI                  Methods for Function indexI
indexRep-methods        Methods for Function indexRep
levelplot-methods       Methods for function levelplot.
local2Solar             Local time, mean solar time and UTC time zone.
mergesolaR              Merge solaR objects
optimShd                Shadows calculation for a set of distances
                        between elements of a PV grid connected plant.
prodEx                  Productivity of a set of PV systems of a PV
                        plant.
prodGCPV                Performance of a grid connected PV system.
prodPVPS                Performance of a PV pumping system
pumpCoef                Coefficients of centrifugal pumps.
readBD                  Daily or intradaily values of global horizontal
                        irradiation and ambient temperature from a
                        local file or a data.frame.
readG0dm                Monthly mean values of global horizontal
                        irradiation.
shadeplot               Methods for Function shadeplot
solaR.theme             solaR theme
window                  Methods for extracting a time window
writeSolar              Exporter of solaR results
xyplot-methods          Methods for function xyplot in Package 'solaR'
}
}

\author{
Oscar Perpiñán Lamigueiro

Maintainer: Oscar Perpiñán Lamigueiro <oscar.perpinan@gmail.com>
}
