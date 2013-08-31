solar
=====

The `solaR` package allows for reproducible research both for
photovoltaics (PV) systems performance and solar radiation. It
includes a set of classes, methods and functions to calculate the sun
geometry and the solar radiation incident on a photovoltaic generator
and to simulate the performance of several applications of the
photovoltaic energy. This package performs the whole calculation
procedure from both daily and intradaily global horizontal irradiation
to the final productivity of grid-connected PV systems and water
pumping PV systems.

It is designed using a set of `S4` classes whose core is a group of
slots with multivariate time series. The classes share a variety of
methods to access the information and several visualization
methods. In addition, the package provides a tool for the visual
statistical analysis of the performance of a large PV plant composed
of several systems.

Although `solaR` is primarily designed for time series associated to a
location defined by its latitude/longitude values and the temperature
and irradiation conditions, it can be easily combined with spatial
packages for space-time analysis.

# Documentation #

The best place to learn how to use the package is the companion paper
published by the Journal of Statistical Software:
http://www.jstatsoft.org/v50/i09/

[This book](http://procomun.wordpress.com/documentos/libroesf/) (in
Spanish) contains detailed information about solar radiation and
photovoltaic systems. In
[my articles](http://oscarperpinan.github.io/) I frequently use
`solaR`. Besides, I publish news and examples about `solaR` at
[my blog](http://procomun.wordpress.com/).
