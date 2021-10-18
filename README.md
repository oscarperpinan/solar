solaR
=====
[![CRAN](https://www.r-pkg.org/badges/version/solaR)](https://www.r-pkg.org/pkg/solaR)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/solaR)](https://www.r-pkg.org/pkg/solaR)
[![Build Status](https://github.com/oscarperpinan/solar/workflows/R-CMD-check/badge.svg)](https://github.com/oscarperpinan/solar/actions)

[![DOI](https://upload.wikimedia.org/wikipedia/commons/1/11/DOI_logo.svg)](https://doi.org/10.18637/jss.v050.i09)


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

# Software #

The stable version of solaR is hosted at
[CRAN](https://cran.r-project.org/package=solaR). The development
version is available at
[GitHub](https://github.com/oscarperpinan/solar/).

Install the stable version with:

    install.packages('solaR')

You can install the development version with the [`remotes`](https://github.com/r-lib/remotes) package:

	remotes::install_github('oscarperpinan/solar')

or with [`devtools`](https://github.com/r-lib/devtools):

    devtools::install_github('oscarperpinan/solar')

# Documentation #

The best place to learn how to use the package is the companion paper
published by the Journal of Statistical Software:

Perpiñán Lamigueiro, O. (2012). solaR: Solar Radiation and
Photovoltaic Systems with R. Journal of Statistical Software, 50(9),
1–32. https://doi.org/10.18637/jss.v050.i09

[This book](https://oscarperpinan.github.io/esf/) (in
Spanish) contains detailed information about solar radiation and
photovoltaic systems. In
[my articles](https://oscarperpinan.github.io/) I frequently use
`solaR`. 

# Citation #

If you use `solaR`, please cite it in any publication reporting
results obtained with this software:

Perpiñán Lamigueiro, O. (2012). solaR: Solar Radiation and
Photovoltaic Systems with R. Journal of Statistical Software, 50(9),
1–32. https://doi.org/10.18637/jss.v050.i09

A BibTeX entry for LaTeX users is:

    @Article{,
        title = {{solaR}: Solar Radiation and Photovoltaic Systems with {R}},
        author = {Oscar Perpi{\~n}{\'a}n},
        journal = {Journal of Statistical Software},
        year = {2012},
        volume = {50},
        number = {9},
        pages = {1--32},
		doi = {10.18637/jss.v050.i09}
      }

