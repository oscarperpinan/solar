solaR
=====
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.8709.png)](http://dx.doi.org/10.5281/zenodo.8709)

[![Build Status](https://travis-ci.org/oscarperpinan/solar.svg?branch=master)](https://travis-ci.org/oscarperpinan/solar)

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
[CRAN](http://cran.r-project.org/package%3DsolaR). The development
version is available at
[GitHub](http://github.com/oscarperpinan/solar/).

Install the stable version with:

    install.packages('solaR')

To install the development version you need the `devtools` package:

    install.packages('devtools')
	devtools::install_github('solar', 'oscarperpinan')



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

# Citation #

If you use `solaR`, please cite it in any publication reporting
results obtained with this software:

    Oscar Perpiñán (2012). solaR: Solar Radiation and Photovoltaic
    Systems with R, Journal of Statistical Software, 50(9), 1-32. URL
		http://www.jstatsoft.org/v50/i09/.

A BibTeX entry for LaTeX users is:

    @Article{,
        title = {{solaR}: Solar Radiation and Photovoltaic Systems with {R}},
        author = {Oscar Perpi{\~n}{\'a}n},
        journal = {Journal of Statistical Software},
        year = {2012},
        volume = {50},
        number = {9},
        pages = {1--32},
        url = {http://www.jstatsoft.org/v50/i09/},
      }

