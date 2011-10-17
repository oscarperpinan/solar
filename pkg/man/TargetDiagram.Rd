\name{C_TargetDiagram}
\Rdversion{1.0}
\encoding{UTF-8}
\alias{TargetDiagram}
\alias{analyzeData}

\title{Statistical analysis of a PV system with the Target Diagram}

\description{In a PV plant, the individual systems are theoretically
  identical and their performance along the time should be the same. Due to their
  practical differences --power tolerance, dispersion losses, dust--,
  the individual performance of each system will deviate from the average
  behaviour. However, when a system is performing correctly, these deviations
  are constrained inside a range and should not be regarded as sign
  of malfunctioning.

  If these common deviations are assumed as a random process, a statistical
  analysis of the performance of the whole set of systems can identify
  a faulty system as the one that departs significantly from the mean
  behaviour.

  These functions compare the daily performance of each system with a reference (for
  example, the median of the whole set) during  a time period of N days
  preceding the current day. They calculate a set of statistics of the performance of the
  PV plant as a whole, and another set of the comparison with the
  reference. This statistical analysis can be summarised with a graphical tool
  named "Target Diagram", which plots together the root mean square
  difference, the average difference and the standard deviation of the
  difference. Besides, this diagram includes the sign of the difference of
  the standard deviations of the system and the reference.
}
\usage{
analyzeData(x, ref=NULL)
TargetDiagram(x, end, ndays, ref=NULL, color=NULL, cex=0.8,...)
}
\arguments{

  \item{x}{A \code{zoo} object with several columns. This object
    represent the time evolution of a set of units (e.g. PV generators of
    a large system) which are supposed to show a similar behaviour.}

  \item{ref}{A \code{zoo} object to be used as the reference unit. If
    \code{ref=NULL} (default), the reference is the median of the set.}

  \item{end}{A \code{Date} or \code{POSIXct} object (same class as the
    index of \code{x}). It defines the last
    day of the window to be included in the analysis.}

  \item{ndays}{A numeric vector, where each element is the number of days to be included in each
    analysis.}

  \item{color}{If \code{color=NULL} (default) the \code{plot} is black
    and white and each analysis is contained in different areas.}

  \item{cex}{Size of the labels.}

  \item{...}{Arguments to be read by \code{xyplot}.}
}

\value{The result of \code{TargetDiagram} is a \code{list} with two
  components:
  \describe{
    \item{plot: }{a \code{trellis} object with the plot.}
    \item{stat: }{a \code{zoo} object with \code{err} component of the result of
      \code{\link{analyzeData}}.}
  }
  The result of \code{analyzeData} is a \code{list} with two
  components:
  \describe{
    \item{stat: }{a \code{zoo} object with the time evolution of several
      statistics (mean, median, standard deviation, median absolute
      deviation and interquantile range) of the set as a whole.}

    \item{err: }{a \code{data.frame} with the same number of rows as the
      number of columns of the \code{x} object. It contains several columns
      with the statistics of the diference between each unit and the
      reference (see the references for details.)}
  }
}
\references{
  \itemize{
    \item Jolliff, J.; Kindle, J. C.; Shulman, I.; Penta, B.; Friedrichs,
      M. A. M.; Helber, R. & Arnone, R. A. Summary diagrams for coupled
      hydrodynamic-ecosystem model skill assessment Journal of Marine
      Systems, 2009, 76, 64-82.

    \item O. Perpiñán, Statistical analysis of the performance and
      simulation of a two-axis tracking PV system, Solar Energy,
      83:11(2074–2085),
      2009.\url{http://oa.upm.es/1843/1/PERPINAN_ART2009_01.pdf}

    \item Taylor, K. E. Summarizing multiple aspects of model performance
      in a single diagram, Program for Climate Model Diagnosis and
      Intercomparison, 2000, \url{http://www-pcmdi.llnl.gov/publications/pdf/55.pdf}

  }
}

\author{Oscar Perpiñán Lamigueiro.}


\examples{
library(lattice)
library(latticeExtra)

data(prodEx)

prodStat<-analyzeData(prodEx)
xyplot(prodStat$stat)
dif<-prodEx-prodStat$stat$Median;
##xyplot(dif[,1:18], superpose=TRUE, auto.key=FALSE)
day=as.Date('2008-8-29')
horizonplot(window(dif, start=day-90, end=day),
            origin=0, layout=c(1, 22), colorkey=TRUE, colorkey.digits=1,
            scales=list(y=list(relation="same")))

###With a external reference
ref1=apply(prodEx, 1, median, na.rm=1)
prodStat1=analyzeData(prodEx, ref=ref1)
identical(prodStat, prodStat1)

###Target Diagram

ndays=c(5, 10, 15, 20)

#Color
library(RColorBrewer)
palette=brewer.pal(n=length(ndays), name='Set1')

TDColor<-TargetDiagram(prodEx, end=day, ndays=ndays,
color=palette)

#B&W
TDbw<-TargetDiagram(prodEx, end=day, ndays=ndays)
}
\keyword{utilities}

