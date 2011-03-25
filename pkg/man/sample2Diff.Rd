\name{C_sample2Diff}
\alias{diff2Hours}
\alias{char2diff}
\alias{sample2Hours}
\alias{P2E}
\Rdversion{1.0}
\encoding{UTF-8}

\title{Small utilities for difftime objects.}
\description{
  \code{diff2Hours} converts a \code{difftime} object into its numeric
  value with \code{units='hours'}.

  \code{char2diff} converts a character description into a
  \code{difftime} object, following the code of
  \code{\link{seq.POSIXt}}.

  \code{sample2Hours} calculates the sampling time in hours described by a character
  or a \code{difftime}.

  \code{P2E} (power to energy) sums a series of power values (for
  example, irradiance) to obtain energy aggregation (for example,
  irradiation) using \code{sample2Hours} for the units conversion.
}
\usage{
diff2Hours(by)
char2diff(by)
sample2Hours(by)
P2E(x, by)
}
\arguments{

  \item{by}{A character for \code{char2diff}, \code{sample2Hours} and \code{P2E}, or
    a \code{difftime} for \code{diff2Hours}, \code{sample2Hours} and
    \code{P2E}.}
  \item{x}{A numeric vector.}
}

\value{A numeric value or a \code{difftime} object.}

\seealso{
  \code{\linkS4class{Sol}}
}

\examples{
char2diff('min')
char2diff('2 s')

sample2Hours('s')
sample2Hours('30 m')

by1<-char2diff('10 min')
sample2Hours(by1)
}
\author{Oscar Perpiñán Lamigueiro}
\keyword{utilities}
