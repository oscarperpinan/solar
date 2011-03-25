\name{A3_calcGef}
\Rdversion{1.3}
\encoding{UTF-8}
\alias{calcGef}

\title{Irradiation and irradiance on the generator plane.}

\description{
  This function obtains the global, diffuse and direct irradiation and
  irradiance on the generator plane from the values of \emph{daily} or \emph{intradaily} global
  irradiation on the horizontal plane. It makes use of the functions
  \code{\link{calcG0}}, \code{\link{fTheta}},
  \code{\link{fInclin}}. Besides, it can calculate the shadows effect with
  the \code{\link{calcShd}} function.
}
\usage{
calcGef(lat,
        modeTrk='fixed',
        modeRad='prom',
        prev,
        prom=list(),
        mapa=list(),
        bd=list(),
        bdI=list(),
        sample='hour',
        keep.night=TRUE,
        corr, f,
        betaLim=90, beta=abs(lat)-10, alfa=0,
        iS=2, alb=0.2, horizBright=FALSE,
        modeShd='',
        struct=list(),
        distances=data.frame())
}
\arguments{
  \item{lat}{numeric, latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}

  \item{modeTrk}{character, to be chosen from \code{'fixed'}, \code{'two'} or \code{'horiz'}. When \code{modeTrk='fixed'} the surface is fixed (inclination and azimuth angles are constant). The performance of a two-axis tracker is calculated with \code{modeTrk='two'}, and \code{modeTrk='horiz'} is the option for an horizontal N-S tracker. Its default value is \code{modeTrk='fixed'}}

  \item{modeRad, prom, mapa, bd, bdI}{Information about the source data of the
    global irradiation. See \code{\link{calcG0}} for details.}

  %    \item{modeRad}{A character string, describes the kind of source data of the
  % global irradiation. It can be \code{modeRad='prom'} for monthly mean
  % calculations. With this option, a set of 12 values inside \code{prom} must
  % be provided. When \code{modeRad='mapa'} the source data is downloaded
  % from \url{www.mapa.es/siar} with the information provided in
  % \code{mapa}. If \code{modeRad='bd'} the information is read from a
  % file defined by \code{bd}.}

  \item{prev}{A \code{G0} object (or something that can be coerced to a
    \code{G0} object). It is used when \code{modeRad='prev'}.}

  % \item{prom}{A list as defined by \code{\link{readG0dm}}. This list
  %   is only required when \code{modoRad='prom'}.}

  %   \item{mapa}{A list as defined by \code{\link{readMAPA}}. This
  %     list is only required  when \code{modoRad='mapa'}.}

  %    \item{bd}{\code{list} as defined by \code{\link{readBD}}. This list
  %    is only required when \code{modoRad='bd'}.}
  \item{sample, keep.night}{See \code{\link{calcSol}} for details.}
  % \item{sample}{\code{character}, containing one of ‘"sec"’, ‘"min"’,
  %       ‘"hour"’.  This can optionally be preceded by a (positive or
  %       negative) integer and a space, or followed by ‘"s"’ (used by
  %       \code{\link{seq.POSIXt}})}

  %     \item{keep.night}{\code{logical} When it is TRUE (default) the
  %       time series includes the night.}

  \item{corr, f}{See \code{\link{calcG0}} for details.}
  
  \item{beta}{numeric, inclination angle of the surface
    (degrees). It is only needed when \code{modeTrk='fixed'}.}

  \item{betaLim}{numeric, maximum value of the inclination angle
    for a tracking surface. Its default value is 90 (no
    limitation))}

  \item{alfa}{numeric, azimuth angle of the surface (degrees). It is
    positive to the West. It is only needed when
    \code{modeTrk='fixed'}. Its default value is \code{alfa=0}}

  \item{iS}{integer, degree of dirtiness. Its value must be included in
    the set (1,2,3,4). \code{iS=1} corresponds to a clean surface while
    \code{iS=4} is the selection for a dirty surface. Its default value is
    2.}

  \item{alb}{numeric, albedo reflection coefficient. Its default value is 0.2}

  \item{modeShd, struct, distances}{See \code{\link{calcShd}} for
    details.}

  \item{horizBright}{logical, with default value FALSE. If TRUE, the
    horizon brightness correction proposed by Reind et al. is used.}


  %   \item{modeShd}{character, defines the type of shadow calculation. In
  %     this version of the package the effect of the shadow is calculated
  %     as a proportional reduction of the circumsolar diffuse and direct
  %     irradiances. This type of approach is selected with
  %     \code{modeShd='area'}. In future versions other approaches which
  %     relate the geometric shadow and the electrical connections of the PV
  %     generator will be available. If \code{modeTrk='horiz'} it is
  %     possible to calculate the effect of backtracking with
  %     \code{modeShd='bt'}. If \code{modeShd=c('area','bt')} the
  %     backtracking method will be carried out and therefore no shadows
  %     will appear. Finally, for two-axis trackers it is possible to select
  %     \code{modeShd='prom'} in order to calculate the effect of shadows on
  %     an average tracker (see \code{\link{fSombra6}}).
  % The result will include three variables (\code{Gef0}, \code{Def0} and \code{Bef0}) with the
  %     irradiance/irradiation without shadows as a reference.}

  %  \item{struct}{\code{list}.It is only needed if \code{modeShd!=''}. When \code{modeTrk='fixed'} or \code{modeTrk='horiz'} only a component named \code{L}, which is the height (meters) of the tracker, is needed. For two-axis trackers (\code{modeTrk='two'}), an additional component named \code{W}, the width of the tracker, is required. Moreover, two components named \code{Nrow} and \code{Ncol} are included under this list. These components define, respectively, the number of rows and columns of the whole set of trackers in the PV plant.}

  %  \item{distances}{It is only needed when
  %    \code{modeShd!=''}. A\code{data.frame} with a component named
  %    \code{Lew}, being the distance between horizontal NS and two-axis trackers along
  %    the East-West direction, a component named \code{Lns} for two-axis
  %    trackers or a component named \code{D} for the distance between fixed
  %    surfaces. An additional component named \code{H} can be included with
  %    the relative height (meters) between surfaces.}
}

\value{A \code{Gef} object.}

\references{Perpiñán, O, Energía Solar Fotovoltaica, 2011.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\link{calcG0}},
  \code{\link{fTheta}},
  \code{\link{fInclin}},
  \code{\link{calcShd}}.
}
\examples{
###12 Average days.

G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)

##Fixed surface, default values of inclination and azimuth.

gef<-calcGef(lat=37.2, modeRad='prom', prom=list(G0dm=G0dm, Ta=Ta))
print(gef)
xyplot(gef)

##Two-axis surface, no limitation angle.

gef2<-calcGef(lat=37.2, modeRad='prom', prom=list(G0dm=G0dm, Ta=Ta), modeTrk='two')
print(gef2)
xyplot(gef2)

\dontrun{
###Irradiation data from mapa.es/siar

##Fixed surface

gefMapa<-calcGef(lat=41,
             modeRad='mapa',
             mapa=list(prov=28,est=3,
               start='01/01/2009', end='31/12/2009'),
             keep.night=FALSE)
print(gefMapa)
xyplot(gefMapa)
##Two-axis tracker, using the previous result.
##'gefMapa' is internally coerced to a 'G0' object.

gefMapa2<-calcGef(lat=41, modeRad='prev', prev=gefMapa, modeTrk='two')
print(gefMapa2)
xyplot(gefMapa2)

###Shadows between two-axis trackers, again using the gefMapa result.

struct=list(W=23.11, L=9.8, Nrow=2, Ncol=8)
distances=data.frame(Lew=40, Lns=30, H=0)

gefShd<-calcGef(lat=41, modeRad='prev',
                prev=gefMapa, modeTrk='two',
                modeShd=c('area', 'prom'),
                struct=struct, distances=distances)
print(gefShd)
##The Gef0, Bef0 and Def0 values are the same as those contained in the
##                gefMapa2 object
}
}

\keyword{utilities}
\keyword{constructors}
