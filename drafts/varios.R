library(zoo)

d1 <- as.Date('1968-1-1')
d2 <- as.Date('2007-12-31')
D <- seq(d1, d2, by='1 day')
N=length(D)
z1=zoo(rnorm(N), D)

Ds <- sample(D)
Ns <- length(Ds)

Dx <- as.POSIXct(Ds)
zx <- zoo(rnorm(Nx), Dx)

##Error
Zerr <- merge(z1, zx)
head(Zerr)

##Ahora sí
zs <- zoo(rnorm(Nx), Ds)
Zok <- merge(z1, zs)
head(Zok)


evaluarfuncion <- function(f, a){
  f(a)
}

x3 <- function(x) x^3
x2 <- function(x) x^2

evaluarfuncion(x3, 2)
evaluarfuncion(x2, 2)


evaluarfuncion(function(x)x^3, 2)


outlier<- function(x){
  five <- apply(x, 2, fivenum)
  iqr <- five[4L,]-five[2L,]
  lim1 <- five[4L,]+1.5*iqr
  lim2 <- five[2L,]-1.5*iqr
  upper <- sweep(x, 2, lim1)
  lower <- sweep(x, 2, lim2)
  out <- (upper>0)|(lower<0)
  out
  }
##otra forma
stats <- apply(df, 2, boxplot.stats, do.out=TRUE, coef=1.5)
blist.out <- lapply(stats, "[[", "out")

lat=-15
fBTd('serie')[340]
sol <- calcSol(lat, BTd=fBTd('serie')[340], sample='1 min')
solZ <- as.data.frameI(sol, complete=TRUE, day=TRUE)
idx <-as.POSIXct('2011-12-06 10:00:00') 
sol <- calcSol(lat, BTd=idx, sample='1 min')

lat=d2r(lat)
cosw.ew <- with(solZ, tan(decl)/tan(lat))
cosw.ew[cosw.ew>1] <- 1
cosw.ew[cosw.ew< -1] <- -1
w.ew <- acos(cosw.ew)
Cew <- with(solZ, abs(w)<=w.ew)*2-1
Cns <- with(solZ, lat*(lat-decl)>=0)*2-1
Cw <- with(solZ, w>=0)*2-1
cbind(solZ[c("decl", "w", "aman")], w.ew,Cew, Cns, Cw)

sinazs <- with(solZ, cos(decl)*sin(w)/cos(AlS))
cosAzS <- with(solZ, sign(lat) * (cos(decl) * cos(w) * sin(lat) - cos(lat) * sin(decl))/cos(AlS))
cosAzS[cosAzS>=1] <- 1
cosAzS[cosAzS< -1] <- -1

azs <- acos(cosAzS)*sign(sinazs)



AzS <- Cew*Cns*azs+(1-Cew*Cns)/2*Cw*pi


#############
time <- rep(1:365, 10)
z <- zoo(0.01*time+10*cos(time/365)+0.1*rnorm(365*10), seq(from=as.Date('2010-01-01'), by='1 day', length=10*365))
zt <- ts(as.ts(z), frequency=365)

zt.stl <- stl(zt, 'per')
plot(zt.stl)

library(lattice)
library(latticeExtra)
library(hexbin)

panel.hist.splom<-function(x, ...){
  yrng <- current.panel.limits()$ylim
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- yrng[1] + 0.95 * diff(yrng) * y / max(y)
  panel.rect(breaks[-nB], yrng[1], breaks[-1], y,  
             col="lightgray", ...)
  diag.panel.splom(x, ...)
}
panel.density.splom <- function(x, ...){
  yrng <- current.panel.limits()$ylim
  d <- density(x)
  d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
  panel.lines(d, col='black')
  diag.panel.splom(x, ...)
}

splom(~iris[1:4], par.settings=custom.theme.2(pch=19, cex=0.8, alpha=0.5),
 diag.panel = panel.density.splom,
       lower.panel = function(x, y, ...){
         panel.xyplot(x, y, ..., col = 'lightblue')
         panel.loess(x, y, ..., col = 'red')
     }
      )

 lat <- matrix(seq(90,-90, len=50)*pi/180, 50, 50, byrow=TRUE)
     long <- matrix(seq(-180, 180, len=50)*pi/180, 50, 50)
     
     r <- 6378.1 # radius of Earth in km
     x <- r*cos(lat)*cos(long)
     y <- r*cos(lat)*sin(long)
     z <- r*sin(lat)
     
##

open3d()
bg3d('black')
earth <- persp3d(x, y, z, col="white", 
 ##              texture=system.file("textures/sunsleep.png",package="rgl"),
        texture="/home/oscar/Fotos/00_Imagenes/Varios/mapa_mundo.png",
##       texture="/usr/lib/R/site-library/rgl/textures/sunsleep.png",
        specular="black", axes=FALSE, box=FALSE, xlab="", ylab="", zlab="",
        normal_x=x, normal_y=y, normal_z=z)
earth2 <- persp3d(x+r, y+r, z+r, col="white", 
 ##              texture=system.file("textures/sunsleep.png",package="rgl"),
        texture="/home/oscar/Fotos/00_Imagenes/Varios/mapa_mundo.png",
##       texture="/usr/lib/R/site-library/rgl/textures/sunsleep.png",
        specular="black", axes=FALSE, box=FALSE, xlab="", ylab="", zlab="",
        normal_x=x, normal_y=y, normal_z=z)

##play3d(spin3d(), duration=10)
light3d(theta=10, specular='red')

library(misc3d)

 attach(quakes)
d <- kde3d(quakes$long, quakes$lat, -depth, n = 40)
v <-contour3d(d$d, exp(-12),d$x/22, d$y/28, d$z/640, color="green",
color2="gray", draw=FALSE)
p <- pointsTetrahedra(quakes$long/22, quakes$lat/28,-depth/640, size = 0.005)
drawScene(list(v, p))


###
dn=1:365
lat=d2r(seq(-90, 90, 1))
red <- expand.grid(dn=dn, lat=lat)
decl=d2r(23.45)*sin(2*pi*(red$dn+284)/365); #declinación
cosWs=-tan(red$lat)*tan(decl)

ws=suppressWarnings(-acos(cosWs)) #Amanecer, definido como ángulo negativo (antes del mediodia)
polar <- which(is.nan(ws))
ws[polar] <- (-pi)*(cosWs[polar] < -1) + (0)*(cosWs[polar] >1)

ws2 <- -acos(-tan(decl)*tan(red$lat)); #Amanecer, definido como ángulo negativo (antes del mediodia)
red <- cbind(red, data.frame(decl, ws, ws2))

x11(); levelplot(ws-ws2~dn*r2d(lat), data=red, par.settings=custom.theme.2())


####
Cinv=rbind(c(-1.195, 4.508e-2, -3.251e-5)/6000,
  c(8.06e-3, -4.161e-6, 2.859e-8),
  c(3.53e-6, 5.667e-9, -8.161e-12)*6000)

Vdc=seq(400, 600, 100)
PdcN=c(0.6, 0.7, 0.8)
V=cbind(1, Vdc, Vdc^2)
Ki <- t(apply(V, 1, function(x, M) colSums(x*t(M)), Cinv))
res <- cbind(Pdc, Ki)

A=Ki[,3];
B=Ki[,2]+1
C=Ki[,1]-(PdcN);
                                        #Potencia AC normalizada al inverter
result=(-B+sqrt(B^2-4*A*C))/(2*A);


VP <- expand.grid(Vdc=seq(350, 600, 1), Pdc=seq(0.1, 1, 0.01))

solvePac <- function(x, Cinv){
  Vdc=x[1]
  PdcN=x[2]
  V <- c(1, Vdc, Vdc^2)
  Ki=t(colSums(V*t(Cinv)))
  A=Ki[3];
  B=Ki[2]+1
  C=Ki[1]-(PdcN);
  result <- (-B+sqrt(B^2-4*A*C))/(2*A)
  result
}

#solvePac(c(500, 0.6), Cinv)

VP$Pac <- apply(VP, 1, solvePac, Cinv)
VP$eff <- with(VP, Pac/Pdc)
xyplot(Pac/Pdc~Pac, groups=Vdc, data=VP, type='l')
contourplot(eff*100~Vdc*Pac, data=VP, region=TRUE, cuts=6)

inclinGrid <- expand.grid(Gef=seq(100, 1000, 10), Ta=seq(0, 35, 1))
x <- fProd(inclinGrid)
###polar day
lat=seq(-90, 90, 1)

dayLength <- matrix(ncol=length(lat), nrow=365)

for (i in seq_along(lat)) dayLength[,i]=2*abs(fSolD(lat=lat[i], fBTd('serie'))$ws)

dayLength=cbind(expand.grid(day=fBTd('serie'), lat=lat), length=r2h(c(dayLength)))
levelplot(length~day*lat, data=dayLength)


####data.table
library(data.table)
n<-100000
grp1<-sample(1:750, n, replace=T)
grp2<-sample(1:750, n, replace=T)
d<-data.frame(x=rnorm(n), y=rnorm(n), grp1=grp1, grp2=grp2, n,
replace=T)


DT = data.table(d)
system.time(rdataT1 <- DT[,list(mean(x),mean(y)),by=list(grp1,grp2)])

system.time(rdataT2 <- DT[,list(mean(x),mean(y)),by='grp1 ,grp2'])

setkey(DT, grp1, grp2)
system.time(rdataT3 <- DT[,list(mean(x),mean(y)),by='grp1 ,grp2'])

####

library(solaR)
library(hexbin)
data(RedEstaciones)
table(RedEstaciones$NomProv)

aranjuez <- readMAPA(prov=28, est=3, start='01/01/2004', end='31/12/2010')

aranjuezDF <- subset(as.data.frame(getData(aranjuez)),
                     select=c('TempMedia', 'TempMax', 'TempMin',
                       'HumedadMedia', 'DirViento', 'EtPMon',
                       'Precipitacion', 'G0'))

trellis.device(file='splomExample.pdf', pdf)

splom(aranjuezDF,
      panel=panel.hexbinplot,
      colramp=BTC,
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm=TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d)
        diag.panel.splom(x, ...)
      },
      lower.panel = function(x, y, ...){
        panel.hexbinplot(x, y, ...)
        panel.loess(x, y, ..., col = 'red')
      },
      pscale=0, varname.cex=0.7
      )

dev.off()

trellis.focus('panel', 1, 1)
idx <- panel.link.splom(pch=13, cex=0.6, col='green')
aranjuezDF[idx,]

hexplom(aranjuezDF,
   #   panel=panel.hexbinplot,
        colramp=BTC,
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm=TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d)
        diag.panel.splom(x, ...)
      },
      lower.panel = function(x, y, ...){
        panel.hexbinplot(x, y, ...)
        panel.loess(x, y, ..., col = 'red')
      },
      pscale=0, varname.cex=0.7
      )

####
EstMadrid <- subset(RedEstaciones, NomProv=='Madrid')
nEstMadrid <- nrow(EstMadrid)
namesMadrid <- EstMadrid$NomEst

## meteoMadrid <- lapply(1:nEstMadrid, function(x){try(readMAPA(prov=28, est=x, start='01/01/2009', end='31/12/2010'))})
## names(meteoMadrid) <- namesMadrid

## okMadrid <- lapply(meteoMadrid, class)!='try-error'
## meteoMadrid <- meteoMadrid[okMadrid]

## mergeMeteo <- function(...)mergesolaR(..., var='G0')
## xxx <- do.call(mergesolaR, c(meteoMadrid, var='G0'))

## do.call(horizonsolaR, meteoMadrid)


prodMadrid <- lapply(1:nEstMadrid,
                     function(x){try(prodGCPV(lat=41, modeRad='mapa',
                                                  mapa=list(prov=28, est=x,
                                                    start='01/01/2009', end='31/12/2010'))
                                     )})
names(prodMadrid) <- namesMadrid
okMadrid <- lapply(prodMadrid, class)!='try-error'
prodMadrid <- prodMadrid[okMadrid]

##do.call(horizonsolaR, prodMadrid)

YfMadrid <- do.call(mergesolaR, c(prodMadrid, var='Yf'))

horizonplot(YfMadrid-rowMeans(YfMadrid),
            origin=0,
            scales=list(y=list(relation='same')),
            colorkey=TRUE)

x11(); TargetDiagram(YfMadrid, end=as.POSIXct('2010-12-31'), ndays=c(10, 20, 30, 40, 50, 60))

x11(); do.call(compare, prodMadrid)

