##http://www.astro.uu.nl/~strous/AA/en/reken/zonpositie.html
lon=5
lat=52
##hh <- as.POSIXct('2004-04-01 12:00:00 UTC')
hh1 <- as.POSIXct('1985-01-01 00:00:00 UTC')
hh2 <- as.POSIXct('2050-12-31 00:00:00 UTC')
hh <- seq(hh1, hh2, 'hour')
jd <- as.numeric(julian(hh, origin='2000-01-01 12:00:00 UTC'))

##http://www.astro.uu.nl/~strous/AA/en/reken/zonpositie.html
meanAnomaly = (357.5291 + 0.98560028*jd)%%360
coefC=c(1.9148, 0.02, 0.0003)
sinC=sin(outer(1:3, d2r(meanAnomaly), '*'))
C = colSums(coefC*sinC)
trueAnomaly=(meanAnomaly + C)%%360
eclipLong=(trueAnomaly + 282.9372)%%360
excen=23.435

sinEclip=sin(d2r(eclipLong))
cosEclip=cos(d2r(eclipLong))

sinExcen=sin(d2r(excen))
cosExcen=cos(d2r(excen))

declination=r2d(asin(sinEclip*sinExcen))
ascension=r2d(atan2(sinEclip*cosExcen, cosEclip))%%360

##local mean sidereal time, LMST
##en la web usan -lon, pero asigna <0 a longitudes al Este
lmst=(280.1600+360.9856235*jd+lon)%%360
hourAngle=(lmst-ascension)
hourAngle <- hourAngle + 360*(hourAngle< -180) - 360*(hourAngle>180)

##Michalsky
meanLongM=(280.460+0.9856474*jd)%%360
meanAnomalyM=(357.528+0.9856003*jd)%%360
eclipLongM=(meanLongM +1.915*sin(d2r(meanAnomalyM))+0.02*sin(d2r(2*meanAnomalyM)))%%360
excenM=23.439-0.0000004*jd

sinEclipM=sin(d2r(eclipLongM))
cosEclipM=cos(d2r(eclipLongM))

sinExcenM=sin(d2r(excenM))
cosExcenM=cos(d2r(excenM))

declinationM=r2d(asin(sinEclipM*sinExcenM))
ascensionM=r2d(atan2(sinEclipM*cosExcenM, cosEclipM))%%360

##local mean sidereal time, LMST
gmstM=(15*(6.697375 + 0.0657098242*jd + hms(hh)))%%360
lmstM=(gmstM+lon)%%360

hourAngleM=(lmstM-ascensionM)
hourAngleM <- hourAngleM + 360*(hourAngleM < -180) - 360*(hourAngleM>180)
####
dn <- doy(hh)                        #día del año

decl=23.45*sin(2*pi*(dn+284)/365)

X = 2*pi*(dn-1)/365
decl2 = 0.006918 - 0.399912*cos(X) + 0.070257*sin(X) - 0.006758*cos(2*X) + 0.000907*sin(2*X) - 0.002697*cos(3*X) + 0.001480*sin(3*X) #Spencer, Search 2 (5), 172

cooper <- declination-decl
spencer <- declination-r2d(decl2)
cooperM <- declinationM-decl
spencerM <- declinationM-r2d(decl2)
m <- declination-declinationM

compDecl <- zoo(data.frame(cooper, spencer, cooperM, spencerM, m), hh)
xyplot(compDecl, superpose=TRUE)
##Ecuación del tiempo, minutos
  ##según Alan M.Whitman "A simple expression for the equation of time"
  ##EoT=ts-t, donde ts es la hora solar real y t es la hora solar media
  ##Valores negativos implican que el sol real se retrasa respecto al medio

M=2*pi/365.24*dn
EoT.min=229.18*(-0.0334*sin(M)+0.04184*sin(2*M+3.5884))
EoT=h2r(EoT.min/60)                   #radianes

TO <- local2Solar(hh, lon=lon)
TO <- hms(TO)
w<-h2r(TO-12)+EoT

compHour <- zoo(data.frame(hourAngle=hourAngle-r2d(w), hourAngleM=hourAngleM-r2d(w), m=hourAngle-hourAngleM), hh)
xyplot(compHour, superpose=TRUE)



## meanLong=d2r(280.46 + 0.9856474*time)
## meanAnomaly=d2r(357.528+0.9856003*time)
## eclipLongitude=d2r(meanLong + 1.915*sin(meanAnomaly)+0.02*sin(2*meanAnomaly))
## eclipObliq=d2r(23.439-4e-7*time)

## decl=d2r(declination)
## w=d2r(hourAngle)
## lat=d2r(lat)
## signLat=1

##   cosThzS<-sin(decl)*sin(lat)+cos(decl)*cos(w)*cos(lat)
  
##   AlS=asin(cosThzS) ##Altura del sol
## r2d(AlS)
##   cosAzS=signLat*(cos(decl)*cos(w)*sin(lat)-cos(lat)*sin(decl))/cos(AlS)
## r2d(acos(cosAzS))

## sol <- calcSol(lat=52, BTi=local2Solar(hh, lon=5))
## solD <- as.zooD(sol)
## r2d(solD$decl)
## solI <- as.zooI(sol)
## r2d(solI$AzS)
## r2d(solI$AlS)

