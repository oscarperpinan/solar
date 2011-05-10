##http://www.astro.uu.nl/~strous/AA/en/reken/zonpositie.html
lon=5
lat=52
hh <- as.POSIXct('2004-04-01 12:00:00 UTC')
jd <- as.numeric(julian(hh, origin='2000-01-01 12:00:00 UTC'))

meanAnomaly = (357.5291 + 0.98560028*jd)%%360
coefC=c(1.9148, 0.02, 0.0003)
sinC=sin(1:3*d2r(meanAnomaly))
C = sum(coefC*sinC)
trueAnomaly=(meanAnomaly + C)%%360

eclipLong=(trueAnomaly + 282.9372)%%360

sinEclip=sin(d2r(eclipLong))
cosEclip=cos(d2r(eclipLong))
sinExcen=sin(d2r(23.45))
cosExcen=cos(d2r(23.45))

declination=r2d(asin(sinEclip*sinExcen))##radianes
ascension=r2d(atan2(sinEclip*cosExcen, cosEclip))

##local sidereal time
##en la web usan -lon, pero asigna <0 a longitudes al Este
siderealTime=(280.1600+360.9856235*jd+lon)%%360

hourAngle=siderealTime-ascension




## meanLong=d2r(280.46 + 0.9856474*time)
## meanAnomaly=d2r(357.528+0.9856003*time)
## eclipLongitude=d2r(meanLong + 1.915*sin(meanAnomaly)+0.02*sin(2*meanAnomaly))
## eclipObliq=d2r(23.439-4e-7*time)
decl=d2r(declination)
w=d2r(hourAngle)
lat=d2r(lat)
signLat=1

  cosThzS<-sin(decl)*sin(lat)+cos(decl)*cos(w)*cos(lat)
  
  AlS=asin(cosThzS) ##Altura del sol
r2d(AlS)
  cosAzS=signLat*(cos(decl)*cos(w)*sin(lat)-cos(lat)*sin(decl))/cos(AlS)
r2d(acos(cosAzS))

sol <- calcSol(lat=52, BTi=local2Solar(hh, lon=5))
solD <- as.zooD(sol)
r2d(solD$decl)
solI <- as.zooI(sol)
r2d(solI$AzS)
r2d(solI$AlS)
