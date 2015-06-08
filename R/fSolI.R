fSolI <- function(solD, sample='hour', BTi,
                  EoT=TRUE, keep.night=TRUE,
                  method='michalsky'){

  Bo <- 1367 ##Constante Solar

  lat <- d2r(attr(solD, 'lat'))
  signLat <- ifelse(sign(lat)==0, 1, sign(lat)) ##Cuando lat=0, sign(lat)=0. Lo cambio a sign(lat)=1

  if (missing(BTi)){
    sampleDiff <- char2diff(sample)
    start.sol <- start(solD)              #index(solD)[1]
    end.sol <- end(solD) #tail(index(solD), 1) o tambien index(solD)[length[index(solD)]
    BTi <- seq(start.sol, end.sol+86400-1, by = sampleDiff)
}

    
  ##Para escoger sólo aquellos días que están en solD, 
  ##por ejempo para días promedio
  ##o para días que no están en la base de datos
  sun <- as.data.table(solD)
  sun$day <- as.IDate(index(solD))
  setkey(sun, day)

  tt <- IDateTime(BTi)


  sun <- sun[tt]


  ## if (EoT) {EoT <- sol.rep$EoT} else {EoT <- 0}
       
  methods <- c('cooper', 'spencer', 'michalsky', 'strous')
  method <- match.arg(method, methods)

  sun[,
      w := switch(method,
            cooper = ,
            spencer = {
                TO <- hms(as.POSIXct(day, itime))
                h2r(TO-12) + EoT
            },
            michalsky = {
                jd = as.numeric(julian(as.POSIXct(day, itime),
                    origin='2000-01-01 12:00:00 UTC'))
                meanLong <- (280.460+0.9856474*jd)%%360
                meanAnomaly <- (357.528+0.9856003*jd)%%360
                eclipLong <- (meanLong +1.915*sin(d2r(meanAnomaly))+0.02*sin(d2r(2*meanAnomaly)))%%360
                excen <- 23.439-0.0000004*jd
                
                sinEclip <- sin(d2r(eclipLong))
                cosEclip <- cos(d2r(eclipLong))
                cosExcen <- cos(d2r(excen))
                
                ascension <- r2d(atan2(sinEclip*cosExcen, cosEclip))%%360

                ##local mean sidereal time, LMST
                ##TO has been previously corrected with local2Solar in order
                ##to include the longitude, daylight savings, etc.
                TO <- hms(as.POSIXct(day, itime))
                lmst <- (h2d(6.697375 + 0.0657098242*jd + TO))%%360
                w <- (lmst-ascension)
                w <- d2r(w + 360*(w < -180) - 360*(w > 180))
            },
            strous = {
                meanAnomaly  <-  (357.5291 + 0.98560028*jd)%%360
                coefC <- c(1.9148, 0.02, 0.0003)
                sinC <- sin(outer(1:3, d2r(meanAnomaly), '*'))
                C  <-  colSums(coefC*sinC)
                trueAnomaly <- (meanAnomaly + C)%%360
                eclipLong <- (trueAnomaly + 282.9372)%%360
                excen <- 23.435
                
                sinEclip <- sin(d2r(eclipLong))
                cosEclip <- cos(d2r(eclipLong))
                cosExcen <- cos(d2r(excen))
                
                ascension <- r2d(atan2(sinEclip*cosExcen, cosEclip))%%360
                
                ##local mean sidereal time, LMST
                lmst <- (280.1600+360.9856235*jd)%%360
                w <- (lmst-ascension)
                w <- d2r(w + 360*(w< -180) - 360*(w>180))
            }
                  )]
  sun[, aman := abs(w) <= abs(ws)] ##TRUE if between sunrise and sunset

  ##Angulos solares
  sun[, cosThzS := sin(decl) * sin(lat) + cos(decl) * cos(w) * cos(lat)]
  ## cosThzS[cosThzS>1]<-1

  sun[, AlS := asin(cosThzS)]

  sun[, cosAzS := signLat * (cos(decl) * cos(w) * sin(lat) -
                                 cos(lat) * sin(decl)) / cos(AlS)
      ]
  ## cosAzS[cosAzS > 1] <- 1
  ## cosAzS[cosAzS < -1] <- -1

  ##Angulo azimutal del sol. Positivo hacia el oeste.
  sun[, AzS := sign(w) * acos(cosAzS)] 

  ##Irradiancia extra-atmosférica
  sun[, Bo0 := Bo*eo*cosThzS]
  ## Bo0[!aman] <- 0 ##Bo0 is 0 outside the sunrise-sunset period
  
  ##Generador empirico de Collares-Pereira y Rabl 
  sun[, rd := Bo0/Bo0d]
  sun[, rg := {
          a <- 0.409 - 0.5016 * sin(ws + pi/3)
          b <- 0.6609 + 0.4767 * sin(ws + pi/3)
          rg <- rd * (a + b * cos(w))
      }]
    
  ##Resultados

  ## if (!keep.night){ ##No conservamos todo aquello en lo que aman==FALSE
  ##   resultDF <- resultDF[aman==TRUE,]
  ##   seqby.match <- seqby.match[aman==TRUE]
  ##   mtch <- mtch[aman==TRUE]
  ## } else {}
  
  ## result <- zoo(resultDF, order.by=seqby.match)  
  ## attr(result, 'match') <- mtch
  ## attr(result, 'lat') <- r2d(lat)
  ## attr(result, 'sample') <- sampleDiff
  
  ## result
  sun
}
