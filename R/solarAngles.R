## Declination
declination <- function(x, method = 'michalsky')
{
    ## x is a IDate
    x <- as.IDate(x)
    ## Day of Year
    dn <- yday(x)
    ## Days from 2000-01-01
    origin <- as.IDate('2000-01-01')
    jd <- as.numeric(x - origin)
    X <- 2 * pi * (dn-1)/365
    
    switch(method,
           cooper = {
               ##Cooper, P.I., Solar Energy, 12, 3 (1969). "The Absorption of Solar Radiation in Solar Stills"
               d2r(23.45) * sin(2 * pi * (dn + 284)/365)
           },
           spencer = {
               ##Spencer, Search 2 (5), 172
               ##http://www.mail-archive.com/sundial@uni-koeln.de/msg01050.html
               0.006918 - 0.399912 * cos(X) + 0.070257 * sin(X) -
                   0.006758 * cos(2 * X) + 0.000907 * sin(2 * X) -
                       0.002697 * cos(3 * X) + 0.001480 * sin(3 * X) 
           }, 
           strous={
               meanAnomaly  <-  (357.5291 + 0.98560028 * jd)%%360
               coefC <- c(1.9148, 0.02, 0.0003)
               sinC <- sin(outer(1:3, d2r(meanAnomaly), '*'))
               C  <-  colSums(coefC * sinC)
               trueAnomaly <- (meanAnomaly + C)%%360
               eclipLong <- (trueAnomaly + 282.9372)%%360
               excen <- 23.435
               sinEclip <- sin(d2r(eclipLong))
               sinExcen <- sin(d2r(excen))
               asin(sinEclip * sinExcen)
           }, 
           michalsky={
               meanLong <- (280.460+0.9856474 * jd)%%360
               meanAnomaly <- (357.528+0.9856003 * jd)%%360
               eclipLong <- (meanLong +1.915 * sin(d2r(meanAnomaly))+0.02 * sin(d2r(2 * meanAnomaly)))%%360
               excen <- 23.439-0.0000004 * jd
               sinEclip <- sin(d2r(eclipLong))
               sinExcen <- sin(d2r(excen))
               asin(sinEclip * sinExcen)
           },
           stop('Unknown method')
           )
}

eccentricity <- function(x, method = 'michalsky')
{
    x <- as.IDate(x)
    ## Day of Year
    dn <- yday(x)
    X <- 2 * pi * (dn-1)/365

    switch(method,
           cooper = 1 + 0.033*cos(2*pi*dn/365),
           spencer = , 
           michalsky = , 
           strous = 1.000110 + 0.034221*cos(X) +
               0.001280*sin(X) + 0.000719*cos(2*X) +
                   0.000077*sin(2*X),
           stop('Unknown method')
           )
}

sunrise <- function(x, lat, ...,
                      decl = declination(x, ...))
{
    cosWs <- -tan(d2r(lat)) * tan(decl)
    #sunrise, negative since it is before noon
    ws <- suppressWarnings(-acos(cosWs))
    ##Polar day/night
    polar <- which(is.nan(ws))        
    ws[polar] <- -pi * (cosWs[polar] < -1) + 0 * (cosWs[polar] > 1)
    ws    
}

dayLength <- function(x, lat, ...)
{
    ws <- sunrise(x, lat, ...)
    2 * abs(ws)
}

##Equation of Time, minutes según Alan M.Whitman "A simple expression
##for the equation of time" EoT=ts-t, donde ts es la hora solar real y
##t es la hora solar media. Valores negativos implican que el sol real
##se retrasa respecto al medio
eot <- function(x){
    ## x is a IDate
    x <- as.IDate(x)
    ## Day of Year
    dn <- yday(x)
    M <- 2 * pi/365.24 * dn
    EoT.min <- 229.18 * (-0.0334 * sin(M) +
                             0.04184 * sin(2 * M + 3.5884))
    ## Radians
    h2r(EoT.min / 60)                   
}

## Extraterrestrial Irradiance
bo0d <- function(x, lat, ...,
                 ws = sunrise(x, lat, ...),
                 decl = declination(x, ...),
                 eo = eccentricity(x, ...)
                 )
{
    ## solar constant
    Bo <- 1367
    lat <- d2r(lat)
    ## El signo negativo se debe a la definición de ws
    -24/pi * Bo * eo * (ws * sin(lat) * sin(decl) +
                            cos(lat) * cos(decl) * sin(ws)
                        ) 
}

## Solar Hour Angle
sunHour <- function(x, method = 'michalsky', EoT = 0)
{
    if (inherits(x, 'data.table')) {
            tt <- x[, as.POSIXct(date, time)]
            time <- x$time
        }
    else {
        tt <- as.POSIXct(x, tz = 'UTC')
        time <- as.ITime(tt)
        
    }
    ## Official Local time
    TO <- as.numeric(time)/3600 ## hours
    ## Days from 2000-01-01
    origin <- as.POSIXct('2000-01-01 12:00:00 UTC', tz = 'UTC')
    jd <- as.numeric(tt - origin)

    switch(method,
           cooper = ,
           spencer = {
               h2r(TO - 12) + EoT
           },
           michalsky = {
               meanLong <- (280.460 + 0.9856474 * jd) %% 360
               meanAnomaly <- (357.528 + 0.9856003 * jd) %% 360
               eclipLong <- (meanLong + 1.915 * sin(d2r(meanAnomaly)) +
                                 0.02 * sin(d2r(2*meanAnomaly))) %% 360
               excen <- 23.439 - 0.0000004 * jd
               
               sinEclip <- sin(d2r(eclipLong))
               cosEclip <- cos(d2r(eclipLong))
               cosExcen <- cos(d2r(excen))
               
               ascension <- r2d(atan2(sinEclip * cosExcen,
                                      cosEclip)) %% 360

               ##local mean sidereal time, LMST. TO has been previously
               ##corrected with local2Solar in order to include the
               ##longitude, daylight savings, etc.
               lmst <- (h2d(6.697375 + 0.0657098242 * jd + TO)) %% 360
               w <- (lmst - ascension)
               d2r(w + 360 * (w < -180) - 360 * (w > 180))
           },
           strous = {
               meanAnomaly  <-  (357.5291 + 0.98560028*jd)%%360
               coefC <- c(1.9148, 0.02, 0.0003)
               sinC <- sin(outer(1:3, d2r(meanAnomaly), '*'))
               C  <-  colSums(coefC * sinC)
               trueAnomaly <- (meanAnomaly + C) %% 360
               eclipLong <- (trueAnomaly + 282.9372) %% 360
               excen <- 23.435
               
               sinEclip <- sin(d2r(eclipLong))
               cosEclip <- cos(d2r(eclipLong))
               cosExcen <- cos(d2r(excen))
               
               ascension <- r2d(atan2(sinEclip * cosExcen,
                                      cosEclip)) %% 360
               
               ##local mean sidereal time, LMST
               lmst <- (280.1600 + 360.9856235 * jd)%%360
               w <- (lmst - ascension)
               d2r(w + 360 * (w < -180) - 360 * (w > 180))
           }
           )
}
