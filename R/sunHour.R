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
