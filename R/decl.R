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
