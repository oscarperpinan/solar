bo0d <- function(x, lat, ...,
                 ws = dayLength(x, lat, ...),
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
