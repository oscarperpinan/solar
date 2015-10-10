fSolI <- function(solD, sample = 'hour', BTi,
                  EoT = TRUE, keep.night = TRUE,
                  method = 'michalsky')
{

    Bo <- 1367 ##Constante Solar

    lat <- d2r(attr(solD, 'lat'))
    signLat <- ifelse(sign(lat)==0, 1, sign(lat)) ##Cuando lat=0, sign(lat)=0. Lo cambio a sign(lat)=1

    if (missing(BTi)){
        dd <- solD$date
        Ndays <- length(dd)
        t1 <- as.ITime('00:00:00')
        tN <- as.ITime('23:59:59')
        d1 <- as.POSIXct(dd[1], t1)
        dN <- as.POSIXct(dd[Ndays], tN)
        BTi <- seq(d1, dN, by = sample)
    }

    BTi <- data.table(date = as.IDate(BTi),
                      time = as.ITime(BTi))
    
    ##Para escoger sólo aquellos días que están en solD, 
    ##por ejempo para días promedio
    ##o para días que no están en la base de datos
    sun <- solD[BTi]
    setkey(sun, date, time)
    ## Reorder columns so date and time are first and second ones
    setcolorder(sun, c('date', 'time', names(solD)[-1]))
    
    EoT <- ifelse(isTRUE(EoT), sun$EoT, 0)
    
    methods <- c('cooper', 'spencer', 'michalsky', 'strous')
    method <- match.arg(method, methods)

    sun[, w := sunHour(BTi, method, EoT)]

    sun[, aman := abs(w) <= abs(ws)] ##TRUE if between sunrise and sunset

    ##Angulos solares
    sun[, cosThzS := sin(decl) * sin(lat) +
              cos(decl) * cos(w) * cos(lat)]
    sun[cosThzS > 1, cosThzS := 1]

    sun[, AlS := asin(cosThzS)]

    sun[, cosAzS := signLat * (cos(decl) * cos(w) * sin(lat) -
                                   cos(lat) * sin(decl)) / cos(AlS)
        ]
    sun[abs(cosAzS) > 1, cosAzS := 1 * sign(cosAzS)]

    ##Angulo azimutal del sol. Positivo hacia el oeste.
    sun[, AzS := sign(w) * acos(cosAzS)] 

    ##Irradiancia extra-atmosférica
    sun[, Bo0 := Bo * eo * cosThzS]
    ##Bo0 is 0 outside the sunrise-sunset period
    sun[aman != TRUE, Bo0 := 0]
        
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
    
    sun
}
