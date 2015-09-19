eot <- function(x){
    ## x is a IDate
    x <- as.IDate(x)
    ## Day of Year
    dn <- yday(x)
    ##Equation of Time, minutes según Alan M.Whitman "A simple
    ##expression for the equation of time" EoT=ts-t, donde ts es la
    ##hora solar real y t es la hora solar media. Valores negativos
    ##implican que el sol real se retrasa respecto al medio
    M <- 2 * pi/365.24 * dn
    EoT.min <- 229.18 * (-0.0334 * sin(M) +
                             0.04184 * sin(2 * M + 3.5884))
    ## Radians
    h2r(EoT.min / 60)                   
}
