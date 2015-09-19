dayLength <- function(x, lat, ...,
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

