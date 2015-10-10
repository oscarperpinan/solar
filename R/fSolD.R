fSolD <- function(d, lat, method='michalsky'){

  if (abs(lat) > 90){
    lat <- sign(lat) * 90
    warning(paste('Latitude outside acceptable values. Set to', lat))
  }
  d <- as.IDate(d)
  ##Declination
  decl <- declination(d, method = method)
  ##Distancia sol-tierra, 1/r2
  ##ro=1.496E8                        #distancia media Tierra-Sol (km)
  eo <- eccentricity(d, method = method)
  EoT <- eot(d)
  ws <- dayLength(d, lat, method = method,
                  decl = decl)
  Bo0d <- bo0d(d, lat, method = method,
               ws = ws, decl = decl, eo = eo)

  result <- data.table(date = d,
                       decl = decl,
                       eo = eo,
                       ws = ws,
                       Bo0d = Bo0d,
                       EoT = EoT)
  attr(result, 'lat') = lat
  setkey(result, date)
  result
}
