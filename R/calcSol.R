calcSol <- function(d, lat, sample = 'hour', BTi,
                    EoT = TRUE, keep.night = TRUE,
                    method = 'michalsky'){

  if (missing(BTi)){
    solD <- fSolD(d = d, lat = lat, method = method)
    solI <- fSolI(solD, sample = sample, EoT = EoT,
                  keep.night = keep.night, method = method)
  } else { ##utilizo BTi
    BTd <- unique(as.IDate(BTi))
    solD <- fSolD(d = d, lat = lat, method=method)
    solI <- fSolI(solD, BTi = BTi,
                  EoT = EoT,
                  keep.night = keep.night,
                  method = method)
  }
  attr(solD, 'lat') <- NULL
  attr(solI, 'lat') <- NULL
  result <- new('Sol',
                lat = lat,
                solD = solD,
                solI = solI,
                sample = sample,
                method = method)
  return(result)
}
