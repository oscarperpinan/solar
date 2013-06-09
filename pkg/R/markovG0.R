## load('MTM.RData')
## MTM <- read.table('~/R/solar/drafts/ktm.dat')
## Ktm <- c(0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,1)#limites de ktm para elegir matriz
## ##segun la matriz elegida, estos son los valores máximos y mínimos del kt DIARIO
## Ktlim <- rbind(c(0.031,0.058,0.051,0.052,0.028,0.053,0.044,0.085,0.01,0.319),
##                c(0.705,0.694,0.753,0.753,0.807,0.856,0.818,0.846,0.842,0.865))
markovG0 <- function(G0dm, solD){
  timeIndex <- index(solD)
  Bo0d <- solD$Bo0d
  Bo0dm <- aggregate(Bo0d, by=as.yearmon, FUN=mean)
  ktm <- G0dm/Bo0dm

  ##Calcula con que matriz debe trabajar para cada mes
  whichMatrix <- findInterval(ktm, Ktm)

  Ktd <- state <- numeric(length(timeIndex))
  state[1] <- 1
  Ktd[1] <- ktm[state[1]]
  for (i in 2:length(timeIndex)){
    iMonth <- month(timeIndex)[i]
    colMonth <- whichMatrix[iMonth]
    rng <- Ktlim[, colMonth]
    classes <- seq(rng[1], rng[2], length=11)
    matMonth <- MTM[(10*colMonth-9):(10*colMonth),]
    ## http://www-rohan.sdsu.edu/~babailey/stat575/mcsim.r
    state[i] <- sample(1:10, size=1, prob=matMonth[state[i-1],])
    Ktd[i] <- runif(1, min=classes[state[i]], max=classes[state[i]+1])
  }
  G0dmMarkov <- aggregate(Ktd * Bo0d, as.yearmon(timeIndex), FUN=mean)
  fix <- na.locf(G0dm/G0dmMarkov, x=as.POSIXct, xout=timeIndex)
  G0d <- Ktd * Bo0d * fix
  G0d
  }
