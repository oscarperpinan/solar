
> lat = 37.2
> SolD <- fSolD(lat, BTd = fBTd(mode = "prom"))
> SolI <- fSolI(SolD, sample = "10 min", keep.night = FALSE)
> mon = month.abb
> p <- xyplot(r2d(AlS) ~ r2d(AzS), groups = month, data = SolI, 
     type = "l", col = "black", 
     xlab = expression(psi[s]), 
     ylab = expression(gamma[s]))
> p + glayer({
     idx <- round(length(x)/2 + 1)
     panel.text(x[idx], y[idx], mon[group.value], pos = 3, offset = 0.2, 
         cex = 0.8)
 })

> lat=37.2
> BTd=fBTd(mode='serie')
> solStrous <- fSolD(lat, BTd, method='strous')
> solSpencer <- fSolD(lat, BTd, method='spencer')
> solCooper <- fSolD(lat, BTd, method='cooper')
> solMichalsky <- fSolD(lat, BTd, method='michalsky')
> decDif <- solMichalsky$decl - cbind(solStrous$decl, 
      solSpencer$decl, solCooper$decl)
> names(decDif) <- c('strous', 'spencer', 'cooper')

> BTd = fBTd(mode = "serie")
> SolD <- fSolD(lat, BTd[100])
> SolI <- fSolI(SolD, sample = "hour")
> G0d = zoo(5000, index(SolD))
> fCompD(SolD, G0d, corr = "Page")

               Fd    Ktd  G0d  D0d  B0d
2011-04-10 0.4123 0.5201 5000 2062 2938

> fCompD(SolD, G0d, corr = "CPR")

               Fd    Ktd  G0d  D0d  B0d
2011-04-10 0.5658 0.5201 5000 2829 2171

> sol <- calcSol(lat, fBTd(mode = "prom"), sample = "hour", keep.night = FALSE)
> G0dm = c(2.766, 3.491, 4.494, 5.912, 6.989, 7.742, 7.919, 7.027, 
     5.369, 3.562, 2.814, 2.179) * 1000
> Ta = c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 
     17.2, 15.2)
> BD <- readG0dm(G0dm = G0dm, Ta = Ta, lat = 37.2)
> compD <- fCompD(sol, BD, corr = "Page")
> compI <- fCompI(sol, compD)

> data("helios")
> names(helios) = c("date", "G0", "TempMax", "TempMin")
> bd = df2Meteo(helios, dates.col = "date", lat = 41, source = "helios-IES", 
     format = "%Y/%m/%d")
> bd

Object of class  Meteo 

Source of meteorological information: bd-helios-IES 
Latitude of source:  41 degrees

Meteorological Data:
     Index                           G0           TempMax         TempMin      
 Min.   :2009-01-01 00:00:00   Min.   :  326   Min.   : 1.41   Min.   :-37.50  
 1st Qu.:2009-04-08 12:00:00   1st Qu.: 2523   1st Qu.:14.41   1st Qu.:  1.95  
 Median :2009-07-07 00:00:00   Median : 4746   Median :23.16   Median :  7.91  
 Mean   :2009-07-04 21:29:54   Mean   : 4812   Mean   :22.59   Mean   :  5.32  
 3rd Qu.:2009-10-03 12:00:00   3rd Qu.: 7140   3rd Qu.:31.06   3rd Qu.: 15.11  
 Max.   :2009-12-31 00:00:00   Max.   :11254   Max.   :38.04   Max.   : 24.80

> Aranjuez <- readSIAR(28, 3, "01/01/2009", "31/12/2009")
> xyplot(G0 ~ TempMedia | month, data = Aranjuez, type = c("p", "r"))

> lat = 41
> sol = calcSol(lat, BTd = indexD(Aranjuez), sample = "hour")
> Temp <- fTemp(sol, Aranjuez)
> wTemp = window(Temp, start = as.POSIXct("2009-03-01"), 
      end = as.POSIXct("2009-03-31"))
> xyplot(wTemp, col = "black", ylab = "T") + 
      layer_(panel.xblocks(x, DoY, col = c("lightgray", "white")))

> g0 <- calcG0(lat = 37.2, modeRad = "siar", dataRad = list(prov = 28, 
     est = 3, start = "01/01/2009", end = "31/12/2009"))

> lat = 20.77
> lon = -156.9339
> dat <- read.zoo(file, 
     col.names = c("date", "hour", "G0", "B", "D0", "Ta"), 
     index = list(1, 2), 
     FUN = function(d, h) as.POSIXct(paste(d, h), 
         format = "%m/%d/%Y %H:%M", tz = "HST"), 
     FUN2 = function(x) local2Solar(x, lon), 
     header = TRUE, sep = ",")
> dat$B0 <- dat$G0 - dat$D0

> NRELMeteo <- zoo2Meteo(dat, lat = lat, source = "NREL-La Ola-Lanai")

> g0NREL <- calcG0(lat = lat, modeRad = "bdI", dataRad = NRELMeteo, 
     corr = "none")

> g0BRL <- calcG0(lat = lat, modeRad = "bdI", dataRad = NRELMeteo, 
     corr = "BRL")

> gef <- calcGef(lat = 37.2, modeRad = "prev", dataRad = g0, 
      beta = 30)
> xyplot(Gef/G ~ cosTheta | month, data = gef, type = c("p", 
      "smooth"), cex = 0.4, alpha = 0.5)

> structHoriz = list(L = 4.83)
> distHoriz = data.frame(Lew = structHoriz$L * 4, H = 0)
> gefBT = calcGef(lat = 37.2, dataRad = prom, sample = "10 min", 
     modeTrk = "horiz", modeShd = "bt", betaLim = 60, distances = distHoriz, 
     struct = structHoriz)

> inclin = data.frame(Gef = c(200, 400, 600, 800, 1000), Ta = 25)
> fProd(inclin)

   Gef Ta    Tc   Voc   Isc  Vmpp   Impp   Vdc    Idc   Pac   Pdc   EffI
1  200 25 31.75 673.3 10.34 533.1  9.586 533.1  9.586  4212  4737 0.9164
2  400 25 38.50 655.4 20.68 516.3 19.090 516.3 19.090  8275  9137 0.9334
3  600 25 45.25 637.5 31.02 499.6 28.506 499.6 28.506 11972 13202 0.9346
4  800 25 52.00 619.7 41.36 483.0 37.824 483.0 37.824 15323 16936 0.9325
5 1000 25 58.75 601.8 51.70 466.5 47.037 466.5 47.037 18342 20342 0.9293

> inclin = data.frame(Gef = 800, Ta = 30)
> gen1 = list(Nms = 10, Nmp = 11)
> inv1 = list(Ki = c(0.01, 0.025, 0.05), Pinv = 25000, Vmin = 420, 
     Vmax = 750, Gumb = 20)
> prod = fProd(inclin, generator = gen1, inverter = inv1)
> print(prod)

  Gef Ta Tc   Voc   Isc  Vmpp  Impp Vdc   Idc   Pac   Pdc   EffI
1 800 30 57 505.3 41.36 392.3 37.68 420 33.83 11943 13169 0.9346

> with(prod, Vdc * Idc/(Vmpp * Impp))

[1] 0.961

> ProdFixed <- prodGCPV(lat = lat, dataRad = prom, keep.night = FALSE)
> Prod2x <- prodGCPV(lat = lat, dataRad = prom, modeTrk = "two", 
     keep.night = FALSE)
> ProdHoriz <- prodGCPV(lat = lat, dataRad = prom, modeTrk = "horiz", 
     keep.night = FALSE)

> EstMadrid <- subset(SIAR, Provincia == "Madrid")
> nEstMadrid <- nrow(EstMadrid)
> namesMadrid <- EstMadrid$Estacion
> prodMadrid <- lapply(1:nEstMadrid, function(x) {
     try(prodGCPV(lat = 41, modeRad = "siar", dataRad = list(prov = 28, 
         est = x, start = "01/01/2009", end = "31/12/2010")))
 })

> names(prodMadrid) <- namesMadrid
> okMadrid <- lapply(prodMadrid, class) != "try-error"
> prodMadrid <- prodMadrid[okMadrid]
> YfMadrid <- do.call(mergesolaR, prodMadrid)

> horizonplot(YfMadrid - rowMeans(YfMadrid), origin = 0, 
     scales = list(y = list(relation = "same")), colorkey = TRUE))

> struct2x = list(W = 23.11, L = 9.8, Nrow = 2, Ncol = 8)
> dist2x = data.frame(Lew = 40, Lns = 30, H = 0)
> prod2xShd <- prodGCPV(lat = lat, dataRad = prom, modeTrk = "two", 
     modeShd = "area", struct = struct2x, distances = dist2x)

> structHoriz = list(L = 4.83)
> distHoriz = data.frame(Lew = structHoriz$L * 4, H = 0) 
> prodHorizShd <- prodGCPV(lat = lat, dataRad = prom, sample = "10 min", 
     modeTrk = "horiz", modeShd = "area", betaLim = 60, distances = distHoriz, 
     struct = structHoriz)

> prodHorizBT <- prodGCPV(lat = lat, dataRad = prom, sample = "10 min", 
     modeTrk = "horiz", modeShd = "bt", betaLim = 60, distances = distHoriz, 
     struct = structHoriz)

> comp <- compare(ProdFixed, Prod2x, ProdHoriz, prod2xShd, 
     prodHorizShd, prodHorizBT)
> head(comp)

  values  ind      name
1   1836  G0d ProdFixed
2   1969 Gefd ProdFixed
3   1506   Yf ProdFixed
4   1836  G0d    Prod2x
5   2961 Gefd    Prod2x
6   2235   Yf    Prod2x

> compL <- compareLosses(ProdFixed, Prod2x, ProdHoriz, prod2xShd, 
     prodHorizShd, prodHorizBT)
> head(compL)

         id  values      name
1   Shadows 0.00000 ProdFixed
2       AoI 0.05894 ProdFixed
3 Generator 0.08392 ProdFixed
4        DC 0.07441 ProdFixed
5  Inverter 0.07038 ProdFixed
6        AC 0.02973 ProdFixed

> struct2x = list(W = 23.11, L = 9.8, Nrow = 2, Ncol = 8)

> dist2x = list(Lew = c(30, 50), Lns = c(20, 50))

> ShdM2x <- optimShd(lat = lat, dataRad = prom, modeTrk = "two", 
     modeShd = c("area", "prom"), distances = dist2x, struct = struct2x, 
     res = 5, prog = FALSE)

> shadeplot(ShdM2x)

> data("pumpCoef")
> CoefSP8A44 <- subset(pumpCoef, Qn == 8 & stages == 44)
> fSP8A44 <- fPump(pump = CoefSP8A44, H = 40)

> SP8A44 = with(fSP8A44, {
     Pac = seq(lim[1], lim[2], by = 100)
     Pb = fPb(Pac)
     etam = Pb/Pac
     Ph = fPh(Pac)
     etab = Ph/Pb
     f = fFreq(Pac)
     Q = fQ(Pac)
     result = data.frame(Q, Pac, Pb, Ph, etam, etab, f)
 })
> SP8A44$etamb = with(SP8A44, etab * etam) 

> lab = c(expression(eta[motor]), expression(eta[pump]), expression(eta[mp]))
> p <- xyplot(etam + etab + etamb ~ Pac, data = SP8A44, type = "l", 
     ylab = "Efficiency")
> p + glayer(panel.text(x[1], y[1], lab[group.number], pos = 3))

> Pg = seq(3000, 5500, by = 500)
> H = seq(50, 80, by = 5)
> NmgSP8A44 <- NmgPVPS(pump = CoefSP8A44, Pg = Pg, H = H, Gd = 6000, 
     title = "Selection of Pumps")

> hh <- as.POSIXct('2011-05-01 11:00:00', tz='CET')
> latitude <- seq(70, -70, -1)
> longitude <- seq(-180, 180, 1)
> horaLong <- local2Solar(hh, longitude)

> solList <- lapply(latitude, calcSol, BTi = horaLong)
> Bo0List <- lapply(solList, function(x) as.data.frameI(x)$Bo0) 
> Bo0 <- do.call('c', Bo0List)
> Bo0[is.na(Bo0)] <- 0

> Bo0DF <- expand.grid(lon = longitude, lat = latitude)
> Bo0DF$Bo0 <- c(Bo0)
> proj <- CRS('+proj=latlon +ellps=WGS84') 
> Bo0SP <- SpatialPixelsDataFrame(points = Bo0DF[,1:2],
      data=Bo0DF["Bo0"], proj4string = proj)

> paleta=colorRampPalette(rev(brewer.pal('Greys', n=9)))
> p <- spplot(Bo0SP, scales = list(draw = TRUE), col.regions = paleta,
      cuts = 50)
> world <- map("world", plot = FALSE)
> world_sp <- map2SpatialLines(world, proj4string = proj)
> p2 <- p+layer(sp.lines(world_sp, lwd = 0.5))

> library("raster")
> old <- setwd('CMSAF') ##folder where the files are stored
> listFich <- dir(pattern = '2008')
> stackSIS <- stack(listFich)
> stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
> setwd(old)

> foo <- function(x, ...){
               gef <- calcGef(lat = x[1], dataRad = list(G0dm = x[2:13]))
               result <- as.data.frameY(gef)[c('Gefd', 'Befd', 'Defd')]
               as.numeric(result)
  }

> latLayer <- init(SISmm, v = 'y')
> gefS <- calc(stack(latLayer, SISmm), foo,
             filename = 'CMSAF/gefCMSAF',
             overwrite = TRUE)
> layerNames(gefS) <- c('Gefd', 'Befd', 'Defd')

> library("maptools")
> library("rasterVis")
> proj <- CRS(projection(SISmm))
> mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string = proj)
> levelplot(gefS, layers = 'Gefd') + layer(sp.lines(mapaSHP, lwd = 0.7))

> data("prodEx")
> ndays = c(5, 10, 15, 20)
> palette = brewer.pal(n = length(ndays), name = "Set1")
> TDColor <- TargetDiagram(prodEx, end = day, 
      ndays = ndays, 
      color = palette)

> TDMadrid <- TargetDiagram(YfMadrid, 
     end = as.POSIXct("2010-12-31"), 
     ndays = c(10, 20, 30, 40, 50, 60), 
     cex = 0.7)
