 # Copyright (C) 2011, 2010, 2009 Oscar Perpiñán Lamigueiro
 #
 # This program is free software; you can redistribute it and/or
 # modify it under the terms of the GNU General Public License
 # as published by the Free Software Foundation; either version 2
 # of the License, or (at your option) any later version.
 #
 # This program is distributed in the hope that it will be useful,
 # but WITHOUT ANY WARRANTY; without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 # GNU General Public License for more details.
 #
 # You should have received a copy of the GNU General Public License
 # along with this program; if not, write to the Free Software
 # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 #/
fSolI<-function(solD, sample='hour', BTi, EoT=TRUE, keep.night=TRUE, method='michalsky'){

  Bo=1367 ##Constante Solar

  lat=d2r(attr(solD, 'lat'))
  signLat=ifelse(sign(lat)==0, 1, sign(lat)) ##Cuando lat=0, sign(lat)=0. Lo cambio a sign(lat)=1

  if (missing(BTi)){
    ## ##Copiado de seq.POSIXt
    ## ##Necesario aquí porque seq.POSIXt acepta también "days", "months", etc.
    ## by2 <- strsplit(sample, " ", fixed = TRUE)[[1L]]
    ## if (length(by2) > 2L || length(by2) < 1L) 
    ##   stop("invalid 'by' string")
    ## valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours"))
    ## if (is.na(valid)) 
    ##   stop("invalid string for 'sample'")
    sampleDiff <- char2diff(sample)
    start.sol<-start(solD)              #index(solD)[1]
    end.sol<-end(solD) #tail(index(solD), 1) o tambien index(solD)[length[index(solD)]
    seqby=seq(start.sol, end.sol+86400-1, by=sampleDiff)
  } else {
    seqby=BTi
    sampleDiff=median(diff(BTi))
  }
    
  ##Para escoger sólo aquellos días que están en solD, 
  ##por ejempo para días promedio
  ##o para días que no están en la base de datos
  seqby.day<-truncDay(seqby)          #format(seqby, '%Y-%m-%d')
  solD.day<-index(solD)               #format(index(solD), '%Y-%m-%d')
  mtch<-match(seqby.day, solD.day, nomatch = 0) ##Obtengo los índices de solD.day para los que hay correspondencia con seqby.day
  mtch.in=which(mtch>0)                #which(seqby.day %in% solD.day)
  mtch=mtch[mtch>0]
  ##un objeto zoo no permite que haya repeticiones en el índice, así que debo transformarlo a data.frame
  sol.rep<-data.frame(solD)[mtch,]    
                                        #nrep<-length(seqMatch)/dim(solD)[1]
                                        #sol.rep<-as.data.frame(lapply(as.data.frame(coredata(solD)),
                                        #   FUN=function(x)rep(x,each=nrep)))
  ##Obtengo los instantes de seqby que se corresponden con días en solD    
  ##Es útil cuando hay huecos en la base de datos o cuando trabajo en modo "prom"
  seqby.match<-seqby[mtch.in]

  ##Obtengo las variables de solD
  decl<-sol.rep$decl
  ##lat<-sol.rep$lat
  ws<-sol.rep$ws
  Bo0d<-sol.rep$Bo0d
  eo<-sol.rep$eo
  if (EoT) {EoT=sol.rep$EoT} else {EoT=0}
       
  jd <- as.numeric(julian(seqby.match, origin='2000-01-01 12:00:00 UTC'))
  TO=hms(seqby.match)

  methods=c('cooper', 'spencer', 'michalsky', 'strous')
  method=match.arg(method, methods)

  w=switch(method,
    cooper = h2r(TO-12)+EoT,
    spencer = h2r(TO-12)+EoT,
    michalsky = {
      meanLong=(280.460+0.9856474*jd)%%360
      meanAnomaly=(357.528+0.9856003*jd)%%360
      eclipLong=(meanLong +1.915*sin(d2r(meanAnomaly))+0.02*sin(d2r(2*meanAnomaly)))%%360
      excen=23.439-0.0000004*jd

      sinEclip=sin(d2r(eclipLong))
      cosEclip=cos(d2r(eclipLong))
      cosExcen=cos(d2r(excen))

      ascension=r2d(atan2(sinEclip*cosExcen, cosEclip))%%360

      ##local mean sidereal time, LMST
      ##TO has been previously corrected with local2Solar in order
      ##to include the longitude, daylight savings, etc.
      lmst=(h2d(6.697375 + 0.0657098242*jd + TO))%%360
      w=(lmst-ascension)
      w <- d2r(w + 360*(w < -180) - 360*(w > 180))
    },
    strous = {
      meanAnomaly = (357.5291 + 0.98560028*jd)%%360
      coefC=c(1.9148, 0.02, 0.0003)
      sinC=sin(outer(1:3, d2r(meanAnomaly), '*'))
      C = colSums(coefC*sinC)
      trueAnomaly=(meanAnomaly + C)%%360
      eclipLong=(trueAnomaly + 282.9372)%%360
      excen=23.435

      sinEclip=sin(d2r(eclipLong))
      cosEclip=cos(d2r(eclipLong))
      cosExcen=cos(d2r(excen))

      ascension=r2d(atan2(sinEclip*cosExcen, cosEclip))%%360

      ##local mean sidereal time, LMST
      ##TO has been previously corrected with local2Solar in order
      ##to include the longitude, daylight savings, etc.
      lmst=(280.1600+360.9856235*jd)%%360
      w=(lmst-ascension)
      w <- d2r(w + 360*(w< -180) - 360*(w>180))
    }
    )
  aman<-abs(w)<=abs(ws) ##TRUE if between sunrise and sunset

  ##Angulos solares
  cosThzS<-sin(decl)*sin(lat)+cos(decl)*cos(w)*cos(lat)
  ## is.na(cosThzS) <- (!aman)
  cosThzS[cosThzS>1]<-1

  AlS=asin(cosThzS) ##Altura del sol

  cosAzS=signLat*(cos(decl)*cos(w)*sin(lat)-cos(lat)*sin(decl))/cos(AlS)
  ## is.na(cosAzS) <- (!aman)
  cosAzS[cosAzS > 1] <- 1
  cosAzS[cosAzS < -1] <- -1

  AzS=sign(w)*acos(cosAzS) ##Angulo azimutal del sol. Positivo hacia el oeste.

  ##Irradiancia extra-atmosférica
  Bo0<-Bo*eo*cosThzS
  is.na(Bo0) <- !aman ##Bo0 is NA outside the sunrise-sunset period
  
  ##Generador empirico de Collares-Pereira y Rabl 
  a=0.409-0.5016*sin(ws+pi/3)
  b=0.6609+0.4767*sin(ws+pi/3)

  rd<-Bo0/Bo0d
  rg<-rd*(a+b*cos(w))
    
###Resultados
  resultDF<-data.frame(w, aman, cosThzS, AlS, AzS, Bo0, rd, rg)

  if (!keep.night){ ##No conservamos todo aquello en lo que aman==FALSE
    resultDF <- resultDF[aman==TRUE,]
    seqby.match <- seqby.match[aman==TRUE]
    mtch <- mtch[aman==TRUE]
  } else {}
  
  result <- zoo(resultDF, order.by=seqby.match)  
  attr(result, 'match')=mtch
  attr(result, 'lat')=r2d(lat)
  attr(result, 'sample')=sampleDiff
  
  result
}
