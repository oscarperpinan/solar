 # Copyright (C) 2011, 2010 Oscar Perpiñán Lamigueiro
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
prodGCPV<-function(lat,
                   modeTrk='fixed', 
                   modeRad='prom',
                   dataRad,
                   prev,
                   prom, mapa, bd, bdI, 
                   sample='hour',
                   keep.night=TRUE,
                   sunGeometry='michalsky',
                   corr, f,
                   betaLim=90, beta=abs(lat)-10, alfa=0,
                   iS=2, alb=0.2, horizBright=TRUE, HCPV=FALSE,
                   module=list(), 
                   generator=list(),
                   inverter=list(), 
                   effSys=list(), 
                   modeShd='',    
                   struct=list(), 
                   distances=data.frame()
                   ){

  stopifnot(is.list(module),
            is.list(generator),
            is.list(inverter),
            is.list(effSys),
            is.list(struct),
            is.data.frame(distances))
	
  if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
    modeShd[which(modeShd=='bt')]='area'
    warning('backtracking is only implemented for modeTrk=horiz')}
		
  if (modeRad!='prev'){               #No utilizamos un cálculo previo

    radEf<-calcGef(lat=lat, modeTrk=modeTrk, modeRad=modeRad,
                   dataRad=dataRad,
                   prom=prom, mapa=mapa, bd=bd, bdI=bdI,
                   sample=sample, keep.night=keep.night,
                   sunGeometry=sunGeometry,
                   corr=corr, f=f,
                   betaLim=betaLim, beta=beta, alfa=alfa,
                   iS=iS, alb=alb, horizBright=horizBright, HCPV=HCPV,
                   modeShd=modeShd, struct=struct, distances=distances)
		
  } else { #Utilizamos un cálculo previo de calcG0, calcGef o prodSFCR
    if (!missing(prev) & missing(dataRad)){
      dataRad=prev
      warning('Use of the "prev" argument is deprecated. You should use dataRad instead.')
    }

    stopifnot(class(dataRad) %in% c('G0', 'Gef', 'ProdGCPV'))
    radEf <- switch(class(dataRad),
                    G0=calcGef(lat=lat,
                      modeTrk=modeTrk, modeRad='prev',
                      dataRad=dataRad,
                      betaLim=betaLim, beta=beta, alfa=alfa,
                      iS=iS, alb=alb, horizBright=horizBright, HCPV=HCPV,
                      modeShd=modeShd, struct=struct, distances=distances),
                    Gef=dataRad,
                    ProdGCPV=as(dataRad, 'Gef')
                    )
  }

                                        
  ##Producción 
  ##=======================================
	
  prodI<-fProd(radEf,module,generator,inverter,effSys)
  module=attr(prodI, 'module')
  generator=attr(prodI, 'generator')
  inverter=attr(prodI, 'inverter')
  effSys=attr(prodI, 'effSys')
  
  ##Cálculo de valores diarios, mensuales y anuales
  ##=======================================
  DayOfMonth=c(31,28,31,30,31,30,31,31,30,31,30,31) ###OJO
  Pg=generator$Pg                                   #Wp
  
  if (radEf@type=='prom') {
    prodDm=aggregate(prodI[,c('Pac', 'Pdc')]/1000,
      by=as.yearmon, FUN=P2E, radEf@sample) #kWh
    names(prodDm)=c('Eac', 'Edc')
    prodDm$Yf=prodDm$Eac/(Pg/1000)
    
    prodD=prodDm*1000                   #Wh
    prodD$Yf=prodD$Yf/1000
    index(prodD) <- indexD(radEf) ##para que sea compatible con G0D

    prody=zoo(t(colSums(prodDm*DayOfMonth)),
      unique(year(index(prodDm))))
  } else {
    prodD=aggregate(prodI[,c('Pac', 'Pdc')],
      by=truncDay, FUN=P2E, radEf@sample) #Wh
    names(prodD)=c('Eac', 'Edc')
    prodD$Yf=prodD$Eac/Pg
    
    prodDm=aggregate(prodD/1000, by=as.yearmon, mean, na.rm=1)
    prody=aggregate(prodD/1000, by=year, sum, na.rm=1)

    prodDm$Yf=prodDm$Yf*1000
    prody$Yf=prody$Yf*1000
  }
  
  result <- new('ProdGCPV',
                radEf,                  #contains 'Gef'
                prodD=prodD,
                prodDm=prodDm,
                prody=prody,
                prodI=prodI,
                module=module,
                generator=generator,
                inverter=inverter,
                effSys=effSys
                )
}
