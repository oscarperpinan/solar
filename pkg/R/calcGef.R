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
calcGef<-function(lat,
                  modeTrk='fixed',      #c('two','horiz','fixed')
                  modeRad='prom', #'prev','prom', 'aguiar','mapa','bd', 'bdI'
                  prev,
                  prom=list(),
                  mapa=list(),
                  bd=list(),
                  bdI=list(),
                  sample='hour',
                  keep.night=TRUE,
                  sunGeometry='michalsky',
                  corr, f,
                  betaLim=90, beta=abs(lat)-10, alfa=0,
                  iS=2, alb=0.2, horizBright=TRUE, HCPV=FALSE,
                  modeShd='',    #modeShd=c('area','bt','prom')
                  struct=list(), #list(W=23.11, L=9.8, Nrow=2, Ncol=8), 
                  distances=data.frame() #data.frame(Lew=40, Lns=30, H=0)){
                  ){

  stopifnot(is.list(struct), is.data.frame(distances))
	
  if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
    modeShd[which(modeShd=='bt')]='area'
    warning('backtracking is only implemented for modeTrk=horiz')}
		
  if (modeRad!='prev'){                 #No utilizamos un cálculo prev
    radHoriz<-calcG0(lat=lat, modeRad=modeRad,
                     prom=prom, mapa=mapa, bd=bd, bdI=bdI,
                     sample=sample, keep.night=keep.night,
                     sunGeometry=sunGeometry,
                     corr=corr, f=f)
  } else {                     #Utilizamos un cálculo prev de calcG0
    radHoriz <- as(prev, 'G0') ##OJO: ¿hace falta comprobar que coinciden lat y otras?
  } 

###Paso a inclinada y radiación efectiva
  BT=("bt" %in% modeShd) 
  angGen<-fTheta(radHoriz, beta, alfa, modeTrk, betaLim, BT, struct, distances)
  inclin<-fInclin(radHoriz, angGen, iS, alb, horizBright, HCPV)

###Valores diarios, mensuales y anuales
  DayOfMonth=c(31,28,31,30,31,30,31,31,30,31,30,31) ###OJO
  
  if (radHoriz@type=='prom') {
    Gefdm=aggregate(inclin[,c('Bo', 'Bn', 'G', 'D', 'B', 'Gef', 'Def', 'Bef')]/1000,
      by=as.yearmon, FUN=P2E, radHoriz@sample) #kWh
    names(Gefdm)=paste(names(Gefdm), 'd', sep='')

    GefD=Gefdm*1000                  #Wh
    index(GefD) <- indexD(radHoriz)  ##para que sea compatible con G0D
    
    Gefy=zoo(t(colSums(Gefdm*DayOfMonth)),
      unique(year(index(Gefdm))))
  } else {
    GefD=aggregate(inclin[,c('Bo','Bn', 'G', 'D', 'B', 'Gef', 'Def', 'Bef')],
      by=truncDay, FUN=P2E, radHoriz@sample) #Wh
    names(GefD)=paste(names(GefD), 'd', sep='')

    Gefdm=aggregate(GefD/1000, by=as.yearmon, mean, na.rm=1)
    Gefy=aggregate(GefD/1000, by=year, sum, na.rm=1)
  }


###Resultado antes de sombras
  result0=new('Gef',
    radHoriz,                           #Gef contains 'G0'
    Theta=angGen,
    GefD=GefD,
    Gefdm=Gefdm,
    Gefy=Gefy,
    GefI=inclin,
    iS=iS,
    alb=alb,
    modeTrk=modeTrk,
    modeShd=modeShd,
    angGen=list(alfa=alfa, beta=beta, betaLim=betaLim),
    struct=struct,
    distances=distances
    )
###Cálculo de sombras
  if (modeShd=='' ||        #Si modeShd=='' no hace calculo de sombras
      ('bt' %in% modeShd)) {            #tampoco si hay backtracking
    return(result0)
  } else {
    result <- calcShd (result0, modeTrk, modeShd, struct, distances)
    return(result)
  }
}

