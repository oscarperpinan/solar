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
fInclin<-function(compI, angGen, iS=2, alb=0.2, horizBright=FALSE){
  ##compI es class='G0'
  ##angGen es 'zoo', resultado de fTheta

###Preparo argumentos
  stopifnot(iS %in% 1:4)
  ang<-as.data.frame(coredata(angGen))
  Beta=ang$Beta
  Alfa=ang$Alfa
  cosTheta=ang$cosTheta
  
  comp<-as.data.frameI(compI, complete=TRUE)
  aman <- comp$aman
  B0=comp$B0
  Bo0=comp$Bo0
  D0=comp$D0
  G0=comp$G0
  cosThzS=comp$cosThzS
  is.na(cosThzS) <- !aman
    
###Método N.Martin para suciedad e incidencia no perpendicular
  Suc=rbind(c(1, 0.17, -0.069),c(0.98,.2,-0.054),c(0.97,0.21,-0.049),c(0.92,0.27,-0.023))
  FTb=(exp(-cosTheta/Suc[iS,2])-exp(-1/Suc[iS,2]))/(1-exp(-1/Suc[iS,2]))
  FTd=exp(-1/Suc[iS,2]*(4/(3*pi)*(sin(Beta)+(pi-Beta-sin(Beta))/(1+cos(Beta)))+Suc[iS,3]*(sin(Beta)+(pi-Beta-sin(Beta))/(1+cos(Beta)))^2))
  FTr=exp(-1/Suc[iS,2]*(4/(3*pi)*(sin(Beta)+(Beta-sin(Beta))/(1-cos(Beta)))+Suc[iS,3]*(sin(Beta)+(Beta-sin(Beta))/(1-cos(Beta)))^2))
    
###Metodo Hay and Davies para tratamiento difusa
  B=B0*cosTheta/cosThzS*(cosThzS>0.007) #El factor cosThzS>0.007 hace falta para eliminar resultados erroneos cerca del amanecer
  k1=B0/(Bo0)
  factor=1+sqrt(B0/G0)*sin(Beta/2)^3
  Di=D0*(1-k1)*(1+cos(Beta))/2*(horizBright*factor)  
  Dc=D0*k1*cosTheta/cosThzS*(cosThzS>0.007)
  R=alb*G0*(1-cos(Beta))/2
  D=(Di+Dc)
###Irradiancia extraterrestre en el plano inclinado
  Bo=Bo0*cosTheta/cosThzS*(cosThzS>0.007) 
###Irradiancia directa normal (DNI)
  Bn=B0/cosThzS
###Suma de componentes
  G=B+D+R
  Ref=R*Suc[iS,1]*(1-FTr)
  Ref[is.nan(FTr)]<-0         #Cuando cos(Beta)=1, FTr=NaN. Anulo Ref.
  Dief=Di*Suc[iS,1]*(1-FTd)
  Dcef=Dc*Suc[iS,1]*(1-FTb)
  Def=Dief+Dcef
  Bef=B*Suc[iS,1]*(1-FTb)
  Gef=Bef+Def+Ref

###Resultado
  result<-zoo(data.frame(Bo, Bn,
                         G, D, Di, Dc, B, R,
                         FTb, FTd, FTr,
                         Dief, Dcef, Gef, Def, Bef, Ref), 
              order.by=indexI(compI))
}
