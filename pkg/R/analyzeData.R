 # Copyright (C) 2010 Oscar Perpiñán Lamigueiro
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
analyzeData<-function(x, ref=NULL){

###Estadísticos por filas
###(comportamiento del conjunto de variables a lo largo del tiempo)
  Mean<-apply(x, 1, mean, na.rm=1)
  Median<-apply(x, 1,median, na.rm=1)
  Desv<-apply(x, 1,sd, na.rm=1)
  Mad<-apply(x, 1,mad, na.rm=1)
  IQR<-apply(x, 1,IQR, na.rm=1)
  x.stat<-cbind(Mean, Median, Desv, Mad, IQR)

###Referencia con sus estadísticos
  if (is.null(ref)) {ref<-Median}
  MediaRef<-mean(ref,na.rm=1)
  SDRef<-sd(ref,na.rm=1)

###Diferencia de cada variable (columna) respecto a la referencia
  Dif<-x-ref

###Estadísticos de cada variable (por columnas) en el periodo completo
  SDUnit<-sd(x, na.rm=1) ##SD de CADA variable
  ME<-apply(Dif,2,mean,na.rm=1)
  RMSDc<-apply(Dif,2,sd,na.rm=1)
  DifSD<-SDUnit-SDRef

  ##Valores relativos (respecto a la desv estandar de la referencia)
  rRMSDc<-RMSDc/SDRef
  rME<-ME/SDRef
  RMSD<-sqrt(RMSDc^2+ME^2)
  rRMSD<-RMSD/SDRef

  result<-list(stat=zoo(x.stat, index(x)),
               err=data.frame(Unit=names(x),
                 ME, rME,
                 RMSDc, rRMSDc,
                 RMSD, rRMSD,
                 DifSD)
               )
  return(result)
}
