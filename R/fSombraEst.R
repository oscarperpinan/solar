 # Copyright (C) 2010, 2009 Oscar Perpiñán Lamigueiro
 #
 # This program is free software you can redistribute it and/or
 # modify it under the terms of the GNU General Public License
 # as published by the Free Software Foundation either version 2
 # of the License, or (at your option) any later version.
 #
 # This program is distributed in the hope that it will be useful,
 # but WITHOUT ANY WARRANTY without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 # GNU General Public License for more details.
 #
 # You should have received a copy of the GNU General Public License
 # along with this program if not, write to the Free Software
 # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 #/
fSombraEst<-function(angGen, distances, struct){
  stopifnot(is.list(struct),is.data.frame(distances))
###Preparo datos de partida
  distances=distances/struct$L
  Alfa=coredata(angGen$Alfa)
  Beta=coredata(angGen$Beta)
  AlS=coredata(angGen$AlS)
  AzS=coredata(angGen$AzS)
  cosTheta=coredata(angGen$cosTheta)
  h=distances$H                   #Debe estar previamente normalizada
  d=distances$D                   
###Cálculos
  s=cos(Beta)+cos(Alfa-AzS)*(sin(Beta)+h)/tan(AlS)
  FC=sin(AlS)/sin(Beta+AlS)
  SombraCond=(s-d>0)
  FS=(s-d)*SombraCond*FC*(cosTheta>0)
###Resultado
  FS=FS*(FS>0)
  FS[FS>1]<-1
  return(zoo(FS, index(angGen)))

}


