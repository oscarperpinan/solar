 # Copyright (C) 2010, 2009 Oscar Perpiñán Lamigueiro
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
fSombraHoriz<-function(angGen, distances, struct){
  stopifnot(is.list(struct),is.data.frame(distances))
###Preparo datos de partida	
  distances=distances/struct$L
  AzS=angGen$AzS
  AlS=angGen$AlS
  Beta=angGen$Beta
  lew=distances$Lew              #Debe estar previamente normalizada
###Cálculos
  Beta0=atan(abs(sin(AzS)/tan(AlS)))
  FS=1-lew*cos(Beta0)/cos(Beta-Beta0)
  SombraCond=(FS>0)
###Resultado
  FS=FS*SombraCond
  FS[FS>1]<-1
  return(zoo(FS, index(angGen)))
}

