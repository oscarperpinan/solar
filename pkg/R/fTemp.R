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
fTemp<-function(sol, BD){
  ##sol es un objeto con class='Sol'
  ##BD es un objecto con class='Meteo', cuyo slot 'data' contiene dos columnas llamadas "TempMax" y "TempMin"

  stopifnot(class(sol)=='Sol')
  stopifnot(class(BD)=='Meteo')
  stopifnot(identical(indexD(sol), indexD(BD)))

  indSol<-indexI(sol)	
  ind.rep<-indexRep(sol)
  	
  TempMax=coredata(BD@data$TempMax)[ind.rep]
  TempMin=coredata(BD@data$TempMin)[ind.rep]
  ws=coredata(sol@solD$ws)[ind.rep]
  w=coredata(sol@solI$w)

###Genera secuencia de temperatura a partir de Maxima y Minima de base de datos

  Tm=(TempMin+TempMax)/2
  Tr=(TempMax-TempMin)/2

  wp=pi/4

  a1=pi*12*(ws-w)/(21*pi+12*ws)
  a2=pi*(3*pi-12*w)/(3*pi-12*ws)
  a3=pi*(24*pi+12*(ws-w))/(21*pi+12*ws)

  T1=Tm-Tr*cos(a1)
  T2=Tm+Tr*cos(a2)
  T3=Tm-Tr*cos(a3)

  Ta=T1*(w<=ws)+T2*(w>ws&w<=wp)+T3*(w>wp)

###Resultado
  result<-zoo(Ta, indSol)
}
			
