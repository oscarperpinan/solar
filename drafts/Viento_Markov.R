 # Wind speed series with MTM and R
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
##############################################################################################

## Procedimiento de generación de series sintéticas de velocidades de viento a partir de una fdp
## basado en los artículos "A Markov method for simulating non-gaussian wind speed time series",
## de G.M. McNerney y P.S. Veers, Sandia Laboratories, 1985, http://www.sandia.gov/wind/other/841227.pdf
## y en el artículo "Estimation of extreme wind speeds with very long return periods" de M.D.G Dukes y
## J.P. Palutikof, Journal of applied meteorology, 1994. 
## http://journals.ametsoc.org/doi/pdf/10.1175/1520-0450%281995%29034%3C1950%3AEOEWSW%3E2.0.CO%3B2

MeanSpeed<-6
MaxSpeed<-30
nStates<-30;

nRows<-nStates;
nColumns<-nStates;

##Función de distribución de probabilidad de Velocidades (Matriz P)
LCateg<-MaxSpeed/nStates; ##Rango que ocupa cada estado


WindSpeed=seq(LCateg/2,MaxSpeed-LCateg/2,by=LCateg);##Obtengo un vector de velocidades
                                                    ##centradas en el valor medio de cada categoría 
Shape=2; ##una fdp Rayleigh es una weibull con factor de forma 2
##Una rayleigh es una weibull con factor de escala igual a sigma*sqrt(2), 
##y la media de una rayleigh es sigma*sqrt(pi/2).
Scale=2*MeanSpeed/sqrt(pi);

fdpWind<-dweibull(WindSpeed,shape=Shape, scale=Scale);
fdpWind<-fdpWind/sum(fdpWind); ##Normalizo para conseguir que sume la unidad
##------------------------------------------------------

##Correlación entre estados (Matriz G)
g<-function(x){2^(-abs(x))} ##función de correlación decreciente entre estados
G<-matrix(nrow=nRows,ncol=nColumns)
G <- row(G)-col(G)
G <- g(G)

##--------------------------------------------------------


##Proceso iterativo para calcular la matriz P (probabilidad inicial)
P0<-diag(fdpWind);   ##Valor inicial de la MATRIZ P
p0<-fdpWind;  ##Valor inicial del VECTOR p

##Este cálculo iterativo se debe hacer hasta que no se supere un error determinado
##Ahora, como algo provisional, fijo el número de iteraciones
steps=10;
P=P0; 
p=p0; 
for (i in 1:steps){
	r<-P%*%G%*%p;
	r<-as.vector(r/sum(r));##El resultado anterior es en forma matriz. Lo cambio a vector
	p=p+0.5*(p0-r)
	P=diag(p)}
##-------------------------------------------------------------

##Matriz de Transición de Markov
N=diag(1/as.vector(p%*%G));##Matriz de normalización
MTM=N%*%G%*%P ##Matriz de Transición de Markov

MTMcum<-t(apply(MTM,1,cumsum));##A partir de la MTM genero la acumulada
##-------------------------------------------

##Cálculo de la serie a partir de la MTMcum
LSerie<-24*60;##genera un valor por minuto para 1 día
RandNum1<-runif(LSerie);##Número aleatorio para elegir entre estados
State<-InitialState<-1;##asume que el estado inicial es el 1 (esto debe cambiarse al concatenar días)
StatesSeries=InitialState;

##Se selecciona el estado siguiente a aquel en el que el número aleatorio supera al valor de probabilidad acumulada
for (i in 2:LSerie) {##i tiene que empezar en 2!!
	State=min(which(RandNum1[i]<=MTMcum[State,]));
	##if (is.infinite(Estado)) {Estado=1}; ##cuando no se cumple la condición anterior max genera -Inf
	StatesSeries=c(StatesSeries,State)}
	
RandNum2<-runif(LSerie); ##Numero aleatorio para elegir entre velocidades dentro de un estado

SpeedSeries=WindSpeed[StatesSeries]-0.5+RandNum2*LCateg;##El 0.5 se debe a que el vector VIENTOS está centrado en el valor medio de cada estado


library(MASS)
print(fitdistr(SpeedSeries, 'weibull'))




