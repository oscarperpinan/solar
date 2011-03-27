fAguiar<-function(G0dm, SolD) {

#    fAguiar: generación de series sintéticas de radiación global
#    Copyright (c) 2009, Oscar Perpiñán Lamigueiro

#    Este programa es software libre: usted puede redistribuirlo y/o modificarlo 
#    bajo los términos de la Licencia Pública General GNU publicada 
#    por la Fundación para el Software Libre, ya sea la versión 3 
#    de la Licencia, o (a su elección) cualquier versión posterior.

#    Este programa se distribuye con la esperanza de que sea útil, pero 
#    SIN GARANTÍA ALGUNA; ni siquiera la garantía implícita 
#    MERCANTIL o de APTITUD PARA UN PROPÓSITO DETERMINADO. 
#    Consulte los detalles de la Licencia Pública General GNU para obtener 
#    una información más detallada. 

#    Debería haber recibido una copia de la Licencia Pública General GNU 
#    junto a este programa. 
#    En caso contrario, consulte <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
#G0dm son las 12 medias mensuales de irradiación global diaria en el plano horizontal
#SolD es el resultado de fSolD
#SolD es un data frame que contiene las siguientes variables: Ano, DiaAno, Mes, DiaMes, decl, eo, ws, Bo0d
#Esta función devuelve un data.frame que contiene las variables de SolD, y además ktm, ktd y G0d.
#_______________________________________________________________________________________________

Bo0dm<-aggregate(SolD["Bo0d"],list(Mes=SolD$Mes),FUN=mean);

G0dmDF<-cbind(Bo0dm,data.frame(G0dm,ktm=G0dm/Bo0dm$Bo0d));
G0dDF<-merge(SolD,G0dmDF[c("Mes","ktm")]) 
G0dDF$Ktd<-NA;

attach(G0dDF)
MTM=read.table('scriptsR/ktm.dat'); #Generación de secuencia diaria de radiación.
MTMcum<-t(apply(MTM,1,cumsum));


Ktm=c(0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,1);#limites de ktm para elegir matriz
Ktlim=rbind(c(0.031,0.058,0.051,0.052,0.028,0.053,0.044,0.085,0.01,0.319),c(0.705,0.694,0.753,0.753,0.807,0.856,0.818,0.846,0.842,0.865));#segun la matriz elegida, estos son los valores máximos y mínimos del kt DIARIO


iMes=apply(outer(Ktm,G0dmDF$ktm,'<='),2,sum);#Calcula con que matriz debe trabajar para cada mes

#DiasMes=as.numeric(tapply(DiaMes,Mes,max))

for (i in 1:12){
	LongSerie<-DiasMes[i];#genera un valor para cada día del mes
	RandNum1<-runif(LongSerie);#Número aleatorio para elegir entre estados
	RandNum2<-runif(LongSerie); #Numero aleatorio para elegir entre kt dentro de una categoria
	MTMcumMes<-MTMcum[(10*iMes[i]-9):(10*(iMes[i])),];#Matriz correspondiente a este Mes
	Categorias=seq(Ktlim[1,iMes[i]],Ktlim[2,iMes[i]],l=11)#divido el rango (KtMIN,KtMAX) correspondiente a la MTM de este mes en 10 intervalos (por tanto, la secuencia tiene 10+1 elementos)
	LCategoria=(Ktlim[2,iMes[i]]-Ktlim[1,iMes[i]])/10;#Longitud de las categorias
	for (j in 1:LongSerie) {
		if (i==1&j==1) {kt0=ktm[12]#para calcular el kt del 1 de Enero, necesito el valor del kt del día anterior. 
	    #Se asume para este kt el valor del ktm de Diciembre 
			} else
		if (j==1) {kt0=G0dDF$Ktd[(Mes==i-1)&(DiaMes==DiasMes[i-1])]#Para el primer dia de cada mes, el kt anterior es el del último día del mes anterior
			} else
			{kt0=G0dDF$Ktd[(Mes==i)&(DiaMes==j-1)]}
		EstadoAnterior<-apply(outer(Categorias,kt0,'<='),2,sum);#Fila correspondiente al kt anterior
		Estado=min(which(RandNum1[j]<=MTMcumMes[EstadoAnterior,]));#Se selecciona el estado siguiente a aquel en el que el número aleatorio supera al valor de probabilidad acumulada
		G0dDF[Mes==i&DiaMes==j,]$Ktd=Categorias[Estado]+RandNum2[j]*LCategoria; #Elige el valor de kt correspondiente a ese estado
				}
		}

detach(G0dDF);
G0dDF$Ktd<-G0dDF$Ktd*G0dDF$ktm/ave(G0dDF$Ktd,G0dDF$Mes,FUN=mean);#normalizo para obtener los ktm requeridos
G0dDF$G0d<-with(G0dDF,Bo0d*Ktd);




result<-G0dDF;

result}


