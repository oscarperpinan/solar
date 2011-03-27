PruebaEstacion<-function(Estacion,NomEst=TRUE){
      Prov=Estacion[2];
      Est=Estacion[1];
      EstURL<-paste('http://www.mapa.es/siar/exportador.asp?T=DD&P=',Prov,'&E=',Est,'&I=01/12/2009&F=01/12/2009',sep='')
      tryCatch(x<-scan(EstURL,what='character',nlines=1,encoding='latin1'),error=function(e){});
      #if (length(x)==1) {result=NULL}
      #else {
      if (length(x)>1) {
	    I1<-min(grep('\\(',x))
	    I2<-min(grep('\\)',x))
	    I3<-min(grep('\\:',x))
	    NomProvincia=paste(x[I1:I2],collapse='_');
	    NomProvincia=substr(NomProvincia,2,nchar(NomProvincia)-2)#elimino los paréntesis y la coma
	    NomEstacion=paste(x[(I3+1):(I1-1)],collapse='_');
	    #if (NomEst==TRUE) {result=paste(paste(Prov,NomProvincia,sep='-'),
		#			    paste(Est,NomEstacion,sep='-'),sep=':')
		#			    }
	    #else {result=paste(Prov,NomProvincia,sep='-')}}
	    result<-data.frame(Prov=Prov,Est=Est,NomProv=NomProvincia,NomEst=NomEstacion)}
	else result<-NULL;		    }
	

	    
	
      
Red<-expand.grid(E=1:200,P=1:200)
#for (i seq_along(ProvinciaRed){
#					test=PruebaEstacion(ProvinciaRed[i,])}
ProvinciaMAPA<-apply(Red,1,PruebaEstacion)

RedEstaciones=do.call(rbind,ProvinciaMAPA)

#Red<-expand.grid(E=1:27,P=Provincias);
#RedMAPA<-apply(Red,1,PruebaEstacion)
#Estaciones<-Red[which(!sapply(RedMAPA,is.null)),]


RedEstacion<-expand.grid(Est=1:27,Prov=Provincias,NomEst=NA,NomProv=NA);
#RedEstacion<-RedEstacion[1:5,];
for (i in 1:dim(RedEstacion)[1]){
	Estacion=RedEstacion[i,];
	Prov=Estacion[2];
	Est=Estacion[1];
	EstURL<-paste('http://www.mapa.es/siar/exportador.asp?T=DD&P=',Prov,'&E=',Est,'&I=31/12/2008&F=31/12/2008',sep='')
	x<-scan(EstURL,what='character',nlines=1,encoding='latin1');
	if (length(x)>1) {
	      I1<-min(grep('\\(',x))
	      I2<-min(grep('\\)',x))
	      I3<-min(grep('\\:',x))
	      NomProv=paste(x[I1:I2],collapse='_');
	      NomProv=substr(NomProv,2,nchar(NomProv)-2)#elimino los paréntesis y la coma
	      NomEst=paste(x[(I3+1):(I1-1)],collapse='_');
	      RedEstacion[i,]$NomProv=NomProv;
	      RedEstacion[i,]$NomEst=NomEst;
	      EstURL<-paste('http://www.mapa.es/siar/exportador.asp?T=DD&P=',  Prov,'&E=',Est,
			    '&I=01/01/2000&F=31/12/2009',sep='')
	      BD<-read.table(EstURL,header=T,skip=1,fill=T,dec=',')
	      Fecha=strptime(BD$Fecha2,format="%d/%m/%Y")
	      BD$G<-BD$Radiacion/3.6*1000#Cambio de unidades. G debe ir en Wh/m2, NO en kWh/m2
	      BD$Radiacion<-NULL
	      BTd<-cbind(IDd=as.numeric(as.numeric((Fecha))),
			with(Fecha,data.frame(Ano=year+1900,DiaAno=yday+1,Mes=mon+1,DiaMes=mday)))
	      BD<-cbind(BTd,BD)
	      NomFichero=paste('Datos/MAPA/',paste(Prov,NomProv,Est,NomEst,sep='-'),'.txt',sep='');
	      write.table(BD,NomFichero)}
				}
RedEstacion<-RedEstacion[!is.na(RedEstacion$NomEst),];
RedEstacion$ID<-1:dim(RedEstacion)[1];#Primary KEY para la base de datos
#Cuando añada Estaciones a la base de datos deberé hacerlo incrementando este índice.
#La primary key no tiene por qué tener una relación semántica con los datos del registro!!
write.table(RedEstacion,'Datos/MAPA/RedEstaciones.txt')

      
library(RMySQL)
drv=dbDriver('MySQL')
con=dbConnect(drv,dbname='DatosMAPA',password='kr0p0tk1n')

RedEstacion<-read.table('Datos/MAPA/RedEstaciones.txt',header=T)
dbWriteTable(con,"RedEstacion",RedEstacion,row.names=FALSE)
#Si añado más estaciones deberé utilizar append=TRUE


pb <- txtProgressBar(min = 0, max = dim(RedEstacion)[1], style = 3)
setTxtProgressBar(pb, 0)
#una tabla para Radiación, otra para temperatura, otra para viento, y otra para "agua" (humedad y precipitación)
for (i in RedEstacion$ID){
	Estacion<-RedEstacion[i,];
	NomFichero=with(Estacion,paste('Datos/MAPA/',paste(Prov,NomProv,Est,NomEst,sep='-'),'.txt',sep=''));
	DatosEstacion<-read.table(NomFichero,header=T);
	with(DatosEstacion,{
		dbWriteTable(con,'Radiacion',data.frame(IDd,G,ID=RedEstacion[i,]$ID),row.names=FALSE,append=TRUE);
		dbWriteTable(con,'Viento',data.frame(IDd,VelViento, DirViento, VelVientoMax,DirVientoVelMax,HorMinVelMax,
						ID=RedEstacion[i,]$ID),row.names=FALSE,append=TRUE);
		dbWriteTable(con,'Temperatura',data.frame(IDd,TempMedia,TempMax,HorMinTempMax,TempMin, HorMinTempMin,
						ID=RedEstacion[i,]$ID),row.names=FALSE,append=TRUE);
		dbWriteTable(con,'Agua',data.frame(IDd,HumedadMedia,HumedadMax,HorMinHumMax,HumedadMin,HorMinHumMin,
						Precipitacion,EtPMon,
						ID=RedEstacion[i,]$ID),row.names=FALSE,append=TRUE);

			    })
	rm(DatosEstacion);
	setTxtProgressBar(pb, i)
			      }
close(pb)
dbDisconnect(con)


Radiacion<-dbReadTable(con,'Radiacion')
Viento<-dbReadTable(con,'Viento')
Temperatura<-dbReadTable(con,'Temperatura')
Agua<-dbReadTable(con,'Agua')
RedEstacion<-dbReadTable(con,'RedEstacion')
Fecha<-as.POSIXlt(Radiacion$IDd,origin="1970-01-01");
BTd<-cbind(IDd=Radiacion$IDd,with(Fecha,data.frame(Ano=year+1900,DiaAno=yday+1,Mes=mon+1,DiaMes=mday)))
