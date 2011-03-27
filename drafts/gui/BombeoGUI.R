library(gWidgets)
options("guiToolkit"="tcltk")
data(CoefBomba)
data(RedEstaciones)

fProv<-function(h,...) {
	x=svalue(h$obj)
	tryCatch(fl[]$NomEst[]<-as.vector(subset(RedEstaciones,
										NomProv==x)$NomEst),error=function(e){})}
fQn<-function(h,...) {
	x=svalue(h$obj)
	tryCatch(fl[]$Etapas[]<-as.vector(subset(CoefBomba,
										Qn==x)$Etapas),error=function(e){})}

	
NomProvInicial<-levels(RedEstaciones$NomProv)[1]
NomEstInicial=as.vector(subset(RedEstaciones,NomProv==NomProvInicial)$NomEst)

QnInicial<-unique(CoefBomba$Qn)[1]
EtapasInicial=as.vector(subset(CoefBomba,Qn==QnInicial)$Etapas)

			
Bombeolst <- list(
				title="SFB",
				type = "ggroup",
				use.scrollwindow=TRUE,
				horizontal = FALSE,
				children = list(
																		
										list(type="fieldset",
											label='MAPA.es/siar',
											#depends.on='ModoRad',
											#depends.FUN=function(value) value=='mapa',
											#depends.signal="addHandlerClicked",
											children = list(
															list(name='lat',
																label='Latitud',
																type="gedit",
																width=4,
																text="", 
																coerce.with= as.numeric),
												
														   list(name = "NomProv",
																label = "Provincia",
																type = "gdroplist",
																items=levels(RedEstaciones$NomProv),
																handler=fProv
																),
														   list(name = "NomEst",
																label = "Estacion",
																type = "gdroplist",
																items=NomEstInicial
															),
														   list(name = "FechaInicio",
																type = "gedit",  
																width=10,                          
																label = "Fecha de Inicio",
																text = "01/01/2009",
																coerce.with = as.character),
														   list(name = "FechaFinal",
																type="gedit",
																width=10,
																label = "Fecha de Fin ",
																text = "31/12/2009",
																coerce.with = as.character)
															)),										
									
									list(type="fieldset",
											label='Datos Generales',
											children=list(
												list(type="gedit",
														name='beta',
														label='Inclinacion (º)',
														text="", 
														width=2,
														coerce.with = as.numeric),
												list(type="gedit",
														name='alfa',
														label='Orientacion (º)',
														text="0",
														width=3, 
														coerce.with = as.numeric),
												list(type="gedit",
													name='Hte',
													label='Altura manometrica (m)',
													text="",
													width=3, 
													coerce.with = as.numeric)
												
														)),
										list(type="fieldset",
											label='Bomba',
											children=list(
													 list(name = "Qn",
															label = "Qn",
															type = "gdroplist",
															items=unique(CoefBomba$Qn),
															handler=fQn
															),
													   list(name = "Etapas",
															label = "Etapas",
															type = "gdroplist",
															items=EtapasInicial
														))
												),
										list(type="fieldset",
											label='Generador FV',
											children=list(
													list(type="gedit",
														name='Pg',
														label='Potencia del generador (Wp)',
														text="", 
														width=5,
														coerce.with = as.numeric),
													list(type="gedit",
														name='lambda',
														label='Coeficiente Potencia-Temperatura',
														text="-0.0045", 
														width=7,
														coerce.with = as.numeric)
																					
											))
											))
										
												
										

					

w <- gwindow("Calculo de Funcionamiento de un SFB")
g <- ggroup(horizontal = FALSE, cont = w)
fl <- gformlayout(Bombeolst, cont = g, expand=TRUE)

bg <- ggroup(cont = g)
addSpring(bg)
b <- gbutton("OK", cont = bg)
asgFrame = gframe("Asignar resultado a:",cont=bg, anchor=c(-1,0), expand=TRUE)         
asg<-gedit('result',cont=asgFrame)

bg2 <- ggroup(cont = g,horizontal=FALSE)
addSpring(bg2)
chk<-gcheckbox(text='Grabar?', checked=TRUE,cont=bg2)
asgFrame2 = gframe("Grabar en fichero: ",cont=bg2, anchor=c(-1,0), expand=TRUE)         
asg2<-gedit('result',cont=asgFrame2)

addHandlerChanged(b, function(h,...) {
		out <- svalue(fl)
		
		outMAPA=subset(RedEstaciones,(NomProv==out$NomProv&NomEst==out$NomEst))
		MAPA=cbind(outMAPA,out[c('FechaInicio','FechaFinal')])
		
		BD=LeeMAPA(MAPA)
		#BD=with(MAPA,LeeMAPA(Provincia,Estacion,FechaInicio,FechaFinal))
		
		BTd=fBTd(Modo='BaseDatos',FechaBaseDatos=BD$Fecha,FormatoFecha="%d/%m/%Y")
		SolD<-fSolD(lat=out$lat,BTd=BTd);
		SolI<-fSolI(SolD,Nm=1)
		
		TaBD<-BD[c("TempMax","TempMin")]
		TaBD=cbind(SolD["IDd"],TaBD)
		Ta=fTemp(SolI,TaBD)$Ta
		
		G0dDF=cbind(SolD,data.frame(G0d=BD$G*1000,Ktd=BD$G/SolD$Bo0d*1000))
		
		#CompD<-fCompD(SolD,BD$G,corr='CPR')
		CompD<-fCompD(G0dDF,corr='CPR')
		CompI<-fCompI(CompD,SolI)
		AngGen<-fTheta(CompI,beta=out$beta,alfa=out$alfa,ModoSeg='est')
		#AngGen<-fTheta(CompI,beta=out$beta,alfa=out$alfa,modoSeg='est')
		Inclin<-fInclin(CompI,AngGen)
	
		
		Ct=(47-20)/800;
		Tc=Ta+Ct*Inclin$Gef;
		Pdc=out$Pg*Inclin$Gef/1000*(1+out$lambda*(Tc-25))
		Pdc[is.na(Pdc)]=0
		
		Coeficientes<-subset(CoefBomba,Qn==out$Qn&Etapas==out$Etapas)
		FuncionesBomba<-fBomba(Bomba=Coeficientes,H=out$Hte)
		
		Bomba=with(FuncionesBomba,{
						Pm=fPm(Pdc)
						Pb=fPb(Pdc)
						etam=Pb/Pm
						Ph=fPh(Pdc)
						etab=Ph/Pb
						f=fFrecuencia(Pdc)
						Caudal=fCaudal(Pdc)
						
						result=data.frame(Q=Caudal,Pdc=Pdc,Pm=Pm,Pb=Pb,Ph=Ph,etam=etam,etab=etab,f=f)})
		Bomba[Pdc==0,]=0;
		Bomba<-cbind(Inclin[c('IDd','DiaAno','DiaMes','Mes','Ano','G0','Gef')],Bomba)
		BombaD<-aggregate(Bomba[c("Q","Pdc","Gef","G0")],by=Bomba["IDd"], FUN=function(x){sum(x,na.rm=1)})
		names(BombaD)[3]<-"Edc"
		BombaD<-cbind(unique(Bomba[c("Ano","Mes","DiaAno","DiaMes")]),BombaD);#Añado variables de identificación
		BombaM<-aggregate(BombaD[c("Q","Edc","Gef","G0")],by=BombaD[c("Mes","Ano")], FUN=function(x){mean(x,na.rm=1)})
		print(BombaM)
		BombaAnual<-aggregate(Bomba[c("Q","Pdc","Gef","G0")],by=Bomba["Ano"], FUN=function(x){sum(x,na.rm=1)})
		names(BombaAnual)[3]<-"Edc"
		
		fich=paste(svalue(asg2),'.csv',sep='')
		fichD=paste(svalue(asg2),'Diario.csv',sep='')
		fichM=paste(svalue(asg2),'Mensual.csv',sep='')
		
		if (svalue(chk)) {
								write.table(Bomba,file=fich,sep=';',col.names=NA,dec=',')
								write.table(BombaD,file=fichD,sep=';',col.names=NA,dec=',')
								write.table(BombaM,file=fichM,sep=';',col.names=NA,dec=',')}	
		
		result=list(inst=Bomba,diario=BombaD,mensual=BombaM,anual=BombaAnual)
				
		asignar=make.names(svalue(asg))
		assign(asignar,result,envir = .GlobalEnv)
		
})			
		




