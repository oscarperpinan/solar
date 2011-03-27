#    Copyright (c) 2010, Oscar Perpiñán.

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
NmgSFBgui<-function(toolkit='tcltk'){
	data(CoefBomba)
	
	stopifnot('gWidgets' %in% .packages())
	stopifnot(toolkit %in% c('tcltk', 'RGtk2'))
	if (toolkit=='tcltk') {	stopifnot('gWidgetstcltk' %in% .packages())
									options("guiToolkit"="tcltk")
								} else  {
									stopifnot('gWidgetsRGtk2' %in% .packages())
									options("guiToolkit"="RGtk2")}


	

	fQn<-function(h,...) {
		x=svalue(h$obj)
		tryCatch(fl[]$Etapas[]<-as.vector(subset(CoefBomba,
											Qn==x)$Etapas),error=function(e){})}

	fEtapas<-function(...) {
		x=svalue(fl)
		Nombre=paste('Nomograma_SP',x$Qn,'A',x$Etapas,sep='')
		Potencia=subset(CoefBomba,Qn==x$Qn&Etapas==x$Etapas)$Pmn
		tryCatch(svalue(asg)<-Nombre, error=function(e){})
		tryCatch(svalue(asg2)<-Nombre,error=function(e){})
		tryCatch(svalue(fl[]$Pmn)<-Potencia,error=function(e){})
		
		}	
	QnInicial<-unique(CoefBomba$Qn)[1]
	EtapasInicial=as.vector(subset(CoefBomba,Qn==QnInicial)$Etapas)
	PmnInicial=CoefBomba[1,]$Pmn

				
	Bombeolst <- list(
				title="SFB",
				type = "ggroup",
				use.scrollwindow=TRUE,
				horizontal = FALSE,
				children = list(
																		
									
									list(type="fieldset",
											label='Radiación',
											children=list(
												
												list(type="gedit",
														name='Gd',
														label='Radiación (Wh/m2)',
														text="0",
														width=5, 
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
															items=EtapasInicial,
															handler=fEtapas),
														list(type="glabel",
															name='Pmn',
															label='Potencia',
															text=PmnInicial,
															width=5, 
															coerce.with = as.numeric)
														)
												),
										list(type="fieldset",
											label='Generador',
											children=list(
													list(type="gedit",
														name='Pmin',
														label='Potencia mínima (Wp)',
														text="", 
														width=5,
														coerce.with = as.numeric),
													list(type="gedit",
														name='Pmax',
														label='Potencia máxima (Wp)',
														text="",
														width=5, 
														coerce.with = as.numeric)
											)		
											),
										list(type="fieldset",
											label='Altura',
											children=list(
													list(type="gedit",
														name='Hmin',
														label='Altura mínima (m)',
														text="", 
														width=5,
														coerce.with = as.numeric),
													list(type="gedit",
														name='Hmax',
														label='Altura máxima (m)',
														text="",
														width=5, 
														coerce.with = as.numeric)
											))
								))
										
												
										

							

		w <- gwindow("Nomograma de un SFB")
		g <- ggroup(horizontal = FALSE, cont = w)
		fl <- gformlayout(Bombeolst, cont = g, expand=TRUE)

		datos=svalue(fl)
		Nombre=paste('Nomograma_SP',datos$Qn,'A',datos$Etapas,sep='')
		bg <- ggroup(cont = g)
		addSpring(bg)
		b <- gbutton("OK", cont = bg)
		asgFrame = gframe("Asignar resultado a:",cont=bg, anchor=c(-1,0), expand=TRUE)         
		asg<-gedit(Nombre,cont=asgFrame)

		bg2 <- ggroup(cont = g,horizontal=FALSE)
		addSpring(bg2)
		chk<-gcheckbox(text='Grabar?', checked=TRUE,cont=bg2)
		asgFrame2 = gframe("Grabar en fichero: ",cont=bg2, anchor=c(-1,0), expand=TRUE)         
		asg2<-gedit(Nombre,cont=asgFrame2)



		addHandlerChanged(b, function(h,...) {
				out <- svalue(fl)
				
				
				Coeficientes<-subset(CoefBomba,Qn==out$Qn&Etapas==out$Etapas)
				Pfv=with(out,seq(Pmin,Pmax,by=100));
				H=with(out,seq(Hmin,Hmax,by=5));
				result<-NmgSFB(Bomba=Coeficientes,Pfv=Pfv,H=H,Gd=out$Gd)
				print(result$D)
				
						
				asignar=make.names(svalue(asg))
				assign(asignar,result,envir = .GlobalEnv)
				
				fich=paste(svalue(asg2),'.csv',sep='')
				fichDib=paste(svalue(asg2),'.pdf',sep='')
				
				if (svalue(chk)) {
										write.table(result$D,file=fich,sep=';',col.names=NA,dec=',')
										trellis.device(pdf,file=fichDib,
															title='Nomograma_Bombeo_solaR');
										print(result$dibujo)
										dev.off()

										}		
		
	})			
		


}
