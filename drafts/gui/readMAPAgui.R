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
LeeMAPAgui<-function(toolkit='tcltk'){

	data(RedEstaciones)
	
	stopifnot('gWidgets' %in% .packages())
	stopifnot(toolkit %in% c('tcltk', 'RGtk2'))
	if (toolkit=='tcltk') {	stopifnot('gWidgetstcltk' %in% .packages())
									options("guiToolkit"="tcltk")
								} else  {
									stopifnot('gWidgetsRGtk2' %in% .packages())
									options("guiToolkit"="RGtk2")}
	
	
	
	fProv<-function(h,...) {
		x=svalue(h$obj)
		tryCatch(fl[]$NomEst[]<-as.vector(subset(RedEstaciones,
											NomProv==x)$NomEst),error=function(e){})}
	fEst<-function(h,...) {
		x=svalue(h$obj)
		tryCatch(svalue(asg)<-x, error=function(e){})
		tryCatch(svalue(asg2)<-x,error=function(e){})
		}

	NomProvInicial<-levels(RedEstaciones$NomProv)[1]
	NomEstInicial=as.vector(subset(RedEstaciones,NomProv==NomProvInicial)$NomEst)

	LeeMAPAlst <- list(
				title="mapa.es/siar",
				type = "ggroup",
				children = list(								
									list(type="fieldset",
										label='MAPA.es/siar',
										#depends.on='ModoRad',
										#depends.FUN=function(value) value=='mapa',
										#depends.signal="addHandlerClicked",
										children = list(
													   list(name = "NomProv",
															label = "Provincia",
															type = "gdroplist",
															items=levels(RedEstaciones$NomProv),
															handler=fProv
															),
													   list(name = "NomEst",
															label = "Estacion",
															type = "gdroplist",
															items=NomEstInicial,
															handler=fEst
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
														))																			
))
										

					

w <- gwindow("mapa.es/siar")
g <- ggroup(horizontal = FALSE, cont = w)
fl <- gformlayout(LeeMAPAlst, cont = g, expand=TRUE)

bg <- ggroup(cont = g,horizontal=FALSE)
addSpring(bg)
b <- gbutton("Descargar", cont = bg)
asgFrame = gframe("Asignar resultado a:",cont=bg, anchor=c(-1,0), expand=TRUE)        
asg<-gedit(svalue(fl)$NomEst,cont=asgFrame)
 
bg2 <- ggroup(cont = g,horizontal=FALSE)
addSpring(bg2)
chk<-gcheckbox(text='Grabar?', checked=TRUE,cont=bg2)
asgFrame2 = gframe("Grabar en fichero: ",cont=bg2, anchor=c(-1,0), expand=TRUE)         
asg2<-gedit(svalue(fl)$NomEst,cont=asgFrame2)

addHandlerChanged(b, function(h,...) {
		out <- svalue(fl)
		#out$Estacion <- with(RedEstaciones,Est[NomEst==out$NomEst])
		#out$Provincia <- with(RedEstaciones,unique(Prov[NomProv==out$NomProv]))
		#MAPA=out[c('Provincia','Estacion','FechaInicio','FechaFinal')]
		outMAPA=subset(RedEstaciones,(NomProv==out$NomProv&NomEst==out$NomEst))
		MAPA=cbind(outMAPA,out[c('FechaInicio','FechaFinal')])
		result=with(MAPA,LeeMAPA(Provincia,Estacion,FechaInicio,FechaFinal))
		Fecha=fBTd(Modo='BaseDatos',FechaBaseDatos=result$Fecha,FormatoFecha="%d/%m/%Y")
		result<-cbind(Fecha,result)
		G0dm=aggregate(result["G"],result["Mes"],FUN=function(x)mean(x,na.rm=1))
		asignar=svalue(asg)
		asignarG0dm=paste(asignar,'G0dm',sep='')
		asignar=make.names(asignar)
		asignarG0dm=make.names(asignarG0dm)
		assign(asignar,result,envir = .GlobalEnv)
		assign(asignarG0dm,G0dm,envir = .GlobalEnv)
		print(G0dm)
		fich=paste(svalue(asg2),'.csv',sep='')
		fichG0dm=paste(svalue(asg2),'G0dm.csv',sep='')
		if (svalue(chk)) {
								write.table(result,file=fich,sep=';',col.names=NA,dec=',')
								write.table(G0dm,file=fichG0dm,sep=';',col.names=NA,dec=',')}		
		
})		
}
