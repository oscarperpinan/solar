#    Copyright (c) 2009-2010, Oscar Perpiñán.

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
prodSFCRgui<-function(toolkit='tcltk'){
	
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

		
	NomProvInicial<-levels(RedEstaciones$NomProv)[1]
	NomEstInicial=as.vector(subset(RedEstaciones,NomProv==NomProvInicial)$NomEst)

			
	prodSFCRlst <- list(
				title="Calculo de Productividad de un SFCR",
				type = "gnotebook",
				#columns=4,
				#horizontal = FALSE,
				children = list(
							list(
								type="ggroup",
								horizontal=FALSE,
								use.scrollwindow=TRUE,
								#columns = 4,
								label = "Radiacion",
								children=list(
											list(type="fieldset",
												label='',
												children=list(
													list(name='lat',
														label='Latitud (grados)',
														type="gedit",
														width=4,
														text="", 
														coerce.with= as.numeric),
													list(
														name='modoRad',
														label='Origen de datos',
														type="gradio",
														items=c('prev','prom','mapa','bd'),
														selected=2),#'prom'
													list(type="gedit",
														label='Data.frame previo',
														name='previo',
														width=8,
														text=""),
													list(type="gedit",
														name='Temp',
														label='Temperatura ambiente',
														#depends.on='modoRad',
														#depends.FUN=function(value) value=='prom',
														coerce.with = as.numeric,
														text="25",
														width=3),
													list(type="gedit",
														name='Nm',
														label='Numero de muestras por hora',
														text="1",	
														width=2,												
														coerce.with = as.numeric)
														)),
											list(type="fieldset",
												label='G0dm (Wh/m2)',
												columns=6,
												children=list(
													list(
													name='Ene',
													label='Ene',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Feb',
													label='Feb',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Mar',
													label='Mar',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Abr',
													label='Abr',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='May',
													label='May',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Jun',
													label='Jun',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Jul',
													label='Jul',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Ago',
													label='Ago',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Sep',
													label='Sep',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Oct',
													label='Oct',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Nov',
													label='Nov',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													),
													list(
													name='Dic',
													label='Dic',
													type="gedit",
													width=4,
													text="",
													coerce.with = as.numeric
													))),
																														
											list(type="fieldset",
													label='MAPA.es/siar',
													#depends.on='modoRad',
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
													label='Base de Datos',
													columns=2,
													#depends.on='modoRad',
													#depends.FUN=function(value) value=='bd',
													children=list(
														BaseDatos=list(
																	name='BaseDatos',
																	label='Base de Datos',
																	type="gedit",
																	width=8,
																	text=""
																	),
														FormatoFecha=list(
																	name='FormatoFecha',
																	label='Formato de Fecha',
																	type="gedit",
																	width=10,
																	coerce.with=as.character,
																	text="%d/%m/%Y")
															))
												)),
											
						list(type="ggroup",
							horizontal=FALSE,
							use.scrollwindow=TRUE,
							label = "Sistema",
							children=list(
										list(type="fieldset",
											label='Datos Generales',
											children=list(
												list(type="gradio",
														name='modoSeg',
														label='Modo de Seguimiento',
														items=c('est','doble','horiz'),
														selected=1),#selecciono 'est' por defecto
												list(name='BetaLim',
														label='Limite de inclinacion (grados)',
														type="gedit",
														text="90", 
														width=2,
														coerce.with = as.numeric),
												list(type="gedit",
														name='beta',
														label='Inclinacion (grados)',
														text="", 
														width=2,
														coerce.with = as.numeric),
												list(type="gedit",
														name='alfa',
														label='Orientacion (grados)',
														text="0",
														width=3, 
														coerce.with = as.numeric),
												list(type="gedit",
													name='iS',
													label='Indice de suciedad',
													text="2",
													width=2, 
													coerce.with = as.numeric),
												list(type="gedit",
													name='alb',
													label='Factor Albedo',
													text="0.2", 
													width=3,
													coerce.with = as.numeric)
														)),
										list(type="fieldset",
											label='Generador',
											children=list(
													list(type="gedit",
															name='Nms',
															label='Nms',
															text="12",
															width=2, 
															coerce.with = as.numeric),
													list(type="gedit",
															name='Nmp',
															label='Nmp',
															text="11",
															width=2,
															coerce.with = as.numeric))
															), 
																
										list(type="fieldset",
											columns=4,
											label='Modulo',
											children=list(
													list(type="gedit",
														name='Vocn',
														label='Vocn',
														text="57.6", 
														width=5,
														coerce.with = as.numeric),
													list(type="gedit",
														name='Iscn',
														label='Iscn',
														text="4.7", 
														width=3,
														coerce.with = as.numeric),
													list(type="gedit",
														name='Vmn',
														label='Vmn',
														text="46.08",
														width=5, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='Imn',
														label='Imn',
														text="4.35",
														width=4, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='Ncs',
														label='Ncs',
														text="96",
														width=2, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='Ncp',
														label='Ncp',
														text="1",
														width=2, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='CoefVT',
														label='CoefVT',
														text="0.0023",
														width=6, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='TONC',
														label='TONC',
														text="47",
														width=2, 
														coerce.with = as.numeric)
														)),
							
									
													
									list(type='fieldset',
										label='Inversor',
										columns=3,
										children=list(
													list(type="gedit",
														name='Ki0',
														label='Ki0',
														text="0.01",
														width=5, 
														coerce.with = as.numeric
														),
													list(type="gedit",
														name='Ki1',
														label='Ki1',
														text="0.025",
														width=5,
														coerce.with = as.numeric
														),
													list(type="gedit",
														name='Ki2',
														label='Ki2',
														text="0.05",
														width=5, 
														coerce.with = as.numeric
														),
													list(type="gedit",
														name='Pinv',
														label='Pinv',
														text="25000",
														width=6, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='Vmin',
														label='Vmin',
														text="420",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='Vmax',
														label='Vmax',
														text="750",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='Gumb',
														label='Gumb (W/m2)',
														text="20",
														width=3, 
														coerce.with = as.numeric))
														),

									list(type="fieldset",
										columns=4,
										label='EffSys (%)',
										children=list(
													list(type="gedit",
														name='ModQual',
														label='ModQual',
														text="3",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='ModDisp',
														label='ModDisp',
														text="2",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='OhmDC',
														label='OhmDC',
														text="1.5",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='OhmAC',
														label='OhmAC',
														text="1.5",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='MPP',
														label='MPP',
														text="1",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='TrafoMT',
														label='TrafoMT',
														text="1",
														width=3, 
														coerce.with = as.numeric),
													list(type="gedit",
														name='Disp',
														label='Disp',
														text="0.5",
														width=3, 
														coerce.with = as.numeric))
														)
											)),
								
							list(type="ggroup",
								#columns = 2,
								label = "Sombra",
								children=list(
										list(type="fieldset",
											columns=4,
											label='',
											children=list(
										
														list(type="gcheckbox",
																name='sombra',
																label='Sombra?',
																checked=FALSE),
														list(type="gcheckbox",
																name='BT',
																label='Backtracking?',
																checked=FALSE),
														list(type="gcheckbox",
																name='prom',
																label='Promedio?',
																checked=FALSE),
														list(type="gcheckbox",
																name='SombOptim',
																label='Optimizacion de Sombra?',
																checked=FALSE))
											),
												

									list(type="fieldset",
											columns=2,
											label='Estructura',
											children=list(
												list(type="gedit",
													name='W',
													label='W (m)',
													width=5,
													text="23.11", 
													coerce.with = as.numeric),
													#depends.on='modoSeg',
													#depends.FUN=function(value) value=='doble'),
												list(type="gedit",
													name='L',
													label='L (m)',
													text="9.8",
													width=4, 
													coerce.with = as.numeric),
												list(type="gedit",
													name='Nfilas',
													label='Nfilas',
													text="2",
													width=2, 
													coerce.with = as.numeric),
													#depends.on='modoSeg',
													#depends.FUN=function(value) value=='doble'),
												list(type="gedit",
													name='Ncol',
													label='Ncol',		
													text="8",
													width=2, 
													coerce.with = as.numeric)
													#depends.on='modoSeg',
													#depends.FUN=function(value) value=='doble'))
													)),

									list(type="fieldset",
										columns=2,
										labels='Distancias',
										children=list(
												list(type="gedit",
													text="",
													width=3, 
													name='Leo',
													label='Leo (m)',
													coerce.with = as.numeric),
												list(type="gedit",
													name='Lns',
													label='Lns (m)',
													width=3,
													text="", 
													coerce.with = as.numeric),
													#depends.on='modoSeg',
													#depends.FUN=function(value) value=='doble'),
												list(type="gedit",
													name='D',
													label='D (m)',
													text="",
													width=3, 
													coerce.with = as.numeric),
													#depends.on='modoSeg',
													#depends.FUN=function(value) value=='est'),
												list(type="gedit",
													name='H',
													label='H (m)',
													width=3,
													text="0", 
													coerce.with = as.numeric))
													),
										list(type="fieldset",
										columns=2,
										labels='Optimizacion de Separaciones',
										children=list(
												list(type="gedit",
													text="",
													width=3, 
													name='minLeo',
													label='Leo minimo (m)',
													coerce.with = as.numeric),
												list(type="gedit",
													text="",
													width=3, 
													name='maxLeo',
													label='Leo maximo (m)',
													coerce.with = as.numeric),
												list(type="gedit",
													text="",
													width=3, 
													name='minLns',
													label='Lns minimo (m)',
													coerce.with = as.numeric),
												list(type="gedit",
													name='maxLns',
													label='Lns maximo (m)',
													width=3,
													text="", 
													coerce.with = as.numeric),
													#depends.on='modoSeg',
													#depends.FUN=function(value) value=='doble'),
												list(type="gedit",
													name='minD',
													label='D minimo (m)',
													text="",
													width=3, 
													coerce.with = as.numeric),
													#depends.on='modoSeg',
													#depends.FUN=function(value) value=='est'),
												list(type="gedit",
													name='maxD',
													label='D maximo (m)',
													text="",
													width=3, 
													coerce.with = as.numeric),
												list(type="gedit",
													name='res',
													label='Resolución (m)',
													text="1",
													width=3, 
													coerce.with = as.numeric)
												)
												)
												))
												
))
										

					

w <- gwindow("Calculo de Productividad de un SFCR")
g <- ggroup(horizontal = FALSE, cont = w)
fl <- gformlayout(prodSFCRlst, cont = g, expand=TRUE)
bg <- ggroup(cont = g)
addSpring(bg)
b <- gbutton("OK", cont = bg)
asgFrame = gframe("Asignar resultado a:",cont=bg, anchor=c(-1,0), expand=TRUE)         
asg<-gedit('result',cont=asgFrame)

addHandlerChanged(b, function(h,...) {
		out <- svalue(fl)
		
		outMAPA=subset(RedEstaciones,(NomProv==out$NomProv&NomEst==out$NomEst))
		MAPA=cbind(outMAPA,out[c('FechaInicio','FechaFinal')])
		
		G0dm=as.vector(unlist(out[c('Ene','Feb','Mar','Abr','May','Jun',
				'Jul','Ago','Sep','Oct','Nov','Dic')]))
		
		out$previo<-svalue(out$previo)
		
		out$BaseDatos<-svalue(out$BaseDatos)
		
		modulo=out[c('Vocn','Iscn','Vmn','Imn','Ncs','Ncp','CoefVT','TONC')]
		generador=out[c('Nms','Nmp')]
		inversor=out[c('Pinv','Vmin', 'Vmax','Gumb')]
		inversor$Ki=as.vector(unlist(out[c('Ki0','Ki1','Ki2')]))
		EffSys=out[c('ModQual','ModDisp','OhmDC','OhmAC','MPP','TrafoMT','Disp')]
		
		modoSombra=NULL
		if (out$sombra) modoSombra=c(modoSombra,'area')
		if (out$BT) modoSombra=c(modoSombra,'bt')
		if (out$prom) modoSombra=c(modoSombra,'prom')
		estruct=out[c('W', 'L', 'Nfilas', 'Ncol')]
		res=out$res
		
		if (out$SombOptim) {
										distancias=switch(out$modoSeg,
															doble={Leo=with(out,c(minLeo,maxLeo))
																		Lns=with(out,c(minLns,maxLns))
																		distancias=list(Leo=Leo,Lns=Lns)},
															horiz={Leo=with(out,c(minLeo,maxLeo))
																	distancias=list(Leo=Leo)},
															est={D=with(out,c(minD,maxD))
																	distancias=list(D=D)})
										result<-with(out,
															optimSombra(lat,G0dm,Temp,modoSeg,modoRad,
															previo, MAPA,BaseDatos, FormatoFecha,
															Nm, BetaLim,beta,alfa, iS,alb,
															modulo,generador,inversor, EffSys,
															modoSombra, estruct,distancias))
										plot(result)
										} else {
											distancias=as.data.frame(out[c('Leo','Lns','D','H')])
											result<-with(out,
															prodSFCR(lat,G0dm,Temp,modoSeg,modoRad,
															previo, MAPA,BaseDatos, FormatoFecha,
															Nm, BetaLim,beta,alfa, iS,alb,
															modulo,generador,inversor, EffSys,
															modoSombra, estruct,distancias))}
		
		asignar=make.names(svalue(asg))
		assign(asignar,result,envir = .GlobalEnv)
		})			
}		


