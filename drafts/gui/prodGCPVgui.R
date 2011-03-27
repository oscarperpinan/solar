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


iniList <- list(type="fieldset",
                label='Datos Iniciales',
                children=list(
                  list(name='lat',
                       label='Latitud (grados)',
                       type="gedit",
                       width=4,
                       text="", 
                       coerce.with= as.numeric),
                  list(
                       name='modeRad',
                       label='Origen de datos',
                       type="gradio",
                       horizontal=TRUE,
                       items=c('prev','prom','mapa','bd'),
                       selected=2),     #'prom'
                  list(type="gedit",
                       name='sample',
                       label='Sample',
                       text="hour",	
                       width=10,
                       coerce.with = as.character)
                  ))
                  
promList <- list(type="fieldset",
                 label='G0dm (Wh/m2)',
                 depends.on='modeRad',
                 depends.FUN=function(value) {value=='prom'},
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
                        )))

TaList <- list(type="fieldset",
               label='Ambient Temperature',
               depends.on='modeRad',
               depends.FUN=function(value) {value=='prom'},
               columns=6,
               children=list(
                 list(
                      name='EneTa',
                      label='Ene',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='FebTa',
                      label='Feb',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='MarTa',
                      label='Mar',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='AbrTa',
                      label='Abr',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='MayTa',
                      label='May',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='JunTa',
                      label='Jun',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='JulTa',
                      label='Jul',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='AgoTa',
                      label='Ago',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='SepTa',
                      label='Sep',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='OctTa',
                      label='Oct',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='NovTa',
                      label='Nov',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      ),
                 list(
                      name='DicTa',
                      label='Dic',
                      type="gedit",
                      width=2,
                      text="25",
                      coerce.with = as.numeric
                      )))

prevList <- list(type='fieldset',
                 label='Previous object',
                 depends.on='modeRad',
                 depends.FUN=function(value) {value=='prev'},
                 children= list(
                   list(name='prev',
                        label='',
                        type='gedit',
                        width=10,
                        text='',
                        coerce.with=as.character                       
                        )
                   )
                 )

mapaList <- list(type="fieldset",
                 label='MAPA.es/siar',
                 depends.on='modeRad',
                 depends.FUN=function(value) {value=='mapa'},
                 children = list(
                   list(name = "NomProv",
                        label = "Provincia",
                        type = "gdroplist",
                        items=levels(RedEstaciones$NomProv)
                                        #                        handler=fProv
                        ),
                   list(name = "NomEst",
                        label = "Estacion",
                        type = "gdroplist",
                        items={
                          NomProvInicial<-levels(RedEstaciones$NomProv)[1]
                          NomEstInicial=as.vector(subset(
                            RedEstaciones, NomProv==NomProvInicial)$NomEst)
                          res=NomEstInicial
                        }
                        ),
                   list(name = "start",
                        type = "gedit",  
                        width=10,                          
                        label = "Fecha de Inicio",
                        text = "01/01/2009",
                        coerce.with = as.character),
                   list(name = "end",
                        type="gedit",
                        width=10,
                        label = "Fecha de Fin ",
                        text = "31/12/2009",
                        coerce.with = as.character)
                   ))
                  
bdList <- list(type="fieldset",
               label='Base de Datos',
               columns=2,
               depends.on='modeRad',
               depends.FUN=function(value) {value=='bd'},
               children=list(
                 BaseDatos=list(
                   name='BaseDatos',
                   label='',
                   text='',
                   type="gfilebrowse",
                   quote=FALSE,
                   coerce.with=as.character,
                   width=8
                   ),
                 FormatoFecha=list(
                   name='FormatoFecha',
                   label='Formato de Fecha',
                   type="gedit",
                   width=10,
                   coerce.with=as.character,
                   text="%d/%m/%Y")
                 ))

radList <- list(type="ggroup",
                horizontal=FALSE,
                use.scrollwindow=TRUE,
                label = "Radiacion",
                children=list(
                  iniList,
                  promList,
                  TaList,
                  mapaList,
                  bdList,
                  prevList
                  )
                )

####
generalList <-list(type="fieldset",
                   label='Datos Generales',
                   children=list(
                     list(type="gradio",
                          name='modeTrk',
                          label='Modo de Seguimiento',
                          items=c('fixed','two','horiz'),
                          selected=1),  #selecciono 'est' por defecto
                     list(name='betaLim',
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
                     ))

generatorList <-list(type="fieldset",
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
                            coerce.with = as.numeric)
                     ))

moduleList <-list(type="fieldset",
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
                    ))

inverterList <- list(type='fieldset',
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
                            coerce.with = as.numeric)
                       ))

lossesList <- list(type="fieldset",
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
                          coerce.with = as.numeric)
                   ))
                     
systemList <- list(type="ggroup",
                   horizontal=FALSE,
                   height=600,
                   use.scrollwindow=TRUE,
                   label = "Sistema",
                   children=list(
                     generalList,
                     moduleList,
                     generatorList,
                     inverterList,
                     lossesList)
                   )
#################
                     
shdOptionsList <- list(type="fieldset",
                       columns=4,
                       label='Opciones',
                       children=list(
                         list(type="gcheckbox",
                              name='sombra',
                              text='Sombra',
                              checked=FALSE),
                         list(type="gcheckbox",
                              name='BT',
                              text='Backtracking',
                              checked=FALSE),
                         list(type="gcheckbox",
                              name='prom',
                              text='Promedio',
                              checked=FALSE)
                         ))

structList <- list(type="fieldset",
                       columns=2,
                       label='Estructura',
                       children=list(
                         list(type="gedit",
                              name='W',
                              label='W (m)',
                              width=5,
                              text="23.11", 
                              coerce.with = as.numeric),
                              depends.on='modeTrk',
                              depends.FUN=function(value) value=='two'),
                         list(type="gedit",
                              name='L',
                              label='L (m)',
                              text="9.8",
                              width=4, 
                              coerce.with = as.numeric),
                         list(type="gedit",
                              name='Nrow',
                              label='Nfilas',
                              text="2",
                              width=2, 
                              coerce.with = as.numeric),
                              depends.on='modeTrk',
                              depends.FUN=function(value) value=='two'),
                         list(type="gedit",
                              name='Ncol',
                              label='Ncol',		
                              text="8",
                              width=2, 
                              coerce.with = as.numeric)
                              depends.on='modoTrk',
                              depends.FUN=function(value) value=='two'))
                         ))

distList <-list(type="fieldset",
                       columns=2,
                       label='Distancias',
                       children=list(
                         list(type="gedit",
                              text="",
                              width=3, 
                              name='Lew',
                              label='Lew (m)',
                              coerce.with = as.numeric),
                         list(type="gedit",
                              name='Lns',
                              label='Lns (m)',
                              width=3,
                              text="", 
                              coerce.with = as.numeric),
                              depends.on='modoTrk',
                              depends.FUN=function(value) value=='two'),
                         list(type="gedit",
                              name='D',
                              label='D (m)',
                              text="",
                              width=3, 
                              coerce.with = as.numeric),
                              depends.on='modoTrk',
                              depends.FUN=function(value) value=='fixed'),
                         list(type="gedit",
                              name='H',
                              label='H (m)',
                              width=3,
                              text="0", 
                              coerce.with = as.numeric)
                         )
                       )
                     
shdList <- list(type="ggroup",
                label = "Sombra",
                horizontal=FALSE,
                children=list(
                  shdOptionsList,
                  structList,
                  distList)
                )
                  
                  ##     ),
                  ## list(type="fieldset",
                  ##      columns=2,
                  ##      labels='Optimizacion de Separaciones',
                  ##      children=list(
                  ##        list(type="gedit",
                  ##             text="",
                  ##             width=3, 
                  ##             name='minLew',
                  ##             label='Lew minimo (m)',
                  ##             coerce.with = as.numeric),
                  ##        list(type="gedit",
                  ##             text="",
                  ##             width=3, 
                  ##             name='maxLew',
                  ##             label='Lew maximo (m)',
                  ##             coerce.with = as.numeric),
                  ##        list(type="gedit",
                  ##             text="",
                  ##             width=3, 
                  ##             name='minLns',
                  ##             label='Lns minimo (m)',
                  ##             coerce.with = as.numeric),
                  ##        list(type="gedit",
                  ##             name='maxLns',
                  ##             label='Lns maximo (m)',
                  ##             width=3,
                  ##             text="", 
                  ##             coerce.with = as.numeric),
                  ##                       #depends.on='modoSeg',
                  ##                       #depends.FUN=function(value) value=='doble'),
                  ##        list(type="gedit",
                  ##             name='minD',
                  ##             label='D minimo (m)',
                  ##             text="",
                  ##             width=3, 
                  ##             coerce.with = as.numeric),
                  ##                       #depends.on='modoSeg',
                  ##                       #depends.FUN=function(value) value=='est'),
                  ##        list(type="gedit",
                  ##             name='maxD',
                  ##             label='D maximo (m)',
                  ##             text="",
                  ##             width=3, 
                  ##             coerce.with = as.numeric),
                  ##        list(type="gedit",
                  ##             name='res',
                  ##             label='Resolución (m)',
                  ##             text="1",
                  ##             width=3, 
                  ##             coerce.with = as.numeric)
                  ##        )
                  ##      )
                 

prodGCPVgui<-function(toolkit='tcltk'){	
  data(RedEstaciones)
  ## stopifnot('gWidgets' %in% .packages()) 
  stopifnot(toolkit %in% c('tcltk', 'RGtk2'))
  if (toolkit=='tcltk') {
    stopifnot('gWidgetstcltk' %in% .packages())
    options("guiToolkit"="tcltk")
  } else  {
    stopifnot('gWidgetsRGtk2' %in% .packages())
    options("guiToolkit"="RGtk2")}
  ##Creamos la pantalla
  w <- gwindow("Calculo de Productividad de un GCPV", height=900, width=600)
  g <- ggroup(horizontal = FALSE, cont = w)
  g1 <- ggroup(cont=g, expand=TRUE)
  g2 <- ggroup(cont=g, horizontal=TRUE)

  fl <- gnotebook(container=g1, expand=TRUE)
  fl1 <- gformlayout(radList, cont = fl, expand=TRUE)
  fl2 <- gformlayout(systemList, cont = fl, expand=TRUE)
  fl3 <- gformlayout(shdList, cont = fl, expand=TRUE)
  pl <- ggraphics(container=fl)
  txt <- gtext(container=fl)
  names(fl) <- c('Radiation', 'System', 'Shade', 'Plot', 'Result')
  svalue(fl) <- 1 ##primera página

  addSpring(g2)
  asgFrame = gframe("Asignar resultado a:",cont=g2, anchor=c(-1,0), expand=TRUE)         
  asg<-gedit('result', cont=asgFrame)
  b <- gbutton("OK", cont = g2)

  addHandlerChanged(fl1[]$NomProv, function(h,...){
    x=svalue(h$obj)
    tryCatch(fl1[]$NomEst[]<-as.vector(subset(RedEstaciones,
NomProv==x)$NomEst),error=function(e){})
  })


##Función que maneja el botón OK
  addHandlerChanged(b, function(h,...){
    outRad <- svalue(fl1)
    outSystem <- svalue(fl2)
    outShade <- svalue(fl3)

    lat=outRad$lat
    alfa=outSystem$alfa
    beta=outSystem$beta
    betaLim=outSystem$betaLim
    iS=outSystem$iS
    alb=outSystem$alb
    keep.night=TRUE
    sample=outRad$sample

    modeTrk=outSystem$modeTrk
    
    modeRad=outRad$modeRad
    
    outMAPA=subset(RedEstaciones,(NomProv==outRad$NomProv&NomEst==outRad$NomEst))
    
    mapa=list(prov=outMAPA$Provincia, est=outMAPA$Estacion, start=outRad$start, end=outRad$end)
		
    G0dm=as.vector(unlist(outRad[c('Ene','Feb','Mar','Abr','May','Jun',
      'Jul','Ago','Sep','Oct','Nov','Dic')]))
    Ta=as.vector(unlist(outRad[c('EneTa','FebTa','MarTa','AbrTa','MayTa','JunTa',
      'JulTa','AgoTa','SepTa','OctTa','NovTa','DicTa')]))

    prom=list(G0dm=G0dm, Ta=Ta)
    
    prev<-outRad$prev
		
    bd<-list(file=outRad$BaseDatos, format=outRad$format, source=outRad$BaseDatos)
		
    module=outSystem[c('Vocn','Iscn','Vmn','Imn','Ncs','Ncp','CoefVT','TONC')]
    generator=outSystem[c('Nms','Nmp')]
    inverter=outSystem[c('Pinv','Vmin', 'Vmax','Gumb')]
    inverter$Ki=as.vector(unlist(outSystem[c('Ki0','Ki1','Ki2')]))
    effSys=outSystem[c('ModQual','ModDisp','OhmDC','OhmAC','MPP','TrafoMT','Disp')]
		
    modeShd=NULL
    if (outShade$sombra) modeShd=c(modeShd,'area')
    if (outShade$BT) modeShd=c(modeShd,'bt')
    if (outShade$prom) modeShd=c(modeShd,'prom')
    if (is.null(modeShd)) modeShd=''
    
    struct=outShade[c('W', 'L', 'Nrow', 'Ncol')]

##res=out$res
		
## if (out$SombOptim) {
##   distancias=switch(out$modoSeg,
##     doble={Lew=with(out,c(minLew,maxLew))
##            Lns=with(out,c(minLns,maxLns))
##            distancias=list(Lew=Lew,Lns=Lns)},
##     horiz={Lew=with(out,c(minLew,maxLew))
##            distancias=list(Lew=Lew)},
##     est={D=with(out,c(minD,maxD))
##          distancias=list(D=D)})
##   result<-with(out,
##                optimSombra(lat,G0dm,Temp,modoSeg,modoRad,
##                            previo, MAPA,BaseDatos, FormatoFecha,
##                            Nm, BetaLim,beta,alfa, iS,alb,
##                            modulo,generador,inversor, EffSys,
##                            modeShd, estruct,distancias))
##   plot(result)
## } else {
    distances=as.data.frame(outShade[c('Lew','Lns','D','H')])

    result <- prodGCPV(lat, modeTrk, modeRad,
                       prev, prom, mapa, bd,
                       sample, keep.night,
                       betaLim, beta, alfa, iS, alb,
                       module, generator, inverter, effSys,
                       modeShd, struct, distances)
    focus <- pl
    print(xyplot(result))
svalue(fl) <- 4                     #Muestro página del plot
    svalue(txt) <- capture.output(print(result))

    asignar=make.names(svalue(asg))
    assign(asignar,result,envir = .GlobalEnv)
  })
}
