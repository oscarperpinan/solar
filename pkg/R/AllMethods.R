 # Copyright (C) 2011, 2010 Oscar Perpiñán Lamigueiro
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
setGeneric('xyplot')
setGeneric('levelplot')
setGeneric('as.data.frame')

setGeneric('getData', function(object){standardGeneric('getData')})
setMethod('getData',##Solo definido para Meteo, de forma que siempre devuelve valores de partida
          signature=(object='Meteo'),
          definition=function(object){
            result=object@data
            return(result)
          }
          )

setGeneric('getG0', function(object){standardGeneric('getG0')})
setMethod('getG0',##Solo definido para Meteo, de forma que siempre devuelve valores de partida
          signature=(object='Meteo'),
          definition=function(object){
            result=getData(object)
            return(result$G0)
          }
          )

###Latitud
setGeneric('getLat', function(object, units='rad'){standardGeneric('getLat')})

setMethod('getLat',
          signature=(object='Sol'),
          definition=function(object, units='rad'){
            stopifnot(units %in% c('deg', 'rad'))
            res=switch(units,
              rad=d2r(object@lat),
              deg=object@lat)
            return(res)
          }
          )

setMethod('getLat',
          signature=(object='Meteo'),
          definition=function(object, units='rad'){
            stopifnot(units %in% c('deg', 'rad'))
            res=switch(units,
              rad=d2r(object@latData),
              deg=object@latData)
            return(res)
          }
          )
setMethod('getLat',
          signature=(object='G0'),
          definition=function(object, units='rad'){
            getLat(as(object, 'Sol'), units=units)
          }
          )

###Indices

setGeneric('indexD', function(object){standardGeneric('indexD')})
setMethod('indexD',
          signature=(object='Meteo'),
          definition=function(object){
            return(index(object@data))
          }
          )

setMethod('indexD',
          signature=(object='Sol'),
          definition=function(object){
            return(index(object@solD))
          }
          )

setMethod('indexD',
          signature=(object='G0'),
          definition=function(object){
            indexD(as(object, 'Sol'))
          }
          )


setGeneric('indexI', function(object){standardGeneric('indexI')})
setMethod('indexI',
          signature=(object='Sol'),
          definition=function(object){
            return(index(object@solI))
          }
          )

setGeneric('indexRep', function(object){standardGeneric('indexRep')})
setMethod('indexRep',
          signature=(object='Sol'),
          definition=function(object){
            return(object@match)
          }
          )

###as.zooM
setGeneric('as.zooM', function(object, complete=FALSE){standardGeneric('as.zooM')})

setMethod('as.zooM',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            return(object@G0dm)
          }
          )

setMethod('as.zooM',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE){
            res0 <- object@Gefdm
            if (complete) {
              res1 <- as.zooM(as(object, 'G0'))
              return(CBIND(res1, res0))
            } else {
              return(res0)
            }
          }
          )

setMethod('as.zooM',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE){
            res0 <- object@prodDm
            if (complete) {
              res1 <- as.zooM(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
            } else {
              return(res0)
            }
          }
          )

setMethod('as.zooM',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE){
            res0 <- object@prodDm
            if (complete) {
              res1 <- as.zooM(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
            } else {
              return(res0)
            }
          }
          )

###as.zooY
setGeneric('as.zooY', function(object, complete=FALSE){standardGeneric('as.zooY')})

setMethod('as.zooY',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            return(object@G0y)
          }
          )

setMethod('as.zooY',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE){
            res0 <- object@Gefy
            if (complete) {
              res1 <- as.zooY(as(object, 'G0'))
              return(CBIND(res1, res0))
            } else {
              return(res0)
            }
          }
          )

setMethod('as.zooY',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE){
            res0 <- object@prody
            if (complete) {
              res1 <- as.zooY(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
            } else {
              return(res0)
            }
          }
          )

setMethod('as.zooY',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE){
            res0 <- object@prody
            if (complete) {
              res1 <- as.zooY(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
            } else {
              return(res0)
            }
          }
          )

###as.zooD
setGeneric('as.zooD', function(object, complete=FALSE){standardGeneric('as.zooD')})

setMethod('as.zooD',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE){#complete esta por compatibilidad con los otros metodos
            res <- object@solD
            return(res)
          }
          )

setMethod('as.zooD',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'Sol'))
            res2 <- object@G0D
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('G0d', 'D0d', 'B0d')])}
          }
          )

setMethod('as.zooD',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'G0'), complete=TRUE)
            res2 <- object@GefD
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('Gefd', 'Defd', 'Befd')])
            }
          }
          )


setMethod('as.zooD',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'Gef'), complete=TRUE)
            res2 <- object@prodD
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('Eac', 'Edc', 'Yf')])
            }
          }
          )

setMethod('as.zooD',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'Gef'), complete=TRUE)
            res2 <- object@prodD
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('Eac', 'Qd', 'Yf')])
            }
          }
          )

###as.zooI
setGeneric('as.zooI',
           function(object, complete=FALSE, day=FALSE){standardGeneric('as.zooI')})

setMethod('as.zooI',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@solI
            if (day) {
              ind <- indexRep(object)
              res2 <- coredata(object@solD)[ind,]
              res0=coredata(res0)
              return(zoo(cbind(res0, res2), indexI(object)))
            } else {return(res0)}
          }
          )

setMethod('as.zooI',
          signature=(object='G0'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@G0I
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'Sol'), day=day))
              res0=coredata(res0)
              Ta <- coredata(object@Ta)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res2 <-coredata(object@G0D)[ind,]
                res <- zoo(cbind(res1, res2, res0, Ta), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res0, Ta), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('G0', 'B0', 'D0')])
            }
          }
          )

setMethod('as.zooI',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@GefI
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'G0'),
                                       complete=complete, day=day))
              res2 <- coredata(object@Theta)
              res0=coredata(res0)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res3 <-coredata(object@GefD)[ind,]
                res <- zoo(cbind(res1, res2, res3, res0), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res2, res0), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('Gef', 'Bef', 'Def')])
            }
          }
          )

setMethod('as.zooI',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@prodI
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'Gef'),
                                       complete=complete, day=day))
              res0=coredata(res0)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res2 <-coredata(object@prodD)[ind,]
                res <- zoo(cbind(res1, res2, res0), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res0), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('Pac', 'Pdc')])
            }
          }
          )

setMethod('as.zooI',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@prodI
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'Gef'),
                                       complete=complete, day=day))
              res0=coredata(res0)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res2 <-coredata(object@prodD)[ind,]
                res <- zoo(cbind(res1, res2, res0), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res0), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('Pac', 'Q')])
            }
          }
          )

###as.data.frameI
setGeneric('as.data.frameI',
           function(object, complete=FALSE, day=FALSE){standardGeneric('as.data.frameI')})

setMethod('as.data.frameI',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
            zoo0=as.zooI(object, complete=complete, day=day)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$day=doy(ind)##Incorporo dia, mes y año como columnas del data.frame
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
          }
          )

###as.data.frameD
setGeneric('as.data.frameD', function(object, complete=FALSE){standardGeneric('as.data.frameD')})

setMethod('as.data.frameD',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooD(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$day=doy(ind)##Incorporo dia, mes y año como columnas del data.frame
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
          }
          )

###as.data.frameM
setGeneric('as.data.frameM', function(object, complete=FALSE){standardGeneric('as.data.frameM')})

setMethod('as.data.frameM',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooM(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
          }
          )

###as.data.frameY
setGeneric('as.data.frameY', function(object, complete=FALSE){standardGeneric('as.data.frameY')})

setMethod('as.data.frameY',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooY(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$year=ind
            return(data0)
          }
          )

###show

setMethod('show', 'Meteo',
          function(object){
            cat('Object of class ', class(object),'\n\n')
            cat('Source of meteorological information: ')
            cat(paste(object@type, object@source, sep='-'),'\n')
            cat('Latitude of source: ',
                paste(round(getLat(object,'deg'), 1), 'degrees\n\n'))
            cat('Meteorological Data:\n')
            print(summary(getData(object)))
          }
          )

header <-function(object){
  cat('Object of class ', class(object),'\n\n')
  cat('Source of meteorological information: ')
  cat(paste(object@type, object@source, sep='-'),'\n\n')
  cat('Latitude of source: ',
      paste(round(getLat(as(object, 'Meteo'),'deg'), 1), 'degrees\n'))
  cat('Latitude for calculations: ',
      paste(round(getLat(object, 'deg'),1), 'degrees\n\n'))
}

setMethod('show', 'Sol',
          function(object){
            cat('Object of class ', class(object),'\n\n')
            cat('Latitude:',
                paste(round(getLat(object, 'deg'),1), 'degrees\n\n'))
            cat('Daily values:\n')
            print(summary(object@solD))
            cat('\nIntradaily values:\n')
            print(summary(object@solI))
          }
          )

setMethod('show', 'G0',
          function(object){
            header(object)
            cat('Monthly averages:\n')
            print(as.zooM(object))
            cat('\nYearly values:\n')
            print(as.zooY(object))          }
          )

## setMethod('show', 'Gef',
##           function(object){
##             header(object)
##             cat('Monthly averages (kWh/m²):\n')
##             print(object@Gefdm)
##             cat('\nYearly values (kWh/m²):\n')
##             print(object@Gefy)
##           }
##           )

setMethod('show', 'Gef',
          function(object){
            callNextMethod()
            cat('-----------------\n')
            cat('Mode of tracking: ', object@modeTrk,'\n')
            if (object@modeTrk=='fixed'){
              cat('    Inclination: ', object@angGen$beta, '\n')
              cat('    Orientation: ', object@angGen$alfa, '\n')
            } else {
              cat('    Inclination limit:', object@angGen$betaLim, '\n')
            }
            ## cat('Monthly averages (kWh/kWp):\n')
            ## print(object@prodDm)
            ## cat('\nYearly values (kWh/kWp):\n')
            ## print(object@prody)
          }
          )

setMethod('show', 'ProdGCPV',
          function(object){
            callNextMethod()
            cat('-----------------\n')
            cat('Generator:\n')
            cat('    Modules in series: ', object@generator$Nms, '\n')
            cat('    Modules in parallel: ', object@generator$Nmp, '\n')
            cat('    Nominal power (kWp): ',
                round(object@generator$Pg/1000, 1), '\n\n')

            ## cat('Monthly averages (kWh/kWp):\n')
            ## print(object@prodDm)
            ## cat('\nYearly values (kWh/kWp):\n')
            ## print(object@prody)
          }
          )

setMethod('show', 'ProdPVPS',
          function(object){
            callNextMethod()
            cat('-----------------\n')
            cat('Pump:\n')
            cat('    Qn: ', object@pump$Qn, '\n')
            cat('    Stages: ', object@pump$stages, '\n')
            cat('Height (m): ', object@H, '\n')
            cat('Generator (Wp): ', object@Pg, '\n')
            ## cat('Monthly averages:\n')
            ## print(object@prodDm)
            ## cat('\nYearly values:\n')
            ## print(object@prody)
          }
          )

###XYPLOT

## myTheme <- custom.theme.2(pch=19, cex=0.8, alpha=0.6, region=rev(brewer.pal(9, 'YlOrRd')))
## myTheme$strip.background$col='transparent'
## lattice.options(default.theme=myTheme)

solaR.theme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
solaR.theme$strip.background$col='lightgray'
solaR.theme$strip.shingle$col='transparent'


setMethod('xyplot',
          signature=c(x='formula', data='zoo'),
          definition=function(x, data, par.settings=solaR.theme,...){            
            data0=as.data.frame(data)
            ind=index(data)
            data0$day=doy(ind) ##Incorporo dia, mes y año para facilitar la formula.
            data0$month=month(ind)
            data0$year=year(ind)
            if (!('w' %in% names(data0))){
              data0$w=h2r(hms(ind)-12) ##hora solar en radianes
            }
            xyplot(x, data0, par.settings=par.settings,
                   strip=strip.custom(strip.levels=c(TRUE, TRUE)),...)
          }
          )

setMethod('xyplot',
          signature=c(x='formula', data='Meteo'),
          definition=function(x, data, ...){
            data0=getData(data)
            xyplot(x, data0, ...) ##es un zoo, luego ahora aplica el método data='zoo'
          }
          )

setMethod('xyplot',
          signature=c(x='formula', data='Sol'),
          definition=function(x, data, ...){
            data0=as.zooI(data, complete=TRUE, day=TRUE)
            xyplot(x, data0, ...)
          }
          )

setMethod('xyplot',
          signature=c(x='formula', data='G0'),
          definition=function(x, data, ...){
            data0=as.zooI(data, complete=TRUE, day=TRUE)
            xyplot(x, data0, ...)
          }
          )


setMethod('xyplot',
          signature=c(x='Meteo', data='missing'),
          definition=function(x, data, par.settings=solaR.theme,
            strip=FALSE, strip.left=TRUE,...){
            x0=getData(x)
            N=ncol(x0)
            xyplot(x0, par.settings=par.settings,
                   layout=c(1, N),
                   scales=list(cex=0.6, rot= 0),
                   strip=strip, strip.left=TRUE,
                   par.strip.text=list(cex=0.6),
                   ...)
          }
          )

setMethod('xyplot',
          signature=c(x='G0', data='missing'),
          definition=function(x, data, par.settings=solaR.theme, ...){
            x0=as.zooD(x, complete=FALSE)
            xyplot(x0, par.settings=par.settings,
                   ...,
                   superpose=TRUE,
                   auto.key=list(space='right'),
                   ylab='Wh/m²')
          }
          )

setMethod('xyplot',
          signature=c(x='ProdGCPV', data='missing'),
          definition=function(x, data, par.settings=solaR.theme, ...){
            x0=as.zooD(x, complete=FALSE)
            xyplot(x0, layout=c(1, 3),
                   strip=FALSE,
                   strip.left=TRUE,
                   par.settings=par.settings, ...)
          }
          )

setMethod('xyplot',
          signature=c(x='ProdPVPS', data='missing'),
          definition=function(x, data, par.settings=solaR.theme, ...){
            x0=as.zooD(x, complete=FALSE)
            xyplot(x0, layout=c(1, 3),
                   strip=FALSE,
                   strip.left=TRUE,
                   par.settings=par.settings, ...)
          }
          )

###LEVELPLOT
setMethod('levelplot',
          signature=c(x='formula', data='zoo'),
          definition=function(x, data,
            par.settings=solaR.theme,
##            panel=panel.levelplot.raster, interpolate=TRUE,...){
            ...){
            data0=as.data.frame(data)
            ind=index(data)
            data0$day=doy(ind) ##Incorporo dia, mes y año para facilitar la formula.
            data0$month=month(ind)
            data0$year=year(ind)
            if (!('w' %in% names(data0))){
              data0$w=h2r(hms(ind)-12) ##hora solar en radianes
            }
            levelplot(x, data0, par.settings=par.settings,
 ##                     panel=panel, interpolate=interpolate,
                      ...)
          }
          )


setMethod('levelplot',
          signature=c(x='formula', data='Meteo'),
          definition=function(x, data, ...){
            data0=getData(data)
            levelplot(x, data0, ...)##data0 es un zoo, luego ahora aplica el método data='zoo'
          }
          )

setMethod('levelplot',
          signature=c(x='formula', data='Sol'),
          definition=function(x, data, ...){
            data0=as.zooI(data, complete=TRUE, day=TRUE)
            levelplot(x, data0, ...)
          }
          )

setMethod('levelplot',
          signature=c(x='formula', data='G0'),
          definition=function(x, data, ...){
            data0=as.zooI(data, complete=TRUE, day=TRUE)
            levelplot(x, data0, ...)
          }
          )


###Métodos para SHADE
setMethod('as.data.frame', 'Shade',
          function(x, ...){
            res <- cbind(x@distances,
                         data.frame(FS=x@FS, GRR=x@GRR, Yf=x@Yf)
                         )
            return(res)
          }
          )

setMethod('show', 'Shade',
          function(object){
            header(object)
            cat('Dimensions of structure:\n')
            print(object@struct)
            cat('Shade calculation mode:\n')
            print(object@modeShd)
            cat('Productivity without shadows:\n')
            print(as(object, 'ProdGCPV'))##Referencia, sin sombras
            cat('Summary of results:\n')
            print(summary(as.data.frame(object)))
          }
          )


setMethod('xyplot',
          signature=c(x='formula', data='Shade'),
          definition=function(x, data, ...){
            data0=as.data.frame(data)
            xyplot(x, data0, ...)
          }
          )

setGeneric('shadeplot', function(x, ...)standardGeneric('shadeplot'))

setMethod('shadeplot', signature(x='Shade'),
          function(x, 
                   main='',
                   xlab=expression(L[ew]),
                   ylab=expression(L[ns]),
                   n=9, ...){
            red=x@distances
            FS.loess=x@FS.loess
            Yf.loess=x@Yf.loess
            struct=x@struct
            mode=x@modeTrk
            if (mode=='two'){
              Lew=seq(min(red$Lew),max(red$Lew),length=100)
              Lns=seq(min(red$Lns),max(red$Lns),length=100)
              Red=expand.grid(Lew=Lew,Lns=Lns)
              FS=predict(FS.loess,Red)
              Red$FS=as.numeric(FS)
              AreaG=with(struct,L*W)
              GRR=Red$Lew*Red$Lns/AreaG
              Red$GRR=GRR
              FS.m<-matrix(1-FS,
                           nrow=length(Lew),
                           ncol=length(Lns))
              GRR.m<-matrix(GRR,
                            nrow=length(Lew),
                            ncol=length(Lns))
              niveles=signif(seq(min(FS.m),max(FS.m),l=n+1),3)
              pruebaCB<-("RColorBrewer" %in% .packages())
              if (pruebaCB) {
                paleta=rev(brewer.pal(n, 'YlOrRd'))
              } else {
                paleta=rev(heat.colors(n))}
              par(mar=c(4.1,4.1,2.1,2.1))
              ##alternativa con levelplot y layer
              ## levelplot((1-FS)~Lew*Lns,  data=Red, aspect='iso',
              ##           xlab=xlab, ylab=ylab, main=main,
              ##           subscripts=TRUE, contour=TRUE, lwd=0.6) + 
              ##     layer(panel.contourplot(Lew, Lns, GRR,
              ##                             lty=3, labels=TRUE,
              ##                             region=FALSE, contour=TRUE,
              ##                             subscripts=TRUE), data=Red)
              filled.contour(x=Lew,y=Lns,z=FS.m,#...,
                             col=paleta, #levels=niveles,
                             nlevels=n,
                             plot.title=title(xlab=xlab,
                               ylab=ylab, main=main),
                             plot.axes={
                               axis(1);axis(2);
                               contour(Lew, Lns, FS.m,
                                       nlevels=n, #levels=niveles,
                                       col="black", labcex=.8,  add=TRUE)
                               contour(Lew, Lns, GRR.m,
                                       col="black", lty=3, labcex=.8, add=TRUE)
                               grid(col="white",lty=3)},
                             key.title=title("1-FS",cex.main=.8))
            }
            if (mode=='horiz') {
              Lew=seq(min(red$Lew),max(red$Lew),length=100)
              FS=predict(FS.loess,Lew)
              GRR=Lew/struct$L
              plot(GRR,1-FS,main=main,type='l',...)
              grid()    }
            if (mode=='fixed'){
              D=seq(min(red$D),max(red$D),length=100)
              FS=predict(FS.loess,D)
              GRR=D/struct$L
              plot(GRR,1-FS,main=main,type='l',...)
              grid()    }
          }
          )

####LOSSES
setGeneric('losses', function(object){standardGeneric('losses')})

setMethod('losses',
          signature=(object='Gef'),
          definition=function(object){ 
            dat <- as.data.frameY(object, complete=TRUE)
            isShd=('Gef0d' %in% names(dat)) ##is there shadows?
            if (isShd) {
              shd <- with(dat, mean(1-Gefd/Gef0d))
              eff <- with(dat, mean(1-Gef0d/Gd))
            } else {
              shd <- 0
              eff <- with(dat, mean(1-Gefd/Gd))
            }
            result <- data.frame(id=c('Shadows', 'AoI'), values=c(shd, eff))
            result
          }
          )

setMethod('losses',
          signature=(object='ProdGCPV'),
          definition=function(object){
            DayOfMonth=c(31,28,31,30,31,30,31,31,30,31,30,31) ###OJO
            dat <- as.data.frameY(object, complete=TRUE)
            module0=object@module
            module0$CoefVT=0 ##No losses with temperature
            ## p0 <- prodGCPV(lat=object@lat, modeTrk=object@modeTrk,
            ##                modeRad='prev', prev=object,
            ##                module=module0, generator=object@generator,
            ##                inverter=object@inverter, effSys=object@effSys)
            ## p0Y <- as.data.frameY(p0)
            ## temp <- mean(1-dat$Edc/p0Y$Edc)
            Pg=object@generator$Pg
            Nm=1/sample2Hours(object@sample)
            datI <- as.zooI(object, complete=TRUE)
            if (object@type=='prom'){
              YfDC0=sum(monthlySum(datI$Vmpp*datI$Impp)/Pg*DayOfMonth)
              YfAC0=sum(monthlySum(datI$Pdc*datI$EffI)/Pg*DayOfMonth)
            } else {
              YfDC0 <- yearlySum(datI$Vmpp*datI$Impp)/Pg
              YfAC0 <- yearlySum(datI$Pdc*datI$EffI)/Pg
            }
            gen <- mean(1-YfDC0/dat$Gefd)
            YfDC <- dat$Edc/Pg*1000
            DC=mean(1-YfDC/YfDC0)
            inv=mean(1-YfAC0/YfDC)
            AC=mean(1-dat$Yf/YfAC0)
            result0 <- losses(as(object, 'Gef'))
            result1 <- data.frame(id=c('Generator', 'DC', 'Inverter', 'AC'),
                                  values=c(gen, DC, inv, AC))
            result <- rbind(result0, result1)
            result
          }
          )

###compareLosses
setGeneric('compareLosses', signature='...', function(...){standardGeneric('compareLosses')})

setMethod('compareLosses', 'ProdGCPV',
          definition=function(...){
            dots <- list(...)
            nms0 <- substitute(list(...))
            if (!is.null(names(nms0))){ ##estamos dentro de do.call
              nms <- names(nms0[-1])
            } else {
              nms <- as.character(nms0[-1])
            }
            foo <- function(object, label){
              yY <- losses(object)
              yY <- cbind(yY, name=label)
              yY
            }
            cdata <- mapply(FUN=foo, dots, nms, SIMPLIFY=FALSE)
            z <- do.call(rbind, cdata)
            z$id <- ordered(z$id, levels=c('Shadows', 'AoI', 'Generator', 'DC', 'Inverter', 'AC'))
            p <- dotplot(id~values*100, groups=name, data=z,
                         par.settings=solaR.theme, type='b',
                         auto.key=list(corner=c(0.95,0.2), cex=0.7), xlab='Losses (%)')
            print(p)
            return(z)
          }
          )


####COMPARE
setGeneric('compare', signature='...', function(...){standardGeneric('compare')})

compareFunction <- function(..., vars){
  dots <- list(...)
  nms0 <- substitute(list(...))
  if (!is.null(names(nms0))){ ##estamos dentro de do.call
    nms <- names(nms0[-1])
  } else {
    nms <- as.character(nms0[-1])
  }
  foo <- function(object, label){
    yY <- colMeans(as.data.frameY(object, complete=TRUE)[vars])
    yY <- cbind(stack(yY), name=label)
    yY
  }
  cdata <- mapply(FUN=foo, dots, nms, SIMPLIFY=FALSE)
  z <- do.call(rbind, cdata)
  z$ind <- ordered(z$ind, levels=vars)
  p <- dotplot(ind~values, groups=name, data=z, type='b',
               par.settings=solaR.theme)
  print(p+glayer(panel.text(x[length(x)], y[length(x)],
                            label=group.value, cex=0.7, pos=3, srt=45)))
  return(z)
}


setMethod('compare',
          signature='G0',
          definition=function(...){
            vars <- c('D0d', 'B0d', 'G0d')
            res <- compareFunction(..., vars=vars)
            return(res)
          }
          )

setMethod('compare',
          signature='Gef',
          definition=function(...){
            vars <- c('Defd', 'Befd', 'Gefd')
            res <- compareFunction(..., vars=vars)
            return(res)
          }
          )

setMethod('compare',
          signature='ProdGCPV',
          definition=function(...){
            vars <- c('G0d', 'Gefd', 'Yf')
            res <- compareFunction(..., vars=vars)
            return(res)
          }
          )

###merge y horizon
setGeneric('mergesolaR', signature='...', function(...){standardGeneric('mergesolaR')})

fooMeteo <- function(object, var){yY <- getData(object)[,var]}

fooG0 <- function(object, var){yY <- as.zooD(object)[,var]}

mergeFunction <- function(..., foo, var){
  dots <- list(...)
  dots <- lapply(dots, as, class(dots[[1]])) ##el primer elemento es el que dicta la clase a todos
  nms0 <- substitute(list(...))
  if (!is.null(names(nms0))){ ##estamos dentro de do.call
    nms <- names(nms0[-1])
  } else { ##llamada convencional
    nms <- as.character(nms0[-1])
  }
  cdata <- sapply(dots, FUN=foo, var, simplify=FALSE)
  names(cdata) <- nms
  z <- do.call(merge, cdata)
  z
}

setMethod('mergesolaR', 
          signature='Meteo',
          definition=function(...){
            res <- mergeFunction(..., foo=fooMeteo, var='G0')
            res
          }
          )

setMethod('mergesolaR', 
          signature='G0',
          definition=function(...){
            res <- mergeFunction(..., foo=fooG0, var='G0d')
            res
          }
          )

setMethod('mergesolaR', 
          signature='Gef',
          definition=function(...){
            res <- mergeFunction(..., foo=fooG0, var='Gefd')
            res
          }
          )

setMethod('mergesolaR', 
          signature='ProdGCPV',
          definition=function(...){
            res <- mergeFunction(..., foo=fooG0, var='Yf')
            res
          }
          )

setMethod('mergesolaR', 
          signature='ProdPVPS',
          definition=function(...){
            res <- mergeFunction(..., foo=fooG0, var='Yf')
            res
          }
          )

## setGeneric('horizonsolaR', signature='...', function(...){standardGeneric('horizonsolaR')})

## setMethod('horizonsolaR',
##           signature='Meteo',
##           definition=function(...){
##             z <- mergesolaR(..., var='G0')
##             z <- z-rowMeans(z, na.rm=1)
##             horizonplot(z, colorkey=TRUE)
##           }
##           )

## setMethod('horizonsolaR',
##           signature='G0',
##           definition=function(...){
##             z <- mergesolaR(..., var='G0d')
##             z <- z-rowMeans(z, na.rm=1)
##             horizonplot(z, colorkey=TRUE)
##           })


## setMethod('horizonsolaR',
##           signature='Gef',
##           definition=function(...){
##             z <- mergesolaR(..., var='Gefd')
##             z <- z-rowMeans(z, na.rm=1)
##             horizonplot(z, colorkey=TRUE)
##           })

## setMethod('horizonsolaR',
##           signature='ProdGCPV',
##           definition=function(...){
##             z <- mergesolaR(..., var='Yf')
##             z <- z-rowMeans(z, na.rm=1)
##             horizonplot(z, colorkey=TRUE)
##           })

###splom
## splomsolaR <- function(x, ...){
##   splom(x,
##         panel=panel.hexbinplot,
##         diag.panel = function(x, ...){
##           yrng <- current.panel.limits()$ylim
##           d <- density(x, na.rm=TRUE)
##           d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
##           panel.lines(d)
##           diag.panel.splom(x,...)
##         },
##         lower.panel = function(x, y, ...){
##           panel.hexbinplot(x, y, ...)
##           panel.loess(x, y, ..., col = 'red')
##         },
##         pscale=0, varname.cex=0.7
##         )
##   }

## setMethod('splom',
##           signature='Meteo',
##           definition=function(x, ...){
##             df <- as.data.frame(getData(x))
##             splomsolaR(df)
##           }
##           )



## setGeneric('compareSplom', signature='...', function(...){standardGeneric('compareSplom')})

## setMethod('compareSplom',
##           signature='Meteo',
##           definition=function(...){
##             z <- mergesolaR(..., var='G0')
##             df <- as.data.frame(z)
##             splomsolaR(df)
##           }
##           )

## setMethod('compareSplom',
##           signature='G0',
##           definition=function(...){
##             z <- mergesolaR(..., var='G0d')
##             df <- as.data.frame(z)
##             splomsolaR(df)
##           }
##           )

## setMethod('compareSplom',
##           signature='Gef',
##           definition=function(...){
##             z <- mergesolaR(..., var='Gefd')
##             df <- as.data.frame(z)
##             splomsolaR(df)
##           }
##           )

## setMethod('compareSplom',
##           signature='ProdGCPV',
##           definition=function(...){
##             z <- mergesolaR(..., var='Yf')
##             df <- as.data.frame(z)
##             splomsolaR(df)
##           }
##           )

##WINDOW
## setGeneric('window')

## ## start <- as.POSIXct('2011-11-01 12:00:00')
## ## end <- as.POSIXct('2011-12-13 16:00:00')

## setMethod('window',
##           signature='Meteo',
##           definition=function(x, start, end,...){
##             if (!is.null(start)) start <- truncDay(start)
##             if (!is.null(end)) end <- truncDay(end)+86400-1
##             x@data <- window(x@data, start=start, end=end, ...)
##             x
##           }
##           )

## setMethod('window',
##           signature='Sol',
##           definition=function(x, start, end, ...){
##             if (!is.null(start)) start <- truncDay(start)
##             if (!is.null(end)) end <- truncDay(end)+86400-1
##             solI <- x@solI
##             idxI <- index(solI)
##             match <- x@match
##             if (is.null(start)){
##               if (is.null(end)){
##                 wIdx <- seq_along(idxI)
##               } else {
##                 wIdx <- which(idxI <= end)
##               }
##             } else {
##               if (is.null(end)){
##                 wIdx <- which(idxI >= start)
##               } else {
##                 wIdx <- which(idxI >= start & idxI <= end)
##               }}
##             x@solI <- solI[wIdx,]
##             x@match <- match[wIdx]
##             x@solD <- window(x@solD, start=start, end=end)
##             x
##             }
##           )

## setMethod('window',
##           signature='G0',
##           definition=function(x, start, end, ...){
##             if (!is.null(start)) start <- truncDay(start)
##             if (!is.null(end)) end <- truncDay(end)+86400-1
##             sol <- window(as(x, 'Sol'), start=start, end=end, ...) ##Sol method
##             meteo <- window(as(x, 'Meteo'), start=start, end=end, ...) ##Meteo method
##             g0Iw <- window(x@G0I, start=start, end=end,...) ##zoo method
##             Taw <- window(x@Ta, start=start, end=end,...) ##zoo method
##             ##GENERAR G0d, G0dm, G0dy
##             g0dw <- window(x@G0D, start=start, end=end)
## ##            g0dmw <- window(x@G0dm, start=as.yearmon(start), end=as.yearmon(end))
## ##            g0yw <- window(x@G0y, start=year(start), end=year(end))
##             result <- new('G0',
##                           meteo,
##                           sol,
##                           G0D=g0dw,
##                           G0dm=g0dmw,
##                           G0y=g0yw,
##                           G0I=g0Iw,
##                           Ta=Taw)
##             result
##           }
##           )
