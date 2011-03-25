 # Copyright (C) 2010, 2009 Oscar Perpiñán Lamigueiro
 #
 # This program is free software you can redistribute it and/or
 # modify it under the terms of the GNU General Public License
 # as published by the Free Software Foundation either version 2
 # of the License, or (at your option) any later version.
 #
 # This program is distributed in the hope that it will be useful,
 # but WITHOUT ANY WARRANTY without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 # GNU General Public License for more details.
 #
 # You should have received a copy of the GNU General Public License
 # along with this program if not, write to the Free Software
 # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 #/
TargetDiagram<-function(x, end, ndays, ref=NULL, color=NULL, cex=0.8, ...){
  stopifnot(class(index(x))==class(end))

  if ('POSIXct' %in% class(end)) {
    nDays=ndays*24*3600
    } else {
      nDays=ndays
      }
  Analisis<-data.frame()
  for (i in 1:length(ndays)){
    start=end-(nDays[i]-1)
    if (start<start(x)) start=start(x)
    data.w<-window(x, start=start, end=end)
    AnalisisTemp<-analyzeData(data.w, ref)$err
    AnalisisTemp$ndays<-ndays[i]
    Analisis<-rbind(Analisis,AnalisisTemp)}
	
  Unitfc=factor(Analisis$Unit)     #elimino los levels que no utilizo
  NDaysfc<-factor(Analisis$ndays)
  Radio<-signif(quantile(Analisis$RMSD)[2:5],2)

  Circ<-expand.grid(Theta=seq(0,2*pi,length=100),R=Radio)
  Circ$X<-with(Circ,R*sin(Theta))
  Circ$Y<-with(Circ,R*cos(Theta))

  my.pch=1:nlevels(Unitfc)
	
  if (is.null(color)) {
	
    p<-xyplot(ME~RMSDc*sign(DifSD)|ndays,data=Analisis, cex=cex, ...,
              xlab=expression(sigma["D"]%.%"sign("*sigma^"*"*")"), #Explicado en ?plotmath
              ylab=expression(bar(D)),
              aspect='iso',
              col='black',
              strip=strip.custom(strip.levels=c(TRUE,TRUE),
                strip.names=c(TRUE,TRUE),
                bg='gray', fg='transparent'),
              panel=function(x,y,cex=cex,...){
                panel.text(x,y, labels=Unitfc, cex=cex, ...)
                panel.abline(h=0,v=0,lty=2,col='gray')
                for (i in 1:4){
                  with(Circ,
                       panel.xyplot(X[R==Radio[i]],Y[R==Radio[i]],
                                    lty=2,type='l',col='grey'))
                }
                panel.text(x=Radio,y=0,labels=signif(Radio,1),
                           pos=4,cex=0.6,...)},			
                                        #key = list(space = "right", 
                                        #		adj = 1,
                                        #		title='Unit',
                                        #		cex.title=1,
                                        #		text = list(levels(Unitfc)), 
                                        #		points = list(pch = my.pch), 
                                        #		rep = FALSE))
              )
  } else {
    my.fill=color          #utiliza my.fill para agrupar por periodos
    p<-xyplot(ME~RMSDc*sign(DifSD),data=Analisis, cex=cex, ...,
              xlab=expression(sigma["D"]%.%"sign("*sigma^"*"*")"), #Explicado en ?plotmath
              ylab=expression(bar(D)),
              aspect='iso',
              panel=function(x,y,cex=cex,...){
                col=my.fill[NDaysfc]
                panel.text(x,y, labels=Unitfc, col=col, cex=cex, ...)
                panel.abline(h=0,v=0,lty=2,col='gray')
                for (i in 1:4){
                  with(Circ,
                       panel.xyplot(X[R==Radio[i]],Y[R==Radio[i]],
                                    lty=2,type='l',col='grey'))}
                panel.text(x=Radio,y=0,labels=signif(Radio,2),pos=4,cex=0.6,...)},
              key = list(space = "right",  adj = 1,
                title='ndays',
                text = list(levels(NDaysfc)),
                points=list(pch=16, col=my.fill),
                                        #points = list(pch = c(NA,rep(16,nlevels(NDaysfc))), col = c(NA,my.fill)), 
					#text = list(c('Unit',levels(Unitfc))), 
					#points = list(pch = c(NA,my.pch)), 
					#text = list(c('Periodo',levels(NDaysfc))), 
					#points = list(pch = c(NA,rep(16,nlevels(NDaysfc))), col = c(NA,my.fill)), 
                rep = FALSE))
  }
  print(p)
  result<-list(plot=p, stat=Analisis)
}

