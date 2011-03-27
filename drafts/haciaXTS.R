BD<-LeeMAPA(28,3,'01/01/2006','31/12/2008')
FechaBaseDatos=BD$Fecha; 
FormatoFecha="%d/%m/%Y"
fecha=as.Date(FechaBaseDatos,format=FormatoFecha, tz='CET')
BDx<-xts(BD[,-1],fecha)

#No funciona
Nm=60
tiempo<-seq(0,length=Nm*24,by=1/Nm)
hora<-trunc(tiempo)
minuto<-trunc((tiempo-hora)*60)
segundo<-trunc((tiempo-hora)*3600-minuto*60)


library(xts)

x=rnorm(365)
y=rnorm(365)
z=rnorm(365)
data=data.frame(x,y,z)
index=seq(as.Date('2009-01-01'),as.Date('2009-12-31'),by='day')

dataXTS<-xts(data,index)

xyplot(dataXTS)

xyplot(x~y,data=as.data.frame(dataXTS),groups=months(index))

library(tis)

dataTIS<-tis(data,start='2009-01-01',tif='daily')
plot(dataTIS)

dataFORT<-fortify.tis(dataTIS)
xyplot(x~y,groups=dayOfWeek(date),data=dataFORT)


#####

x<-zoo(rnorm(12), order.by=seq(as.Date("2000-1-1"), as.Date("2000-12-31"), by='day'))
startx<-unclass(as.POSIXct(start(x)))
startxUTC<-as.POSIXct(startx, tz='UTC', origin='1970-01-01')
endx<-unclass(as.POSIXct(end(x)))
endxUTC<-as.POSIXct(endx, tz='UTC', origin='1970-01-01')

#sin tantas vueltas
startUTC<-as.POSIXct('2000-1-1',tz='UTC')
endUTC<-as.POSIXct('2000-12-31',tz='UTC')
x<-zoo(data.frame(a=rnorm(12), b=rnorm(12), c=rnorm(12)), order.by=seq(startUTC, endUTC, by='day'))
#xx<-zoo(matrix(NA, ncol=dim(coredata(x))[2]), order.by=seq(start(x), end(x)+86400-1, by='10 min'))

sec=seq(start(x), end(x)+86400-1, by='10 min')
#as.numeric(mean(diff(indSol),0.3), units='hours')



nrep<-length(sec)/dim(x)[1]
xrep<-sapply(as.data.frame(coredata(x)),FUN=function(x)rep(x,each=nrep))
xx<-zoo(xrep, sec)
#coredata(xx)<-xrep
#names(xx)<-names(x)


#Otra forma, más lenta...
#pp<-MATCH(index(x), index(xx),nomatch=0)
#coredata(xx)[pp,]<-xrep
#xxx<-na.locf(xx)

eee<-readMAPA(prov=28, est=3, start='01/01/2004', end='31/12/2009')



####XTS


startUTC<-as.POSIXct('2000-1-1',tz='UTC')
endUTC<-as.POSIXct('2000-12-31',tz='UTC')
x<-xts(data.frame(a=rnorm(366), b=rnorm(366), c=rnorm(366)), order.by=seq(startUTC, endUTC, by='day'), tz='UTC')

by='hour'
sec=seq(start(x), end(x)+86400-1, by=by)
#as.numeric(mean(diff(indSol),0.3), units='hours')



nrep<-length(sec)/dim(x)[1]
xrep<-sapply(as.data.frame(coredata(x)),FUN=function(x)rep(x,each=nrep))
xx<-xts(xrep, order.by=sec, tz='UTC')
