x0<-as.POSIXct("2006-01-08 10:07:52", tz='UTC')

ch1<-format(x0, tz='Asia/Dubai') # 2006-01-08 14:07:52 pero en representación character
x2<-as.POSIXct(ch1, tz='Asia/Dubai') #2006-01-08 14:07:52 GST
#No se puede hacer directamente con as.POSIXct(x0, tz='Asia/Dubai') porque:
#as.POSIXct.default
#function (x, tz = "", ...) 
#{
#    if (inherits(x, "POSIXct")) 
 #       return(x) ###Devuelve x sin hacer caso de tz!!!!
x2-x0 # 0 horas de diferencia

###Otra forma, pero más rápido!!
###Sin embargo, parece que es más peligrosa...RNews 4/1
ch2<-as.POSIXlt(x0, tz='Asia/Dubai')
x3<-as.POSIXct(ch2)
x3-x0 #0 horas de diferencia
###Se entiende que sea más rápido mirando el código de format.POSIXct:
#function (x, format = "", tz = "", usetz = FALSE, ...) 
#{
#    if (!inherits(x, "POSIXct")) 
#        stop("wrong class")
#    if (missing(tz) && !is.null(tzone <- attr(x, "tzone"))) 
#       tz <- tzone
#    structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, 
#        ...), names = names(x))
#}
#Llama a as.POSIXlt, y luego a format.POSIXlt
#A su vez, format.POSIXlt realiza varias operaciones para hacer la conversión

x0<-as.POSIXct(rep("2006-01-08 10:07:52", 100000), tz='UTC')
system.time({ch1<-format(x0, tz='Asia/Dubai') ; x2<-as.POSIXct(ch1, tz='Asia/Dubai')})
system.time({ch2<-as.POSIXlt(x0, tz='Asia/Dubai');  x3<-as.POSIXct(ch2)})


####Otra utilidad
x1<-as.POSIXct(ch1, tz='UTC') #2006-01-08 14:07:52 UTC
x1-x0 #4 horas de diferencia
