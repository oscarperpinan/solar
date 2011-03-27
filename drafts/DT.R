library(data.table)

start=as.POSIXct('2010-01-01', tz='UTC')
end=as.POSIXct('2010-12-31', tz='UTC')
idx=seq(start, end, by='1 hour')
N=length(idx)
b=rnorm(N)
x=data.table(IDateTime(idx), a=seq(5, 100, l=N), b=b, c=2)
setkey(x, idate, itime)

system.time(xag <- x[j=list(A=sum(a), B=sum(b), C=sum(c)), by=idate])

system.time(print(xyplot(a~b, data=x)))

library(zoo)
library(solaR)##para truncDay
z=zoo(data.frame(a=seq(5, 100, l=N), b=b, c=2), idx)
system.time(zag <- aggregate(z, truncDay, sum))
system.time(print(xyplot(a~b, data=z)))

