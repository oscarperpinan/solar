 # Copyright (C) 2010, 2009 Oscar Perpiñán Lamigueiro
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
fBTd<-function(mode='prom',
                   year=as.POSIXlt(Sys.Date())$year+1900,
                   start=paste('01-01-',year,sep=''),
                   end=paste('31-12-',year,sep=''), 
                   format='%d-%m-%Y'){##,
##                   dates.bd, format.bd='%d-%m-%Y'){
  promDays<-c(17,14,15,15,15,10,18,18,18,19,18,13)
  dates=switch(mode,
  ##  bd=as.POSIXct(dates.bd, tz='UTC', format=format.bd),
    serie={
      start.<-as.POSIXct(start, format=format, tz='UTC')
      end.<-as.POSIXct(end, format=format, tz='UTC')
      res<-seq(start., end., by="1 day")
    },
    prom=as.POSIXct(paste(year, 1:12, promDays, sep='-'), tz='UTC')
    )
  dates
}
