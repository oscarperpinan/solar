 # Copyright (C) 2010 Oscar Perpiñán Lamigueiro
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
checkG0Ta <- function(x, maxmin=FALSE){
  stopifnot('G0' %in% names(x))

  Tnames <- c('TempMax', 'TempMin', 'Ta')
  mtch <- Tnames %in% names(x)

  ##La condición es que esté Ta o que
  ##con la opción maxmin activada, estén TempMax Y TempMin
  condition <- (maxmin & all(mtch[1:2])) || mtch[3]

  if(!condition){
    x$Ta=25
    warning('Ambient temperature data is not available. A new column with a constant value has been added.')
  }
  return(x)
}

readMAPA<-function(prov, est, start, end, lat=0, format='%d/%m/%Y'){
  formatMAPA='%d/%m/%Y'
  if (format!='%d/%m/%Y') { #Cambio formato de fecha al que necesita mapa.es/siar
    start=format(as.Date(start, format=format), formatMAPA)
    end=format(as.Date(end, format=format), formatMAPA)}
  URL=paste('http://www.mapa.es/siar/exportador.asp?T=DD&P=',
    prov,'&E=',est,'&I=',
    start,'&F=',end,sep='')
  cat('Downloading data from www.mapa.es/siar...\n')
    
  BD<-read.table(URL,header=TRUE,skip=1,fill=TRUE,dec=',', as.is=TRUE)
  fecha<-as.POSIXct(BD$Fecha2, tz='UTC', format=formatMAPA)
  BD$G0<-BD$Radiacion/3.6*1000 #Cambio de unidades. G debe ir en Wh/m2, NO en kWh/m2
  BD$Radiacion<-NULL          #eliminamos esta variable
    
  BD.zoo<-zoo(BD[,-1], order.by=fecha)
  result<-new(Class='Meteo',
              latData=lat,                    #conseguir de geonames
              data=BD.zoo,
              type='mapa',
              source=paste('Est:', est, 'Prov:', prov)
              )
  result}

readG0dm<-function(G0dm, Ta=25, lat=0,
                   year= as.POSIXlt(Sys.Date())$year+1900, 
                   promDays=c(17,14,15,15,15,10,18,18,18,19,18,13), 
                   source=''){	
  index=as.POSIXct(paste(year, 1:12, promDays, sep='-'), tz='UTC')
  G0dm=as.numeric(G0dm)
  Ta=as.numeric(Ta)
  G0dm.zoo<-zoo(data.frame(G0=G0dm, Ta=Ta), index)
  result<-new(Class='Meteo',
              latData=lat,
              data=G0dm.zoo,
              type='prom',
              source=source
              )
  result
}

readBD<-function(file,  lat, 
                 format="%d/%m/%Y",
                 header=TRUE, fill=TRUE, dec='.', sep=';',
                 dates.col='date', 
                 source=file){
  stopifnot(is.character(dates.col) || is.numeric(dates.col))
  bd=read.table(file, header=header, fill=fill, dec=dec, sep=sep)
  bd <- checkG0Ta(bd, maxmin=TRUE)
  dates.bd=bd[[dates.col]]
  bd[[dates.col]] <- NULL ##No sigue adelante
  index=as.POSIXct(dates.bd, tz='UTC', format=format)
  bd.zoo<-zoo(bd, index)
  result<-new(Class='Meteo',
              latData=lat, 
              data=bd.zoo,
              type='bd',
              source=source
              )
  result
}

readBDi<-function(file,  lat, 
                  format="%d/%m/%Y %H:%M:%S",
                  header=TRUE, fill=TRUE, dec='.', sep=';',
                  time.col='time',
                  source=file){
  stopifnot(is.character(time.col) || is.numeric(time.col))
  bd=read.table(file, header=header, fill=fill, dec=dec, sep=sep)
  bd <- checkG0Ta(bd)
  time.bd=bd[[time.col]]
  bd[[time.col]] <- NULL ##No sigue adelante
  index=as.POSIXct(time.bd, tz='UTC', format=format)
  bd.zoo<-zoo(bd, index)
  result<-new(Class='Meteo',
              latData=lat, 
              data=bd.zoo,
              type='bdI',
              source=source
              )
  result
}

df2Meteo <- function(file, lat,
                     format="%d/%m/%Y",
                     dates.col='date',
                     source=''){
  stopifnot(is.character(dates.col) || is.numeric(dates.col))
  file <- checkG0Ta(file, maxmin=TRUE)
  dates.bd=file[[dates.col]]
  file[[dates.col]] <- NULL##No sigue adelante
  index=as.POSIXct(dates.bd, tz='UTC', format=format)
  bd.zoo<-zoo(file, index)
  result<-new(Class='Meteo',
              latData=lat, 
              data=bd.zoo,
              type='bd',
              source=source
              )
  result
}  

dfI2Meteo <- function(file, lat,
                     format="%d/%m/%Y %H:%M:%S",
                     time.col='time',
                     source=''){
  stopifnot(is.character(time.col) || is.numeric(time.col))
  file <- checkG0Ta(file)
  time.bd=file[[time.col]]
  file[[time.col]] <- NULL##No sigue adelante
  index=as.POSIXct(time.bd, tz='UTC', format=format)
  bd.zoo<-zoo(file, index)
  result<-new(Class='Meteo',
              latData=lat, 
              data=bd.zoo,
              type='bdI',
              source=source
              )
  result
}  

zoo2Meteo <- function(file, lat, source=''){
  ## if (is.null(dim(data))) {##Si data es un vector, asumo que es irradiancia global
  ##   dim(data) <- c(length(data), 1)
  ##   names(data) <- 'G0'
  ##   }
  file <- checkG0Ta(file, maxmin=TRUE)
  sample <- median(diff(index(file)))
  IsDaily <- as.numeric(sample, units='days')>=1
  type=ifelse(IsDaily, 'bd', 'bdI')
  result <- new(Class='Meteo',
                latData=lat,
                data=file,
                type=type,
                source=source)
  return(result)
}

