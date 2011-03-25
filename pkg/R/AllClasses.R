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
setOldClass('zoo')
setOldClass('loess')
setOldClass('difftime')

setClass(
         Class='Meteo', ##datos de radiación y temperatura
         representation=representation(
           latData='numeric',       #latitud, en grados, >0 si Norte
           data='zoo',          #datos, incluyendo G (Wh/m2) y Ta (ºC)
           type='character',    #a elegir entre 'prom', 'bd', 'mapa', 'bdI'
           source='character' #información sobre el origen de los datos
           ),
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='Sol', ##Angulos del sol
         representation=representation(
           lat='numeric',             #latitud, en grados, >0 si Norte
           solD='zoo',                #angulos diarios
           solI='zoo',                #angulos intradiarios
           match='numeric', #indices de solD que coinciden con días de solI
           sample='difftime'
           ),
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='G0',
         representation=representation(
           G0D='zoo',                #resultado de fCompD
           G0dm='zoo',               #aggregate, medias mensuales
           G0y='zoo',                #aggregate, valores anuales
           G0I='zoo',                #resultado de fCompI
           Ta='zoo'),                 #Temperatura ambiente intradiaria
         ##             sample='difftime'#según lo pasado a fSolI
         contains=c('Meteo','Sol'),
         validity=function(object) {
           return(TRUE)}
         )

setClass(
         Class='Gef',
         representation=representation(
           GefD='zoo',       #aggregate, valores diarios
           Gefdm='zoo',      #aggregate, medias mensuales
           Gefy='zoo',       #aggregate, valores anuales
           GefI='zoo',       #resultado de fInclin
           Theta='zoo',     #resultado de fTheta
           iS='numeric',     #indice de suciedad OJO ¿pasar a INTEGER?
           alb='numeric',    #albedo
           modeTrk='character',         #modo de seguimiento
           modeShd='character',         #modo de sombra
           angGen='list',               # incluye alfa, beta y betaLim
           struct='list',               #dimensiones de la estructura
           distances='data.frame'       #distancias entre estructuras
           ),
         contains='G0',
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='ProdGCPV',
         representation=representation(
           prodD='zoo',                 #aggregate, valores diarios
           prodDm='zoo',                #aggregate, medias mensuales
           prody='zoo',                 #aggregate, valores anuales
           prodI='zoo',                 #resultado de fProd
           module='list',
           generator='list',
           inverter='list',
           effSys='list'
           ),
         contains='Gef',
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='ProdPVPS',
         representation=representation(
           prodD='zoo',                 #aggregate, valores diarios
           prodDm='zoo',                #aggregate, medias mensuales
           prody='zoo',                 #aggregate, valores anuales
           prodI='zoo',                 #resultado de fProd
           Pg='numeric',
           H='numeric',
           pump='list',
           converter='list',
           effSys='list'
           ),
         contains='Gef',
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='Shade',
         representation=representation(
           FS='numeric',
           GRR='numeric',
           Yf='numeric',
           FS.loess='loess',
           Yf.loess='loess',
           modeShd='character',
           struct='list',
           distances='data.frame',
           res='numeric'
           ),
         contains='ProdGCPV',##Resultado de prodGCPV sin sombras (Prod0)
         validity=function(object) {return(TRUE)}
         )
