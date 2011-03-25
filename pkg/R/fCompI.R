 # Copyright (C) 2011, 2010, 2009 Oscar Perpiñán Lamigueiro
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
fCompI<-function(sol, compD, G0I, corr='none', f){
  
###Indices temporales
  if (class(sol)=='Sol') { ##sol viene de calcSol
    solI <- sol@solI
    mtch <- sol@match
    sample <- sol@sample
  } else { ##sol es zoo, resultado de fSolI
    solI <- sol
    mtch <- attr(sol, 'match')
    sample <- attr(sol, 'sample')
  }

  indSol<-index(solI)

  rd=coredata(solI$rd)
  rg=coredata(solI$rg)
  aman=coredata(solI$aman)
  Bo0=coredata(solI$Bo0)
  cosThzS=coredata(solI$cosThzS)

  if (missing(G0I)) { ##Utilizo compD

    comp.rep<-data.frame(compD)[mtch, c('Ktd', 'G0d', 'D0d', 'B0d')]    

    ##Componentes        
    D0d=comp.rep$D0d
    G0d=comp.rep$G0d
    B0d=comp.rep$B0d

    ##Calculos
    D0<-D0d*rd*aman
    G0<-G0d*rg*aman
    B0<-G0-D0

    ##Pongo a NA todos los valores nulos o negativos.
    neg=(B0<=0)|(D0<=0)|(G0<=0)
    is.na(G0) <- neg
    is.na(D0) <- neg
    is.na(B0) <- neg
    
    ##Normalizo para que se conserve el valor de radiación diaria
    day<-truncDay(indSol)               #Dia del año
    ##Nm=1/sample2Hours(sample)
    
    ##OJO cambiar función sum por trapz o simpson
    ##foo=function(x)sum(x,na.rm=1)/Nm
    ##normalizo para mantener el valor de radiacion diaria
    D0<-D0*D0d/ave(D0,list(day), FUN=function(x) P2E(x, sample)) 
    G0<-G0*G0d/ave(G0,list(day), FUN=function(x) P2E(x, sample)) 
    B0<-B0*B0d/ave(B0,list(day), FUN=function(x) P2E(x, sample))

	
    kt=G0/Bo0
    fd=D0/G0
  
  } else { ##Utilizo G0I
    
    ## if (class(G0I)=='Meteo') {
    ##   IrrData=getData(G0I)
    ## } else {
    ##   IrrData=G0I
    ## }                                   #Irrdata es un zoo

    if (corr!='none'){ 
      
      if (class(G0I)=='Meteo') {
        G0=coredata(getG0(G0I))
      } else {                          #G0I es un zoo
        if (NCOL(G0I)>1) {              ##¿Es multivariante
          G0=coredata(G0I$G0) # Si es así, trabajo sólo con la variable G0
        } else {
          G0=coredata(G0I)
        }
      }                                 

      is.na(G0) <- (G0>Bo0 | !aman)

      kt=G0/Bo0
	
      fd=switch(corr,
        EKDh=FdKtEKDh(kt),
        CLIMEDh=FdKtCLIMEDh(kt),
        BRL=FdKtBRL(kt, sol), ##Correlacion global-difusa intradiaria propuesta por Boland et al.
        user=f(kt, sol),      ##Correlación propuesta por el usuario
        stop('Wrong descriptor of the correlation fd-kt.')
        )

      D0=fd*G0
      B0=G0-D0


    } else { ##corr=='none', y por tanto G0 es multivariante con G0, D0 y B0

      if (class(G0I)=='Meteo') {
        IrrData=getData(G0I)
      } else {                          #G0I es un zoo
        IrrData=G0I
      }
      
      is.na(IrrData) <- (IrrData$G0>Bo0 | !aman)

      D0=coredata(IrrData$D0)
      B0=coredata(IrrData$B0)
      G0=coredata(IrrData$G0)


      
      kt=G0/Bo0
      fd=D0/G0

    }
  }
  result <- zoo(data.frame(kt, fd, G0, D0, B0), order.by=indSol)
  attr(result, 'match') <- mtch

  return(result)
}


