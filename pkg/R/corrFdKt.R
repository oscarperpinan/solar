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
### Medias mensuales
FdKtPage <- function(Ktd){##Page para medias mensuales
  Fd=1-1.13*Ktd
  return(Fd)
}

FdKtLJ <- function(Ktd){##Liu y Jordan para medias mensuales
  Fd=(Ktd<0.3)*0.595774 +
    (Ktd>=0.3 & Ktd<=0.7)*(1.39-4.027*Ktd+5.531*Ktd^2-3.108*Ktd^3)+
      (Ktd>0.7)*0.215246
  return(Fd)
  }

### Diarios
FdKtCPR <- function(Ktd){##Collares-Pereira y Rabl para diarios
  Fd=(0.99*(Ktd<=0.17))+
    (Ktd>0.17 & Ktd<0.8)*(1.188-2.272*Ktd+9.473*Ktd^2-21.856*Ktd^3+14.648*Ktd^4)+
      (Ktd>=0.8)*0.2426688      
  return(Fd)
}

FdKtEKDd <- function(Ktd, sol){##Erbs, Klein y Duffie para diarios
  if (class(sol)=='Sol') {
    ws <- as.data.frameD(sol)$ws
    } else {ws <- coredata(sol$ws)}
  
  WS1=(abs(ws)<1.4208)
  Fd=WS1*((Ktd<0.715)*(1-0.2727*Ktd+2.4495*Ktd^2-11.9514*Ktd^3+9.3879*Ktd^4)+
    (Ktd>=0.715)*(0.143))+
      !WS1*((Ktd<0.722)*(1+0.2832*Ktd-2.5557*Ktd^2+0.8448*Ktd^3)+
            (Ktd>=0.722)*(0.175))
  return(Fd)
}

FdKtCLIMEDd <- function(Ktd){##CLIMED1 para diarios
  Fd=(Ktd<=0.13)*(0.952)+
    (Ktd>0.13 & Ktd<=0.8)*(0.868+1.335*Ktd-5.782*Ktd^2+3.721*Ktd^3)+
      (Ktd>0.8)*0.141
  return(Fd)
}

### Horarios

FdKtEKDh <- function(kt){##Erbs, Klein y Duffie para horarios
  fd=(kt<=0.22)*(1-0.09*kt)+
    (kt>0.22 & kt<=0.8)*(0.9511-0.1604*kt+4.388*kt^2-16.638*kt^3+12.336*kt^4)+
      (kt>0.8)*0.165
  return(fd)
}

FdKtCLIMEDh <- function(kt){##CLIMED2 para horarios
  fd=(kt<=0.21)*(0.995-0.081*kt)+
    (kt>0.21 & kt<=0.76)*(0.724+2.738*kt-8.32*kt^2+4.967*kt^3)+
      (kt>0.76)*0.180
  return(fd)
}
  
FdKtBRL <- function(kt, sol){##Boland et al.

  if (class(sol)=='Sol') {
    sample=sol@sample
    aux <- as.zooI(sol, day=TRUE)
  } else {
    sample=median(diff(index(sol)))
  }

  idx=index(aux)
  w=coredata(aux$w)
  aman=coredata(aux$aman)
  Bo0=coredata(aux$Bo0)
  Bo0d=coredata(aux$Bo0d)
  AlS=coredata(aux$AlS)

  ##Calculo de Ktd
  G0=kt*Bo0
  day <- truncDay(idx)
  G0d=ave(G0, list(day),FUN=function(x) P2E(x, sample))
  Ktd=coredata(G0d/Bo0d)
  Ktd[!aman]=0
##Cálculo de la persistencia
  kt=zoo(kt, idx)
  ktNA=na.omit(kt)  
  iDay=truncDay(index(ktNA))
  x=rle(as.numeric(iDay))$lengths
  xLast=cumsum(x)
  xFirst=xLast-x+1

  lag1=lag(ktNA, 1, na.pad=TRUE)
  lag1[xLast] <- ktNA[xLast-1]
  lag_1=lag(ktNA, -1, na.pad=TRUE)
  lag_1[xFirst] <- ktNA[xFirst+1]
  K=cbind(kt, lag1, lag_1, media=1/2*(lag1+lag_1))
  pers=coredata(K$media)

  ##Cálculo de fd

  fd=(1+exp(-5.38+6.63*kt+0.006*r2h(w)-0.007*r2d(AlS)+1.75*Ktd+1.31*pers))^(-1)


  return(fd)
}
