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
calcSol <- function(lat, BTd, sample='hour', BTi, EoT=TRUE, keep.night=TRUE, method='michalsky'){

  if (missing(BTi)){
    solD<-fSolD(lat, BTd=BTd, method=method)
    solI<-fSolI(solD, sample=sample, EoT=EoT, keep.night=keep.night, method=method)
    match <- attr(solI, 'match')
    sample <- attr(solI, 'sample')
  } else { ##utilizo BTi
    BTd=unique(truncDay(BTi))
    solD <- fSolD(lat, BTd=BTd, method=method)
    solI <- fSolI(solD, BTi=BTi, EoT=EoT, keep.night=keep.night, method=method)
    match <- attr(solI, 'match')
    sample <- attr(solI, 'sample')
  }
  attr(solD, 'lat') <- NULL
  attr(solI, 'lat') <- NULL
  attr(solI, 'match') <- NULL
  attr(solI, 'sample') <- NULL
  result <- new('Sol',
                lat=lat,
                solD=solD,
                solI=solI,
                match=match,
                sample=sample,
                method=method)
  return(result)
}
