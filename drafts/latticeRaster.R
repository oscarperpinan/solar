library(hexbin)##for splom

##Customization of lattice
xscale <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

myTheme=custom.theme.2(pch=19, cex=0.7,
  region=rev(brewer.pal(9, 'YlOrRd')))
myTheme$strip.background$col='transparent'##'lightgray'
myTheme$strip.shingle$col='transparent'
myTheme$strip.border$col='transparent'

##Auxiliary function for densityplot, histogram and bwplot
raster2dat <- function(x, FUN, maxpixels){
  nl <- nlayers(x)
  dat <- sampleRandom(x, maxpixels)
  dat <- as.data.frame(dat)
  names(dat) <- 1:nl
  dat <- stack(dat)
  z <- getZ(x)
  if (!missing(FUN) & !is.null(z)){
    FUN <- match.fun(FUN)   
    dat$ind <- factor(FUN(z))[dat$ind]
  } else {
    nms <- layerNames(x)
    nms <- reorder(factor(nms), 1:nl)
    dat$ind <- nms[dat$ind]
  }
  dat
}

##Densityplot
setGeneric('densityplot')
setMethod('densityplot',
          signature=c(x='RasterLayer', data='missing'),
          definition=function (x, maxpixels = 1e+05,
            xlab='', ylab='', main='', col='black',...){
            dat <- sampleRandom(x, maxpixels)
            densityplot(dat,
                        data=NULL,
                        pch='.', col=col,
                        xlab=xlab, ylab=ylab) 
          }
          )

  
setMethod('densityplot',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function (x, layer, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            par.settings=myTheme,...){
            if (!missing(layer)) x <- subset(x, layer)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              p <- densityplot(~values,
                               data=dat, groups=ind,
                               scales=list(x=list(relation='free'),
                                 y=list(relation='free', draw=FALSE)),
                               breaks=100, par.settings=par.settings, pch='.',
                               xlab=xlab, ylab=ylab,
                               panel=panel.superpose,
                               panel.groups=function(x, group.value, col.line,...){
                                 panel.densityplot(x, col.line=col.line, plot.points=FALSE,...)
                                 d <- density(x)
                                 i <- which.max(d$y)
                                 ltext(d$x[i],d$y[i],group.value,adj=c(0.3,0),col=col.line, cex=0.7)
                               }
                               )
            } else {
              p <- densityplot(x, maxpixels = maxpixels, main = main, xlab=xlab, ylab=ylab,...)
            }
            p
          }
          )

                 
##Histogram
setGeneric('histogram')
setMethod('histogram',
          signature=c(x='RasterLayer', data='missing'),
          definition=function (x, maxpixels = 1e+05, breaks=100,
            xlab='', main='', col='gray',...){
            dat <- sampleRandom(x, maxpixels)
            p <- histogram(dat,
                           data=NULL,
                           breaks=breaks, col=col,
                           xlab=xlab, main=main)
            p
          }
)  


setMethod('histogram',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function (x, layer, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xscale.components=xscale,
            yscale.components=yscale,
            par.settings=myTheme,
            ...) {
            if (!missing(layer)) x <- subset(x, layer)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              p <- histogram(~values|ind, data=dat,
                             as.table=as.table,
                             par.settings=par.settings,
                             between=between,
                             xscale.components=xscale.components,
                             yscale.components=yscale.components,
                             scales=list(x=list(relation='free'),
                               y=list(relation='free',
                                 draw=FALSE)),
                             breaks=100, col='gray',
                             xlab='',
                             strip.names=c(TRUE, TRUE))
            } else {
              p <- histogram(x, maxpixels = maxpixels, main = main, xlab=xlab,...)
            }
            p
          }
          )

###BOXPLOT and VIOLINPLOT
setGeneric('bwplot')
setMethod('bwplot',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function(x, layer, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            ##between=list(x=0.5, y=0.2),
            ##as.table=TRUE,
            ## xscale.components=xscale,
            ## yscale.components=yscale,
            par.settings=myTheme,
            ...) {
            if (!missing(layer)) x <- subset(x, layer)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              bwplot(values~ind,
                     data=dat, 
                     xlab=xlab, ylab=ylab,
                     horizontal=FALSE,
                     panel = function(..., box.ratio) {
                       panel.violin(..., col = "lightblue",
                                    varwidth = FALSE, box.ratio = box.ratio)
                       panel.bwplot(..., col='black',
                                    cex=0.8, pch='|', fill='gray', box.ratio = .1)
                     },
                     par.settings = list(box.rectangle=list(col='black'),
                       plot.symbol = list(pch='.', cex = 0.1)),
                     scales=list(x=list(rot=45, cex=0.5))
                     )
            }
          }
          )


##Splom
setGeneric('splom')

setMethod('splom',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function(x, maxpixels=1e5, plot.loess=FALSE, varname.cex=0.6,...){
            nms <- layerNames(x)
            dat <- sampleRandom(x, maxpixels)
            colnames(dat) <- nms
            diag.panel = function(x,...){
              yrng <- current.panel.limits()$ylim
              d <- density(x)
              d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
              panel.lines(d)
              diag.panel.splom(x,...)
            }
            lower.panel = function(x, y, plot.loess=plot.loess,...){
              panel.hexbinplot(x, y, ...)
              if (plot.loess) panel.loess(x, y, ..., col = 'red')
            }
            splom(~dat,
                  colramp=BTC,
                  varname.cex=varname.cex, 
                  plot.loess=plot.loess,
                  panel=panel.hexbinplot,
                  diag.panel=diag.panel,
                  lower.panel=lower.panel,
                  pscale=0, ...)
          }
          )


###Customized spplot
          ## mySPplot <- function(x,
          ##                      names.attr=names(x),
          ##                      par.settings=myTheme,
          ##                      between=list(x=0.5, y=0.2),
          ##                      as.table=TRUE,
          ##                      xscale.components=xscale,
          ##                      yscale.components=yscale,
          ##                      ...){
          ##   p <-spplot(x, names.attr=names.attr,
          ##              par.settings=par.settings,
          ##              scales=list(draw=TRUE),
          ##              between=between,
          ##              as.table=as.table,
          ##              xscale.components=xscale.components,
          ##              yscale.components=yscale.components)
          ##   p
          ## }

          ## setMethod('spplot',
          ##           'Raster',
          ##           function(obj, size=1e+05, ...){
          ##             smp<- sampleRegular(obj, size=size, asRaster=TRUE)
          ##             SP <- as(smp, 'SpatialGridDataFrame')
          ##             mySPplot(SP, names.attr=layerNames(obj), ...)
          ##             }
          ##             )
