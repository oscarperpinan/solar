analyzeData <- function(x, na.rm = TRUE){
    .Deprecated("'tdr' package")
    myStats <- function(x){
        cbind(mean(x, na.rm = na.rm),
              median(x, na.rm = na.rm),
              sd(x, na.rm = na.rm),
              mad(x, na.rm = na.rm),
              IQR(x, na.rm = na.rm)
              )
    }

    xStats <- apply(x, 1, myStats)
    xStats <- zoo(t(xStats), index(x))
    names(xStats) <- c('mean', 'median', 'sd', 'mad', 'IQR')
    xStats
}
