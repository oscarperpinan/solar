.onLoad <- function(lib, pkg){
  Sys.setenv(TZ='UTC')
  cat('Time Zone set to UTC.\n')
  ## if (!require(lattice, quietly = TRUE)) 
  ##   warning('lattice package could not be loaded. Some funcionality of solaR may not be available')
  ## if (!require(latticeExtra, quietly = TRUE)) 
  ##   warning('latticeExtra package could not be loaded. Some funcionality of solaR may not be available')
  ## require(zoo, quietly = TRUE)
  ## require(methods)
}
