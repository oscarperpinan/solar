.onLoad <- function(lib, pkg){
  Sys.setenv(TZ='UTC')
  packageStartupMessage('Time Zone set to UTC.\n')
}
