##extraido de plot.TimeSeries.R de CMSAF_examples
library(ncdf)
library(RNetCDF)

# Open the netcdf-file
nc <- open.ncdf('~/temp/SIS_METCH_hrv_cor_moe_110_regular_200401.nc')

# Retrieve the data from the first variable in the netcdf-file 
varname <- nc$var[[1]]$name
field <- get.var.ncdf(nc, varname)
unit <- att.get.ncdf(nc, varname,"units")$value
missval <- att.get.ncdf(nc,varname,"_FillValue")$value
# Close the file
close.ncdf(nc)

# Set the missing data to NA, considering scale.factor and add.offset
scale.factor <- 1.
add.offset <- 0.
#Derive the scale factor and the offset
has.scale <- nc$var[[1]]$hasScaleFact
if (has.scale) scale.factor <- nc$var[[1]]$scaleFact
has.offset <- nc$var[[1]]$hasAddOffset
if (has.offset) add.offset <- nc$var[[1]]$addOffset
# Set the missing values to NA
na.ind <- which(field == missval*scale.factor + add.offset)
field[na.ind] <- NA

#--------------------------------------------------#

# determine the location 
londim <- nc$dim[["lon"]]
lon <- londim$vals
latdim <- nc$dim[["lat"]]
lat <- latdim$vals

#--------------------------------------------------#

# retrieve the time variable
timedim <- nc$dim[["time"]]
nt <- timedim$len
time.unit <- timedim$units
time <- timedim$vals

# Create a R-date-object 
##cambiar
date.time <- as.Date(utcal.nc(time.unit,time,type="s"))

library(spacetime)
proj <- CRS('+proj=latlon +ellps=WGS84')
coords <- expand.grid(lon=signif(lon, 4), lat=signif(lat, 5))
sp <- SpatialPixels(SpatialPoints(coords, proj4string=proj))
data <- data.frame(G0=c(field))
stData <- STFDF(sp, time=xts(seq_along(time), as.Date(time)), data=data)

stplot(stData, scales=list(draw=TRUE))
