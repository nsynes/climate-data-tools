##################
# PARAMS
##################
varnames <- c('tasmin','tasmax','pr')

fileNamePattern <- 'C:/dev/ForestResearch/climate-data-tools/data/netCDF/12km/%s_rcp85_land-rcm_eur_12km_15_mon_198012-208011.nc'
dirOut <- 'C:/dev/ForestResearch/climate-data-tools/data/geotif/rcp85'

##################
##################








library(stars)
library(raster)
library(dplyr)
library(PCICt)
library(lubridate)
#https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation

fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( mean(x, na.rm=TRUE) )
  } else {
    return( x[i] )
  }
}


ncToRaster <- function(pathNc, varname, dirOut) {
  dir.create(file.path(dirOut, varname))
  nc <- read_ncdf(pathNc, var=varname)
  st_crs(nc)<-"+proj=ob_tran +o_proj=longlat +lon_0=378 +o_lon_p=0 +o_lat_p=39.25 +a=6371229 +b=6371229 +to_meter=0.0174532925199 +wktext"
  nctransform<-st_transform_proj.stars(nc, st_crs(4326))
  
  # Formulated like this rather than two y, m for loops because data starts in Dec-1980 and ends Nov-2080:
  # pr_rcp85_land-rcm_eur_12km_15_mon_198012-208011
  i<-1
  y<-1980
  m<-12
  for (i in c(1:1200)) {
    cat(y,'-',m,'\n')
    
    singleSlice<-slice.stars(nctransform, index=i, along = "time")
    df <- as.data.frame(singleSlice)
    mat<- as.matrix(df)
    e <- extent(mat[,1:2])
    x <- raster(e, resolution=0.1622) #resolution=0.1081 is ~12km. #resolution=0.1622 is ~18km.
    r <- rasterize(mat[, 1:2], x, mat[,3], fun=mean)
    crs(r) <- CRS('+init=EPSG:4326') #"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    r2 <- focal(r, w = matrix(1,3,3), fun = fill.na,pad = TRUE, na.rm = FALSE )
    writeRaster(r2, file.path(dirOut, varname, sprintf('%s_%s-%s.tif',varname,y,m)))
    
    if (m<12) {
      m<-m+1
    } else {
      m<-1
      y<-y+1
    }
  }
}


for (varname in varnames) {
  pathNc <- sprintf(fileNamePattern, varname)
  ncToRaster(pathNc, varname, dirOut)
}





# FROM: https://stackoverflow.com/questions/46001573/convert-a-netcdf-time-variable-to-an-r-date-object
# But this doesn't work with the UKCP18 data. I think they UKCP18 defined the time format incorrectly
# Unclear from CEDA what exactly is the start date, and the end date doesn't make sense for the mnumber of records:
# Start time: 1980-01-01T00:00:00
# End time: 2080-12-30T23:59:59
# or:
# a 100 year period, 1981-2080
# or:
# at 12km Resolution for 1980-2080
# or:
# offset specified within the dataset: 1980-12-16
# https://catalogue.ceda.ac.uk/uuid/8c6c0ae2c25947168826a70d2241b797
if (FALSE) {
require("ncdf4")
f1<-nc_open(pathNc)
getNcTime(f1)

getNcTime <- function(nc) {
  require(lubridate)
  ncdims <- names(nc$dim) #get netcdf dimensions
  timevar <- ncdims[which(ncdims %in% c("time", "Time", "datetime", "Datetime", "date", "Date"))[1]] #find time variable
  times <- ncvar_get(nc, timevar)
  if (length(timevar)==0) stop("ERROR! Could not identify the correct time variable")
  timeatt <- ncatt_get(nc, timevar) #get attributes
  timedef <- strsplit(timeatt$units, " ")[[1]]
  timeunit <- timedef[1]
  tz <- timedef[5]
  timestart <- strsplit(timedef[4], ":")[[1]]
  if (length(timestart) != 3 || timestart[1] > 24 || timestart[2] > 60 || timestart[3] > 60 || any(timestart < 0)) {
    cat("Warning:", timestart, "not a valid start time. Assuming 00:00:00\n")
    warning(paste("Warning:", timestart, "not a valid start time. Assuming 00:00:00\n"))
    timedef[4] <- "00:00:00"
  }
  if (! tz %in% OlsonNames()) {
    cat("Warning:", tz, "not a valid timezone. Assuming UTC\n")
    warning(paste("Warning:", timestart, "not a valid start time. Assuming 00:00:00\n"))
    tz <- "UTC"
  }
  timestart <- ymd_hms(paste(timedef[3], timedef[4]), tz=tz)
  f <- switch(tolower(timeunit), #Find the correct lubridate time function based on the unit
              seconds=seconds, second=seconds, sec=seconds,
              minutes=minutes, minute=minutes, min=minutes,
              hours=hours,     hour=hours,     h=hours,
              days=days,       day=days,       d=days,
              months=months,   month=months,   m=months,
              years=years,     year=years,     yr=years,
              NA
  )
  suppressWarnings(if (is.na(f)) stop("Could not understand the time unit format"))
  timestart + f(times)
}
}
