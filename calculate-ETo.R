##################
# PARAMS
##################
yearsList <- c(1981:2079)
monthsList<-c(1:12)
dirTasmin <- 'C:/dev/ForestResearch/netCDFdata/rcp85/tasmin'
dirTasmax <- 'C:/dev/ForestResearch/netCDFdata/rcp85/tasmax'
dirPr <- 'C:/dev/ForestResearch/netCDFdata/rcp85/pr'

dirOut <- 'C:/dev/ForestResearch/netCDFdata/rcp85/ETo'

# NOTE UKCP18 monthly data units are not as required by SPEI::hargreaves,
# so they are change in memory in this script:
# precipitation: mm/day
# tasmax: it says °C, but is in fact kelvin (centrigrade = Kelvin - 273.15)
# tasmin: it says °C, but is in fact kelvin (centrigrade = Kelvin - 273.15)
##################
##################




library(SPEI) # for hargreaves
library(raster)
library(zoo)
library(lubridate) # for calculating days in month
# https://gis.stackexchange.com/questions/240775/applying-speihargreaves-function-to-time-series-from-each-pixel-rasterbrick-us

har <- function(Tmin, Tmax, Prec, lat) {
  #cat('\n## hargreaves...')
  #cat('\nTmin: ', length(Tmin))
  #cat('\nTmax: ', length(Tmax))
  #cat('\nPrec: ', length(Prec))
  #cat('\nlat: ', length(lat))
  value <- SPEI::hargreaves(Tmin, Tmax, lat, Pre=Prec, na.rm=TRUE)
  #cat('\nValue: ',length(value))
  return(value)
}


for (y in yearsList) {
  cat('\nYear: ', y)
  for (m in monthsList) {
    cat('\n\tMonth: ', m)
    
    rasterDate <- as.Date(sprintf("%s-%s-01", y, (m)))
    
    pathTasmin <- file.path(dirTasmin, sprintf("tasmin_%s-%s.tif", y, m))
    pathTasmax <- file.path(dirTasmax, sprintf("tasmax_%s-%s.tif", y, m))
    pathPr <- file.path(dirPr, sprintf("pr_%s-%s.tif", y, m))
    
    # NOTE changing units of the UKCP18 climate variables
    # temp from Kelvin to centigrade
    # prec from mm/day to mm/month
    tasmin <- brick(raster(pathTasmin)-273.15)
    tasmax <- brick(raster(pathTasmax)-273.15)
    pr <- brick(raster(pathPr)*days_in_month(rasterDate))
    
    tasmin <- setZ(tasmin, rasterDate)
    names(tasmin) <- as.yearmon(getZ(tasmin))
    
    tasmax <- setZ(tasmax, rasterDate)
    names(tasmax) <- as.yearmon(getZ(tasmax))
    
    pr <- setZ(pr, rasterDate)
    names(pr) <- as.yearmon(getZ(pr))
    
    lat <- setValues(tasmin, coordinates(tasmin)[, "y"])
    
    outBrick <- raster::overlay(tasmin, tasmax, pr, lat, fun = har)
    outBrick <- setZ(outBrick, rasterDate)
    names(outBrick) <- format(as.yearmon(getZ(outBrick)), '%Y-%m')
    
    writeRaster(outBrick, sprintf(file.path(dirOut, 'ETo-%s.tif'), gsub('\\.','-',substring(names(outBrick), 2))), bylayer=TRUE)
  }
}




