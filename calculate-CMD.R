##################
# Params
##################
yearsList <- c(1981:2079)
monthsList<-c(1:12)
dirETo <- 'C:/dev/ForestResearch/climate-data-tools/data/geotif/rcp85/ETo'
dirPr <- 'C:/dev/ForestResearch/climate-data-tools/data/geotif/rcp85/pr'

dirOut <- 'C:/dev/ForestResearch/climate-data-tools/data/geotif/rcp85/CMD'

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


calcCMD <- function(EtoPrDiff) {
  EtoPrDiff <- EtoPrDiff[!is.na(EtoPrDiff)]
  if (length(EtoPrDiff) > 0) {
    CMD <- 0
    accumulator <- 0
    for (a in EtoPrDiff) {
      if (accumulator >= 0) {
        accumulator <- max(accumulator + a, 0)
        if (accumulator > CMD) CMD <- accumulator
      } else {
        accumulator <- 0
      }
    }
  } else {
    CMD <- NA
  }
  
  return(CMD)
}



for (year in yearsList) {
  cat('\nYear: ', year)
  
  dates <- seq(as.Date(sprintf("%s-%s-01", year, (monthsList[1]))), as.Date(sprintf("%s-%s-01", year, monthsList[length(monthsList)])), by="month")
  daysInMonth <- lapply(dates, days_in_month)

  listETo <- paste0(file.path(dirETo, "ETo-"), year, "-", sprintf('%02d',monthsList), '.tif')
  listPr <- paste0(file.path(dirPr, "pr_"), year, "-", monthsList, '.tif')
  
  # NOTE prec is in mm/day units, so multiply by daysInMonth
  Eto <- lapply(listETo, raster)
  Pr <- mapply('*', lapply(listPr, raster), daysInMonth)
  
  EtoPrDiff <- brick(mapply('-', Eto, Pr, SIMPLIFY = FALSE))
  EtoPrDiff <- setZ(EtoPrDiff, dates)
  names(EtoPrDiff) <- as.yearmon(getZ(EtoPrDiff))
  
  rasterCMD <- raster::calc(EtoPrDiff, fun = calcCMD)
  
  writeRaster(rasterCMD, sprintf(file.path(dirOut, sprintf('CMD-%s.tif', year))), overwrite=TRUE)
  
}
