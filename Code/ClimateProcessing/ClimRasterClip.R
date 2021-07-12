#### PJ Cover Dynamics: Code to clip climate rasters to the study area
## Created by: Emily Schultz
## Created on: 7 Jul 2021


# Required packages
library(raster) # also loads required sp package

# Set climate data file pathway
PPT.path <-  "./DaymetClimate/PPT/"
TMin.path <-  "./DaymetClimate/TMin/"
TMax.path <-  "./DaymetClimate/TMax/"

# Get climate raster data files
pptFiles <- list.files(path = PPT.path, pattern = glob2rx("*.tif"), full.names = TRUE)
tminFiles <- list.files(path = TMin.path, pattern = glob2rx("*.tif"), full.names = TRUE)
tmaxFiles <- list.files(path = TMax.path, pattern = glob2rx("*.tif"), full.names = TRUE)

ppt <- stack(pptFiles)
tmin <- stack(tminFiles)
tmax <- stack(tmaxFiles)

# Get percent cover raster data file (needed to get extent of study area)
PJcover <- raster("./PJCover/PJmask.tif")

# Change climate raster projection to match percent cover raster
newproj<-projection(PJcover)
ppt_nad83<-projectRaster(ppt,crs=newproj)
tmin_nad83<-projectRaster(tmin,crs=newproj)
tmax_nad83<-projectRaster(tmax,crs=newproj)

# Extract extent of percent cover raster
extent <- extent(PJcover)

# Clip climate rasters to percent cover extent

ppt_cropped <- crop(ppt_nad83,extent)
tmin_cropped <- crop(tmin_nad83,extent)
tmax_cropped <- crop(tmax_nad83,extent)

# Export clipped climate rasters
writeRaster(ppt_cropped, "./DaymetClimate/pptStack.tif", overwrite = T)
writeRaster(tmin_cropped, "./DaymetClimate/tminStack.tif", overwrite = T)
writeRaster(tmax_cropped, "./DaymetClimate/tmaxStack.tif", overwrite = T)

