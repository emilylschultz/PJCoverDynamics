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

# Extract extent of percent cover raster

# Clip climate rasters to percent cover extent
