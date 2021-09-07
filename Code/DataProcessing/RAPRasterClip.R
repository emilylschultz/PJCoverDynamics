#### PJ Cover Dynamics: Code to clip RAP percent cover rasters to the study area
## Created by: Emily Schultz
## Created on: 26 Jul 2021


# Required packages
library(raster) # also loads required sp package

# Set RAP percent cover data file pathway
RAP.path <-  "./PJCover/tree/"

# Get RAP percent cover raster data files
RAPFiles <- list.files(path = RAP.path, pattern = glob2rx("*202*.tif"), full.names = TRUE) # Do one decade at a time because of memory limitations

RAP <- stack(RAPFiles)

RAP <- crop(RAP,c(-121, -117, 37, 42))

# Get percent cover raster data file (needed to get extent of study area)
PJcover <- raster("./PJCover/PJmask.tif")

# Change climate raster projection to match percent cover raster
newproj<-projection(PJcover)
RAP_nad83<-projectRaster(RAP,crs=newproj)

# Extract extent of percent cover raster
extent <- extent(PJcover)

# Clip climate rasters to percent cover extent
RAP_nad83 <- resample(RAP_nad83,PJcover)

RAP_cropped <- crop(RAP_nad83,extent)

# Export clipped climate rasters
writeRaster(RAP_cropped, "./PJCover/RAPStack_2020s.tif", overwrite = T)

