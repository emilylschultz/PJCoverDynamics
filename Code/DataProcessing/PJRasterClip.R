#### PJ Cover Dynamics: Code to clip PJ percent cover rasters to the study area
## Created by: Emily Schultz
## Created on: 20 Oct 2021

library(raster)

# Set RAP percent cover data file pathway
PJ.path <-  "./PJCover/PJ/"

# Get RAP percent cover raster data files
PJFiles <- list.files(path = PJ.path, pattern = glob2rx("*.tif"), full.names = TRUE) # Do one decade at a time because of memory limitations

pjcover <- stack(PJFiles)

pjmask <- raster("./PJCover/PJ/US_200EVT_pj_aoi1.tif")

clipped <- raster("./PJcover/PJmask_old.tif")

extent <- extent(clipped)
extent <- extent + c(-10000,-9000,6500,0)

pjcover <- crop(pjcover,extent)
plot(pjcover)

pjmask <- crop(pjmask,extent)
plot(pjmask)

writeRaster(pjcover, "./PJCover/PJStack_clipped.tif", overwrite = T)
writeRaster(pjmask, "./PJCover/PJmask.tif", overwrite = T)

