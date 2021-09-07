#### PJ Cover Dynamics: Code to calculate head load from DEM
## Created by: Emily Schultz
## Created on: 12 Jul 2021

library(raster)

# Load USGS DEMs
DEM.path <-  "./EnvData/DEM/"
DEMFiles <- list.files(path = DEM.path, pattern = glob2rx("*.tif"), full.names = TRUE)

tile1 <- raster(DEMFiles[1])
tile2 <- raster(DEMFiles[2])
tile3 <- raster(DEMFiles[3])
tile4 <- raster(DEMFiles[4])
tile5 <- raster(DEMFiles[5])
tile6 <- raster(DEMFiles[6])
tile7 <- raster(DEMFiles[7])
tile8 <- raster(DEMFiles[8])
tile9 <- raster(DEMFiles[9])
tile10 <- raster(DEMFiles[10])
tile11 <- raster(DEMFiles[11])

# merge DEM tiles into a single raster
tiles <- list(tile1,tile2,tile3,tile4,tile5,tile6,tile7,tile8,tile9,tile10,tile11)
elev <- do.call(merge,tiles)

# Extract elevation, slope, and aspect
slope = terrain(elev, opt='slope')
aspect = terrain(elev, opt='aspect',unit="degrees")
aspectFolded <- setValues(aspect, (180-abs(values(aspect)-180))*(pi/180))

# Calculate latitude from DEM (code adapted from https://www.sciencebase.gov/catalog/file/get/5a53e3f6e4b01e7be23087c7?f=__disk__15%2F5b%2Fe0%2F155be00ffe9fe8a6641bd3717090406a526226ff&transform=1&allowOpen=true)
cels <- 1:ncell(elev)
lats <- yFromCell(elev, cels) # extract latitudes
lats_rad <- lats*(pi/180) # convert degrees to radians

latRaster <- setValues(elev, lats_rad) # set raster values to latitude

# Calculate heat load from elevation, slope, and aspect (code adapted from https://www.sciencebase.gov/catalog/file/get/5a53e3f6e4b01e7be23087c7?f=__disk__15%2F5b%2Fe0%2F155be00ffe9fe8a6641bd3717090406a526226ff&transform=1&allowOpen=true)
heatload <- setValues(latRaster,-1.467+1.582*cos(values(latRaster))*cos(values(slope))-1.5*cos(values(aspectFolded))*sin(values(slope))*sin(values(latRaster))-0.262*sin(values(latRaster))*sin(values(slope))+0.607*sin(values(aspectFolded))*sin(values(slope)))
heatload <- setValues(heatload, exp(values(heatload)))

# Load heat load categories from https://www.sciencebase.gov/catalog/item/5a53e3f6e4b01e7be23087c7
heatload_cat <- raster("./EnvData/HeatLoad_Classes/Western_US_30m_Exp_HeatLoad_Albers_USGS_Geometric_Interval_6_Classes.tif")

# Compare heat load categories to calculated heat load
# might need to remove some rasters to free up memory: rm(tile1,tile2,tile3,tile4,tile5,tile6,tile7,tile8,tile9,tile10,tile11,tiles,slope,aspect,aspectFolded,latRaster)
heatload_reproject<-projectRaster(heatload,crs=projection(heatload_cat))

heatload_cat_cropped <- crop(heatload_cat,extent(heatload))

plot(heatload_cat_cropped)
plot(heatload) # for formal comparison, would need to calculate heatload for all of western US

# Load percent cover raster (needed to get extent of study area)
PJcover <- raster("./PJCover/PJmask.tif")

newproj<-projection(PJcover)
heatload_reproject<-projectRaster(heatload,crs=newproj)

# Extract extent of percent cover raster
extent <- extent(PJcover)

# Clip climate rasters to percent cover extent

heatload_reproject <- resample(heatload_reproject,PJcover)
heatload_cropped <- crop(heatload_reproject,extent)

# Export heat load raster
writeRaster(heatload_cropped, file = "./EnvData/heatload.tif", overwrite = T, format="GTiff")
