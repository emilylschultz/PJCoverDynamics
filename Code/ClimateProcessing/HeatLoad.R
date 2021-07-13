#### PJ Cover Dynamics: Code to calculate head load from DEM
## Created by: Emily Schultz
## Created on: 12 Jul 2021

library(raster)

# Get DEM from raster package
#state<- spData::us_states %>% sf::st_transform(4326)
elev<-getData(name = "alt",
							country = "USA")

# Extract elevation, slope, and aspect
elev<-elev[[1]]
slope = terrain(elev, opt='slope')
aspect = terrain(elev, opt='aspect')
aspectFolded <- setValues(aspect, (180-abs(values(aspect)-180)))

# Calculate latitude from DEM (code adapted from https://www.sciencebase.gov/catalog/file/get/5a53e3f6e4b01e7be23087c7?f=__disk__15%2F5b%2Fe0%2F155be00ffe9fe8a6641bd3717090406a526226ff&transform=1&allowOpen=true)
cels <- 1:ncell(elev)
lats <- yFromCell(elev, cels) # extract latitudes

latRaster <- setValues(elev, lats) # set raster values to latitude
latRaster_rad <- setValues(latRaster, values(latRaster)*(pi/180)) # convert degrees to radians

# Calculate heat load from elevation, slope, and aspect (code adapted from https://www.sciencebase.gov/catalog/file/get/5a53e3f6e4b01e7be23087c7?f=__disk__15%2F5b%2Fe0%2F155be00ffe9fe8a6641bd3717090406a526226ff&transform=1&allowOpen=true)
heatload <- setValues(latRaster,-1.467+1.582*cos(values(latRaster))*cos(values(slope))-1.5*cos(values(aspectFolded))*sin(values(slope))*sin(values(latRaster))-0.262*sin(values(latRaster))*sin(values(slope))+0.607*sin(values(aspectFolded))*sin(values(slope)))

# Load heat load categories from https://www.sciencebase.gov/catalog/item/5a53e3f6e4b01e7be23087c7

# Compare heat load categories to calculated heat load

# Export heat load raster
writeRaster(heatload, file = "./EnvData/heatload.tif", overwrite = T, format="GTiff")
