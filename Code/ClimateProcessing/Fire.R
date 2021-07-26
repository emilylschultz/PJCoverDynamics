#### PJ Cover Dynamics: Code to rasters of fire occurrence and add to percent cover dataframe
## Created by: Emily Schultz
## Created on: 23 Jul 2021

library(raster)
library(gdalUtils)
library(RColorBrewer)

# Load percent cover rasters
PC.path <-  "./PJCover/"

PCFiles <- list.files(path = PC.path, pattern = glob2rx("Clipped*.tif"), full.names = TRUE)

PJcover <- stack(PCFiles)

# Make rasters with change in percent cover
d_PJcover<-PJcover[[1:16]]
for(i in 2:length(PCFiles)){
	values(d_PJcover[[(i-1)]]) <- values(PJcover[[i]])-values(PJcover[[(i-1)]])
}


pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))
cuts<-c(seq(min(na.omit(values(d_PJcover))),0,length=6),
				seq(0,max(na.omit(values(d_PJcover))),length=6)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(d_PJcover, breaks=cuts, col=pal_div(11))

for (i in 1:16){
	plot(d_PJcover[[i]], breaks=cuts, col=pal_div(11))
}

# Get fire raster data files
Fire.path <-  "./EnvData/Fire/"

FireFiles <- list.files(path = Fire.path, pattern = glob2rx("*.hdf"), full.names = TRUE)

filename <- substr(FireFiles,25,31)
filename <- paste0("Fire", filename, ".tif")


for (i in 1:1){
	sds <- get_subdatasets(files[i])
	gdal_translate(sds[1], dst_dataset = filename[i])
}

# Upload PJ presence/absence data
mask <- raster(paste0(PC.path, "PJmask.tif"))

# Reproject and resample fire rasters

# Summarize fire occurrence over water year (Oct-Sep)

# Add fire occurrence to dataframe