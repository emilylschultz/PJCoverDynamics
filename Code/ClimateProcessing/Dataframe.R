#### PJ Cover Dynamics: Code to create data frame with climate, heat load, and percent cover data
## Created by: Emily Schultz
## Created on: 16 Jul 2021

# Load required packages
library(raster)

# Get environment raster data files
ppt <- stack("./EnvData/pptStack.tif")
tmin <- stack("./EnvData/tminStack.tif")
tmax <- stack("./EnvData/tmaxStack.tif")
heatload <-raster("./EnvData/heatload.tif")

# Get percent cover raster data files
PC.path <-  "./PJCover/"

PCFiles <- list.files(path = PC.path, pattern = glob2rx("Clipped*.tif"), full.names = TRUE)

PJcover <- stack(PCFiles)

# Upload PJ presence/absence data
mask <- raster(paste0(PC.path, "PJmask.tif"))

# Set percent cover to NA where PJ are absent (confirm this with Bob)
PJcover <- PJcover*mask # use mask (presence/absence) raster to set percent cover to 0
values(Allyrs)[values(Allyrs) == 0] = NA # convert 0 to NA

# Rename rasters in stacks

names(PJcover) <- as.character(2000:2016)

clim_months <- numeric(0) # vector to store year and month from 1980 to 2020
for(i in 1980:2020){
	for(j in 1:12){
		year_month <- paste0(i, "_", j) # character variable with year and month
		clim_months <- c(clim_months,year_month) # add variable to vector
	}
}

names(ppt) <- clim_months
names(tmin) <- clim_months
names(tmax) <- clim_months

# Extract raster values and add to data frame
	# Columns: Year t, Year t+1, location (center of pixel?), pc t, pc t+1, heat load, ppt (Oct-Sep),tmin (Oct-Sep), tmax (Oct-Sep)
PJdata <- data.frame(matrix(ncol= 43,nrow = 0))
colnames(PJdata) <- c("Year_t", "Year_t1", "location.x", "location.y", "PC_t", "PC_t", "Heatload",
											paste0("ppt_", as.character(c(10:12,1:9))), 
											paste0("tmin_", as.character(c(10:12,1:9))), paste0("tmax_", as.character(c(10:12,1:9))))
	
PJdata <- data.frame(Year_t = sort(rep(2000:2015,(nrow(PJcover)*ncol(PJcover)))), 
					 Year_t1 = sort(rep(2001:2016,(nrow(PJcover)*ncol(PJcover)))),
					 location.x = rep(coordinates(PJcover)[,1],length(2000:2015)), 
					 location.y = rep(coordinates(PJcover)[,2],length(2000:2015)), 
					 PC_t = as.vector(as.matrix(PJcover)[,1:16]), 
					 PC_t1 = as.vector(as.matrix(PJcover)[,2:17]),
					 Heatload = rep(values(heatload),length(2000:2015)))

head(PJdata)
		 
#	Calculate new variables
	# Calculated columns: change in pc, total ppt, ave tmin, ave tmax