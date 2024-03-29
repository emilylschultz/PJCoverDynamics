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

PCFiles <- list.files(path = PC.path, pattern = glob2rx("RAP*.tif"), full.names = TRUE)

PJcover <- stack(PCFiles)

# Upload PJ presence/absence data
mask <- raster(paste0(PC.path, "PJmask.tif"))

PJcover_mask <- PJcover*mask # use mask (presence/absence) raster to set percent cover to 0 where PJ is absent

# Rename rasters in stacks

names(PJcover) <- as.character(1984:2020)
names(PJcover_mask) <- as.character(1984:2020)

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
	# Columns: Year t, Year t+1, location (center of pixel?), pc t, pc t+1, change in pc, heat load, total water year ppt (Oct-Sep),average water year tmin (Oct-Sep), average water year tmax (Oct-Sep)

#Extract data from climate rasters and put in matrix format
start_ind <- which(clim_months=="1982_10") # start with Oct 1982 (two year lag before first year of pj cover data)
end_ind <- which(clim_months=="2020_9") # end with Sep 2020

heatload_vec <- values(heatload)
ppt_mat <- as.matrix(ppt)[,start_ind:end_ind] 
tmin_mat <- as.matrix(tmin)[,start_ind:end_ind] 
tmax_mat <- as.matrix(tmax)[,start_ind:end_ind] 

total_ppt <- numeric(0)
ave_tmin <- numeric(0)
ave_tmax <- numeric(0)
for (i in 1:38){
	#sum_ppt <- rowSums(ppt_mat[,(1+(i-1)*12):(12+(i-1)*12)]) # calculate total ppt for each water year (Oct-Sep) for 2001 to 2016
	#total_ppt <- cbind(total_ppt,sum_ppt) # add to ppt vector
	#sum_tmin <- rowMeans(tmin_mat[,(1+(i-1)*12):(12+(i-1)*12)]) # calculate average tmin for each water year (Oct-Sep) for 2001 to 2016
	sum_tmin <- apply(tmin_mat[,(1+(i-1)*12):(12+(i-1)*12)],1,min) # calculate average tmin for each water year (Oct-Sep) for 2001 to 2016
	ave_tmin <- cbind(ave_tmin,sum_tmin) # add to tmin vector
	
	#sum_tmax <- rowMeans(tmax_mat[,(1+(i-1)*12):(12+(i-1)*12)]) # calculate average tmax for each water year (Oct-Sep) for 2001 to 2016
	sum_tmax <- apply(tmax_mat[,(1+(i-1)*12):(12+(i-1)*12)],1,max) # calculate average tmin for each water year (Oct-Sep) for 2001 to 2016
	ave_tmax <- cbind(ave_tmax,sum_tmax) # add to tmax vector
}

# Remove fire pixels
fire <- which((values(PJcover[[20]])-values(PJcover[[19]])) < (-10))

# Save matrix of pc values and vectors of location values
pc_mat_RAP <- as.matrix(PJcover)
pc_mat_RAP[fire,20]<-NA
location.x.RAP = coordinates(PJcover)[,1]
location.y.RAP = coordinates(PJcover)[,2]
save(pc_mat_RAP,location.x.RAP,location.y.RAP,file="./Output/Climate_mat_RAP.rda")
pc_mat_mask_RAP <- as.matrix(PJcover_mask)
pc_mat_mask_RAP[fire,20]<-NA
save(pc_mat_mask_RAP,location.x.RAP,location.y.RAP,file="./Output/PJcoverMask_mat_RAP.rda")
noPJ <- which(values(mask)==0)
save(noPJ,file="./Output/PJMask_mat.rda")

# Save matrices of climate data
save(total_ppt,ave_tmin,ave_tmax,heatload_vec,file="./Output/Climate_mat.rda")

PJdata <- data.frame(Year_t = sort(rep(1984:2019,(nrow(PJcover)*ncol(PJcover)))), 
										 Year_t1 = sort(rep(1985:2020,(nrow(PJcover)*ncol(PJcover)))),
										 location.x = rep(coordinates(PJcover)[,1],length(1984:2019)), 
										 location.y = rep(coordinates(PJcover)[,2],length(1984:2019)), 
										 PC_t = as.vector(as.matrix(PJcover)[,1:36]), # percent cover in year t 
										 PC_t1 = as.vector(as.matrix(PJcover)[,2:37]), # percent cover in year t+1
										 PC_t_mask = as.vector(as.matrix(PJcover_mask)[,1:36]), # percent cover in year t 
										 PC_t1_mask = as.vector(as.matrix(PJcover_mask)[,2:37]), # percent cover in year t+1
										 Heatload = rep(values(heatload),length(1984:2019)),
										 PPT = total_ppt, Tmin = ave_tmin, Tmax = ave_tmax) # water year climate variables calculated above
PJdata$d_PC <- PJdata$PC_t1 - PJdata$PC_t # calculate change in percent cover
PJdata$d_PC_mask <- PJdata$PC_t1_mask - PJdata$PC_t_mask # calculate change in percent cover
PJdata$log_PC_t <- log(PJdata$PC_t)
PJdata$log_PC_t1 <- log(PJdata$PC_t1)
PJdata$d_log_PC <- PJdata$log_PC_t1 - PJdata$log_PC_t
PJdata$log_PC_t_pos <- log(PJdata$PC_t+2)
PJdata$log_PC_t1_pos <- log(PJdata$PC_t1+2)
PJdata$d_log_PC_pos <- PJdata$log_PC_t1_pos-PJdata$log_PC_t_pos

head(PJdata)

PJdata$Fire <- ifelse(PJdata$d_PC < (-9) & PJdata$Year_t==2002,1,0) # Remove fire pixels from 2002 to 2003 transition
		 
write.csv(PJdata,"PJcoverRAP_data.csv")

