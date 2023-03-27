### Code to set up data for Gompertz sampler
### Created by: Emily Schultz
### Created on: 21 Sep 2021

# Load data
load("./Output/PJcoverMask_mat.rda")
load("./Output/PJcoverMask_mat_RAP.rda")
load("./OUtput/Climate_mat.rda")
load("./Output/PJMask_mat.rda")

# Remove pixel with no pinyon-juniper
pc_mat <- pc_mat_mask[-noPJ,]
pc_mat_RAP <- pc_mat_mask_RAP[-noPJ,]
total_ppt <- total_ppt[-noPJ,]
ave_tmin <- ave_tmin[-noPJ,]
ave_tmax <- ave_tmax[-noPJ,]
heatload_vec <- heatload_vec[-noPJ]
location.x <- location.x[-noPJ]
location.y <- location.y[-noPJ]

# Calculate normals
tmax <- 37
n_pixel <- nrow(pc_mat)

pc_mat <- t(pc_mat)

pc_mat_RAP <- t(pc_mat_RAP)

PPT_mean_vec <- rowMeans(total_ppt)
Tmin_mean_vec <- rowMeans(ave_tmin)
Tmax_mean_vec <- rowMeans(ave_tmax)

PPT_mean <- matrix(rep(PPT_mean_vec,(tmax+1)),c(n_pixel,(tmax+1)))
Tmin_mean <- matrix(rep(Tmin_mean_vec,(tmax+1)),c(n_pixel,(tmax+1)))
Tmax_mean <- matrix(rep(Tmax_mean_vec,(tmax+1)),c(n_pixel,(tmax+1)))
heatload <- matrix(rep(heatload_vec,(tmax-1)),c(n_pixel,(tmax-1)))

# Calculate deviations from normals
PPT_dev <- total_ppt-PPT_mean
Tmin_dev <- ave_tmin-Tmin_mean
Tmax_dev <- ave_tmax-Tmax_mean

# Scale data
PPT_mean <- scale(PPT_mean)
Tmin_mean <- scale(Tmin_mean)
Tmax_mean <- scale(Tmax_mean)

PPT_dev <- scale(PPT_dev)
Tmin_dev <- scale(Tmin_dev)
Tmax_dev <- scale(Tmax_dev)

heatload <- scale(heatload)

# Transpose data
PPT_mean <- t(PPT_mean)
Tmin_mean <- t(Tmin_mean)
Tmax_mean <- t(Tmax_mean)
heatload <- t(heatload)

PPT_dev<- t(PPT_dev)
Tmin_dev <- t(Tmin_dev)
Tmax_dev <- t(Tmax_dev)

# Calculate averages over 3-year sliding window
PPT_dev_lag <- matrix(NA,tmax-1,ncol(PPT_dev))
Tmin_dev_lag <- matrix(NA,tmax-1,ncol(PPT_dev))
Tmax_dev_lag <- matrix(NA,tmax-1,ncol(PPT_dev))

for(i in 3:38){
	PPT_dev_lag[i-2,]<-colMeans(PPT_dev[(i-2):i,])
	Tmin_dev_lag[i-2,]<-colMeans(Tmin_dev[(i-2):i,])
	Tmax_dev_lag[i-2,]<-colMeans(Tmax_dev[(i-2):i,])
}

PPT_mean <- PPT_mean[3:38,]
Tmax_mean <- Tmax_mean[3:38,]
Tmin_mean <- Tmin_mean[3:38,]

# Combine data into arrays with density-dependent and non-density-dependent predictors
Xdense <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev),c((tmax-1),n_pixel,8)) # change from matrix to array (pixel x year x predictor) of density-dependent predictors
X <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev[-(1:2),],Tmin_dev[-(1:2),],Tmax_dev[-(1:2),],
						 heatload*PPT_mean,heatload*Tmin_mean,heatload*Tmax_mean,heatload*PPT_dev[-(1:2),],heatload*Tmin_dev[-(1:2),],heatload*Tmax_dev[-(1:2),],
								 PPT_mean*Tmin_mean,PPT_mean*Tmax_mean,Tmin_mean*Tmax_mean,
								 PPT_mean*PPT_dev[-(1:2),],Tmin_mean*Tmin_dev[-(1:2),],Tmax_mean*Tmax_dev[-(1:2),]),c((tmax-1),n_pixel,20)) # change from matrix to array (pixel x year x predictor) of density-dependent predictors

Xdense_lag <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmax_mean),c((tmax-1),n_pixel,4)) # change from matrix to array (pixel x year x predictor) of non-density predictors
X_lag <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmax_mean,PPT_dev_lag,Tmax_dev_lag,
						 PPT_mean*PPT_dev_lag,Tmax_mean*Tmax_dev_lag,
						 PPT_mean^2,Tmax_mean^2),c((tmax-1),n_pixel,10)) # change from matrix to array (pixel x year x predictor) of non-density predictors

# Remove pixels with fire
fire <- which(is.na(pc_mat_RAP[20,]))
pc_mat_RAP <- pc_mat_RAP[,-fire]
pc_mat <- pc_mat[,-fire]
X_lag <- X_lag[,-fire,]
Xdense_lag <- Xdense_lag[,-fire,]
location.x <- location.x[-fire]
location.y <- location.y[-fire]

# Save data
save(pc_mat,pc_mat_RAP,X,Xdense,file="SamplerDataMask.rda")
save(pc_mat,pc_mat_RAP,X_lag,Xdense_lag,fire,file="SamplerDataMask_Lag.rda")
save(location.x,location.y,file="locations.rda")


