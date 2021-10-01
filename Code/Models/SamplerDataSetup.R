load("./Output/PJcover_mat.rda")
load("./Output/PJcover_mat_RAP.rda")
load("./OUtput/Climate_mat.rda")

tmax <- 37
n_pixel <- nrow(pc_mat)

#pc_mat[which((pc_mat[,4]-pc_mat[,3])<(-9)),4] <- NA
#pc_mat_RAP[which((pc_mat_RAP[,20]-pc_mat_RAP[,19])<(-9)),20] <- NA
pc_mat <- t(pc_mat)
pc_mat <- rbind(matrix(NA,nrow=20,ncol=n_pixel),pc_mat)
pc_mat_RAP <- t(pc_mat_RAP)
pc_mat_RAP[which(pc_mat_RAP<=0)] <- min(pc_mat_RAP[which(pc_mat_RAP>0)])

PPT_mean_vec <- rowMeans(total_ppt)
Tmin_mean_vec <- rowMeans(ave_tmin)
Tmax_mean_vec <- rowMeans(ave_tmax)

PPT_mean <- matrix(rep(PPT_mean_vec,(tmax-1)),c(n_pixel,(tmax-1)))
Tmin_mean <- matrix(rep(Tmin_mean_vec,(tmax-1)),c(n_pixel,(tmax-1)))
Tmax_mean <- matrix(rep(Tmax_mean_vec,(tmax-1)),c(n_pixel,(tmax-1)))
heatload <- matrix(rep(heatload_vec,(tmax-1)),c(n_pixel,(tmax-1)))

# Calculate deviations from normals
PPT_dev <- total_ppt-PPT_mean
Tmin_dev <- ave_tmin-Tmin_mean
Tmax_dev <- ave_tmax-Tmax_mean

PPT_mean <- scale(PPT_mean)
Tmin_mean <- scale(Tmin_mean)
Tmax_mean <- scale(Tmax_mean)

PPT_dev <- scale(PPT_dev)
Tmin_dev <- scale(Tmin_dev)
Tmax_dev <- scale(Tmax_dev)

PPT_mean <- t(PPT_mean)
Tmin_mean <- t(Tmin_mean)
Tmax_mean <- t(Tmax_mean)
heatload <- t(heatload)

PPT_dev<- t(PPT_dev)
Tmin_dev <- t(Tmin_dev)
Tmax_dev <- t(Tmax_dev)

Xdense <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev),c((tmax-1),n_pixel,8)) # change from matrix to array (pixel x year x predictor) of non-density predictors
X <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev,
						 heatload*PPT_mean,heatload*Tmin_mean,heatload*Tmax_mean,heatload*PPT_dev,heatload*Tmin_dev,heatload*Tmax_dev,
								 PPT_mean*Tmin_mean,PPT_mean*Tmax_mean,Tmin_mean*Tmax_mean,
								 PPT_mean*PPT_dev,Tmin_mean*Tmin_dev,Tmax_mean*Tmax_dev),c((tmax-1),n_pixel,20)) # change from matrix to array (pixel x year x predictor) of non-density predictors

#sample = sample.int(4,n_pixel,replace = T)
#save(sample,file="location_sample.rda")
load("location_sample.rda")

pc_mat<- pc_mat[,which(sample==1)]
pc_mat_RAP <- pc_mat_RAP[,which(sample==1)]
X <- X[,which(sample==1),]
Xdense <- Xdense[,which(sample==1),]

pc_mat<- pc_mat[,1:1000]
pc_mat_RAP <- pc_mat_RAP[,1:1000]
X <- X[,1:1000,]
Xdense <- Xdense[,1:1000,]

save(pc_mat,pc_mat_RAP,X,Xdense,file="SamplerData.rda")
#matrix(1,(tmax-1),n_pixel),