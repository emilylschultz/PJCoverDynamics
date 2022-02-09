load("./Output/PJcoverMask_mat.rda")
load("./Output/PJcoverMask_mat_RAP.rda")
load("./OUtput/Climate_mat.rda")
load("./Output/PJMask_mat.rda")

pc_mat <- pc_mat_mask[-noPJ,]
pc_mat_RAP <- pc_mat_mask_RAP[-noPJ,]
total_ppt <- total_ppt[-noPJ,]
ave_tmin <- ave_tmin[-noPJ,]
ave_tmax <- ave_tmax[-noPJ,]
heatload_vec <- heatload_vec[-noPJ]
location.x <- location.x[-noPJ]
location.y <- location.y[-noPJ]

tmax <- 37
n_pixel <- nrow(pc_mat)

#pc_mat[which((pc_mat[,4]-pc_mat[,3])<(-9)),4] <- NA
#pc_mat_RAP[which((pc_mat_RAP[,20]-pc_mat_RAP[,19])<(-9)),20] <- NA
pc_mat <- t(pc_mat)
missing <- which(is.na(pc_mat),arr.ind=T)
missing_list <- list("2000"=missing[which(missing[,1]==1),2],"2001"=missing[which(missing[,1]==2),2],
										 "2002"=missing[which(missing[,1]==3),2],"2003"=missing[which(missing[,1]==4),2],
										 "2004"=missing[which(missing[,1]==5),2],"2005"=missing[which(missing[,1]==6),2],
										 "2006"=missing[which(missing[,1]==7),2],"2007"=missing[which(missing[,1]==8),2],
										 "2008"=missing[which(missing[,1]==9),2],"2009"=missing[which(missing[,1]==10),2],
										 "2010"=missing[which(missing[,1]==11),2],"2011"=missing[which(missing[,1]==12),2],
										 "2012"=missing[which(missing[,1]==13),2],"2013"=missing[which(missing[,1]==14),2],
										 "2014"=missing[which(missing[,1]==15),2],"2015"=missing[which(missing[,1]==16),2],
										 "2016"=missing[which(missing[,1]==17),2])
#pc_mat <- rbind(matrix(NA,nrow=16,ncol=n_pixel),pc_mat,matrix(NA,nrow=4,ncol=n_pixel))
pc_mat_RAP <- t(pc_mat_RAP)
#pc_mat_RAP[which(pc_mat_RAP<=0)] <- min(pc_mat_RAP[which(pc_mat_RAP>0)])

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

PPT_mean <- scale(PPT_mean)
Tmin_mean <- scale(Tmin_mean)
Tmax_mean <- scale(Tmax_mean)

PPT_dev <- scale(PPT_dev)
Tmin_dev <- scale(Tmin_dev)
Tmax_dev <- scale(Tmax_dev)

heatload <- scale(heatload)

PPT_mean <- t(PPT_mean)
Tmin_mean <- t(Tmin_mean)
Tmax_mean <- t(Tmax_mean)
heatload <- t(heatload)

PPT_dev<- t(PPT_dev)
Tmin_dev <- t(Tmin_dev)
Tmax_dev <- t(Tmax_dev)

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

Xdense <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev),c((tmax-1),n_pixel,8)) # change from matrix to array (pixel x year x predictor) of non-density predictors
X <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev[-(1:2),],Tmin_dev[-(1:2),],Tmax_dev[-(1:2),],
						 heatload*PPT_mean,heatload*Tmin_mean,heatload*Tmax_mean,heatload*PPT_dev[-(1:2),],heatload*Tmin_dev[-(1:2),],heatload*Tmax_dev[-(1:2),],
								 PPT_mean*Tmin_mean,PPT_mean*Tmax_mean,Tmin_mean*Tmax_mean,
								 PPT_mean*PPT_dev[-(1:2),],Tmin_mean*Tmin_dev[-(1:2),],Tmax_mean*Tmax_dev[-(1:2),]),c((tmax-1),n_pixel,20)) # change from matrix to array (pixel x year x predictor) of non-density predictors

Xdense_lag <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev_lag,Tmin_dev_lag,Tmax_dev_lag),c((tmax-1),n_pixel,8)) # change from matrix to array (pixel x year x predictor) of non-density predictors
X_lag <- array(c(matrix(1,(tmax-1),n_pixel),heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev_lag,Tmin_dev_lag,Tmax_dev_lag,
						 heatload*PPT_mean,heatload*Tmin_mean,heatload*Tmax_mean,heatload*PPT_dev_lag,heatload*Tmin_dev_lag,heatload*Tmax_dev_lag,
						 PPT_mean*Tmin_mean,PPT_mean*Tmax_mean,Tmin_mean*Tmax_mean,
						 PPT_mean*PPT_dev_lag,Tmin_mean*Tmin_dev_lag,Tmax_mean*Tmax_dev_lag),c((tmax-1),n_pixel,20)) # change from matrix to array (pixel x year x predictor) of non-density predictors

#sample = sample.int(4,n_pixel,replace = T)
#save(sample,file="location_sample.rda")
#load("location_sample.rda")

#pc_mat<- pc_mat[,which(sample==1)]
#pc_mat_RAP <- pc_mat_RAP[,which(sample==1)]
#X <- X[,which(sample==1),]
#Xdense <- Xdense[,which(sample==1),]

#pc_mat<- pc_mat[,1:1000]
#pc_mat_RAP <- pc_mat_RAP[,1:1000]
#X <- X[,1:1000,]
#Xdense <- Xdense[,1:1000,]

save(pc_mat,pc_mat_RAP,X,Xdense,file="SamplerDataMask.rda")
save(pc_mat,pc_mat_RAP,X_lag,Xdense_lag,file="SamplerDataMask_Lag.rda")
save(location.x,location.y,file="locations.rda")

# Prep data without removing locations where PJ is absent
load("./Output/PJcover_mat.rda")
load("./Output/PJcover_mat_RAP.rda")
load("./OUtput/Climate_mat.rda")
load("./Output/PJMask_mat.rda")

tmax <- 37
n_pixel <- nrow(pc_mat)

#pc_mat[which((pc_mat[,4]-pc_mat[,3])<(-9)),4] <- NA
#pc_mat_RAP[which((pc_mat_RAP[,20]-pc_mat_RAP[,19])<(-9)),20] <- NA
pc_mat <- t(pc_mat)
missing <- which(is.na(pc_mat),arr.ind=T)
missing_list <- list("2000"=missing[which(missing[,1]==1),2],"2001"=missing[which(missing[,1]==2),2],
										 "2002"=missing[which(missing[,1]==3),2],"2003"=missing[which(missing[,1]==4),2],
										 "2004"=missing[which(missing[,1]==5),2],"2005"=missing[which(missing[,1]==6),2],
										 "2006"=missing[which(missing[,1]==7),2],"2007"=missing[which(missing[,1]==8),2],
										 "2008"=missing[which(missing[,1]==9),2],"2009"=missing[which(missing[,1]==10),2],
										 "2010"=missing[which(missing[,1]==11),2],"2011"=missing[which(missing[,1]==12),2],
										 "2012"=missing[which(missing[,1]==13),2],"2013"=missing[which(missing[,1]==14),2],
										 "2014"=missing[which(missing[,1]==15),2],"2015"=missing[which(missing[,1]==16),2],
										 "2016"=missing[which(missing[,1]==17),2])
pc_mat <- rbind(matrix(NA,nrow=16,ncol=n_pixel),pc_mat,matrix(NA,nrow=4,ncol=n_pixel))
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

save(pc_mat,pc_mat_RAP,X,Xdense,missing_list,file="SamplerData.rda")
#matrix(1,(tmax-1),n_pixel),

####
#### Generate knots and plot
####
library(sp)
library(raster)
library(rgeos)
library(reshape2)
library(ggplot2)
library(Matrix)

mask <- raster("./PJCover/PJmask.tif")

Coords<-data.frame(Lon=location.x,Lat=location.y)

x.min=min(location.x)
x.max=max(location.x)
y.min=min(location.y)
y.max=max(location.y)

splits.x <- round((x.max-x.min)/2000)
splits.y <- round((y.max-y.min)/2000)
X=x.min+(x.max-x.min)/splits.x*c(0:splits.x)
Y=y.min+(y.max-y.min)/splits.y*c(splits.y:0)
XY=expand.grid(x=X,y=Y)
XY=XY[-which(XY$x==min(XY$x)),]

Knots=SpatialPoints(coords=XY,proj4string=CRS(proj4string(mask)))
rasValue=extract(mask,XY)
Knots=SpatialPoints(coords=XY[which(rasValue==1),],proj4string=CRS(proj4string(mask))) #
obsPts=SpatialPoints(coords=Coords, proj4string=CRS(proj4string(mask)))
Distances=gDistance(Knots,obsPts,byid=TRUE)
Distances=apply(Distances,2,'min')
my.buffer=150000
Which.include=which(Distances<my.buffer)
# save(Knots,file="BOSS_Knots_SP.Rda")
Knot.cell.distances=gDistance(Knots[Which.include,],obsPts,byid=TRUE)
diff.x=(x.max-x.min)/splits.x #normally 6
diff.y=(y.max-y.min)/splits.y #normally 6
test=(diff.x+diff.y)/2

range_parameter = 4000/3
sigma=range_parameter

#plot knot distances
Knot.distances=gDistance(Knots[Which.include,],Knots[Which.include,],byid=TRUE)
m <- melt(Knot.distances)
ggplot(data=m, aes(x=Var1, y=Var2))+
	geom_raster(aes(z=value, fill=value))

source("./Code/Conn_util_funcs.R")
Knot.Adj=rect_adj(splits.x+1,splits.y+1)
Knot.Adj=Knot.Adj[Which.include,Which.include]
Q.knot=-Knot.Adj
diag(Q.knot)=apply(Knot.Adj,2,'sum')
Q.knot=Matrix(Q.knot)

w=exp(-Knot.cell.distances/sigma) #exponential covariance structure

plot(Knot.cell.distances[,1], w[,1], xlab="Euclidean Distance (meters)", ylab="Covariance")
abline(v = test, col="red", lwd=2)
text(6000 , 0.8, "Distance btwn knots", col="red")

K=w/apply(w,1,'sum')
K.data=list(K=K,Q.knot=Q.knot)
save(K.data,file="./Output/Knot_cell_distances_subset.rda")


####
####  Plot the data with knots overlaid
####
library(plyr)
avgD <- apply(pc_mat,1,mean)
avgD <- data.frame(Lon=location.x,Lat=location.y,cover=avgD)
knotsD <- as.data.frame(Knots)

library(RColorBrewer)
myPalette <- colorRampPalette(brewer.pal(9, "Greens"))
tmp.theme=theme(axis.ticks = element_blank(), axis.text = element_blank(),
								strip.text=element_text(face="bold"),
								axis.title=element_text(size=14),text=element_text(size=16),
								legend.text=element_text(size=12), legend.title=element_text(size=16))

# setwd("/Users/atredenn/Dropbox/sageAbundance_data/")
png(paste0(results_path,'SAGE_Grid_wKnots_subset.png'), width=6, height=3, units = "in", res=200)
g <- ggplot()+
	geom_raster(data=avgD, aes(x=Lon, y=Lat, z=cover, fill=cover))+
	geom_point(data=knotsD, aes(x,y), size=2, color="white")+
	geom_point(data=knotsD, aes(x,y), size=1.5, color="black")+
	scale_fill_gradientn(colours=myPalette(100), name="Percent \nCover")+
	tmp.theme+
	theme(strip.background=element_rect(fill="white"))+
	coord_equal()+
	xlab("Longitude")+
	ylab("Latitude")
print(g)
dev.off()

Knots=Knots[Which.include,]
save(Knots,file="SAGE_Knots_SP_subset.Rda")


