# Code to compare tree cover predicted by the model to latent tree cover
# Created by: Emily Schultz
# Created on: 19 Mar 2023

library(ggplot2)
## Density 
load("./Output/modelOut_dd.rda")
load("./Output/latent_dd.rda")
load("SamplerDataMask_Lag.rda")

mean_lat <- apply(latOut,1:2,mean)

predict_fun_dd <- function(coef=meanBeta_dd,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
	coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens 
}

pred_pc_dd_early <- matrix(NA,17,length(latentkeep))
pred_pc_dd_early[1,]<-latMean_dd[1,latentkeep]

for(i in 2:17){
	pred_pc_dd_early[i,]<-(predict_fun_dd(heatload=X_lag[1,latentkeep,2],ppt=X_lag[1,latentkeep,3],tmax=X_lag[1,latentkeep,4],
																							ppt_dev=X_lag[(i-1),latentkeep,5],tmax_dev=X_lag[(i-1),latentkeep,6],
																							dens=(pred_pc_dd_early[(i-1),])))
}


pred_dpc_dd_early <- exp(pred_pc_dd_early[17,])-exp(pred_pc_dd_early[1,])
lat_dpc_dd_early <- exp(mean_lat[17,])-exp(mean_lat[1,])

pred_pc_dd <- matrix(NA,17,length(latentkeep))
pred_pc_dd[1,]<-latMean_dd[17,latentkeep]

for(i in 2:17){
	pred_pc_dd[i,]<-(predict_fun_dd(heatload=X_lag[1,latentkeep,2],ppt=X_lag[1,latentkeep,3],tmax=X_lag[1,latentkeep,4],
																				ppt_dev=X_lag[(i+16),latentkeep,5],tmax_dev=X_lag[(i+16),latentkeep,6],
																				dens=(pred_pc_dd[(i-1),])))
}


pred_dpc_dd <- exp(pred_pc_dd[17,])-exp(pred_pc_dd[1,])
lat_dpc_dd <- exp(mean_lat[37,])-exp(mean_lat[17,])

latpred_data <- data.frame(Pred_early = pred_dpc_dd_early, Lat_early = lat_dpc_dd_early,
													 Pred_late = pred_dpc_dd, Lat_late = lat_dpc_dd)

Early_plot <- ggplot(latpred_data,aes(x=Lat_early, y = Pred_early)) +
	geom_point() +
	labs(x = "Latent change in cover", y = "Predicted change in cover", title = "1984 - 2000") +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))


Late_plot <- ggplot(latpred_data,aes(x=Lat_late, y = Pred_late)) +
	geom_point() +
	labs(x = "Latent change in cover", y = "Predicted change in cover", title = "2000 - 2016") +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))


png(file="./Output/PredLat_dd.png",9,3,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 2)))

print(Early_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(Late_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 2))

dev.off()

## Density Interaction
load("./Output/modelOut_ddint.rda")
load("./Output/latent_ddint.rda")
load("SamplerDataMask_Lag.rda")

mean_lat <- apply(latOut,1:2,mean)
	
predict_fun_ddint <- function(coef=meanBeta_ddint,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
	coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens +
		coef[12]*dens*heatload + coef[13]*dens*ppt + coef[14]*dens*tmax
}

pred_pc_ddint_early <- matrix(NA,17,length(latentkeep))
pred_pc_ddint_early[1,]<-latMean_ddint[1,latentkeep]

for(i in 2:17){
	pred_pc_ddint_early[i,]<-(predict_fun_ddint(heatload=X_lag[1,latentkeep,2],ppt=X_lag[1,latentkeep,3],tmax=X_lag[1,latentkeep,4],
																							ppt_dev=X_lag[(i-1),latentkeep,5],tmax_dev=X_lag[(i-1),latentkeep,6],
																							dens=(pred_pc_ddint_early[(i-1),])))
}


pred_dpc_ddint_early <- exp(pred_pc_ddint_early[17,])-exp(pred_pc_ddint_early[1,])
lat_dpc_ddint_early <- exp(mean_lat[17,])-exp(mean_lat[1,])

pred_pc_ddint <- matrix(NA,17,length(latentkeep))
pred_pc_ddint[1,]<-latMean_ddint[17,latentkeep]

for(i in 2:17){
	pred_pc_ddint[i,]<-(predict_fun_ddint(heatload=X_lag[1,latentkeep,2],ppt=X_lag[1,latentkeep,3],tmax=X_lag[1,latentkeep,4],
																				ppt_dev=X_lag[(i+16),latentkeep,5],tmax_dev=X_lag[(i+16),latentkeep,6],
																				dens=(pred_pc_ddint[(i-1),])))
}


pred_dpc_ddint <- exp(pred_pc_ddint[17,])-exp(pred_pc_ddint[1,])
lat_dpc_ddint <- exp(mean_lat[37,])-exp(mean_lat[17,])

latpred_data <- data.frame(Pred_early = pred_dpc_ddint_early, Lat_early = lat_dpc_ddint_early,
													 Pred_late = pred_dpc_ddint, Lat_late = lat_dpc_ddint)

Early_plot <- ggplot(latpred_data,aes(x=Lat_early, y = Pred_early)) +
	geom_point() +
	labs(x = "Latent change in cover", y = "Predicted change in cover", title = "1984 - 2000") +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))


Late_plot <- ggplot(latpred_data,aes(x=Lat_late, y = Pred_late)) +
	geom_point() +
	labs(x = "Latent change in cover", y = "Predicted change in cover", title = "2000 - 2016") +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))


png(file="./Output/PredLat.png",9,3,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 2)))

print(Early_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(Late_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 2))

dev.off()

