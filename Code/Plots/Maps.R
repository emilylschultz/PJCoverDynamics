#### PJ Cover Dynamics: Code to create maps of change in percent cover
## Created by: Emily Schultz
## Created on: 9 Feb 2022

# Load required packages
library(raster)

PJcover <- stack("./PJCover/PJStack_clipped.tif")

PC.path <-  "./PJCover/"

PCFiles <- list.files(path = PC.path, pattern = glob2rx("RAP*.tif"), full.names = TRUE)

PJcover_RAP <- stack(PCFiles)

# Upload PJ presence/absence data
mask <- raster("./PJCover/PJmask.tif")

# Set percent cover to NA where PJ are absent (confirm this with Bob)
PJcover_mask <- PJcover*mask # use mask (presence/absence) raster to set percent cover to 0
PJcover_mask_RAP <- PJcover_RAP*mask
values(PJcover_mask)[values(PJcover_mask) == 0] = NA # convert 0 to NA
values(PJcover_mask_RAP)[values(PJcover_mask_RAP) == 0] = NA # convert 0 to NA

# Calculate change in percent cover
d_PJcover <- PJcover_mask[[(dim(PJcover_mask)[3])]]-PJcover_mask[[1]]
d_PJcover_RAP_early <- PJcover_mask_RAP[[17]]-PJcover_mask_RAP[[1]]
d_PJcover_RAP_late <- PJcover_mask_RAP[[(dim(PJcover_mask_RAP)[3]-4)]]-PJcover_mask_RAP[[17]]


library(RColorBrewer)
library(plotrix)
library(tmap)
library(tidyverse)
library(sf)
library(grid)

# Extract state boundaries
state<- spData::us_states %>% sf::st_transform(4326)
# Extract extent of study area
extent<-st_bbox(mask)%>% 
	st_as_sfc()
# Calculate center of study area
cpt <- coordinates(as(extent(mask), "SpatialPolygons"))
cpt <- SpatialPointsDataFrame(coords = cpt, data = as.data.frame(cpt),proj4string = CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

# Create map of western US with point at study area location
us_map = tm_shape(state$geometry[which(state$REGION=="West")]) +
	tm_borders(col="black",lwd=1)+ 
	tm_shape(cpt) + tm_dots(size=0.5,col="#41A097") + #tm_borders(lwd = 1,col="#C48B3A") + tm_fill(col="#C48B3A")+
	tm_layout(frame.lwd=2)

elev<-getData(name = "alt",
							country = "USA",download=F)
elev<-projectRaster(elev[[1]],crs=projection(mask))
elev<-crop(elev,mask)
elev<-resample(elev,mask)
max_elev<-max(values(elev)[which(values(mask)==1)],na.rm=T)
min_elev<-min(values(elev)[which(values(mask)==1)],na.rm=T)
contour<-rasterToContour(elev,levels=seq(min_elev,max_elev,length=10))


# Set up color palette
pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))

cuts<-c(seq(min(na.omit(values(d_PJcover))),0,length=31),
				seq(0,max(na.omit(values(d_PJcover))),length=31)[2:31])

# Create text for legend
cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

#png(file="./Output/PIED_climint_gam_lam_div.png",4,4,units="in",type="cairo",res=600)

# Plot change in percent cover
plot(d_PJcover, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
plot(contour,add=T,col="lightgrey")
#color.legend(-1924400,1888000,-1924000,1892000,
						 #legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
#par(xpd = TRUE)
#legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
#print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

plot(d_PJcover_RAP_early, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
plot(contour,add=T,col="lightgrey")
#color.legend(-1924400,1888000,-1924000,1892000,
						 #legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
#par(xpd = TRUE)
#legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

plot(d_PJcover_RAP_late, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
plot(contour,add=T,col="lightgrey")
#color.legend(-1924400,1888000,-1924000,1892000,
#						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
#par(xpd = TRUE)
#legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
#print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

leg<-tm_shape(d_PJcover)+
	tm_raster(palette=pal_div(61), breaks=cuts[c(1,11,21,31,41,51,61)] ,alpha=1,title="Change in \npercent cover",style="cont",legend.reverse=T)+
	tm_layout(legend.only=T)
save(file="./Output/Plots/d_PC_legend.png", plot=leg)

## Predicted change in percent cover
#load("SamplerDataMask_Lag.rda")
load("SamplerDataMask_Lag.rda")
load("./Output/modelOut.rda")

predict_fun <- function(coef=meanBeta,e=eta,heatload,ppt,tmin,tmax,ppt_dev,tmin_dev,tmax_dev,dens){
		e + coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmin +
		coef[5]*tmax + coef[6]*ppt_dev + coef[7]*tmin_dev + coef[8]*tmax_dev + coef[9]*heatload*ppt +
		coef[10]*heatload*tmin + coef[11]*heatload*tmax + coef[12]*heatload*ppt_dev + coef[13]*heatload*tmin_dev +
		coef[14]*heatload*tmax_dev + coef[15]*ppt*tmin + coef[16]*ppt*tmax + coef[17]*tmin*tmax + 
		coef[18]*ppt*ppt_dev + coef[19]*tmin*tmin_dev + coef[20]*tmax*tmax_dev + dens
}

pred_pc_early <- matrix(NA,16,dim(pc_mat)[2])
pred_pc_early[1,]<-pc_mat_RAP[1,]

for(i in 2:16){
	pred_pc_early[i,]<-exp(predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],
															 ppt_dev=X_lag[1,(i-1),6],tmin_dev=X_lag[1,(i-1),7],tmax_dev=X_lag[1,(i-1),8],
															 dens=log(pred_pc_early[(i-1),])))
}

pred_d_PJcover_early <- mask
values(pred_d_PJcover_early)[values(pred_d_PJcover_early) == 0] = NA

values(pred_d_PJcover_early)[which(values(mask)==1)] <- pred_pc_early[16,]-pred_pc_early[1,]

plot(pred_d_PJcover_early, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")

color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))



pred_pc <- matrix(NA,dim(pc_mat)[1],dim(pc_mat)[2])
pred_pc[1,]<-pc_mat[1,]

for(i in 2:17){
	pred_pc[i,]<-exp(predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],
																		 ppt_dev=X_lag[1,(i+15),6],tmin_dev=X_lag[1,(i+15),7],tmax_dev=X_lag[1,(i+15),8],
																		 dens=log(pred_pc[(i-1),])))
}

pred_d_PJcover <- mask
values(pred_d_PJcover)[values(pred_d_PJcover) == 0] = NA

values(pred_d_PJcover)[which(values(mask)==1)] <- pred_pc[17,]-pred_pc[1,]

plot(pred_d_PJcover, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
plot(contour,add=T,col="lightgrey")

color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


load("./Output/modelOut_dd.rda")
lat_raster <- PJcover_mask_RAP
values(lat_raster[[1]])[which(values(mask)==1)] <-exp(latMean_dd[1,])

predict_fun_dd <- function(coef=meanBeta_dd,e=eta_dd,heatload,ppt,tmin,tmax,ppt_dev,tmin_dev,tmax_dev,dens){
		e + coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmin +
		coef[5]*tmax + coef[6]*ppt_dev + coef[7]*tmin_dev + coef[8]*tmax_dev + coef[9]*heatload*ppt +
		coef[10]*heatload*tmin + coef[11]*heatload*tmax + coef[12]*heatload*ppt_dev + coef[13]*heatload*tmin_dev +
		coef[14]*heatload*tmax_dev + coef[15]*ppt*tmin + coef[16]*ppt*tmax + coef[17]*tmin*tmax + 
		coef[18]*ppt*ppt_dev + coef[19]*tmin*tmin_dev + coef[20]*tmax*tmax_dev + coef[21]*dens 
}

pred_pc_dd_early <- matrix(NA,17,dim(pc_mat)[2])
pred_pc_dd_early[1,]<-latMean_dd[1,]

for(i in 2:17){
	pred_pc_dd_early[i,]<-(predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],
																		 ppt_dev=X_lag[1,(i-1),6],tmin_dev=X_lag[1,(i-1),7],tmax_dev=X_lag[1,(i-1),8],
																		 dens=(pred_pc_dd_early[(i-1),])))
}

pred_d_PJcover_dd_early <- mask
values(pred_d_PJcover_dd_early)[values(pred_d_PJcover_dd_early) == 0] = NA

values(pred_d_PJcover_dd_early)[which(values(mask)==1)] <- exp(pred_pc_dd_early[17,])-exp(pred_pc_dd_early[1,])

plot(pred_d_PJcover_dd_early, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")

color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


pred_pc_dd <- matrix(NA,dim(pc_mat)[1],dim(pc_mat)[2])
pred_pc_dd[1,]<-latMean_dd[17,]

for(i in 2:17){
	pred_pc_dd[i,]<-(predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],
														ppt_dev=X_lag[1,(i+15),6],tmin_dev=X_lag[1,(i+15),7],tmax_dev=X_lag[1,(i+15),8],
														dens=(pred_pc_dd[(i-1),])))
}

pred_d_PJcover_dd <- mask
values(pred_d_PJcover_dd)[values(pred_d_PJcover_dd) == 0] = NA

values(pred_d_PJcover_dd)[which(values(mask)==1)] <- exp(pred_pc_dd[17,])-exp(pred_pc_dd[1,])

cuts<-c(seq(min(na.omit(values(pred_d_PJcover_dd))),0,length=31),
				seq(0,max(na.omit(values(pred_d_PJcover_dd))),length=31)[2:31])

# Create text for legend
cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(pred_d_PJcover_dd, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

# No density effect
meanBeta_nodd <- meanBeta_dd
meanBeta_nodd[21] <- 1

pred_pc_nodd <- matrix(NA,dim(pc_mat)[1],dim(pc_mat)[2])
pred_pc_nodd[1,]<-latMean_dd[17,]

for(i in 2:17){
	pred_pc_nodd[i,]<-(predict_fun_dd(coef=meanBeta_nodd,heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],
																		 ppt_dev=X_lag[1,(i+15),6],tmin_dev=X_lag[1,(i+15),7],tmax_dev=X_lag[1,(i+15),8],
																		 dens=(pred_pc_nodd[(i-1),])))
}

pred_d_PJcover_nodd <- mask
values(pred_d_PJcover_nodd)[values(pred_d_PJcover_nodd) == 0] = NA

values(pred_d_PJcover_nodd)[which(values(mask)==1)] <- exp(pred_pc_nodd[17,])-exp(pred_pc_nodd[1,])

pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))

cuts<-c(seq(min(na.omit(values(pred_d_PJcover_nodd))),0,length=31),
				seq(0,max(na.omit(values(pred_d_PJcover_nodd))),length=31)[2:31])

# Create text for legend
cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(pred_d_PJcover_nodd, breaks=cuts[31:61], col=pal_div(61)[31:61], xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text[4:7],rect.col=pal_div(61)[31:61],align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


dens_eff <- pred_d_PJcover_dd-pred_d_PJcover_nodd
pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))

cuts<-c(seq(min(na.omit(values(dens_eff))),0,length=31),
				seq(0,max(na.omit(values(dens_eff))),length=31)[2:31])

# Create text for legend
cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(dens_eff, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


load("./Output/modelOut_ddint.rda")

predict_fun_ddint <- function(coef=meanBeta,e=eta,heatload,ppt,tmin,tmax,ppt_dev,tmin_dev,tmax_dev,dens){
		e + coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmin +
		coef[5]*tmax + coef[6]*ppt_dev + coef[7]*tmin_dev + coef[8]*tmax_dev + coef[9]*heatload*ppt +
		coef[10]*heatload*tmin + coef[11]*heatload*tmax + coef[12]*heatload*ppt_dev + coef[13]*heatload*tmin_dev +
		coef[14]*heatload*tmax_dev + coef[15]*ppt*tmin + coef[16]*ppt*tmax + coef[17]*tmin*tmax + 
		coef[18]*ppt*ppt_dev + coef[19]*tmin*tmin_dev + coef[20]*tmax*tmax_dev + coef[21]*dens +
		coef[22]*dens*heatload + coef[23]*dens*ppt + coef[24]*dens*tmin + coef[25]*dens*tmax +
		coef[26]*dens*ppt_dev + coef[27]*dens*tmin_dev + coef[28]*dens*tmax_dev
}

pred_pc_ddint_early <- matrix(NA,16,dim(pc_mat)[2])
pred_pc_ddint_early[1,]<-pc_mat_RAP[1,]

for(i in 2:16){
	pred_pc_ddint_early[i,]<-exp(predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],
																					 ppt_dev=X_lag[1,(i-1),6],tmin_dev=X_lag[1,(i-1),7],tmax_dev=X_lag[1,(i-1),8],
																					 dens=log(pred_pc_ddint_early[(i-1),])))
}

pred_d_PJcover_ddint_early <- mask
values(pred_d_PJcover_ddint_early)[values(pred_d_PJcover_ddint_early) == 0] = NA

values(pred_d_PJcover_ddint_early)[which(values(mask)==1)] <- pred_pc_ddint_early[16,]-pred_pc_ddint_early[1,]

plot(pred_d_PJcover_ddint_early, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


pred_pc_ddint <- matrix(NA,dim(pc_mat)[1],dim(pc_mat)[2])
pred_pc_ddint[1,]<-pc_mat[1,]

for(i in 2:17){
	pred_pc_ddint[i,]<-exp(predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],
																 ppt_dev=X_lag[1,(i+15),6],tmin_dev=X_lag[1,(i+15),7],tmax_dev=X_lag[1,(i+15),8],
																 dens=log(pred_pc_ddint[(i-1),])))
}

pred_d_PJcover_ddint <- mask
values(pred_d_PJcover_ddint)[values(pred_d_PJcover_ddint) == 0] = NA

values(pred_d_PJcover_ddint)[which(values(mask)==1)] <- pred_pc_ddint[17,]-pred_pc_ddint[1,]

plot(pred_d_PJcover_ddint, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F) 
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

## Equilibrium density
load("SamplerDataMask_Lag.rda")
load("./Output/modelOut_dd.rda")
pal <- colorRampPalette(brewer.pal(n=9, name = "Greens"))

equil_raster_dd <- mask
values(equil_raster_dd)[values(equil_raster_dd) == 0] = NA

eq_fun <- function(coef=meanBeta,e=eta,heatload,ppt,tmin,tmax,ppt_dev=0,tmin_dev=0,tmax_dev=0){
	  (e + coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmin +
		coef[5]*tmax + coef[6]*ppt_dev + coef[7]*tmin_dev + coef[8]*tmax_dev + coef[9]*heatload*ppt +
		coef[10]*heatload*tmin + coef[11]*heatload*tmax + coef[12]*heatload*ppt_dev + coef[13]*heatload*tmin_dev +
		coef[14]*heatload*tmax_dev + coef[15]*ppt*tmin + coef[16]*ppt*tmax + coef[17]*tmin*tmax + 
		coef[18]*ppt*ppt_dev + coef[19]*tmin*tmin_dev + coef[20]*tmax*tmax_dev)/(1-coef[21]) 
}

equil_dd <- eq_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5])

values(equil_raster_dd)[which(values(mask)==1)] <- exp(equil_dd)

cuts<-seq(min(na.omit(values(equil_raster_dd))),max(na.omit(values(equil_raster_dd))),length=61)
cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(equil_raster_dd, breaks=cuts, col=pal(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="grey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Equilibrium \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


load("./Output/modelOut_ddint.rda")
pal <- colorRampPalette(brewer.pal(n=9, name = "Greens"))

equil_raster_ddint <- mask
values(equil_raster_ddint)[values(equil_raster_ddint) == 0] = NA

eq_fun_ddint <- function(coef=meanBeta,e=eta,heatload,ppt,tmin,tmax,ppt_dev=0,tmin_dev=0,tmax_dev=0){
	(e + coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmin +
		 	coef[5]*tmax + coef[6]*ppt_dev + coef[7]*tmin_dev + coef[8]*tmax_dev + coef[9]*heatload*ppt +
		 	coef[10]*heatload*tmin + coef[11]*heatload*tmax + coef[12]*heatload*ppt_dev + coef[13]*heatload*tmin_dev +
		 	coef[14]*heatload*tmax_dev + coef[15]*ppt*tmin + coef[16]*ppt*tmax + coef[17]*tmin*tmax + 
		 	coef[18]*ppt*ppt_dev + coef[19]*tmin*tmin_dev + coef[20]*tmax*tmax_dev)/(1-(coef[21] + coef[22]*heatload + coef[23]*ppt + coef[24]*tmin + coef[25]*tmax +
		 																																							coef[26]*ppt_dev + coef[27]*tmin_dev + coef[28]*tmax_dev)) 
}

equil_ddint <- eq_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],ppt_dev=0,tmin_dev=0,tmax_dev=0)
equil_ddint[which(equil_ddint>(log(100)))]=log(100)
equil_ddint<-exp(equil_ddint)

values(equil_raster_ddint)[which(values(mask)==1)] <- equil_ddint

cuts<-seq(min(na.omit(values(equil_raster_ddint))),max(na.omit(values(equil_raster_ddint))),length=61)
cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(equil_raster_ddint, breaks=cuts, col=pal(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Equilibrium \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))




## Change in PC without density effects
load("SamplerDataMask_Lag.rda")
load("./Output/modelOut_dd.rda")
pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))

dPC_raster_dd_0 <- mask
values(dPC_raster_dd_0)[values(dPC_raster_dd_0) == 0] = NA

predict_fun_dd <- function(coef=meanBeta,e=eta,heatload,ppt,tmin,tmax,ppt_dev,tmin_dev,tmax_dev,dens){
	e + coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmin +
		coef[5]*tmax + coef[6]*ppt_dev + coef[7]*tmin_dev + coef[8]*tmax_dev + coef[9]*heatload*ppt +
		coef[10]*heatload*tmin + coef[11]*heatload*tmax + coef[12]*heatload*ppt_dev + coef[13]*heatload*tmin_dev +
		coef[14]*heatload*tmax_dev + coef[15]*ppt*tmin + coef[16]*ppt*tmax + coef[17]*tmin*tmax + 
		coef[18]*ppt*ppt_dev + coef[19]*tmin*tmin_dev + coef[20]*tmax*tmax_dev + coef[21]*dens 
}


meanBeta_nodd <- meanBeta_dd
meanBeta_nodd[21] <- 1

dPC_raster_dd_0 <- mask
values(dPC_raster_dd_0)[values(dPC_raster_dd_0) == 0] = NA

dPC_dd_0 <- predict_fun_dd(coef=meanBeta_nodd,heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],ppt_dev=1,tmin_dev=-1,tmax_dev=-1,dens=latMean_dd[37,])
dPC_dd_0 <- exp(dPC_dd_0) - exp(latMean_dd[37,])

values(dPC_raster_dd_0)[which(values(mask)==1)] <- dPC_dd_0


dPC_raster_dd_curr <- mask
values(dPC_raster_dd_curr)[values(dPC_raster_dd_curr) == 0] = NA

dPC_dd_curr <- predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],ppt_dev=1,tmin_dev=-1,tmax_dev=-1,dens=latMean_dd[37,])
dPC_dd_curr <- (exp(dPC_dd_curr) - exp(latMean_dd[37,])) #/pc_mat_RAP[37,]

values(dPC_raster_dd_curr)[which(values(mask)==1)] <- dPC_dd_curr

dPC_curr_0 <- dPC_raster_dd_curr - dPC_raster_dd_0

pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))

min <- min(na.omit(values(dPC_raster_dd_0)))
max <- max(na.omit(values(dPC_raster_dd_0)))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))
legend_text_pos <-(c(cuts_round[31],cuts_round[36],cuts_round[41],cuts_round[46],cuts_round[51],cuts_round[56],cuts_round[61]))

plot(dPC_raster_dd_0, breaks=cuts[31:61], col=pal_div(61)[31:61], xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text_pos,rect.col=pal_div(61)[31:61],align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

min <- min(na.omit(values(dPC_raster_dd_curr)))
max <- max(na.omit(values(dPC_raster_dd_curr)))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(dPC_raster_dd_curr, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


min <- min(na.omit(values(dPC_curr_0)))
max <- max(na.omit(values(dPC_curr_0)))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))
legend_text_neg <-(c(cuts_round[1],cuts_round[6],cuts_round[11],cuts_round[16],cuts_round[21],cuts_round[26],cuts_round[31]))

plot(dPC_curr_0, breaks=cuts[1:31], col=pal_div(61)[1:31], xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text_neg,rect.col=pal_div(61)[1:31],align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


load("./Output/modelOut_ddint.rda")
pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))

predict_fun_ddint <- function(coef=meanBeta_ddint,e=eta_ddint,heatload,ppt,tmin,tmax,ppt_dev,tmin_dev,tmax_dev,dens){
	e + coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmin +
		coef[5]*tmax + coef[6]*ppt_dev + coef[7]*tmin_dev + coef[8]*tmax_dev + coef[9]*heatload*ppt +
		coef[10]*heatload*tmin + coef[11]*heatload*tmax + coef[12]*heatload*ppt_dev + coef[13]*heatload*tmin_dev +
		coef[14]*heatload*tmax_dev + coef[15]*ppt*tmin + coef[16]*ppt*tmax + coef[17]*tmin*tmax + 
		coef[18]*ppt*ppt_dev + coef[19]*tmin*tmin_dev + coef[20]*tmax*tmax_dev + coef[21]*dens +
		coef[22]*dens*heatload + coef[23]*dens*ppt + coef[24]*dens*tmin + coef[25]*dens*tmax +
		coef[26]*dens*ppt_dev + coef[27]*dens*tmin_dev + coef[28]*dens*tmax_dev
}

meanBeta_noddint <- meanBeta_ddint
meanBeta_noddint[21] <- 1

dPC_raster_ddint_0 <- mask
values(dPC_raster_ddint_0)[values(dPC_raster_ddint_0) == 0] = NA

dPC_ddint_0 <- predict_fun_ddint(coef=meanBeta_noddint,heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],ppt_dev=1,tmin_dev=-1,tmax_dev=-1,dens=latMean_dd[37,])
dPC_ddint_0 <- exp(dPC_ddint_0) - exp(latMean_ddint[37,])

values(dPC_raster_dd_0)[which(values(mask)==1)] <- dPC_dd_0


dPC_raster_ddint_curr <- mask
values(dPC_raster_ddint_curr)[values(dPC_raster_ddint_curr) == 0] = NA

dPC_ddint_curr <- predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmin=X_lag[1,,4],tmax=X_lag[1,,5],ppt_dev=0,tmin_dev=0,tmax_dev=0,dens=latMean_ddint[37,])
dPC_ddint_curr <- (exp(dPC_ddint_curr) - exp(latMean_ddint[37,])) #/pc_mat_RAP[37,]

values(dPC_raster_ddint_curr)[which(values(mask)==1)] <- dPC_ddint_curr

dPC_curr_0 <- dPC_raster_dd_curr - dPC_raster_dd_0

pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))

min <- min(na.omit(values(dPC_raster_dd_0)))
max <- max(na.omit(values(dPC_raster_dd_0)))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))
legend_text_pos <-(c(cuts_round[31],cuts_round[36],cuts_round[41],cuts_round[46],cuts_round[51],cuts_round[56],cuts_round[61]))

plot(dPC_raster_dd_0, breaks=cuts[31:61], col=pal_div(61)[31:61], xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text_pos,rect.col=pal_div(61)[31:61],align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

min <- min(na.omit(values(dPC_raster_ddint_curr)))
max <- max(na.omit(values(dPC_raster_ddint_curr)))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(dPC_raster_ddint_curr, breaks=cuts, col=pal_div(61), xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text,rect.col=pal_div(61),align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))


min <- min(na.omit(values(dPC_curr_0)))
max <- max(na.omit(values(dPC_curr_0)))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))
legend_text_neg <-(c(cuts_round[1],cuts_round[6],cuts_round[11],cuts_round[16],cuts_round[21],cuts_round[26],cuts_round[31]))

plot(dPC_curr_0, breaks=cuts[1:31], col=pal_div(61)[1:31], xlab="Longitude", ylab="Latitude",cex.axis=0.8,cex.lab=1.1,
		 legend=F)
plot(contour,add=T,col="lightgrey")
color.legend(-1924400,1888000,-1924000,1892000,
						 legend=legend_text_neg,rect.col=pal_div(61)[1:31],align="rb",gradient="y",cex=0.9)
par(xpd = TRUE)
legend(x=-1925500,y=1894500,legend="Change in \npercent cover",bty = "n",cex=1)
print(us_map, vp = viewport(0.17, 0.31, width = 0.25, height = 0.25))

