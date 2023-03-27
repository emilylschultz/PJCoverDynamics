#### PJ Cover Dynamics: Code to create maps of change in percent cover
## Created by: Emily Schultz
## Created on: 9 Feb 2022

# Load required packages
library(raster)
library(RColorBrewer)
library(plotrix)
library(tmap)
library(tidyverse)
library(sf)
library(grid)
library(MetBrewer)

# Set up color palettes
pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))
pal <- colorRampPalette(brewer.pal(n=9, name = "Greens"))
colors = met.brewer(name="Hokusai3", n=123, type="continuous")

tmap_options(output.dpi=600)

# Load data
load("SamplerDataMask_Lag.rda")
fire <- which(is.na(pc_mat_RAP[20,]))
pc_mat_RAP <- pc_mat_RAP[,-fire]
pc_mat <- pc_mat[,-fire]
X_lag <- X_lag[,-fire,]
Xdense_lag <- Xdense_lag[,-fire,]

PJcover <- stack("./PJCover/PJStack_clipped.tif")

PC.path <-  "./PJCover/"

PCFiles <- list.files(path = PC.path, pattern = glob2rx("RAP*.tif"), full.names = TRUE)

PJcover_RAP <- stack(PCFiles)

# Upload PJ presence/absence data
mask <- raster("./PJCover/PJmask.tif")
values(mask)[which(values(mask==1))[fire]] <- 0

# Set percent cover to NA where PJ are absent
PJcover_mask <- PJcover*mask # use mask (presence/absence) raster to set percent cover to 0
PJcover_mask_RAP <- PJcover_RAP*mask
values(PJcover_mask)[values(PJcover_mask) == 0] = NA # convert 0 to NA
values(PJcover_mask_RAP)[values(PJcover_mask_RAP) == 0] = NA # convert 0 to NA

# Calculate change in percent cover
d_PJcover <- PJcover_mask[[(dim(PJcover_mask)[3])]]-PJcover_mask[[1]] #2000-2016
d_PJcover_RAP_early <- PJcover_mask_RAP[[17]]-PJcover_mask_RAP[[1]] # 1984-2000
d_PJcover_RAP_late <- PJcover_mask_RAP[[(dim(PJcover_mask_RAP)[3]-4)]]-PJcover_mask_RAP[[17]] #2000-2016

## Create map inset showing study site location
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
	tm_shape(cpt) + tm_dots(size=0.1,col="#41A097") + #tm_borders(lwd = 1,col="#C48B3A") + tm_fill(col="#C48B3A")+
	tm_layout(frame.lwd=2)

## Elevation map
alt_data<-getData(name = "alt",
							country = "USA",download=T)
elev<-projectRaster(alt_data[[1]],crs=projection(mask))
elev<-crop(elev,mask)
min_elev <- min(values(elev),na.rm=T)
max_elev <- max(values(elev),na.rm=T)
contour<-rasterToContour(elev,levels=seq(min_elev,max_elev,length=10))

slope = terrain(elev, opt='slope')
aspect = terrain(elev, opt='aspect')

hillshade<-hillShade(slope=slope,aspect=aspect)

elev<-resample(elev,mask)
hillshade<-resample(hillshade,mask)

tm_shape(hillshade)+
	tm_raster(palette="-Greys", style="cont", legend.show=FALSE)+
	tm_shape(elev)+
	tm_raster(alpha=.7, palette=colors, style="cont", title="Elevation (m)")+
	tm_compass(type="arrow", position=c(.15, .05))+
	tm_scale_bar(position = c(0.2, .005), text.size=.8)+
	tm_layout(legend.position= c("left", "bottom"),legend.bg.color="white",legend.frame=T)

## Plot percent cover in different years
load("./Output/modelOut_dd.rda")
cuts<-c(seq(min(na.omit(exp(latMean_dd))),max(na.omit(exp(latMean_dd))),length=5))

latMean_dd_raster1984 <- mask
values(latMean_dd_raster1984)[values(latMean_dd_raster1984) == 0] = NA
values(latMean_dd_raster1984)[which(values(mask)==1)] <- exp(latMean_dd[1,])

latMean_dd_raster2000 <- mask
values(latMean_dd_raster2000)[values(latMean_dd_raster2000) == 0] = NA
values(latMean_dd_raster2000)[which(values(mask)==1)] <- exp(latMean_dd[17,])

latMean_dd_raster2020 <- mask
values(latMean_dd_raster2020)[values(latMean_dd_raster2020) == 0] = NA
values(latMean_dd_raster2020)[which(values(mask)==1)] <- exp(latMean_dd[37,])

cover1984 <- tm_shape(latMean_dd_raster1984) + 
	tm_raster(alpha=.7, palette=colors, breaks=cuts, style="cont", title="Percent Cover") +
	tm_layout(legend.position= c("left", "bottom"),legend.bg.color="white",legend.frame=T)+
	tm_xlab("Easting",size=0.8) +
	tm_ylab("Northing",size=0.8) +
	tm_credits("A. 1984",position=c(0.01,0.9),size=0.8,fontface="bold")

cover2000 <- tm_shape(latMean_dd_raster2000) + 
	tm_raster(alpha=.7, palette=colors, breaks=cuts, style="cont", title="Percent Cover") +
	tm_layout(legend.position= c("left", "bottom"),legend.bg.color="white",legend.frame=T)+
	tm_xlab("Easting",size=0.8) +
	tm_ylab("Northing",size=0.8) +
	tm_credits("B. 2000",position=c(0.01,0.9),size=0.8,fontface="bold")

cover2020 <- tm_shape(latMean_dd_raster2020) + 
	tm_raster(alpha=.7, palette=colors, breaks=cuts, style="cont", title="Percent Cover") +
	tm_layout(legend.position= c("left", "bottom"),legend.bg.color="white",legend.frame=T)+
	tm_xlab("Easting",size=0.8) +
	tm_ylab("Northing",size=0.8) +
	tm_credits("C. 2020",position=c(0.01,0.9),size=0.8,fontface="bold")

# Save cover maps as single file
png(file="./Output/cover.png",10,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 3,
	ncol = 1)))

print(cover1984, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(cover2000, vp = viewport(layout.pos.row = 2,layout.pos.col = 1))
print(cover2020, vp = viewport(layout.pos.row = 3,layout.pos.col = 1))

dev.off()

## Plot change in percent cover

# Break points
min=min(na.omit(values(d_PJcover_RAP_early)))
max=max(na.omit(values(d_PJcover)))

cat_cuts <- c(min,-20,-10,-5,-1,0,1,5,10,20,max)

# Create text for legend
cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

# Create 2000-2016 map for CMS data
d_PJcover_plot <- tm_shape(d_PJcover) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_layout(panel.show=T,panel.labels="2000-2016 CMS",panel.label.size=0.5, outer.margins=c(0.05,0.1,0.05,0.05)) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting",size=0.5) +
	tm_ylab("Northing",size=0.5) +
	tm_credits("B",position=c(0.01,0.8),size=0.7,fontface="bold")

# Create 1984-2000 map for RAP data
d_PJcover_RAP_early_plot <- tm_shape(d_PJcover_RAP_early) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_layout(panel.show=T,panel.labels="1984-2000 RAP",panel.label.size=0.5, outer.margins=c(0.05,0.1,0.05,0.05)) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting",size=0.5) +
	tm_ylab("Northing",size=0.5) +
	tm_credits("A",position=c(0.01,0.8),size=0.8,fontface="bold")

# Create 2000-2016 map for RAP data
d_PJcover_RAP_late_plot <- tm_shape(d_PJcover_RAP_late) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_layout(panel.show=T,panel.labels="2000-2016 RAP",panel.label.size=0.5, outer.margins=c(0.05,0.1,0.05,0.05)) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting",size=0.5) +
	tm_ylab("Northing",size=0.5) +
	tm_credits("C",position=c(0.01,0.8),size=0.8,fontface="bold")

# Create legend for change in percent cover maps
leg<-tm_shape(d_PJcover)+
	tm_raster(palette=pal_div(12), breaks=cat_cuts ,alpha=1,title="",style="fixed",legend.reverse=T)+
	tm_layout(legend.only=T,asp=0.6,legend.position=c(0.5,0.05)) +
	tm_credits("Change in \npercent cover",size=0.9,position=c(0.15,0.4))

### Predicted change in percent cover

## Climate only
load("./Output/modelOut.rda")

# Create function to predict tree cover
predict_fun <- function(coef=meanBeta,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
		coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + dens
}

# Predict tree cover from 1984 to 2000
pred_pc_early <- matrix(NA,16,dim(pc_mat)[2])
pred_pc_early[1,]<-latMean[1,]

for(i in 2:16){
	pred_pc_early[i,]<-exp(predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],
															 ppt_dev=X_lag[1,(i-1),5],tmax_dev=X_lag[1,(i-1),6],
															 dens=log(pred_pc_early[(i-1),])))
}

# Add predicted cover to raster
pred_d_PJcover_early <- mask
values(pred_d_PJcover_early)[values(pred_d_PJcover_early) == 0] = NA

values(pred_d_PJcover_early)[which(values(mask)==1)] <- pred_pc_early[16,]-pred_pc_early[1,]

# Create map
pred_d_PJcover_early_plot <- tm_shape(pred_d_PJcover_early) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting") +
	tm_ylab("Northing")

# Predict tree cover from 2000 to 2016
pred_pc <- matrix(NA,dim(pc_mat)[1],dim(pc_mat)[2])
pred_pc[1,]<-latMean[17,]

for(i in 2:17){
	pred_pc[i,]<-exp(predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],
															 ppt_dev=X_lag[1,(i-1),5],tmax_dev=X_lag[1,(i-1),6],
															 dens=log(pred_pc[(i-1),])))
}

pred_d_PJcover <- mask
values(pred_d_PJcover)[values(pred_d_PJcover) == 0] = NA

values(pred_d_PJcover)[which(values(mask)==1)] <- pred_pc[17,]-pred_pc[1,]

pred_d_PJcover_plot <- tm_shape(pred_d_PJcover) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting") +
	tm_ylab("Northing")

## Climate + density
load("./Output/modelOut_dd.rda")

# Create function to predict tree cover
predict_fun_dd <- function(coef=meanBeta_dd,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
		coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens 
}

# Predict cover for early time period (1984-2000)
pred_pc_dd_early <- matrix(NA,17,dim(pc_mat)[2])
pred_pc_dd_early[1,]<-latMean_dd[1,]

for(i in 2:17){
	pred_pc_dd_early[i,]<-(predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],
																		 ppt_dev=X_lag[(i-1),,5],tmax_dev=X_lag[(i-1),,6],
																		 dens=(pred_pc_dd_early[(i-1),])))
}

# Add predicted values to raster
pred_d_PJcover_dd_early <- mask
values(pred_d_PJcover_dd_early)[values(pred_d_PJcover_dd_early) == 0] = NA

values(pred_d_PJcover_dd_early)[which(values(mask)==1)] <- exp(pred_pc_dd_early[17,])-exp(pred_pc_dd_early[1,])

# Create map
pred_d_PJcover_dd_early_plot <- tm_shape(pred_d_PJcover_dd_early) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_layout(panel.show=T,panel.labels="1984-2000 Climate + density without interactions",panel.label.size=0.5, outer.margins=c(0.05,0.1,0.05,0.05)) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting",size=0.5) +
	tm_ylab("Northing",size=0.5) +
	tm_credits("D",position=c(0.01,0.8),size=0.7,fontface="bold")

# Predict tree cover for late time period (2000-2016)
pred_pc_dd <- matrix(NA,dim(pc_mat)[1],dim(pc_mat)[2])
pred_pc_dd[1,]<-latMean_dd[17,]

for(i in 2:17){
	pred_pc_dd[i,]<-(predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],
																	ppt_dev=X_lag[(i+16),,5],tmax_dev=X_lag[(i+16),,6],
														dens=(pred_pc_dd[(i-1),])))
}

# Add values to raster
pred_d_PJcover_dd <- mask
values(pred_d_PJcover_dd)[values(pred_d_PJcover_dd) == 0] = NA

values(pred_d_PJcover_dd)[which(values(mask)==1)] <- exp(pred_pc_dd[17,])-exp(pred_pc_dd[1,])

# Create map
pred_d_PJcover_dd_plot <- tm_shape(pred_d_PJcover_dd) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_layout(panel.show=T,panel.labels="2000-2016 Climate + density without interactions",panel.label.size=0.5, outer.margins=c(0.05,0.1,0.05,0.05)) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting",size=0.5) +
	tm_ylab("Northing",size=0.5) +
	tm_credits("F",position=c(0.01,0.8),size=0.7,fontface="bold")

## Climate + density, interactions
load("./Output/modelOut_ddint.rda")

# Create function to predict cover
predict_fun_ddint <- function(coef=meanBeta_ddint,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
		coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens +
		coef[12]*dens*heatload + coef[13]*dens*ppt + coef[14]*dens*tmax
}

# Predict cover for early time period (1984-2000)
pred_pc_ddint_early <- matrix(NA,17,dim(pc_mat)[2])
pred_pc_ddint_early[1,]<-latMean_ddint[1,]

for(i in 2:17){
	pred_pc_ddint_early[i,]<-(predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],
																							ppt_dev=X_lag[(i-1),,5],tmax_dev=X_lag[(i-1),,6],
																					 dens=(pred_pc_ddint_early[(i-1),])))
}

# Add values to raster
pred_d_PJcover_ddint_early <- mask
values(pred_d_PJcover_ddint_early)[values(pred_d_PJcover_ddint_early) == 0] = NA

values(pred_d_PJcover_ddint_early)[which(values(mask)==1)] <- exp(pred_pc_ddint_early[17,])-exp(pred_pc_ddint_early[1,])

# Create map
pred_d_PJcover_ddint_early_plot <- tm_shape(pred_d_PJcover_ddint_early) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_layout(panel.show=T,panel.labels="1984-2000 Climate + density with interactions",panel.label.size=0.5, outer.margins=c(0.05,0.1,0.05,0.05)) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting",size=0.5) +
	tm_ylab("Northing",size=0.5) +
	tm_credits("E",position=c(0.01,0.8),size=0.7,fontface="bold")

# Predict tree cover for late time period (2000-2016)
pred_pc_ddint <- matrix(NA,dim(pc_mat)[1],dim(pc_mat)[2])
pred_pc_ddint[1,]<-latMean_ddint[17,]

for(i in 2:17){
	pred_pc_ddint[i,]<-(predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],
																				ppt_dev=X_lag[(i+16),,5],tmax_dev=X_lag[(i+16),,6],
																 dens=(pred_pc_ddint[(i-1),])))
}

# Add values to raster
pred_d_PJcover_ddint <- mask
values(pred_d_PJcover_ddint)[values(pred_d_PJcover_ddint) == 0] = NA

values(pred_d_PJcover_ddint)[which(values(mask)==1)] <- exp(pred_pc_ddint[17,])-exp(pred_pc_ddint[1,])

# Create map
pred_d_PJcover_ddint_plot <- tm_shape(pred_d_PJcover_ddint) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style = "fixed", legend.show=F) +
	tm_layout(panel.show=T,panel.labels="2000-2016 Climate + density with interactions",panel.label.size=0.5, outer.margins=c(0.05,0.1,0.05,0.05)) +
	tm_shape(contour) +
	tm_lines(col="gray60")+
	tm_xlab("Easting",size=0.5) +
	tm_ylab("Northing",size=0.5) +
	tm_credits("G",position=c(0.01,0.8),size=0.7,fontface="bold")

#Arrange observed and predicted change in percent cover plots
png(file="./Output/dPC_plot.png",7.5,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 5,
	ncol = 2, heights=c(0.04,0.24,0.24,0.24,0.24))))

grid.text("1984-2000", vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
grid.text("2000-2016", vp = viewport(layout.pos.row = 1,layout.pos.col = 2))
grid.text("Observed", vp = viewport(0.01,0.75),rot=90)
grid.text("Predicted", vp = viewport(0.01,0.25),rot=90)
grid.lines(x=c(0,1),y=c(0.483,0.483))
print(leg, vp = viewport(layout.pos.row = 2,layout.pos.col = 1))
print(d_PJcover_plot, vp = viewport(layout.pos.row = 2,layout.pos.col = 2))
print(d_PJcover_RAP_early_plot, vp = viewport(layout.pos.row = 3,layout.pos.col = 1))
print(d_PJcover_RAP_late_plot, vp = viewport(layout.pos.row = 3,layout.pos.col = 2))
print(pred_d_PJcover_dd_early_plot, vp = viewport(layout.pos.row = 4,layout.pos.col = 1))
print(pred_d_PJcover_dd_plot, vp = viewport(layout.pos.row = 4,layout.pos.col = 2))
print(pred_d_PJcover_ddint_early_plot, vp = viewport(layout.pos.row = 5,layout.pos.col = 1))
print(pred_d_PJcover_ddint_plot, vp = viewport(layout.pos.row = 5,layout.pos.col = 2))
print(us_map, vp = viewport(0.099,0.56,height=0.065))
dev.off()

### Create maps comparing predicted change in PC for one year (with 2020 as year t), between climate only model and models with density

## Climate only model vs additive density model
load("./Output/modelOut_dd.rda")
load("./Output/modelOut.rda")

# Create functions to predict tree cover
predict_fun <- function(coef=meanBeta,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
	coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + dens
}

predict_fun_dd <- function(coef=meanBeta_dd,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
	coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens 
}

# Predict cover using climate only model
dPC_raster_dd_0 <- mask
values(dPC_raster_dd_0)[values(dPC_raster_dd_0) == 0] = NA

dPC_dd_0 <- predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_dd[37,])
dPC_dd_0 <- exp(dPC_dd_0) - exp(latMean_dd[37,])

values(dPC_raster_dd_0)[which(values(mask)==1)] <- dPC_dd_0

# Predict cover using additive density model
dPC_raster_dd_curr <- mask
values(dPC_raster_dd_curr)[values(dPC_raster_dd_curr) == 0] = NA

dPC_dd_curr <- predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_dd[37,])
dPC_dd_curr <- (exp(dPC_dd_curr) - exp(latMean_dd[37,])) 

values(dPC_raster_dd_curr)[which(values(mask)==1)] <- dPC_dd_curr

dPC_dd_curr_0 <- dPC_raster_dd_curr - dPC_raster_dd_0

# Set up legend text
min <- min(na.omit(c(values(dPC_raster_dd_0),values(dPC_raster_dd_curr),values(dPC_dd_curr_0))))
max <- max(na.omit(c(values(dPC_raster_dd_0),values(dPC_raster_dd_curr),values(dPC_dd_curr_0))))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

# Create plots
cat_cuts <- c(-1.5,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,1.5)

dd_0_plot <- tm_shape(dPC_raster_dd_0) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

dd_curr_plot <- tm_shape(dPC_raster_dd_curr) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

# Arrange and save plots
bbox_new <- st_bbox(dPC_dd_curr_0) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
	st_as_sfc() # ... and make it a sf polygon

dd_curr_0_plot <- tm_shape(dPC_dd_curr_0,bbox=bbox_new) +
	tm_grid(col = "black", n.x = 4, n.y = 2, lines = FALSE,
					labels.rot = c(0, 90)) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed", legend.show=F) +
	tm_layout(outer.margins=c(0.01,0.01,0.01,0.2)) +
	tm_shape(contour) +
	tm_lines(col="gray60") +
	tm_xlab("Easting") +
	tm_ylab("Northing") +
	tm_credits("A",position=c(0.035,0.9)) +
	tm_credits("B",position=c(0.52,0.9)) +
	tm_credits("C",position=c(0.01,0.58))

vp_curr = viewport(0.27, 0.68, width = 0.3)
vp_0 = viewport(0.61, 0.68, width = 0.3)

tmap_save(dd_curr_0_plot,filename="./Output/dd_curr_0_plot.png",
					insets_tm=list(dd_curr_plot,dd_0_plot), insets_vp=list(vp_curr,vp_0),
					height=3000, width=3600, units="px")

## Climate only model vs density interaction model
load("./Output/modelOut_ddint.rda")
load("./Output/modelOut.rda")

# Create functions to predict tree cover
predict_fun <- function(coef=meanBeta,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
	coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + dens
}

predict_fun_ddint <- function(coef=meanBeta_ddint,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
	coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
		coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
		coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens +
		coef[12]*dens*heatload + coef[13]*dens*ppt + coef[14]*dens*tmax
}

# Predict cover using climate only model
dPC_raster_ddint_0 <- mask
values(dPC_raster_ddint_0)[values(dPC_raster_ddint_0) == 0] = NA

dPC_ddint_0 <- predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_ddint[37,])
dPC_ddint_0 <- exp(dPC_ddint_0) - exp(latMean_ddint[37,])

values(dPC_raster_ddint_0)[which(values(mask)==1)] <- dPC_ddint_0

# Predict cover using density interaction model
dPC_raster_ddint_curr <- mask
values(dPC_raster_ddint_curr)[values(dPC_raster_ddint_curr) == 0] = NA

dPC_ddint_curr <- predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_ddint[37,])
dPC_ddint_curr <- (exp(dPC_ddint_curr) - exp(latMean_ddint[37,])) #/pc_mat_RAP[37,]

values(dPC_raster_ddint_curr)[which(values(mask)==1)] <- dPC_ddint_curr

dPC_ddint_curr_0 <- dPC_raster_ddint_curr - dPC_raster_ddint_0

# Set up legend text
min <- min(na.omit(c(values(dPC_raster_ddint_0),values(dPC_raster_ddint_curr),values(dPC_ddint_curr_0))))
max <- max(na.omit(c(values(dPC_raster_ddint_0),values(dPC_raster_ddint_curr),values(dPC_ddint_curr_0))))
cuts<-c(seq(min,0,length=31),
				seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

# Create plots
cat_cuts <- c(-1.5,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,1.5)

ddint_0_plot <- tm_shape(dPC_raster_ddint_0) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

ddint_curr_plot <- tm_shape(dPC_raster_ddint_curr) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

# Arrange and save plots
bbox_new <- st_bbox(dPC_ddint_curr_0) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
	st_as_sfc() # ... and make it a sf polygon

ddint_curr_0_plot <- tm_shape(dPC_ddint_curr_0,bbox=bbox_new) +
	tm_grid(col = "black", n.x = 4, n.y = 2, lines = FALSE,
					labels.rot = c(0, 90)) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed", legend.show=F) +
	tm_layout(outer.margins=c(0.01,0.01,0.01,0.2)) +
	tm_shape(contour) +
	tm_lines(col="gray60") +
	tm_xlab("Easting") +
	tm_ylab("Northing") +
	tm_credits("D",position=c(0.035,0.9)) +
	tm_credits("E",position=c(0.52,0.9)) +
	tm_credits("F",position=c(0.01,0.58))
	
vp_curr = viewport(0.27, 0.68, width = 0.3)
vp_0 = viewport(0.61, 0.68, width = 0.3)
	
curr_0_leg <- tm_shape(dPC_ddint_curr_0) +
	tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed", title="Change in \npercent cover",
						legend.reverse=T) +
	tm_layout(legend.only = T,legend.position=c(0.8,0.4),legend.title.size=1,legend.text.size=0.6)
	
png(file="./Output/curr_0_plot_dd.png",4,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 1)))

print(dd_curr_0_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(dd_curr_plot, vp = viewport(0.27,0.76,height=0.19))
print(dd_0_plot, vp = viewport(0.625,0.76,height=0.19))
print(curr_0_leg, vp = viewport(0.53,0.4))
dev.off()

png(file="./Output/curr_0_plot_ddint.png",7.5,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 1)))

print(ddint_curr_0_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(ddint_curr_plot, vp = viewport(0.27,0.76,height=0.19))
print(ddint_0_plot, vp = viewport(0.625,0.76,height=0.19))
print(curr_0_leg, vp = viewport(0.53,0.4))
dev.off()

## Predictor maps
total_ppt_early <- apply(total_ppt[,3:18],1,mean)
total_ppt_late <- apply(total_ppt[,19:34],1,mean)
ppt_early <- mask
values(ppt_early) <- total_ppt_early
mean_ppt_early <- mean(total_ppt_early)
ppt_late <- mask
values(ppt_late) <- total_ppt_late
mean_ppt_late <- mean(total_ppt_late)

plot(ppt_early, col=met.brewer(name="Hokusai3", n=200, type="continuous"))
plot(ppt_late, col=met.brewer(name="Hokusai3", n=200, type="continuous"))


ave_tmin_early <- apply(ave_tmin[,3:18],1,mean)
ave_tmin_late <- apply(ave_tmin[,19:34],1,mean)
tmin_early <- mask
values(tmin_early) <- ave_tmin_early
mean_tmin_early <- mean(ave_tmin_early)
tmin_late <- mask
values(tmin_late) <- ave_tmin_late
mean_tmin_late <- mean(ave_tmin_late)

plot(tmin_early, col=rev(met.brewer(name="Hokusai3", n=200, type="continuous")))
plot(tmin_late, col=rev(met.brewer(name="Hokusai3", n=200, type="continuous")))


ave_tmax_early <- apply(ave_tmax[,3:18],1,mean)
ave_tmax_late <- apply(ave_tmax[,19:34],1,mean)
tmax_early <- mask
values(tmax_early) <- ave_tmax_early
mean_tmax_early <- mean(ave_tmax_early)
tmax_late <- mask
values(tmax_late) <- ave_tmax_late
mean_tmax_late <- mean(ave_tmax_late)

plot(tmax_early, col=rev(met.brewer(name="Hokusai3", n=200, type="continuous")))
plot(tmax_late, col=rev(met.brewer(name="Hokusai3", n=200, type="continuous")))