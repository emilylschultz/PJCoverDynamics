### Code to create plots for predicted change in tree cover under different climate conditions
### Created by: Emily Schultz
### Created on: 19 Mar 2023

### Create maps comparing predicted change in PC for one year (with 2020 as year t), between climate only model and models with density
### Cool, wet years

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

dPC_dd_0 <- predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=1,tmax_dev=-1,dens=latMean_dd[37,])
dPC_dd_0 <- exp(dPC_dd_0) - exp(latMean_dd[37,])

values(dPC_raster_dd_0)[which(values(mask)==1)] <- dPC_dd_0

# Predict cover using additive density model
dPC_raster_dd_curr <- mask
values(dPC_raster_dd_curr)[values(dPC_raster_dd_curr) == 0] = NA

dPC_dd_curr <- predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=1,tmax_dev=-1,dens=latMean_dd[37,])
dPC_dd_curr <- (exp(dPC_dd_curr) - exp(latMean_dd[37,])) #/pc_mat_RAP[37,]

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

dPC_ddint_0 <- predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=1,tmax_dev=-1,dens=latMean_ddint[37,])
dPC_ddint_0 <- exp(dPC_ddint_0) - exp(latMean_ddint[37,])

values(dPC_raster_ddint_0)[which(values(mask)==1)] <- dPC_ddint_0

# Predict cover using density interaction model
dPC_raster_ddint_curr <- mask
values(dPC_raster_ddint_curr)[values(dPC_raster_ddint_curr) == 0] = NA

dPC_ddint_curr <- predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=1,tmax_dev=-1,dens=latMean_ddint[37,])
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

png(file="./Output/curr_0_plot_dd_cw.png",7.5,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 1)))

print(dd_curr_0_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(dd_curr_plot, vp = viewport(0.27,0.66,height=0.12))
print(dd_0_plot, vp = viewport(0.625,0.66,height=0.12))
print(curr_0_leg, vp = viewport(0.53,0.5))
dev.off()

png(file="./Output/curr_0_plot_ddint_cw.png",7.5,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 1)))

print(ddint_curr_0_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(ddint_curr_plot, vp = viewport(0.27,0.66,height=0.12))
print(ddint_0_plot, vp = viewport(0.625,0.66,height=0.12))
print(curr_0_leg, vp = viewport(0.53,0.5))
dev.off()

### Warm, dry years

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

dPC_dd_0 <- predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=-1,tmax_dev=1,dens=latMean_dd[37,])
dPC_dd_0 <- exp(dPC_dd_0) - exp(latMean_dd[37,])

values(dPC_raster_dd_0)[which(values(mask)==1)] <- dPC_dd_0

# Predict cover using additive density model
dPC_raster_dd_curr <- mask
values(dPC_raster_dd_curr)[values(dPC_raster_dd_curr) == 0] = NA

dPC_dd_curr <- predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=-1,tmax_dev=1,dens=latMean_dd[37,])
dPC_dd_curr <- (exp(dPC_dd_curr) - exp(latMean_dd[37,])) #/pc_mat_RAP[37,]

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

dPC_ddint_0 <- predict_fun(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=-1,tmax_dev=1,dens=latMean_ddint[37,])
dPC_ddint_0 <- exp(dPC_ddint_0) - exp(latMean_ddint[37,])

values(dPC_raster_ddint_0)[which(values(mask)==1)] <- dPC_ddint_0

# Predict cover using density interaction model
dPC_raster_ddint_curr <- mask
values(dPC_raster_ddint_curr)[values(dPC_raster_ddint_curr) == 0] = NA

dPC_ddint_curr <- predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=-1,tmax_dev=1,dens=latMean_ddint[37,])
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

png(file="./Output/curr_0_plot_dd_wd.png",7.5,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 1)))

print(dd_curr_0_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(dd_curr_plot, vp = viewport(0.27,0.66,height=0.12))
print(dd_0_plot, vp = viewport(0.625,0.66,height=0.12))
print(curr_0_leg, vp = viewport(0.53,0.5))
dev.off()

png(file="./Output/curr_0_plot_ddint_wd.png",7.5,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
	nrow = 1,
	ncol = 1)))

print(ddint_curr_0_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(ddint_curr_plot, vp = viewport(0.27,0.66,height=0.12))
print(ddint_0_plot, vp = viewport(0.625,0.66,height=0.12))
print(curr_0_leg, vp = viewport(0.53,0.5))
dev.off()