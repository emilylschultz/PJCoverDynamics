## Change in PC for one year (with 2020 as year t)
load("./Output/modelOut_dd.rda")

dPC_raster_dd_0 <- mask
values(dPC_raster_dd_0)[values(dPC_raster_dd_0) == 0] = NA

predict_fun_dd <- function(coef=meanBeta_dd,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
  coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
    coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
    coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens 
}


meanBeta_nodd <- meanBeta_dd
meanBeta_nodd[21] <- 1

dPC_raster_dd_0 <- mask
values(dPC_raster_dd_0)[values(dPC_raster_dd_0) == 0] = NA

dPC_dd_0 <- predict_fun_dd(coef=meanBeta_nodd,heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_dd[37,])
dPC_dd_0 <- exp(dPC_dd_0) - exp(latMean_dd[37,])

values(dPC_raster_dd_0)[which(values(mask)==1)] <- dPC_dd_0


dPC_raster_dd_curr <- mask
values(dPC_raster_dd_curr)[values(dPC_raster_dd_curr) == 0] = NA

dPC_dd_curr <- predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_dd[37,])
dPC_dd_curr <- (exp(dPC_dd_curr) - exp(latMean_dd[37,])) #/pc_mat_RAP[37,]

values(dPC_raster_dd_curr)[which(values(mask)==1)] <- dPC_dd_curr

dPC_dd_curr_0 <- dPC_raster_dd_curr - dPC_raster_dd_0

min <- min(na.omit(c(values(dPC_raster_dd_0),values(dPC_raster_dd_curr),values(dPC_dd_curr_0))))
max <- max(na.omit(c(values(dPC_raster_dd_0),values(dPC_raster_dd_curr),values(dPC_dd_curr_0))))
cuts<-c(seq(min,0,length=31),
        seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

cat_cuts <- c(min,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,max)
dd_0_plot <- tm_shape(dPC_raster_dd_0) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

dd_curr_plot <- tm_shape(dPC_raster_dd_curr) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

bbox_new <- st_bbox(dPC_dd_curr_0) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

dd_curr_0_plot <- tm_shape(dPC_dd_curr_0,bbox=bbox_new) +
  tm_grid(col = "black", n.x = 4, n.y = 2, lines = FALSE,
          labels.rot = c(0, 90)) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed", title="Change in \npercent cover",
            legend.reverse=T) +
  tm_layout(legend.outside=T,legend.position = c("left","center"),
            legend.outside.size=0.2,legend.title.size=0.8,legend.text.size=0.6) +
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
          insets_tm=list(ddint_curr_plot,ddint_0_plot), insets_vp=list(vp_curr,vp_0),
          height=3000, width=3600, units="px")

load("./Output/modelOut_ddint.rda")

predict_fun_ddint <- function(coef=meanBeta_ddint,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
  coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
    coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
    coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens +
    coef[12]*dens*heatload + coef[13]*dens*ppt + coef[14]*dens*tmax
}

meanBeta_noddint <- meanBeta_ddint
meanBeta_noddint[21] <- 1
meanBeta_noddint[22:28] <- 0

dPC_raster_ddint_0 <- mask
values(dPC_raster_ddint_0)[values(dPC_raster_ddint_0) == 0] = NA

dPC_ddint_0 <- predict_fun_ddint(coef=meanBeta_noddint,heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_ddint[37,])
dPC_ddint_0 <- exp(dPC_ddint_0) - exp(latMean_ddint[37,])

values(dPC_raster_ddint_0)[which(values(mask)==1)] <- dPC_ddint_0

dPC_raster_ddint_curr <- mask
values(dPC_raster_ddint_curr)[values(dPC_raster_ddint_curr) == 0] = NA

dPC_ddint_curr <- predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_ddint[37,])
dPC_ddint_curr <- (exp(dPC_ddint_curr) - exp(latMean_ddint[37,])) #/pc_mat_RAP[37,]

values(dPC_raster_ddint_curr)[which(values(mask)==1)] <- dPC_ddint_curr

dPC_ddint_curr_0 <- dPC_raster_ddint_curr - dPC_raster_ddint_0

min <- min(na.omit(c(values(dPC_raster_ddint_0),values(dPC_raster_ddint_curr),values(dPC_ddint_curr_0))))
max <- max(na.omit(c(values(dPC_raster_ddint_0),values(dPC_raster_ddint_curr),values(dPC_ddint_curr_0))))
cuts<-c(seq(min,0,length=31),
        seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

cat_cuts <- c(min,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,max)
ddint_0_plot <- tm_shape(dPC_raster_ddint_0) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

ddint_curr_plot <- tm_shape(dPC_raster_ddint_curr) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

bbox_new <- st_bbox(dPC_ddint_curr_0) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

ddint_curr_0_plot <- tm_shape(dPC_ddint_curr_0,bbox=bbox_new) +
  tm_grid(col = "black", n.x = 4, n.y = 2, lines = FALSE,
          labels.rot = c(0, 90)) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed", title="Change in \npercent cover",
            legend.reverse=T) +
  tm_layout(legend.outside=T,legend.position = c("left","center"),
            legend.outside.size=0.2,legend.title.size=0.8,legend.text.size=0.6) +
  tm_shape(contour) +
  tm_lines(col="gray60") +
  tm_xlab("Easting") +
  tm_ylab("Northing") +
  tm_credits("D",position=c(0.035,0.9)) +
  tm_credits("E",position=c(0.52,0.9)) +
  tm_credits("F",position=c(0.01,0.58))

vp_curr = viewport(0.27, 0.68, width = 0.3)
vp_0 = viewport(0.61, 0.68, width = 0.3)

tmap_save(ddint_curr_0_plot,filename="./Output/ddint_curr_0_plot.png",
          insets_tm=list(ddint_curr_plot,ddint_0_plot), insets_vp=list(vp_curr,vp_0),
          height=3000, width=3600, units="px")

## Change in PC for one year (with 2020 as year t), average density
load("./Output/modelOut_dd.rda")

predict_fun_dd <- function(coef=meanBeta_dd,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
  coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
    coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
    coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens 
}

dPC_raster_dd_0 <- mask
values(dPC_raster_dd_0)[values(dPC_raster_dd_0) == 0] = NA

dPC_dd_0 <- predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=mean(latMean_dd[37,]))
dPC_dd_0 <- exp(dPC_dd_0) - exp(mean(latMean_dd[37,]))

values(dPC_raster_dd_0)[which(values(mask)==1)] <- dPC_dd_0

dPC_raster_dd_curr <- mask
values(dPC_raster_dd_curr)[values(dPC_raster_dd_curr) == 0] = NA

dPC_dd_curr <- predict_fun_dd(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_dd[37,])
dPC_dd_curr <- (exp(dPC_dd_curr) - exp(latMean_dd[37,])) #/pc_mat_RAP[37,]

values(dPC_raster_dd_curr)[which(values(mask)==1)] <- dPC_dd_curr

dPC_dd_curr_0 <- dPC_raster_dd_curr - dPC_raster_dd_0

min <- min(na.omit(c(values(dPC_raster_dd_0),values(dPC_raster_dd_curr),values(dPC_dd_curr_0))))
max <- max(na.omit(c(values(dPC_raster_dd_0),values(dPC_raster_dd_curr),values(dPC_dd_curr_0))))
cuts<-c(seq(min,0,length=31),
        seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

cat_cuts <- c(min,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,max)
dd_0_plot <- tm_shape(dPC_raster_dd_0) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

dd_curr_plot <- tm_shape(dPC_raster_dd_curr) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

bbox_new <- st_bbox(dPC_dd_curr_0) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

dd_curr_0_plot <- tm_shape(dPC_dd_curr_0,bbox=bbox_new) +
  tm_grid(col = "black", n.x = 4, n.y = 2, lines = FALSE,
          labels.rot = c(0, 90)) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed", title="Change in \npercent cover",
            legend.reverse=T) +
  tm_layout(legend.outside=T,legend.position = c("left","center"),
            legend.outside.size=0.2,legend.title.size=0.8,legend.text.size=0.6) +
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


load("./Output/modelOut_ddint.rda")

predict_fun_ddint <- function(coef=meanBeta_ddint,heatload,ppt,tmax,ppt_dev,tmax_dev,dens){
  coef[1] + coef[2]*heatload + coef[3]*ppt + coef[4]*tmax + 
    coef[5]*ppt_dev + coef[6]*tmax_dev + coef[7]*ppt*ppt_dev + coef[8]*tmax*tmax_dev + 
    coef[9]*ppt*ppt + coef[10]*tmax*tmax + coef[11]*dens +
    coef[12]*dens*heatload + coef[13]*dens*ppt + coef[14]*dens*tmax
}

dPC_raster_ddint_0 <- mask
values(dPC_raster_ddint_0)[values(dPC_raster_ddint_0) == 0] = NA

dPC_ddint_0 <- predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=mean(latMean_ddint[37,]))
dPC_ddint_0 <- exp(dPC_ddint_0) - exp(mean(latMean_ddint[37,]))

values(dPC_raster_ddint_0)[which(values(mask)==1)] <- dPC_ddint_0

dPC_raster_ddint_curr <- mask
values(dPC_raster_ddint_curr)[values(dPC_raster_ddint_curr) == 0] = NA

dPC_ddint_curr <- predict_fun_ddint(heatload=X_lag[1,,2],ppt=X_lag[1,,3],tmax=X_lag[1,,4],ppt_dev=0,tmax_dev=0,dens=latMean_ddint[37,])
dPC_ddint_curr <- (exp(dPC_ddint_curr) - exp(latMean_ddint[37,])) #/pc_mat_RAP[37,]

values(dPC_raster_ddint_curr)[which(values(mask)==1)] <- dPC_ddint_curr

dPC_ddint_curr_0 <- dPC_raster_ddint_curr - dPC_raster_ddint_0

min <- min(na.omit(c(values(dPC_raster_ddint_0),values(dPC_raster_ddint_curr),values(dPC_ddint_curr_0))))
max <- max(na.omit(c(values(dPC_raster_ddint_0),values(dPC_raster_ddint_curr),values(dPC_ddint_curr_0))))
cuts<-c(seq(min,0,length=31),
        seq(0,max,length=31)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

cat_cuts <- c(min,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,max)
ddint_0_plot <- tm_shape(dPC_raster_ddint_0) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

ddint_curr_plot <- tm_shape(dPC_raster_ddint_curr) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed",legend.show=F)

bbox_new <- st_bbox(dPC_ddint_curr_0) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

ddint_curr_0_plot <- tm_shape(dPC_ddint_curr_0,bbox=bbox_new) +
  tm_grid(col = "black", n.x = 4, n.y = 2, lines = FALSE,
          labels.rot = c(0, 90)) +
  tm_raster(breaks=cat_cuts, palette=pal_div(12), style="fixed", title="Change in \npercent cover",
            legend.reverse=T) +
  tm_layout(legend.outside=T,legend.position = c("left","center"),
            legend.outside.size=0.2,legend.title.size=0.8,legend.text.size=0.6) +
  tm_shape(contour) +
  tm_lines(col="gray60") +
  tm_xlab("Easting") +
  tm_ylab("Northing") +
  tm_credits("D",position=c(0.035,0.9)) +
  tm_credits("E",position=c(0.52,0.9)) +
  tm_credits("F",position=c(0.01,0.58))

vp_curr = viewport(0.27, 0.68, width = 0.3)
vp_0 = viewport(0.61, 0.68, width = 0.3)

tmap_save(ddint_curr_0_plot,filename="./Output/ddint_curr_0_plot.png",
          insets_tm=list(ddint_curr_plot,ddint_0_plot), insets_vp=list(vp_curr,vp_0),
          height=3000, width=3600, units="px")