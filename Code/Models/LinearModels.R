#### PJ Cover Dynamics: Code to create linear models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 21 Jul 2021

library(tidyverse)
library(lme4)
library(bbmle)
library(dotwhisker)
library(IDPmisc)
library(DHARMa)

# Load data
PJdata <- read.csv("PJcover_data.csv")

# Remove data points with fire
PJdata <- subset(PJdata,Fire==0)

PJdata <- PJdata %>%
	mutate(PJdata, Location=str_c(as.character(location.x),as.character(location.y))) %>%
	arrange(Year_t,Location) %>%
	mutate(PJdata, Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))

# Summarize climate variable to calculate spatially-varying climate normals
PJdata_space <- PJdata %>%
	group_by(location.x,location.y) %>%
	summarise(PPT_mean=mean(PPT,na.rm=T), Tmin_mean=mean(Tmin,na.rm=T), Tmax_mean=mean(Tmax,na.rm=T))

# Add spacially-varying climate variables, and calculate annual deviations from normals
PJdata <- merge(PJdata,PJdata_space) %>%
	mutate(PPT_dev=PPT-PPT_mean, Tmin_dev=Tmin-Tmin_mean, Tmax_dev=Tmax-Tmax_mean)

# Scale predictor variables
PJdata.scaled <- PJdata %>% 
	mutate_at(scale, .vars = vars(log_PC_t,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev)) %>%
	filter(Year_t==2000)

PJdata.scaled <- na.omit(PJdata.scaled)[1:10000,]

# Linear models (clim = climate only; clim_dens = climate + dens, no density-climate interactions; clim_dens_int = climate + dens, all two-way interactions)
clim <- lm(d_log_PC ~ (Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdata.scaled)
clim_dens <- lm(d_log_PC ~ PC_t + (Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdata.scaled)
clim_dens_int <- lm(d_log_PC ~ (PC_t + Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdata.scaled)

AICtab(clim,clim_dens,clim_dens_int)

res <- simulateResiduals(clim_dens_int)

resSpat <- SpatialPointsDataFrame(coords = cbind(grData_remeas$LON, grData_remeas$LAT), 
																 data = grData_remeas, 
																 proj4string = CRS("+proj=longlat +datum=NAD83"))

testSpatialAutocorrelation(simulationOutput = res, x=PJdata.scaled$location.x, y=PJdata.scaled$location.y)

# Set up ggplot theme
mytheme<-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
							 panel.background = element_blank(), axis.line = element_line(colour = "black"),
							 legend.text=element_text(size=11),legend.title=element_text(size=12),
							 legend.key = element_rect(fill = "white"),axis.text=element_text(size=12),
							 axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
							 axis.line.x = element_line(color="black", size = 0.3),
							 axis.line.y = element_line(color="black", size = 0.3))

# Plot model coefficients
clim_plot <- dwplot(clim,dot_args=list(size=2),
			 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + mytheme
clim_dens_plot <- dwplot(clim_dens,dot_args=list(size=2),
			 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + mytheme
clim_dens_int_plot <- dwplot(clim_dens_int,dot_args=list(size=2),
			 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + mytheme

allmodels_plot <- dwplot(list("Climate Only"=clim,"Climate + Density"=clim_dens,"Climate * Density"=clim_dens_int),
												 dot_args=list(size=2),
												 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + 
	mytheme + theme(legend.title = element_blank())

ggsave(file="./Output/Plots/clim_modelcoef.png", plot=clim_plot,
			 width=7,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/climdens_modelcoef.png", plot=clim_dens_plot,
			 width=7,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/climdensint_modelcoef.png", plot=clim_dens_int_plot,
			 width=7,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/all_modelcoef.png", plot=allmodels_plot,
			 width=8,height=5,units="in",dpi=600)

## Repeat with RAP data

# Load data
PJdataRAP <- read.csv("PJcoverRAP_data.csv")

# Remove data points with fire
PJdataRAP <- subset(PJdataRAP,Fire==0 & Year_t<2016 & Year_t>1999)

# Remove data points with NA, NaN, and Inf
#PJdataRAP <- NaRV.omit(PJdataRAP)

# Summarize climate variable to calculate spatially-varying climate normals
PJdataRAP_space <- PJdataRAP %>%
	group_by(location.x,location.y) %>%
	summarise(PPT_mean=mean(PPT,na.rm=T), Tmin_mean=mean(Tmin,na.rm=T), Tmax_mean=mean(Tmax,na.rm=T))

# Add spacially-varying climate variables, and calculate annual deviations from normals
PJdataRAP <- merge(PJdataRAP,PJdataRAP_space) %>%
	mutate(PPT_dev=PPT-PPT_mean, Tmin_dev=Tmin-Tmin_mean, Tmax_dev=Tmax-Tmax_mean)

# Scale predictor variables
PJdataRAP.scaled <- PJdataRAP %>% mutate_at(scale, .vars = vars(log_PC_t_pos,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))

PJdataRAP.scaled <- select(PJdataRAP.scaled, d_log_PC_pos,log_PC_t_pos,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev)
# Linear models (clim = climate only; clim_dens = climate + dens, no density-climate interactions; clim_dens_int = climate + dens, all two-way interactions)
clim_RAP <- lm(d_log_PC_pos ~ (Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdataRAP.scaled)
clim_dens_RAP <- lm(d_log_PC_pos ~ PC_t + (Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdataRAP.scaled)
clim_dens_int_RAP <- lm(d_log_PC_pos ~ (PC_t + Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdataRAP.scaled)

AICtab(clim_RAP,clim_dens_RAP,clim_dens_int_RAP)

# Set up ggplot theme
mytheme<-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
							 panel.background = element_blank(), axis.line = element_line(colour = "black"),
							 legend.text=element_text(size=11),legend.title=element_text(size=12),
							 legend.key = element_rect(fill = "white"),axis.text=element_text(size=12),
							 axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
							 axis.line.x = element_line(color="black", size = 0.3),
							 axis.line.y = element_line(color="black", size = 0.3))

# Plot model coefficients
clim_plot_RAP <- dwplot(clim_RAP,dot_args=list(size=2),
										vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + mytheme
clim_dens_plot_RAP <- dwplot(clim_dens_RAP,dot_args=list(size=2),
												 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + mytheme
clim_dens_int_plot_RAP <- dwplot(clim_dens_int_RAP,dot_args=list(size=2),
														 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + mytheme

allmodels_plot_RAP <- dwplot(list("Climate Only"=clim_RAP, "Climate + Density"=clim_dens_RAP,"Climate * Density"=clim_dens_int_RAP),
												 dot_args=list(size=2),
												 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + 
	mytheme + theme(legend.title = element_blank())

ggsave(file="./Output/Plots/climRAP_modelcoef.png", plot=clim_plot_RAP,
			 width=7,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/climdensRAP_modelcoef.png", plot=clim_dens_plot_RAP,
			 width=7,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/climdensintRAP_modelcoef.png", plot=clim_dens_int_plot_RAP,
			 width=7,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/allRAP_modelcoef.png", plot=allmodels_plot_RAP,
			 width=8,height=5,units="in",dpi=600)
