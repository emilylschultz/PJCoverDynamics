#### PJ Cover Dynamics: Code to create linear models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 21 Jul 2021

library(tidyverse)
library(lme4)
library(bbmle)
library(dotwhisker)

# Load data
PJdata <- read.csv("PJcover_data.csv")

PJdata <- subset(PJdata,Fire==0)

PJdata_space <- PJdata %>%
	group_by(location.x,location.y) %>%
	summarise(PPT_mean=mean(PPT,na.rm=T), Tmin_mean=mean(Tmin,na.rm=T), Tmax_mean=mean(Tmax,na.rm=T))

PJdata <- merge(PJdata,PJdata_space) %>%
	mutate(PPT_dev=PPT-PPT_mean, Tmin_dev=Tmin-Tmin_mean, Tmax_dev=Tmax-Tmax_mean)

PJdata.scaled <- PJdata %>% mutate_at(scale, .vars = vars(log_PC_t,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))

# Linear models (clim = climate only; clim_dens = climate + dens, no density-climate interactions; clim_dens_int = climate + dens, all two-way interactions)
clim <- lm(d_log_PC ~ (Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdata.scaled)
clim_dens <- lm(d_log_PC ~ log_PC_t + (Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdata.scaled)
clim_dens_int <- lm(d_log_PC ~ (log_PC_t + Heatload + PPT_mean + Tmin_mean + Tmax_mean + PPT_dev + Tmin_dev + Tmax_dev)^2, PJdata.scaled)

AICtab(clim,clim_dens,clim_dens_int)

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
