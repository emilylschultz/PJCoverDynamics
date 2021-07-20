#### PJ Cover Dynamics: Code to create exploratory graphs to visualize percent cover data in PJcover_data.csv
## Created by: Emily Schultz
## Created on: 20 Jul 2021

library(tidyverse)

# Set up ggplot theme
mytheme<-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
							 panel.background = element_blank(), axis.line = element_line(colour = "black"),
							 legend.text=element_text(size=11),legend.title=element_text(size=12),
							 legend.key = element_rect(fill = "white"),axis.text=element_text(size=12),
							 axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
							 axis.line.x = element_line(color="black", size = 0.3),
							 axis.line.y = element_line(color="black", size = 0.3))

# Load data
PJdata <- read.csv("PJcover_data.csv")

# Subset data to make plotting more manageable
ind <- sample(1:nrow(PJdata),5000) # random sample of indices to subset data
PJdata_subset <- PJdata[ind,]

# Aggregate percent cover and climate data over space and time
PJdata_time <- PJdata %>%
	group_by(Year_t) %>%
	summarise(d_PC=mean(d_PC,na.rm=T), d_log_PC=mean(d_log_PC,na.rm=T), PPT=mean(PPT,na.rm=T), Tmin=mean(Tmin,na.rm=T), Tmax=mean(Tmax,na.rm=T))

PJdata_space <- PJdata %>%
	group_by(location.x,location.y) %>%
	summarise(d_PC=mean(d_PC,na.rm=T), d_log_PC=mean(d_log_PC,na.rm=T), PPT=mean(PPT,na.rm=T), Tmin=mean(Tmin,na.rm=T), Tmax=mean(Tmax,na.rm=T))


# Histograms
png(file="./Output/Plots/PC_hist.png",4,4,units="in",type="cairo",res=600)
hist(PJdata$d_PC,xlab="Change in percent cover",main="")
dev.off()

hist(subset(PJdata,d_PC<(0))$d_PC,xlab="Change in percent cover",main="")

png(file="./Output/Plots/PC_hist_minus5.png",4,4,units="in",type="cairo",res=600)
hist(subset(PJdata,d_PC<(-5))$d_PC,xlab="Change in percent cover",main="")
dev.off()

hist(subset(PJdata,d_PC<(-10))$d_PC,xlab="Change in percent cover",main="")
hist(PJdata$d_PC_mask,xlab="Change in percent cover",main="")

png(file="./Output/Plots/heat_hist.png",4,4,units="in",type="cairo",res=600)
hist(PJdata$Heatload,xlab="Heatload",main="")
dev.off()

png(file="./Output/Plots/PPT_hist.png",4,4,units="in",type="cairo",res=600)
hist(PJdata$PPT,xlab="Total water year precipitation (mm)",main="")
dev.off()

png(file="./Output/Plots/Tmin_hist.png",4,4,units="in",type="cairo",res=600)
hist(PJdata$Tmin,xlab="Average water year minumum temperature (C)",main="")
dev.off()

png(file="./Output/Plots/Tmax_hist.png",4,4,units="in",type="cairo",res=600)
hist(PJdata$Tmax,xlab="Average water year maximum temperature (C)",main="")
dev.off()

# Predictor covariation
# Unsurprisingly, tmin and tmax are strongly correlated, but other predictors are not

heat_ppt <- ggplot(data=PJdata_subset,aes(x=Heatload,y=PPT))+
	geom_point() + 
	mytheme
heat_tmin <- ggplot(data=PJdata_subset,aes(x=Heatload,y=Tmin))+
	geom_point() + 
	mytheme
heat_tmax <- ggplot(data=PJdata_subset,aes(x=Heatload,y=Tmax))+
	geom_point() + 
	mytheme

tmin_ppt <- ggplot(data=PJdata_subset,aes(x=Tmin,y=PPT))+
	geom_point() + 
	mytheme
tmax_ppt <- ggplot(data=PJdata_subset,aes(x=Tmax,y=PPT))+
	geom_point() + 
	mytheme
tmin_tmax <- ggplot(data=PJdata_subset,aes(x=Tmin,y=Tmax))+  
	geom_point() + 
	mytheme

# Percent cover vs predictors
heat_pc <- ggplot(data=PJdata_subset,aes(x=Heatload,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Heatload",y="Change in percent cover")+
	mytheme

heat_pc_nofire <- ggplot(data=subset(PJdata_subset,d_PC>(-10)),aes(x=Heatload,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Heatload",y="Change in percent cover")+
	mytheme

ppt_pc <- ggplot(data=PJdata_subset,aes(x=PPT,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

ppt_pc_nofire <- ggplot(data=subset(PJdata_subset,d_PC>(-10)),aes(x=PPT,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

tmin_pc <- ggplot(data=PJdata_subset,aes(x=Tmin,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmin_pc_nofire <- ggplot(data=subset(PJdata_subset,d_PC>(-10)),aes(x=Tmin,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_pc <- ggplot(data=PJdata_subset,aes(x=Tmax,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_pc_nofire <- ggplot(data=subset(PJdata_subset,d_PC>(-10)),aes(x=Tmax,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

pc0_pc <- ggplot(data=PJdata_subset,aes(x=PC_t,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Percent cover time t",y="Change in percent cover")+
	mytheme

pc0_pc_nofire <- ggplot(data=subset(PJdata_subset,d_PC>(-10)),aes(x=PC_t,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Percent cover time t",y="Change in percent cover")+
	mytheme

# Aggregate plots
ppt_pc_time <- ggplot(data=PJdata_time,aes(x=PPT,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

tmin_pc_time <- ggplot(data=PJdata_time,aes(x=Tmin,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_pc_time <- ggplot(data=PJdata_time,aes(x=Tmax,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

ppt_pc_space <- ggplot(data=PJdata_space,aes(x=PPT,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

tmin_pc_space <- ggplot(data=PJdata_space,aes(x=Tmin,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_pc_space <- ggplot(data=PJdata_space,aes(x=Tmax,y=d_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

# Save plots
ggsave(file="./Output/Plots/heat_ppt.png", plot=heat_ppt,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/heat_tmin.png", plot=heat_tmin,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/heat_tmax.png", plot=heat_tmax,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_ppt.png", plot=tmin_ppt,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_ppt.png", plot=tmax_ppt,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_tmax.png", plot=tmin_tmax,
			 width=6,height=5,units="in",dpi=600)

ggsave(file="./Output/Plots/heat_pc.png", plot=heat_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/heat_pc_sub.png", plot=heat_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/ppt_pc.png", plot=ppt_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/ppt_pc_sub.png", plot=ppt_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_pc.png", plot=tmin_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_pc_sub.png", plot=tmin_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_pc.png", plot=tmax_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_pc_sub.png", plot=tmax_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/pc0_pc.png", plot=pc0_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/pc0_pc_sub.png", plot=pc0_pc_nofire,
			 width=6,height=5,units="in",dpi=600)

ggsave(file="./Output/Plots/ppt_pc_time.png", plot=ppt_pc_time,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/ppt_pc_space.png", plot=ppt_pc_space,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_pc_time.png", plot=tmin_pc_time,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_pc_space.png", plot=tmin_pc_space,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_pc_time.png", plot=tmax_pc_time,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_pc_space.png", plot=tmax_pc_space,
			 width=6,height=5,units="in",dpi=600)


## Repeat graphs with log percent cover
# Histograms
png(file="./Output/Plots/PC_hist.png",4,4,units="in",type="cairo",res=600)
hist(PJdata$d_log_PC,xlab="Change in percent cover",main="")
dev.off()

png(file="./Output/Plots/PC_hist_minus5.png",4,4,units="in",type="cairo",res=600)
hist(subset(PJdata,d_log_PC<(-0.5))$d_log_PC,xlab="Change in percent cover",main="")
dev.off()

# Percent cover vs predictors
heat_log_pc <- ggplot(data=PJdata_subset,aes(x=Heatload,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Heatload",y="Change in percent cover")+
	mytheme

heat_log_pc_nofire <- ggplot(data=subset(PJdata_subset,d_log_PC>(-0.5)),aes(x=Heatload,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Heatload",y="Change in percent cover")+
	mytheme

ppt_log_pc <- ggplot(data=PJdata_subset,aes(x=PPT,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

ppt_log_pc_nofire <- ggplot(data=subset(PJdata_subset,d_log_PC>(-0.5)),aes(x=PPT,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

tmin_log_pc <- ggplot(data=PJdata_subset,aes(x=Tmin,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmin_log_pc_nofire <- ggplot(data=subset(PJdata_subset,d_log_PC>(-0.5)),aes(x=Tmin,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_log_pc <- ggplot(data=PJdata_subset,aes(x=Tmax,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_log_pc_nofire <- ggplot(data=subset(PJdata_subset,d_log_PC>(-0.5)),aes(x=Tmax,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

pc0_log_pc <- ggplot(data=PJdata_subset,aes(x=log_PC_t,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Percent cover time t",y="Change in percent cover")+
	mytheme

pc0_log_pc_nofire <- ggplot(data=subset(PJdata_subset,d_log_PC>(-0.5)),aes(x=log_PC_t,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Percent cover time t",y="Change in percent cover")+
	mytheme

# Aggregate plots
ppt_log_pc_time <- ggplot(data=PJdata_time,aes(x=PPT,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

tmin_log_pc_time <- ggplot(data=PJdata_time,aes(x=Tmin,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_log_pc_time <- ggplot(data=PJdata_time,aes(x=Tmax,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

ppt_log_pc_space <- ggplot(data=PJdata_space,aes(x=PPT,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Total water year precipitation (mm)",y="Change in percent cover")+
	mytheme

tmin_log_pc_space <- ggplot(data=PJdata_space,aes(x=Tmin,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year minimum temperature (C)",y="Change in percent cover")+
	mytheme

tmax_log_pc_space <- ggplot(data=PJdata_space,aes(x=Tmax,y=d_log_PC))+
	geom_hline(yintercept=0) +
	geom_point() + 
	labs(x="Average water year maximum temperature (C)",y="Change in percent cover")+
	mytheme

# Save plots

ggsave(file="./Output/Plots/heat_log_pc.png", plot=heat_log_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/heat_log_pc_sub.png", plot=heat_log_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/ppt_log_pc.png", plot=ppt_log_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/ppt_log_pc_sub.png", plot=ppt_log_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_log_pc.png", plot=tmin_log_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_log_pc_sub.png", plot=tmin_log_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_log_pc.png", plot=tmax_log_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_log_pc_sub.png", plot=tmax_log_pc_nofire,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/pc0_log_pc.png", plot=pc0_log_pc,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/pc0_log_pc_sub.png", plot=pc0_log_pc_nofire,
			 width=6,height=5,units="in",dpi=600)

ggsave(file="./Output/Plots/ppt_log_pc_time.png", plot=ppt_log_pc_time,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/ppt_log_pc_space.png", plot=ppt_log_pc_space,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_log_pc_time.png", plot=tmin_log_pc_time,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmin_log_pc_space.png", plot=tmin_log_pc_space,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_log_pc_time.png", plot=tmax_log_pc_time,
			 width=6,height=5,units="in",dpi=600)
ggsave(file="./Output/Plots/tmax_log_pc_space.png", plot=tmax_log_pc_space,
			 width=6,height=5,units="in",dpi=600)