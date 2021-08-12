#### PJ Cover Dynamics: Code to create Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 2 Aug 2021

library(tidyverse)
library(rstan)

# Load dataframes
PJdata <- read.csv("PJcover_data.csv")
PJdataRAP.csv("PJcoverRAP_data.csv")

load("./Output/PJcover_mat.rda")

Location_data <- data.frame(location.x = location.x, location.y = location.y) %>%
	mutate(Location=str_c(as.character(location.x),as.character(location.y))) %>%
	mutate(Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))

# Remove data points with fire
PJdata <- subset(PJdata,Fire==0) 
PJdataRAP <- subset(PJdataRAP,Fire==0) 

PJdata <- merge(PJdata,Location_data)

# Summarize climate variable to calculate spatially-varying climate normals
PJdata_space <- PJdata %>% 
	group_by(location.x,location.y) %>%
	summarise(PPT_mean=mean(PPT,na.rm=T), Tmin_mean=mean(Tmin,na.rm=T), Tmax_mean=mean(Tmax,na.rm=T))

PJdataRAP_space <- PJdataRAP %>%
	group_by(location.x,location.y) %>%
	summarise(PPT_mean=mean(PPT,na.rm=T), Tmin_mean=mean(Tmin,na.rm=T), Tmax_mean=mean(Tmax,na.rm=T))

# Add spacially-varying climate variables, and calculate annual deviations from normals
PJdata <- merge(PJdata,PJdata_space) %>%  
	mutate(PPT_dev=PPT-PPT_mean, Tmin_dev=Tmin-Tmin_mean, Tmax_dev=Tmax-Tmax_mean)

PJdataRAP <- merge(PJdataRAP,PJdataRAP_space) %>%
	mutate(PPT_dev=PPT-PPT_mean, Tmin_dev=Tmin-Tmin_mean, Tmax_dev=Tmax-Tmax_mean)

# Scale predictor variables
PJdata.scaled <- PJdata %>% mutate_at(scale, .vars = vars(Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))

PJdataRAP.scaled <- PJdataRAP %>% mutate_at(scale, .vars = vars(Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))

PJdata.scaled <- na.omit(PJdata.scaled)

#PJdata.scaled <- subset(PJdata.scaled,Year_t == 2000)

# Prep data for stan model
x <- as.matrix(select(PJdata.scaled,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))
n <- nrow(x)
K <- ncol(x)

pc <- as.vector(c(PJdata.scaled$PC_t,PJdata.scaled$PC_t1[which(PJdata.scaled$Year_t==2015)]))
year <- as.vector(c(PJdata.scaled$Year_t,PJdata.scaled$Year_t1[which(PJdata.scaled$Year_t==2015)]))
year_ID=as.numeric(factor(year, levels = unique(sort(year))))
pixel <- as.vector(c(PJdata.scaled$Pixel_ID,PJdata.scaled$Pixel_ID[which(PJdata.scaled$Year_t==2015)]))

n_pc <- length(pc)
n_tot <- nrow(pc_mat)
n_year <- ncol(pc_mat)

rmse <- 9.6
rmse_rap <- 8.77

# Stan model

sink("pjcover.stan")
cat("
    data {
    
    //int<lower=0> K;                // N. predictors 
    //int<lower=0> n;                // N. observations
    int<lower=0> n_pc;                // N. observations
    int<lower=0> n_tot;            // Total number of pixels in percent cover raster
    int<lower=0> n_year;           // Number of years of data
    //matrix[n,K] x;                 // Predictor matrix
    vector<lower=0>[n_pc] pc;         // percent cover (not RAP)
    real[n_pc] year;       // vector of year indices for percent cover data
    real[n_pc] pixel;      // vector of pixel indices for percent cover data
    //vector[n] pc_t_r;              // percent cover (RAP)
    real<lower=0> sigma_pc;        // rmse of pc cover estimates

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    //vector[K] u_beta;                      // other coeff mean
    real u_beta_pc;                          // intercept means

    real<lower=0> sigma_y;                 // residual variation
    
    matrix[n_tot,n_year] log_pc;

    //vector<lower=0>[n] pc_true;                     // true, unobserved percent cover at time t
		//vector<lower=0>[n] pc1_true;                    // true, unobserved percent cover at time t+1

		//real<lower=0> sigma_pc;                // percent cover error
		//real<lower=0> sigma_pc_r;              // percent cover RAP error
		
    }
    
    model {

    u_beta0 ~ normal(0, 100);
    //u_beta ~ normal(0, 100); 
    u_beta_pc ~ normal(0, 100); 
    
    sigma_y ~ cauchy(0,5);

		for(i in 1:n_tot){
			for(y in 1:n_year){
				pc[which(pixel==i & year==y)] ~ normal(exp(log_pc[1,y]),sigma_pc);
			}
		}

    // Percent Cover Model
    
    for(t in 1:16){
        log_pc[,t+1] - log_pc[,t] ~ normal(u_beta0 + u_beta_pc*exp(log_pc[,t]),sigma_y);
    }
    
    //+ x*u_beta 
    }
    
   
    ",fill=T)

sink()


pj_data <- list(n = n, n_tot = n_tot, n_year = n_year, pc = pc, year = year_ID, pixel = pixel, sigma_pc = rmse)

rm(PJdata,PJdata_space)

fit_pj <- stan(file = 'pjcover.stan', data = pj_data, pars = (c("log_pc_t","log_pc_t1")),include=FALSE, 
								 iter = 2000, warmup = 200, chains = 3)
