#### PJ Cover Dynamics: Code to create Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 2 Aug 2021

library(tidyverse)
library(rstan)

# Load dataframes
PJdata <- read.csv("PJcover_data.csv")
PJdataRAP.csv("PJcoverRAP_data.csv")

# Remove data points with fire
PJdata <- subset(PJdata,Fire==0) 
PJdataRAP <- subset(PJdataRAP,Fire==0) 

PJdata <- PJdata %>%
	mutate(PJdata, Location=str_c(as.character(location.x),as.character(location.y))) %>%
	arrange(Year_t,Location) %>%
	mutate(Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))

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

PJdata.scaled <- subset(PJdata.scaled,Year_t == 2000)


# Prep data for stan model
x <- as.matrix(select(PJdata.scaled,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))
pc_t <- as.vector(PJdata.scaled$PC_t)
pc_t1 <- as.vector(PJdata.scaled$PC_t1) # need to combine this with pc_t in a single object

K <- ncol(x)
n <- nrow(x)

rmse <- 9.6
rmse_rap <- 8.77

# Stan model

sink("pjcover.stan")
cat("
    data {
    
    int<lower=0> K;           // N. predictors 
    int<lower=0> n;           // N. observations
    matrix[n,K] x;            // Predictor matrix
    vector<lower=0>[n] pc_t;           // percent cover at time t (not RAP)
    //vector[n] pc_t_r;         // percent cover at time t (RAP)
    vector<lower=0>[n] pc_t1;              // percent cover at time t+1 
    //vector[n] pc_t1_r;         // percent cover at time t+1 (RAP)
    real<lower=0> sigma_pc;      // rmse of pc cover estimates

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    vector[K] u_beta;                      // other coeff mean
    real u_beta_pc;                          // intercept means

    real<lower=0> sigma_y;                 // residual variation
    
    vector[n] log_pc_t;
    vector[n] log_pc_t1;
    
    //vector<lower=0>[n] pc_true;                     // true, unobserved percent cover at time t
		//vector<lower=0>[n] pc1_true;                    // true, unobserved percent cover at time t+1

		//real<lower=0> sigma_pc;                // percent cover error
		//real<lower=0> sigma_pc_r;              // percent cover RAP error
		
    }
    
    model {

    u_beta0 ~ normal(0, 100);
    u_beta ~ normal(0, 100); 
    u_beta_pc ~ normal(0, 100); 
    
    sigma_y ~ cauchy(0,5);

    pc_t ~ normal(exp(log_pc_t),sigma_pc);
    pc_t1 ~ normal(exp(log_pc_t1),sigma_pc);
    

    // Percent Cover Model
    
    log_pc_t1 - log_pc_t ~ normal(u_beta0 + x*u_beta + u_beta_pc*exp(log_pc_t),sigma_y);
    
    }
    
   
    ",fill=T)

sink()


pj_data <- list(K=K, n = n, x=x, pc_t = pc_t, pc_t1 = pc_t1, sigma_pc = rmse)

rm(PJdata,PJdata_space)

fit_pj <- stan(file = 'pjcover.stan', data = pj_data, pars = (c("log_pc_t","log_pc_t1")),include=FALSE, 
								 iter = 2000, warmup = 200, chains = 3)
