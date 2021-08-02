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
PJdata.scaled <- PJdata %>% mutate_at(scale, .vars = vars(log_PC_t,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))

PJdataRAP.scaled <- PJdataRAP %>% mutate_at(scale, .vars = vars(log_PC_t,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev))


# Prep data for stan model
x <- as.matrix(select(PJdata,PPT:Tmax_dev))
pc_t <- as.vector(PJdata$PC_t)
y <- as.vector(PJdata$PC_t1)

K <- ncol(x)
n <- nrow(x)

# Stan model

sink("pjcover.stan")
cat("
    data {
    
    int<lower=0> K;           // N. predictors 
    int<lower=0> n;           // N. observations
    matrix[n,K] x;            // Predictor matrix
    vector[n] pc_t;           // percent cover at time t (not RAP)
    vector[n] pc_t_r;         // percent cover at time t (RAP)
    vector[n] y;              // percent cover at time t+1 

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    vector[K] u_beta;                      // other coeff mean
  
    real<lower=0> sigma_y;                 // residual variation

		real<lower=0> sigma_pc;                // percent cover error
		real<lower=0> sigma_pc_r;              // percent cover RAP error

		real pc_true;                          // true, unobserved percent cover
		
    }
    
 
    
    model {
    vector[nG] mG;
    
    u_beta0 ~ normal(0, 100);

    u_beta ~ normal(0, 100); 

    sigma_y ~ gamma(2,0.01);
    

    // Percent Cover Model

    for(n in 1:G){
    mG[n] = beta0_t[tree[n]]+xG[n]*u_beta;
    }
    
    yG ~ normal(mG,sigma_y);
    
    }
    
   
    ",fill=T)

sink()


pj_data <- list(K=K, n = n, x=x, pc_t = pc_t, pc_t_r = pc_t_r, y=y)

fit_pj <- stan(file = 'pjcover.stan', data = pied_dat, 
								 iter = 1000, warmup = 100, chains = 3)