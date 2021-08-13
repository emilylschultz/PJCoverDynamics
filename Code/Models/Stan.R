#### PJ Cover Dynamics: Code to create Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 2 Aug 2021

library(tidyverse)
library(rstan)

# Load dataframes
PJdata <- read.csv("PJcover_data.csv")
PJdataRAP.csv("PJcoverRAP_data.csv")

load("./Output/PJcover_mat.rda")

## Keep only percent cover matrix pixels that are in PJdata in at least one year
# Create dataframe of location data
Location_data <- data.frame(location.x = location.x, location.y = location.y) %>%
	mutate(Location=str_c(as.character(location.x),as.character(location.y))) 

# Add location variable (combing lat and long) to PJdata
PJdata <- mutate(PJdata,Location=str_c(as.character(location.x),as.character(location.y)))

# Find locations in Location dataframe that match locations in PJdata
matches <- match(Location_data$Location, PJdata$Location)

# Subset Location dataframe and pc matrix to keep only locations found in PJdata
Location_data_subset <- na.omit(Location_data[matches,])
pc_mat_subset <- na.omit(pc_mat[matches,])

# Remove data points with fire
PJdata <- subset(PJdata,Fire==0) 
PJdataRAP <- subset(PJdataRAP,Fire==0) 

# Add variable to Location data subset that gives sequential ID to each pixel (i.e., location)
Location_data_subset <-mutate(Location_data_subset,Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))

# Merge PJdata with Location data subset to add Pixel ID to PJ data
PJdata <- merge(PJdata,Location_data_subset)

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

# Remove observations with NA
PJdata.scaled <- na.omit(PJdata.scaled)

# Subset data (only use to decrease time/memory when checking model code)
PJdata.scaled <- subset(PJdata.scaled,Year_t < 2002)

# Prep data for stan model
x <- as.matrix(select(PJdata.scaled,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev)) # matrix of non-density predictors
n <- nrow(x) # number of predictor observations
K <- ncol(x) # number of predictors

pc <- as.vector(c(PJdata.scaled$PC_t,PJdata.scaled$PC_t1[which(PJdata.scaled$Year_t==2001)])) # vector of percent cover for ALL years
year <- as.vector(c(PJdata.scaled$Year_t,PJdata.scaled$Year_t1[which(PJdata.scaled$Year_t==2001)])) # vector of year for each percent cover observation
year_ID=as.numeric(factor(year, levels = unique(sort(year)))) # vector of sequential year ID for each percent cover observation (used for indexing purposes)
pixel <- as.vector(c(PJdata.scaled$Pixel_ID,PJdata.scaled$Pixel_ID[which(PJdata.scaled$Year_t==2001)])) # vector of sequential pixel ID for each percent cover observation (used for indexing purposes)

n_pc <- length(pc) # number of percent cover observations
n_pixel <- nrow(pc_mat) # number of pixels in percent cover matrix (this will be number of rows in latent state matrix)
n_year <- length(unique(year)) # number of years (this will be number of columns in latent state matrix)

rmse <- 9.6
rmse_rap <- 8.77

# Stan model

sink("pjcover.stan")
cat("
    data {
    
    //int<lower=0> K;                // N. predictors 
    //int<lower=0> n;                // N. observations
    int<lower=0> n_pc;                // N. observations
    int<lower=0> n_pixel;            // Total number of pixels in percent cover raster
    int<lower=0> n_year;           // Number of years of data
    //matrix[n,K] x;                 // Predictor matrix
    vector<lower=0>[n_pc] pc;         // percent cover (not RAP)
    int<lower=1> year[n_pc];       // vector of year indices for percent cover data
    int<lower=1> pixel[n_pc];      // vector of pixel indices for percent cover data
    //vector[n] pc_t_r;              // percent cover (RAP)
    real<lower=0> sigma_pc;        // rmse of pc cover estimates

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    //vector[K] u_beta;                      // other coeff mean
    real u_beta_pc;                          // intercept means

    real<lower=0> sigma_y;                 // residual variation
    
    matrix[n_pixel,n_year] log_pc;

		//real<lower=0> sigma_pc;                // percent cover error
		//real<lower=0> sigma_pc_r;              // percent cover RAP error
		
    }
    
    model {

    u_beta0 ~ normal(0, 100);
    //u_beta ~ normal(0, 100); 
    u_beta_pc ~ normal(0, 100); 
    
    sigma_y ~ cauchy(0,5);

		// Observation model
		for(i in 1:n_pc){
				pc[i] ~ normal(exp(log_pc[pixel[i],year[i]]),sigma_pc);
			}

    // Process Model
    
    for(t in 1:(n_year-1)){
        log_pc[,t+1] - log_pc[,t] ~ normal(u_beta0 + u_beta_pc*exp(log_pc[,t]),sigma_y);
    }
    
    //+ x*u_beta 
    
    //// with the matrix above maybe then tying to loop over years and calulate this for each pixel as a vector, x could also also be a 3d array
    //maybe Something like
    //for Year in 1:Nyear{
    //log_pc_t[l,Year+1] - log_pc_t [,Year] ~ normal(u_beta0 + x[,Year,] *u_beta + u_beta_pc*exp(log_pc_t[,Year]),sigma_y); 
    //}
    }
    
   
    ",fill=T)

sink()


pj_data <- list(n = n, n_tot = n_pixel, n_year = n_year, n_pc = n_pc, pc = pc, year = year_ID, pixel = pixel, sigma_pc = rmse)

rm(PJdata,PJdata_space,Location_data,Location_data_subset,pc_mat,pc_mat_subset,location.x,location.y,PJdata.scaled,pc,pixel,year,year_ID,
	 matches,n_pc,n_pixel,n_year,rmse)

fit_pj <- stan(file = 'pjcover.stan', data = pj_data, pars = (c("log_pc")),include=FALSE, 
								 iter = 2000, warmup = 200, chains = 3)
