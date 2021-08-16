#### PJ Cover Dynamics: Code to create Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 2 Aug 2021

library(tidyverse)
library(rstan)

# Load dataframes
PJdata <- read.csv("PJcover_data.csv")
PJdataRAP.csv("PJcoverRAP_data.csv")

load("./Output/PJcover_mat.rda")
load("./Output/PJcover_mat_RAP.rda")

## Keep only percent cover matrix pixels that are in PJdata in at least one year
# Create dataframe of location data
Location_data <- data.frame(location.x = location.x, location.y = location.y) %>%
	mutate(Location=str_c(as.character(location.x),as.character(location.y))) 

Location_dataRAP <- data.frame(location.x = location.x.RAP, location.y = location.y.RAP) %>%
	mutate(Location=str_c(as.character(location.x.RAP),as.character(location.y.RAP))) 

# Add location variable (combing lat and long) to PJdata
PJdata <- mutate(PJdata,Location=str_c(as.character(location.x),as.character(location.y)))
PJdataRAP <- mutate(PJdata,Location=str_c(as.character(location.x),as.character(location.y)))

# Find locations in Location dataframe that match locations in PJdata
matches <- match(Location_data$Location, PJdata$Location)
matchesRAP <- match(Location_dataRAP$Location, PJdataRAP$Location)

# Subset Location dataframe and pc matrix to keep only locations found in PJdata
Location_data_subset <- na.omit(Location_data[matches,])
pc_mat_subset <- na.omit(pc_mat[matches,])

Location_dataRAP_subset <- na.omit(Location_dataRAP[matchesRAP,])
pc_matRAP_subset <- na.omit(pc_mat_RAP[matchesRAP,])

# Remove data points with fire
PJdata <- subset(PJdata,Fire==0) 
PJdataRAP <- subset(PJdataRAP,Fire==0) 

# Add variable to Location data subset that gives sequential ID to each pixel (i.e., location)
Location_data_subset <-mutate(Location_data_subset,Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))
Location_dataRAP_subset <-mutate(Location_dataRAP_subset,Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))

# Merge PJdata with Location data subset to add Pixel ID to PJ data
PJdata <- merge(PJdata,Location_data_subset)
PJdataRAP <- merge(PJdataRAP,Location_data_subset)

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
PJdataRAP.scaled <- na.omit(PJdataRAP.scaled)

# Subset data (only use to decrease time/memory when checking model code)
PJdata.scaled <- subset(PJdata.scaled,Year_t < 2001)
PJdataRAP.scaled <- subset(PJdata.scaled,Year_t > 1999 & Year_t < 2001)

# Prep data for stan model
x <- as.matrix(select(PJdata.scaled,Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev)) # change from matrix to array (pixel x year x predictor) of non-density predictors
n <- nrow(x) # number of predictor observations
K <- ncol(x) # number of predictors

pc <- as.vector(c(PJdata.scaled$PC_t,PJdata.scaled$PC_t1[which(PJdata.scaled$Year_t==2000)])) # vector of percent cover for ALL years
year <- as.vector(c(PJdata.scaled$Year_t,PJdata.scaled$Year_t1[which(PJdata.scaled$Year_t==2000)])) # vector of year for each percent cover observation
year_ID=as.numeric(factor(year, levels = unique(sort(year)))) # vector of sequential year ID for each percent cover observation (used for indexing purposes)
pixel <- as.vector(c(PJdata.scaled$Pixel_ID,PJdata.scaled$Pixel_ID[which(PJdata.scaled$Year_t==2000)])) # vector of sequential pixel ID for each percent cover observation (used for indexing purposes)

n_pc <- length(pc) # number of percent cover observations
n_pixel <- nrow(pc_mat_subset) # number of pixels in percent cover matrix (this will be number of rows in latent state matrix)
n_year <- length(unique(year)) # number of years (this will be number of columns in latent state matrix)

# Repeat for RAP data
pc_r <- as.vector(c(PJdataRAP.scaled$PC_t,PJdataRAP.scaled$PC_t1[which(PJdataRAP.scaled$Year_t==2000)])) # vector of percent cover for ALL years
year_r <- as.vector(c(PJdataRAP.scaled$Year_t,PJdataRAP.scaled$Year_t1[which(RAP$Year_t==2000)])) # vector of year for each percent cover observation
year_ID_r=as.numeric(factor(year_r, levels = unique(sort(year_r)))) # vector of sequential year ID for each percent cover observation (used for indexing purposes)
pixel_r <- as.vector(c(PJdataRAP.scaled$Pixel_ID,PJdataRAP.scaled$Pixel_ID[which(PJdataRAP.scaled$Year_t==2000)])) # vector of sequential pixel ID for each percent cover observation (used for indexing purposes)

n_pc_r <- length(pc_r) # number of percent cover observations
n_pixel_r <- nrow(pc_matRAP_subset) # number of pixels in percent cover matrix (this will be number of rows in latent state matrix)
n_year_r <- length(unique(year_r)) # number of years (this will be number of columns in latent state matrix)

rmse <- 9.6
rmse_r <- 8.77

# Stan model

sink("pjcover.stan")
cat("
    data {
    
    //int<lower=0> K;                // N. predictors 
    //int<lower=0> n;                // N. observations
    //matrix[n,K] x;                 // Predictor matrix

    int<lower=0> n_pc;               // N. observations (not RAP)
    int<lower=0> n_pixel;            // Total number of pixels in percent cover raster (not RAP)
    int<lower=0> n_year;             // Number of years of data (not RAP)
    int<lower=0> n_pc_r;             // N. observations (RAP)
    int<lower=0> n_pixel_r;          // Total number of pixels in percent cover raster (RAP)
    int<lower=0> n_year_r;           // Number of years of data (RAP)
    
    vector<lower=0>[n_pc] pc;        // percent cover (not RAP)
    int<lower=1> year[n_pc];         // vector of year indices for percent cover data (not RAP)
    int<lower=1> pixel[n_pc];        // vector of pixel indices for percent cover data (not RAP)
    
    vector<lower=o>[n_pc_r] pc_t_r;  // percent cover (RAP)
    int<lower=1> year_r[n_pc_r];     // vector of year indices for percent cover data (RAP)
    int<lower=1> pixel_r[n_pc_r];    // vector of pixel indices for percent cover data (RAP)
    
    real<lower=0> sigma_pc;          // rmse of pc cover estimates
    real<lower=0> sigma_pc_r;        // rmse of pc cover estimates

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    //vector[K] u_beta;                      // other coeff mean
    real u_beta_pc;                        // intercept means

    real<lower=0> sigma_y;                 // residual variation
    
    matrix[n_pixel,n_year] log_pc;         // matrix (rows=pixels, columns=years) of true percent cover values

		//real<lower=0> sigma_pc;                // percent cover error
		//real<lower=0> sigma_pc_r;              // percent cover RAP error
		
    }
    
    model {

    u_beta0 ~ normal(0, 100);
    //u_beta ~ normal(0, 100); 
    u_beta_pc ~ normal(0, 100); 
    log_pc ~ normal(0, 100); 
    
    sigma_y ~ cauchy(0,5);

		// Observation model
		for(i in 1:n_pc){
				pc[i] ~ normal(exp(log_pc[pixel[i],year[i]]),sigma_pc);
		}
			
		for(i in 1:n_pc_r){
				pc_r[i] ~ normal(exp(log_pc[pixel_r[i],year_r[i]]),sigma_pc_r);
		}

    // Process Model
    
        
    for(t in 1:(n_year-1)){
        log_pc[,t+1] - log_pc[,t] ~ normal(u_beta0 + u_beta_pc*exp(log_pc[,t]),sigma_y);
    }
    
    //for(t in 1:(n_year-1)){
    //    log_pc[,t+1] - log_pc[,t] ~ normal(u_beta0 + x[,Year,] *u_beta + u_beta_pc*exp(log_pc[,t]),sigma_y);
    //}
    
    
   
    ",fill=T)

sink()


pj_data <- list(n_pixel = n_pixel, n_year = n_year, n_pc = n_pc, pc = pc, year = year_ID, pixel = pixel, sigma_pc = rmse,
								n_pixel_r = n_pixel_r, n_year_r = n_year_r, n_pc_r = n_pc_r, pc_r = pc_r, year_r = year_ID_r, pixel_r = pixel_r, sigma_pc_r = rmse_r)

rm(PJdata,PJdata_space,Location_data,Location_data_subset,pc_mat,pc_mat_subset,location.x,location.y,PJdata.scaled,pc,pixel,year,year_ID,
	 matches,n_pc,n_pixel,n_year,rmse)

fit_pj <- stan(file = 'pjcover.stan', data = pj_data, pars = (c("log_pc")),include=FALSE, 
								 iter = 2000, warmup = 200, chains = 3)
