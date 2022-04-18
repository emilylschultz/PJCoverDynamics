#### PJ Cover Dynamics: Code to set up stan data for Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 2 Aug 2021

library(tidyverse)
library(rstan)

# Load dataframes
PJdata <- read.csv("PJcover_data.csv")
PJdataRAP <- read.csv("PJcoverRAP_data.csv")

load("./Output/PJcover_mat.rda")
load("./Output/PJcover_mat_RAP.rda")
load("./OUtput/Climate_mat.rda")

## Keep only percent cover matrix pixels that are in PJdata in at least one year
# Create dataframe of location data
#sample = sample.int(4,length(location.x),replace = T)
#save(sample,file="location_sample.rda")
load("location_sample.rda")

Location_data <- data.frame(Index = seq(1,length(location.x)), location.x = location.x, location.y = location.y, Sample = sample) %>%
	mutate(Location=str_c(as.character(location.x),as.character(location.y))) 

Location_dataRAP <- data.frame(Index = seq(1,length(location.x)), location.x = location.x.RAP, location.y = location.y.RAP, Sample = sample) %>%
	mutate(Location=str_c(as.character(location.x.RAP),as.character(location.y.RAP))) 

# Add location variable (combing lat and long) to PJdata
PJdata <- mutate(PJdata,Location=str_c(as.character(location.x),as.character(location.y))) %>%
	select(Location,Year_t,Year_t1,PC_t,PC_t1,Fire)
PJdataRAP <- mutate(PJdataRAP,Location=str_c(as.character(location.x),as.character(location.y))) %>%
	select(Location,Year_t,Year_t1,PC_t,PC_t1,Fire)

# Subset data to only include 100 locations
PJdata<-PJdata[which(PJdata$Location %in% Location_data$Location[which(Location_data$Sample==4)]),]
PJdataRAP<-PJdataRAP[which(PJdataRAP$Location %in% Location_dataRAP$Location[which(Location_dataRAP$Sample==4)]),]

# Subset Location dataframe and pc matrix to keep only locations found in PJdata
Location_data_subset <- Location_data[which(Location_data$Location %in% PJdata$Location),]
pc_mat_subset <- pc_mat[Location_data_subset$Index,]

Location_dataRAP_subset <- Location_dataRAP[which(Location_dataRAP$Location %in% PJdataRAP$Location),]
pc_matRAP_subset <- pc_mat_RAP[Location_dataRAP_subset$Index,]

# Add variable to Location data subset that gives sequential ID to each pixel (i.e., location)
Location_data_subset <-mutate(Location_data_subset,Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))
Location_dataRAP_subset <-mutate(Location_dataRAP_subset,Pixel_ID=as.numeric(factor(Location, levels = unique(Location))))

# Merge PJdata with Location data subset to add Pixel ID to PJ data
PJdata <- merge(PJdata,Location_data_subset)
PJdataRAP <- merge(PJdataRAP,Location_data_subset)

year <- as.vector(c(PJdataRAP$Year_t,PJdataRAP$Year_t1[which(PJdataRAP$Year_t==max(PJdataRAP$Year_t))])) # vector of year for each percent cover observation
PJdata <- mutate(PJdata,Year_ID_t=as.numeric(factor(Year_t, levels = unique(sort(year)))),
								 Year_ID_t1=as.numeric(factor(Year_t1, levels = unique(sort(year)))))
PJdataRAP <- mutate(PJdataRAP,Year_ID_t=as.numeric(factor(Year_t, levels = unique(sort(year)))),
								 Year_ID_t1=as.numeric(factor(Year_t1, levels = unique(sort(year)))))

# Remove data points with fire
PJdata <- subset(PJdata,Fire==0) 
PJdataRAP <- subset(PJdataRAP,Fire==0) 

# Remove observations with NA
PJdata <- na.omit(PJdata)
PJdataRAP <- na.omit(PJdataRAP)

# Prep percent cover data for stan model
pc <- as.vector(c(PJdata$PC_t,PJdata$PC_t1[which(PJdata$Year_t==max(PJdata$Year_t))])) # vector of percent cover for ALL years
year_ID <- as.numeric(c(PJdata$Year_ID_t,PJdata$Year_ID_t1[which(PJdata$Year_t==max(PJdata$Year_t))])) # vector of sequential year ID for each percent cover observation (used for indexing purposes)
pixel <- as.vector(c(PJdata$Pixel_ID,PJdata$Pixel_ID[which(PJdata$Year_t==max(PJdata$Year_t))])) # vector of sequential pixel ID for each percent cover observation (used for indexing purposes)

n_pc <- length(pc) # number of percent cover observations
n_year <- length(unique(year_ID)) # number of years (this will be number of columns in latent state matrix)

# Repeat for RAP data
pc_r <- as.vector(c(PJdataRAP$PC_t,PJdataRAP$PC_t1[which(PJdataRAP$Year_t==max(PJdataRAP$Year_t))])) # vector of percent cover for ALL years
pc_r[which(pc_r<min(pc))]<-min(pc)
year_ID_r <- as.numeric(c(PJdataRAP$Year_ID_t,PJdataRAP$Year_ID_t1[which(PJdataRAP$Year_t==max(PJdataRAP$Year_t))])) # vector of sequential year ID for each percent cover observation (used for indexing purposes)
pixel_r <- as.vector(c(PJdataRAP$Pixel_ID,PJdataRAP$Pixel_ID[which(PJdataRAP$Year_t==max(PJdataRAP$Year_t))])) # vector of sequential pixel ID for each percent cover observation (used for indexing purposes)

n_pc_r <- length(pc_r) # number of percent cover observations
n_year_r <- length(unique(year_ID_r)) # number of years (this will be number of columns in latent state matrix)
n_pixel <- nrow(pc_matRAP_subset) # number of pixels in percent cover matrix (this will be number of rows in latent state matrix)

rmse <- 9.6
rmse_r <- 8.77

## Prep predictor variables

# Calculate spatial "normals"
ppt_mat_subset <- total_ppt[Location_data_subset$Index,]
tmin_mat_subset <- ave_tmin[Location_data_subset$Index,]
tmax_mat_subset <- ave_tmax[Location_data_subset$Index,]
heatload_subset <- heatload_vec[Location_data_subset$Index]

PPT_mean_vec <- rowMeans(ppt_mat_subset)
Tmin_mean_vec <- rowMeans(tmin_mat_subset)
Tmax_mean_vec <- rowMeans(tmax_mat_subset)

PPT_mean <- matrix(rep(PPT_mean_vec,(n_year_r-1)),c(n_pixel,(n_year_r-1)))
Tmin_mean <- matrix(rep(Tmin_mean_vec,(n_year_r-1)),c(n_pixel,(n_year_r-1)))
Tmax_mean <- matrix(rep(Tmax_mean_vec,(n_year_r-1)),c(n_pixel,(n_year_r-1)))
heatload <- matrix(rep(heatload_subset,(n_year_r-1)),c(n_pixel,(n_year_r-1)))

# Calculate deviations from normals
PPT_dev <- ppt_mat_subset-PPT_mean
Tmin_dev <- tmin_mat_subset-Tmin_mean
Tmax_dev <- tmax_mat_subset-Tmax_mean

PPT_mean <- scale(PPT_mean)
Tmin_mean <- scale(Tmin_mean)
Tmax_mean <- scale(Tmax_mean)

PPT_dev <- scale(PPT_dev)
Tmin_dev <- scale(Tmin_dev)
Tmax_dev <- scale(Tmax_dev)

K <- 7 # number of predictors
x <- array(c(heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev),c(n_pixel,(n_year_r-1),K)) # change from matrix to array (pixel x year x predictor) of non-density predictors
I <- 12
x_int <- array(c(heatload*PPT_mean,heatload*Tmin_mean,heatload*Tmax_mean,heatload*PPT_dev,heatload*Tmin_dev,heatload*Tmax_dev,
								 PPT_mean*Tmin_mean,PPT_mean*Tmax_mean,Tmin_mean*Tmax_mean,
								 PPT_mean*PPT_dev,Tmin_mean*Tmin_dev,Tmax_mean*Tmax_dev),c(n_pixel,(n_year_r-1),I)) # change from matrix to array (pixel x year x predictor) of non-density predictors
x_trans <- aperm(x,c(2,1,3)) # transpose array so dims are [pixel,predictor,year], so stan can read [pixel,predictor] as matrix1
x_int_trans <- aperm(x_int,c(2,1,3))

## Stan models
## Three models: climate only, climate + density, climate * density

sink("pjcover_clim.stan")
cat("
    data {
    
    int<lower=0> n_pc;               // N. observations (not RAP)
    int<lower=0> n_pixel;            // Total number of pixels in percent cover raster
    int<lower=0> n_year;             // Number of years of data (not RAP)
    int<lower=0> n_pc_r;             // N. observations (RAP)
    int<lower=0> n_year_r;           // Number of years of data (RAP)
    
    int<lower=0> K;                  // N. predictors 
    int<lower=0> I;                       // N. Interactions 
    matrix[n_pixel,K] x[(n_year_r-1)];      // Predictor matrix
    matrix[n_pixel,I] x_int[(n_year_r-1)];  // Predictor interactions matrix

    vector<lower=0>[n_pc] pc;        // percent cover (not RAP)
    int<lower=1> year[n_pc];         // vector of year indices for percent cover data (not RAP)
    int<lower=1> pixel[n_pc];        // vector of pixel indices for percent cover data (not RAP)
    
    vector<lower=0>[n_pc_r] pc_r;    // percent cover (RAP)
    int<lower=1> year_r[n_pc_r];     // vector of year indices for percent cover data (RAP)
    int<lower=1> pixel_r[n_pc_r];    // vector of pixel indices for percent cover data (RAP)
    
    real<lower=0> sigma_pc;          // rmse of pc cover estimates
    real<lower=0> sigma_pc_r;        // rmse of pc cover estimates

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    vector[K] u_beta;                      // other coeff mean
    vector[I] u_beta_int;                  // interaction coeff mean

    real<lower=0> sigma_y;                 // residual variation
    
    matrix[n_pixel,n_year_r] log_pc;         // matrix (rows=pixels, columns=years) of true percent cover values

		//real<lower=0> sigma_pc;                // percent cover error
		//real<lower=0> sigma_pc_r;              // percent cover RAP error
		
    }
    
    model {

    u_beta0 ~ normal(0, 100);
    u_beta ~ normal(0, 100); 
    u_beta_int ~ normal(0, 100); 
    

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
        log_pc[,t+1] ~ normal(log_pc[,t] + u_beta0 + x[t]*u_beta + x_int[t]*u_beta_int,sigma_y);
    }
    
    }
   
    ",fill=T)

sink()

sink("pjcover_climdens.stan")
cat("
    data {
    
    int<lower=0> n_pc;                    // N. observations (not RAP)
    int<lower=0> n_pixel;                 // Total number of pixels in percent cover raster
    int<lower=0> n_year;                  // Number of years of data (not RAP)
    int<lower=0> n_pc_r;                  // N. observations (RAP)
    int<lower=0> n_year_r;                // Number of years of data (RAP)
    
    int<lower=0> K;                       // N. predictors 
    int<lower=0> I;                       // N. Interactions 
    matrix[n_pixel,K] x[(n_year_r-1)];      // Predictor matrix
    matrix[n_pixel,I] x_int[(n_year_r-1)];  // Predictor interactions matrix

    vector<lower=0>[n_pc] pc;             // percent cover (not RAP)
    int<lower=1> year[n_pc];              // vector of year indices for percent cover data (not RAP)
    int<lower=1> pixel[n_pc];             // vector of pixel indices for percent cover data (not RAP)
    
    vector<lower=0>[n_pc_r] pc_r;         // percent cover (RAP)
    int<lower=1> year_r[n_pc_r];          // vector of year indices for percent cover data (RAP)
    int<lower=1> pixel_r[n_pc_r];         // vector of pixel indices for percent cover data (RAP)
    
    real<lower=0> sigma_pc;               // rmse of pc cover estimates
    real<lower=0> sigma_pc_r;             // rmse of pc cover estimates

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    vector[K] u_beta;                      // other coeff mean
    vector[I] u_beta_int;                  // interaction coeff mean
    real u_beta_pc;                        // intercept means

    real<lower=0> sigma_y;                 // residual variation
    
    matrix[n_pixel,n_year_r] log_pc;         // matrix (rows=pixels, columns=years) of true percent cover values

		//real<lower=0> sigma_pc;                // percent cover error
		//real<lower=0> sigma_pc_r;              // percent cover RAP error
		
    }
    
    model {

    u_beta0 ~ normal(0, 100);
    u_beta ~ normal(0, 100); 
    u_beta_int ~ normal(0, 100); 
    u_beta_pc ~ normal(0, 100); 
    
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
        log_pc[,t+1] ~ normal(log_pc[,t] + u_beta0 + x[t]*u_beta + x_int[t]*u_beta_int + u_beta_pc*exp(log_pc[,t]),sigma_y);
    }
    
    }
   
    ",fill=T)

sink()

sink("pjcover_climdensint.stan")
cat("
    data {
    
    int<lower=0> n_pc;                    // N. observations (not RAP)
    int<lower=0> n_pixel;                 // Total number of pixels in percent cover raster
    int<lower=0> n_year;                  // Number of years of data (not RAP)
    int<lower=0> n_pc_r;                  // N. observations (RAP)
    int<lower=0> n_year_r;                // Number of years of data (RAP)
    
    int<lower=0> K;                       // N. predictors 
    int<lower=0> I;                       // N. Interactions 
    matrix[n_pixel,K] x[(n_year_r-1)];      // Predictor matrix
    matrix[n_pixel,I] x_int[(n_year_r-1)];  // Predictor interactions matrix

    vector<lower=0>[n_pc] pc;             // percent cover (not RAP)
    int<lower=1> year[n_pc];              // vector of year indices for percent cover data (not RAP)
    int<lower=1> pixel[n_pc];             // vector of pixel indices for percent cover data (not RAP)
    
    vector<lower=0>[n_pc_r] pc_r;         // percent cover (RAP)
    int<lower=1> year_r[n_pc_r];          // vector of year indices for percent cover data (RAP)
    int<lower=1> pixel_r[n_pc_r];         // vector of pixel indices for percent cover data (RAP)
    
    real<lower=0> sigma_pc;               // rmse of pc cover estimates
    real<lower=0> sigma_pc_r;             // rmse of pc cover estimates

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    vector[K] u_beta;                      // other coeff mean
    vector[I] u_beta_int;                  // interaction coeff mean
    vector[K] u_beta_densint;              // interaction (desn*env) coeff mean
    real u_beta_pc;                        // intercept means

    real<lower=0> sigma_y;                 // residual variation
    
    matrix[n_pixel,n_year_r] log_pc;         // matrix (rows=pixels, columns=years) of true percent cover values

		//real<lower=0> sigma_pc;                // percent cover error
		//real<lower=0> sigma_pc_r;              // percent cover RAP error
		
    }
    
    model {

    u_beta0 ~ normal(0, 100);
    u_beta ~ normal(0, 100); 
    u_beta_int ~ normal(0, 100); 
    u_beta_pc ~ normal(0, 100); 
    
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
        log_pc[,t+1] ~ normal(log_pc[,t] + u_beta0 + x[t]*u_beta + x_int[t]*u_beta_int + diag_pre_multiply(exp(log_pc[,t]),x[t])*u_beta_densint + u_beta_pc*exp(log_pc[,t]),sigma_y);
    }
    
    }
   
    ",fill=T)

sink()

pj_data <- list(K = K, I = I ,x = x_trans, x_int = x_int_trans, n_pixel = n_pixel, n_year = n_year, n_pc = n_pc, pc = pc, year = year_ID, pixel = pixel, sigma_pc = rmse,
							n_year_r = n_year_r, n_pc_r = n_pc_r, pc_r = pc_r, year_r = year_ID_r, pixel_r = pixel_r, sigma_pc_r = rmse_r)

save(pj_data,file="stan_data_4.rda")
