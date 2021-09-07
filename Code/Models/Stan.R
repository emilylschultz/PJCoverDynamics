#### PJ Cover Dynamics: Code to create Bayesian models of effects of density and climate on PJ percent cover
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
Location_data <- data.frame(location.x = location.x, location.y = location.y) %>%
	mutate(Location=str_c(as.character(location.x),as.character(location.y))) 

Location_dataRAP <- data.frame(location.x = location.x.RAP, location.y = location.y.RAP) %>%
	mutate(Location=str_c(as.character(location.x.RAP),as.character(location.y.RAP))) 

# Add location variable (combing lat and long) to PJdata
PJdata <- mutate(PJdata,Location=str_c(as.character(location.x),as.character(location.y))) 
PJdataRAP <- mutate(PJdataRAP,Location=str_c(as.character(location.x),as.character(location.y)))

# Subset data to only include 100 locations
sub <- sample(unique(PJdata$Location),1000)
match <- match(PJdata$Location,sub)
PJdata<-PJdata[which(!is.na(match)),]
matchRAP <- match(PJdataRAP$Location,sub)
PJdataRAP<-PJdataRAP[which(!is.na(matchRAP)),]

# Find locations in Location dataframe that match locations in PJdata
matches <- match(Location_data$Location, PJdata$Location)
matchesRAP <- match(Location_dataRAP$Location, PJdataRAP$Location)

# Subset Location dataframe and pc matrix to keep only locations found in PJdata
Location_data_subset <- Location_data[which(!is.na(matches)),]
pc_mat_subset <- pc_mat[which(!is.na(matches)),]

Location_dataRAP_subset <- Location_dataRAP[which(!is.na(matchesRAP)),]
pc_matRAP_subset <- pc_mat_RAP[which(!is.na(matchesRAP)),]

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
PJdata.scaled <- PJdata %>%	mutate_at(scale, .vars = vars(Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev)) %>%
	select(-c(PC_t_mask,d_PC_mask,PC_t1_mask))

PJdataRAP.scaled <- PJdataRAP %>% mutate_at(scale, .vars = vars(Heatload,PPT_mean,Tmin_mean,Tmax_mean,PPT_dev,Tmin_dev,Tmax_dev)) %>%
	select(-c(PC_t_mask,d_PC_mask,PC_t1_mask))

# Remove observations with NA
PJdata.scaled <- na.omit(PJdata.scaled)
PJdataRAP.scaled <- na.omit(PJdataRAP.scaled)

# Subset data (only use to decrease time/memory when checking model code)
#PJdata.scaled <- subset(PJdata.scaled,Year_t)
#PJdataRAP.scaled <- subset(PJdataRAP.scaled,Year_t)

# Prep percent cover data for stan model
pc <- as.vector(c(PJdata.scaled$PC_t,PJdata.scaled$PC_t1[which(PJdata.scaled$Year_t==max(PJdata.scaled$Year_t))])) # vector of percent cover for ALL years
year <- as.vector(c(PJdata.scaled$Year_t,PJdata.scaled$Year_t1[which(PJdata.scaled$Year_t==max(PJdata.scaled$Year_t))])) # vector of year for each percent cover observation
year_ID=as.numeric(factor(year, levels = unique(sort(year)))) # vector of sequential year ID for each percent cover observation (used for indexing purposes)
pixel <- as.vector(c(PJdata.scaled$Pixel_ID,PJdata.scaled$Pixel_ID[which(PJdata.scaled$Year_t==max(PJdata.scaled$Year_t))])) # vector of sequential pixel ID for each percent cover observation (used for indexing purposes)

n_pc <- length(pc) # number of percent cover observations
n_pixel <- nrow(pc_matRAP_subset) # number of pixels in percent cover matrix (this will be number of rows in latent state matrix)
n_year <- length(unique(year)) # number of years (this will be number of columns in latent state matrix)

# Repeat for RAP data
pc_r <- as.vector(c(PJdataRAP.scaled$PC_t,PJdataRAP.scaled$PC_t1[which(PJdataRAP.scaled$Year_t==max(PJdataRAP.scaled$Year_t))])) # vector of percent cover for ALL years
pc_r[which(pc_r<min(pc))]<-min(pc)
year_r <- as.vector(c(PJdataRAP.scaled$Year_t,PJdataRAP.scaled$Year_t1[which(PJdataRAP.scaled$Year_t==max(PJdataRAP.scaled$Year_t))])) # vector of year for each percent cover observation
year_ID_r=as.numeric(factor(year_r, levels = unique(sort(year_r)))) # vector of sequential year ID for each percent cover observation (used for indexing purposes)
pixel_r <- as.vector(c(PJdataRAP.scaled$Pixel_ID,PJdataRAP.scaled$Pixel_ID[which(PJdataRAP.scaled$Year_t==max(PJdataRAP.scaled$Year_t))])) # vector of sequential pixel ID for each percent cover observation (used for indexing purposes)

n_pc_r <- length(pc_r) # number of percent cover observations
n_year_r <- length(unique(year_r)) # number of years (this will be number of columns in latent state matrix)

rmse <- 9.6
rmse_r <- 8.77

## Prep predictor variables

# Calculate spatial "normals"
ppt_mat_subset <- na.omit(total_ppt[which(!is.na(matches)),])
tmin_mat_subset <- na.omit(ave_tmin[which(!is.na(matches)),])
tmax_mat_subset <- na.omit(ave_tmax[which(!is.na(matches)),])
heatload_subset <- na.omit(heatload_vec[which(!is.na(matches))])

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
    //int<lower=0> n_pixel_r;          // Total number of pixels in percent cover raster (RAP)
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
        log_pc[,t+1] - log_pc[,t] ~ normal(u_beta0 + x[t]*u_beta + x_int[t]*u_beta_int,sigma_y);
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
    //int<lower=0> n_pixel_r;               // Total number of pixels in percent cover raster (RAP)
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
        log_pc[,t+1] - log_pc[,t] ~ normal(u_beta0 + x[t]*u_beta + x_int[t]*u_beta_int + u_beta_pc*exp(log_pc[,t]),sigma_y);
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
    //int<lower=0> n_pixel_r;               // Total number of pixels in percent cover raster (RAP)
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
        log_pc[,t+1] - log_pc[,t] ~ normal(u_beta0 + x[t]*u_beta + x_int[t]*u_beta_int + diag_pre_multiply(exp(log_pc[,t]),x[t])*u_beta_densint + u_beta_pc*exp(log_pc[,t]),sigma_y);
    }
    
    }
   
    ",fill=T)

sink()

pj_data <- list(K = K, I = I ,x = x_trans, x_int = x_int_trans, n_pixel = n_pixel, n_year = n_year, n_pc = n_pc, pc = pc, year = year_ID, pixel = pixel, sigma_pc = rmse,
							n_year_r = n_year_r, n_pc_r = n_pc_r, pc_r = pc_r, year_r = year_ID_r, pixel_r = pixel_r, sigma_pc_r = rmse_r)

rm(PJdata,PJdata_space,Location_data,Location_data_subset,pc_mat,pc_mat_subset,location.x,location.y,PJdata.scaled,pc,pixel,year,year_ID,
	 matches,n_pc,n_pixel,n_year,rmse)
rm(PJdataRAP,PJdataRAP_space,Location_dataRAP,Location_dataRAP_subset,pc_mat_RAP,pc_matRAP_subset,location.x.RAP,location.y.RAP,PJdataRAP.scaled,pc_r,pixel_r,year_r,year_ID_r,
	 matchesRAP,n_pc_r,n_year_r,rmse_r)
rm(x,x_trans,x_int,x_int_trans,K,heatload,heatload_subset,heatload_vec,total_ppt,ppt_mat_subset,PPT_mean,PPT_mean_vec,PPT_dev,
	 ave_tmin,tmin_mat_subset,Tmin_mean,Tmin_mean_vec,Tmin_dev,ave_tmax,tmax_mat_subset,Tmax_mean,Tmax_mean_vec,Tmax_dev)

options(mc.cores = parallel::detectCores())

fit_pj_clim <- stan(file = 'pjcover_clim.stan', data = pj_data, pars = (c("u_beta0","u_beta","u_beta_int","sigma_y")),include=TRUE, 
								 iter = 2000, warmup = 1000, chains = 3, refresh = 1)

fit_pj_climdens <- stan(file = 'pjcover_climdens.stan', data = pj_data, pars = (c("log_pc")),include=FALSE, 
							 iter = 2000, warmup = 1000, chains = 3, refresh = 1)

fit_pj_climdensint <- stan(file = 'pjcover_climdensint.stan', data = pj_data, pars = (c("log_pc")),include=FALSE, 
							 iter = 2000, warmup = 1000, chains = 3, refresh = 1, sample_file = 'fitPJ.csv')

save(fit_pj_clim,fit_pj_climdens,fit_pj_climdensint,file="fit_PJ_out.rda")
