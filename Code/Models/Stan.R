#### PJ Cover Dynamics: Code to create Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 2 Aug 2021

library(rstan)

# Load dataframes

# Prep data for stan model


# Stan model

sink("pjcover.stan")
cat("
    data {
    
    int<lower=0> K;           // N. predictors 
    int<lower=0> n;           // N. observations
    matrix[n,K] x;            // Predictor matrix
    vector[n] pc_t;           // percent cover at time t (not RAP)
    vector[n] pc_t_r;         // percent cover at time t (RAP)
    vector[n] y;              // log size at time t+1 

    }
    
    parameters {
    
    real u_beta0;                          // intercept means
    vector[K] u_beta;                      // other coeff mean
  
    real<lower=0> sigma_y;                 // Residual variation

    }
    
 
    
    model {
    vector[nG] mG;
    
    u_beta0 ~ normal(0, 100);

    u_beta ~ normal(0, 100); 

    sigma_y ~ gamma(2,0.01);
    

    // GROWTH MODEL

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