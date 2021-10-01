#### PJ Cover Dynamics: Code to run stan Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 10 Sep 2021

library(rstan)

# Load data
load("./Output/stan_data.rda")

options(mc.cores = parallel::detectCores())

# Climate only
fit_pj_clim <- stan(file = 'pjcover_clim.stan', data = pj_data, 
										iter = 2000, warmup = 1000, chains = 3, refresh = 1)

# Climate + density, no interaction
fit_pj_climdens <- stan(file = 'pjcover_climdens.stan', data = pj_data, #pars = (c("log_pc")),include=FALSE, 
												iter = 2000, warmup = 1000, chains = 3, refresh = 1)

# Climate + density, with interaction
fit_pj_climdensint <- stan(file = 'pjcover_climdensint.stan', data = pj_data, #pars = (c("log_pc")),include=FALSE, 
													 iter = 2000, warmup = 1000, chains = 3, refresh = 1, sample_file = 'fitPJ.csv')

save(fit_pj_clim,fit_pj_climdens,fit_pj_climdensint,pj_data,file="fit_PJ_out.rda")