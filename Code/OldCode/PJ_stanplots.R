#### PJ Cover Dynamics: Code to create plots of Bayesian models of effects of density and climate on PJ percent cover
## Created by: Emily Schultz
## Created on: 7 Sep 2021

library(tidyverse)
library(rstan)
library(bayesplot)
library(shinystan)

load("fit_PJ_out.rda")

launch_shinystan(fit_pj_clim)

summary_clim <- summary(fit_pj_clim)$summary
summary_climdens <- summary(fit_pj_climdens)$summary
summary_climdensint <- summary(fit_pj_climdensint)$summary

posterior_clim <- rstan::extract(fit_pj_clim, inc_warmup = TRUE, permuted = FALSE)
posterior_climdens <- rstan::extract(fit_pj_climdens, inc_warmup = TRUE, permuted = FALSE)
posterior_climdensint <- rstan::extract(fit_pj_climdensint, inc_warmup = TRUE, permuted = FALSE)

# Trace plots
color_scheme_set("mix-blue-pink")
p_clim <- mcmc_trace(posterior_clim, pars = c("u_beta0", "u_beta[1]"), n_warmup = 1000, #  
								facet_args = list(nrow = 2, labeller = label_parsed))
p_clim + facet_text(size = 15)

p_climdens <- mcmc_trace(posterior_climdens, pars = c("u_beta0", "u_beta[1]"), n_warmup = 1000, # 
								facet_args = list(nrow = 2, labeller = label_parsed))
p_climdens + facet_text(size = 15)

p_climdensint <- mcmc_trace(posterior_climdensint, pars = c("u_beta0", "u_beta[1]"), n_warmup = 1000, # 
								facet_args = list(nrow = 2, labeller = label_parsed))
p_climdensint + facet_text(size = 15)

# True vs observed percent cover
pc_data <- data.frame(PC=pj_data$pc,Pixel = pj_data$pixel, Year = pj_data$year, Data = "Landsat")
pc_data_rap <- data.frame(PC=pj_data$pc_r,Pixel = pj_data$pixel_r, Year = pj_data$year_r, Data = "RAP")
true_pc_clim <- data.frame(PC=exp(summary_clim[22:(nrow(summary_clim)-1),1]),Pixel = sort(rep(1:100,37)), Year = rep(1:37,100), Data = "True_c")
true_pc_climdens <- data.frame(PC=exp(summary_climdens[23:(nrow(summary_climdens)-1),1]),Pixel = sort(rep(1:100,37)), Year = rep(1:37,100), Data = "True_cd")
true_pc_climdensint <- data.frame(PC=exp(summary_climdensint[30:(nrow(summary_climdensint)-1),1]),Pixel = sort(rep(1:100,37)), Year = rep(1:37,100), Data = "True_cdi")

pc_data_stacked <- rbind(pc_data,pc_data_rap,true_pc_clim,true_pc_climdens,true_pc_climdensint)

true_rap <- ggplot(data=pc_data_all,aes(x=PC_true,y=PC_r,col=as.factor(Pixel))) +
	geom_point() + coord_cartesian(xlim = c(0,50))

true_lndst <- ggplot(data=pc_data_all,aes(x=PC_true,y=PC,col=as.factor(Pixel))) +
	geom_point() + coord_cartesian(xlim = c(0,50))

pixel1 <- ggplot(data=subset(pc_data_stacked,Pixel==1),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel2 <- ggplot(data=subset(pc_data_stacked,Pixel==2),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel3 <- ggplot(data=subset(pc_data_stacked,Pixel==3),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel4 <- ggplot(data=subset(pc_data_stacked,Pixel==4),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel5 <- ggplot(data=subset(pc_data_stacked,Pixel==5),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel6 <- ggplot(data=subset(pc_data_stacked,Pixel==6),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel7 <- ggplot(data=subset(pc_data_stacked,Pixel==7),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel8 <- ggplot(data=subset(pc_data_stacked,Pixel==8),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel9 <- ggplot(data=subset(pc_data_stacked,Pixel==9),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel10 <- ggplot(data=subset(pc_data_stacked,Pixel==10),aes(x=Year,y=PC,col=Data)) +
	geom_line(size=1)

pixel1
pixel2
pixel3
pixel4
pixel5
pixel6
pixel7
pixel8
pixel9
pixel10