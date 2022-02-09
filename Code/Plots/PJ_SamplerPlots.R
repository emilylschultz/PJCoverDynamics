library(ggplot2)

# beta: smaller observation error
# beta2: original values
# beta3: q and r = 2

## Coefficients plot, no density coefficient
meanBeta <- colMeans(betaOut_ddint[burnin:iter,],na.rm=T)
meanBeta <- meanBeta[,-21]
sdBeta <- apply(betaOut_ddint[burnin:iter,],2,sd,na.rm=T)
sdBeta <- sdBeta[,-21]

name_vec <- c("Intercept","Heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
              "Heatload*PPT","Heatload*Tmin","Heatload*Tmax","Heatload*PPTdev","Heatload*Tmindev","Heatload*Tmaxdev",
              "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev")
name_vec_dd <- c("Intercept","Heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
                 "Heatload*PPT","Heatload*Tmin","Heatload*Tmax","Heatload*PPTdev","Heatload*Tmindev","Heatload*Tmaxdev",
                 "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev")
name_vec_ddint <- c("Intercept","Heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
                    "Heatload*PPT","Heatload*Tmin","Heatload*Tmax","Heatload*PPTdev","Heatload*Tmindev","Heatload*Tmaxdev",
                    "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev",
                    "Density*Heatload","Density*Mean PPT","Density*Min T","Density*Max T",
                    "Density*PPT dev","Density*Tmin dev","Density*Tmax dev")
coef_dat <- data.frame(Parameter=name_vec_ddint,Value=meanBeta,sd_plus=meanBeta+sdBeta,sd_minus=meanBeta-sdBeta)
coef_dat$Parameter <- as.character(coef_dat$Parameter)
coef_dat$Parameter <- factor(coef_dat$Parameter, levels=rev(unique(coef_dat$Parameter)))

color_vec <- rev(c("black","#D55E00","#D55E00","#D55E00","#D55E00","black","#D55E00","black","black","black","black","black","black","black",
                   "#D55E00","#D55E00","black","black","black","black"))
color_vec_dd <- rev(c("black","#D55E00","#D55E00","#D55E00","#D55E00","black","#D55E00","black","black","black","black","black","black","black",
                      "#0072B2","#0072B2","black","black","black","black"))
color_vec_ddint <- c(rep("black",7),rev(c("black","#0072B2","#0072B2","#0072B2","#0072B2","black","#0072B2","black","black","black","black","black","black","black",
                                          "#0072B2","#0072B2","black","black","black","black")))
ggplot(coef_dat, aes(x = Parameter, y = Value, color = Parameter)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point() + 
  geom_linerange(aes(x = Parameter, 
                     ymin = sd_minus,
                     ymax = sd_plus),
                 lwd = 1/2) + 
  scale_color_manual(values = color_vec_ddint)+
  coord_flip()+
  theme(axis.text.y = element_text(colour = color_vec_ddint),legend.position = "none")

## Coefficients plot
meanBeta <- colMeans(betaOut_ddint[burnin:iter,],na.rm=T)
sdBeta <- apply(betaOut_ddint[burnin:iter,],2,sd,na.rm=T)
name_vec <- c("Intercept","Heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
              "Heatload*PPT","Heatload*Tmin","Heatload*Tmax","Heatload*PPTdev","Heatload*Tmindev","Heatload*Tmaxdev",
              "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev")
name_vec_dd <- c("Intercept","Heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
                 "Heatload*PPT","Heatload*Tmin","Heatload*Tmax","Heatload*PPTdev","Heatload*Tmindev","Heatload*Tmaxdev",
                 "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev","Density")
name_vec_ddint <- c("Intercept","Heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
                    "Heatload*PPT","Heatload*Tmin","Heatload*Tmax","Heatload*PPTdev","Heatload*Tmindev","Heatload*Tmaxdev",
                    "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev","Density",
                    "Density*Heatload","Density*Mean PPT","Density*Min T","Density*Max T",
                    "Density*PPT dev","Density*Tmin dev","Density*Tmax dev")
coef_dat <- data.frame(Parameter=name_vec_ddint,Value=meanBeta,sd_plus=meanBeta+sdBeta,sd_minus=meanBeta-sdBeta)
coef_dat$Parameter <- as.character(coef_dat$Parameter)
coef_dat$Parameter <- factor(coef_dat$Parameter, levels=rev(unique(coef_dat$Parameter)))

ggplot(coef_dat, aes(x = Parameter, y = Value)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point() + 
  geom_linerange(aes(x = Parameter, 
                     ymin = sd_minus,
                     ymax = sd_plus),
                 lwd = 1/2) + 
  coord_flip()

meanLatent <- apply(latOut,c(1,2),mean,na.rm=T)

latent_plot <- function(meanLatent,latentkeep,N){
  # Add confidence intervals to latent states
  # Add confidence intervals to observations (constant)
  sample <- sample(latentkeep,size=N)
  for(i in sample){
    plot(1:37,exp(meanLatent[,which(latentkeep==i)]),type="l",ylim=c(0,50),ylab="Percent cover",xlab="Time")
    lines(1:37,exp(Y1[,i]),type="l",col="lightblue")
    lines(1:37,exp(Y2[,i]),type="l",col="blue")
  }
}

latent_plot(meanLatent,latentkeep,10)

beta_chain <- function(beta,start,end,names){
  for(i in 1:dim(beta)[2]){
    plot(start:end,beta[start:end,i],type="l",xlab="Iteration",ylab=names[i])
    #ylim=c(min(beta[start:end,i],na.rm=T),max(beta[start:end,i],na.rm=T)))
    #lines(start:end,beta2[start:end,i],type="l",xlab="Iteration",ylab=names[i],col="blue")
    #lines(start:end,beta3[start:end,i],type="l",xlab="Iteration",ylab=names[i],col="green")
  }
}

beta_chain(betaOut,burnin,iter,name_vec)

plot(burnin:iter,procVout[burnin:iter],type="l")
plot(burnin:iter,sampVout[burnin:iter,1],type="l")
plot(burnin:iter,sampVout[burnin:iter,2],type="l")

latent_chain <- function(latent,year,start,end,N){
  sample <- sample.int(300,size=N)
  for(i in sample){
    plot(start:end,latOut[year,i,start:end],type="l")
  }
  
}

latent_chain(latOut,25,1000,end,5)

