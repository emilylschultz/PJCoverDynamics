library(ggplot2)
library(RColorBrewer)

load("./Output/modelOut.rda")
load("./Output/modelOut_dd.rda")
load("./Output/modelOut_ddint.rda")

## Coefficient plots, all
meanBeta_dd_trim <- meanBeta_dd[-21]
sdBeta_dd_trim <- sdBeta_dd[-21]
meanBeta_ddint_trim <- meanBeta_ddint[-(21:28)]
sdBeta_ddint_trim <- sdBeta_ddint[-(21:28)]

name_vec <- c("Intercept","Heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
              "Heatload*PPT","Heatload*Tmin","Heatload*Tmax","Heatload*PPTdev","Heatload*Tmindev","Heatload*Tmaxdev",
              "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev")
coef_dat <- data.frame(Parameter=rep(name_vec,3),Model=c(rep("Climate",20),rep("Density",20),rep("Density Int",20)),
                       Value=c(meanBeta,meanBeta_dd_trim,meanBeta_ddint_trim),
                       sd_plus=c((meanBeta+sdBeta),(meanBeta_dd_trim+sdBeta_dd_trim),(meanBeta_ddint_trim+sdBeta_ddint_trim)),
                       sd_minus=c((meanBeta-sdBeta),(meanBeta_dd_trim-sdBeta_dd_trim),(meanBeta_ddint_trim-sdBeta_ddint_trim)))
coef_dat$Parameter <- as.character(coef_dat$Parameter)
coef_dat$Parameter <- factor(coef_dat$Parameter, levels=rev(unique(coef_dat$Parameter)))

ggplot(coef_dat, aes(x = Parameter, y = Value, color = Model)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point() + 
  geom_linerange(aes(x = Parameter, 
                     ymin = sd_minus,
                     ymax = sd_plus),
                 lwd = 1/2) + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  coord_flip()
  

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


sample <- sample.int(ncol(latMean_dd),size=50)
sample_latMean <- exp(latMean_dd[,sample])
sample_d_latMean <- sample_latMean[2:37,] / sample_latMean[1:36,]
mean_pc <- colMeans(sample_latMean)
order <- sort(mean_pc,index.return=T)$ix

pal <- colorRampPalette(brewer.pal(n=8, name = "Blues"))

matplot(seq(1984,2020),sample_latMean[,order],type="l",col=pal(70)[21:70],xlab = "Year", ylab= "Percent cover")

matplot(sample_latMean[1:36,order],sample_d_latMean[,order],pch=20,col=pal(70)[21:70],xlab = "Percent cover", ylab= "Proportional change in percent cover")
abline(1,0,lwd=2)
