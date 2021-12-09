library(ggplot2)

# beta: smaller observation error
# beta2: original values
# beta3: q and r = 2

meanBeta <- colMeans(betaOut,na.rm=T)
sdBeta <- apply(betaOut,2,sd,na.rm=T)
name_vec <- c("Intercept","heatload","Mean PPT","Min T","Max T","PPT dev","Tmin dev","Tmax dev",
              "heatload*PPT","heatload*Tmin","heatload*Tmax","heatload*PPTdev","heatload*Tmindev","heatload*Tmaxdev",
              "PPT*Tmin","PPT*Tmax","Tmin*Tmax","PPTmean*PPTdev","Tminmean*Tmindev","Tmaxmean*Tmaxdev")
coef_dat <- data.frame(Names=name_vec,mean=meanBeta,sd_plus=meanBeta+sdBeta,sd_minus=meanBeta-sdBeta)

ggplot(coef_dat, aes(x = Names, y = mean)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point() + 
  geom_linerange(aes(x = Names, 
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
    plot(1:37,exp(meanLatent[,which(latentkeep==i)]),type="l",ylim=c(0,50))
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

beta_chain(betaOut,1,50000,name_vec)

plot(25000:50000,procVout[25000:50000],type="l")
plot(25000:50000,sampVout[25000:50000,1],type="l")
plot(25000:50000,sampVout[25000:50000,2],type="l")
hist(sampVout[25000:50000,1])
hist(sampVout[25000:50000,2])

latent_chain <- function(latent,year,start,end,N){
  sample <- sample.int(300,size=N)
  for(i in sample){
    plot(start:end,latOut[year,i,start:end],type="l")
  }
  
}

latent_chain(latOut,25,25000,50000,5)

