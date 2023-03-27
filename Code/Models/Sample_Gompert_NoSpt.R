### Code to build and run Gompertz sampler for PJ cover data
### Created by: Bob Shriver and Emily Schultz
### Created on: 23 May 2022

library(mvtnorm)
library(abind)
library(invgamma)

sampleSigma<-function(y,X,beta,a,b,tmax,dd=F,ddint=F){  ###This function is based on Hooton and Heffly 
  ##y=latent states
  ##X=Xall including all interactions with density   
  X=apply(X,3,c)

  if(dd==T|ddint==T){y=c(y[-1,])} else {y=c(y[-1,])-c(y[-tmax,])}
  n<-dim(X)[1]
  
  tmp.a<-a+n/2
  tmp.b<-b+(0.5*sum((y-X%*%beta)^2))
  
  s2=rinvgamma(1,tmp.a,tmp.b)
  
}

sampleObS<-function(y,Y,a,b,tmin,tmax){  ###This function is based on Hooton and Heffly
  ##y=latent states
  ##X=Xall including all interactions with density
  y<-c(y[tmin:tmax,])
  Y<-c(Y[tmin:tmax,])
  n<-length(y)
  tmp.a<-a+n/2
  tmp.b<-b+(0.5*sum((Y-y)^2,na.rm=T))
  
  return(rinvgamma(1,tmp.a,tmp.b))
  
}

sampleBeta<-function(y,X,beta.mn,beta.var,s2,tmax,dd=F,ddint=F){  ###This function is based on Hooton and Heffly 
  ##y=latent states
  ##X=Xall including all interactions with density   
  X=apply(X,3,c)

  if(dd==T|ddint==T){y=c(y[-1,])} else {y=c(y[-1,])-c(y[-tmax,])}
  p=dim(X)[2]
  Sig.beta.inv=diag(p)/beta.var
  tmp.var<-solve(t(X)%*%X/s2+Sig.beta.inv)
  tmp.mn<-tmp.var%*%(t(X)%*%(y)/s2+Sig.beta.inv%*%beta.mn)
  
  beta=as.vector(rmvnorm(1,tmp.mn,tmp.var,method='chol'))
  return(beta) 
}




sampleLatent<-function(Y1,Y2,y,X,Xall,Xdense=NA,s2,beta,o1,o2,Ndatasets,tmax,dd=F,ddint=F,ddbeta=NA,missing) {  #This function is tricky. I changes for first and last years and years for which we have one or two datasets
  ###Note: s2 is the process variance, o1 is the observation variance for RAP, and o2 is the observation variance for PJ
  #This fucntion is based on https://rpubs.com/jimclark/741274
  #Ndatasets=Vector of length tmax with either 1 or 2 for datasets
  #ddbeta=vector with idexes of beta coefficents related to density
  Vi=0
  v=0
  if (Ndatasets[t]==1){
    Vi=1/o1
    v=(Y1[t,]/o1)
  }
  
  if (Ndatasets[t]==2){
    Vi=1/o1+1/o2
    v=(Y1[t,]/o1)+(Y2[t,]/o2)
    v[missing[[(t-16)]]] <- Y1[t,missing[[(t-16)]]]/o1
  }
  
  
  if(t>1 & dd==F & ddint==F){
    mu1<-y[t-1,]+(Xall[t-1,,]%*%beta) ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu1/s2
  }
  
  if(t>1 & (dd==T | ddint==T)){
    mu1<-(Xall[t-1,,]%*%beta) ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu1/s2
  }
  
  if(t<tmax & dd==F & ddint==F){
    mu2<-y[t+1,]-(Xall[t,,]%*%beta) ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu2/s2
  }
  
  if(t<tmax & dd==T & ddint==F){
    mu2<-(y[t+1,]-(X[t,,]%*%beta[-ddbeta]))/(beta[ddbeta]) ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu2/s2
  }
  
  
  if(t<tmax & dd==T & ddint==T){
    mu2<-(y[t+1,]-(X[t,,]%*%beta[-ddbeta]))/(beta[ddbeta[1]]+Xdense[t,,-1]%*%beta[ddbeta[-1]]) ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu2/s2
  }
  
  V<-1/Vi
  yt<-rnorm(dim(v)[1],c(V*v),sqrt(V))
  return(yt)
  
}  

dev_calc <- function(Y1,Y2,y,X,beta,o1,o2,s2,dd=F,ddint=F){
  ##y=latent states
  ##X=Xall including all interactions with density  
  
  if(dd==T|ddint==T){y=c(y[-1,])} else {y=c(y[-1,])-c(y[-tmax,])}
  Y1=c(Y1[-1,])
  Y2=c(Y2[-1,])
  X=apply(X,3,c)

  lik_y <- dnorm(y,(X%*%beta),sqrt(s2)) # probability of true pc, given model
  lik_Y1 <- dnorm(Y1,y,sqrt(o1)) # probability of observed pc (RAP), given true pc
  lik_Y2 <- dnorm(Y2,y,sqrt(o2)) # probability of observed pc (Fill.), given true pc
  
  # log(product of probabilities) == sum of log(probabilities)
  log_lik_y <- log(lik_y)
  log_lik_Y1 <- log(lik_Y1)
  log_lik_Y2 <- log(lik_Y2)
  log_lik <- sum(log_lik_y) + sum(log_lik_Y1) + sum(log_lik_Y2,na.rm=T)
  
  dev <- (-2)*log_lik
  return(dev)
}

load("SamplerDataMask_Lag.rda")
X <- X_lag
Xdense <- Xdense_lag
rm(X_lag,Xdense_lag)
#X<- Raw covariates without density
#Xdense<-raw covariates that have interactions with density
#Xall includes density 
pc_mat_RAP[which(pc_mat_RAP<=0)] <- min(pc_mat_RAP[which(pc_mat_RAP>0)])
Y1<- log(pc_mat_RAP)#RAP data logged
Y1[which(Y1<0)] <- 0
Y2<- log(pc_mat)#PJ data logged

# Use subset
#N <- round(0.1*ncol(Y1))
#sample <- sample.int(ncol(Y1),N)
#Y1 <- Y1[,sample]
#Y2 <- Y2[,sample]
#X <- X[,sample,]
#Xdense <- Xdense[,sample,]

missing <- which(is.na(Y2),arr.ind=T)
missing_list <- list("2000"=missing[which(missing[,1]==1),2],"2001"=missing[which(missing[,1]==2),2],
                     "2002"=missing[which(missing[,1]==3),2],"2003"=missing[which(missing[,1]==4),2],
                     "2004"=missing[which(missing[,1]==5),2],"2005"=missing[which(missing[,1]==6),2],
                     "2006"=missing[which(missing[,1]==7),2],"2007"=missing[which(missing[,1]==8),2],
                     "2008"=missing[which(missing[,1]==9),2],"2009"=missing[which(missing[,1]==10),2],
                     "2010"=missing[which(missing[,1]==11),2],"2011"=missing[which(missing[,1]==12),2],
                     "2012"=missing[which(missing[,1]==13),2],"2013"=missing[which(missing[,1]==14),2],
                     "2014"=missing[which(missing[,1]==15),2],"2015"=missing[which(missing[,1]==16),2],
                     "2016"=missing[which(missing[,1]==17),2])
Y2 <- rbind(matrix(NA,nrow=16,ncol=ncol(Y1)),Y2,matrix(NA,nrow=4,ncol=ncol(Y1)))
Ndatasets<- c(rep(1,16),rep(2,17),rep(1,4))#Vector of length tmax that specifies whether 1 or 2 datasets are available for a given year
tmin<-1
tmax<- 37 #total number of year
ddbeta <- c(11) # no interaction
ddbeta_int <- c(11:14) # no interaction

######Starting values########
#beta<-solve for ML estimate 
s2<- 0.2 #good guess
noise <- matrix(rnorm(dim(Y1)[1]*dim(Y1)[2],0,0.1),dim(Y1)[1],dim(Y1)[2])
y<- Y1 + noise  #RAP data + small noise? 

o1<- -2*log(mean(pc_mat_RAP,na.rm=T)) + log(8.77^2 + mean(pc_mat_RAP,na.rm=T)^2) #observation variance for RAP data
o2<- -2*log(mean(pc_mat,na.rm=T)) + log(9.6^2 + mean(pc_mat,na.rm=T)^2) #observation variance for PJ data

Xall <- X # climate
Xall_dd <- abind(X,y[-tmax,]) # climate, density, no interactions
Xall_ddint <- abind(X,Xdense*(array(y[-tmax,],dim=dim(Xdense)))) # climate, density, interactions

###Prior Parameters###
beta.mn<-rep(0,dim(Xall)[3])
beta.mn_dd<-rep(0,dim(Xall_dd)[3])
beta.mn_ddint<-rep(0,dim(Xall_ddint)[3])
beta.var<-10^2
a=1
b=1


ro <- 0.5
qo1 <- (ro/o1)+1
qo2 <- (ro/o2)+1

#### end priors####


iter<- 20000 #number of sampler iterations
burnin<-2000
######storage for output#######
betaOut<-matrix(NA,iter,dim(Xall)[3]) #beta output
betaOut_dd<-matrix(NA,iter,dim(Xall_dd)[3]) #beta output
betaOut_ddint<-matrix(NA,iter,dim(Xall_ddint)[3]) #beta output

procVout<-matrix(NA,iter,1) #process variance output 
sampVout<-matrix(NA,iter,2) #process variance output 
latentkeep<-sample(1:ncol(y),300) #indexes for some random latent values to save.
latOut<-array(NA,c(tmax,300,iter))
#resid<-matrix(0,tmax-1,ncol(y))
latMean<-matrix(0,tmax,ncol(y))

devOut<-matrix(NA,iter,1)

#####End Storage##############

# Density independent
for (i in 1:iter){
  
  
  beta<-sampleBeta(y,X=Xall,beta.mn,beta.var,s2,tmax)
  betaOut[i,]<-beta
  
  s2<-sampleSigma(y,X=Xall,beta,a,b,tmax)
  procVout[i,]<-s2
  
  o1 <- sampleObS(y,Y=Y1,a=qo1,b=ro,tmin,tmax)
  o2 <- sampleObS(y,Y=Y2,a=qo2,b=ro,tmin=17,tmax=33)
  sampVout[i,1]<-o1
  sampVout[i,2]<-o2
  
  for (t in 1:tmax){
    
    y[t,]<-sampleLatent(Y1,Y2,y,X=X,Xall=Xall,Xdense=Xdense,s2,beta,o1,o2,Ndatasets,tmax,dd=F,ddint=F,ddbeta=NA,missing=missing_list)
    
    #if(i>burnin & t>1){
    #r <- y[t,] - Xall[t-1,,]%*%beta
    #resid[t-1,] <- resid[t-1,]*((i-(burnin+1))/(i-burnin)) + r*(1/(i-burnin))
    #} 
    
  }
  
  latOut[,,i]<-y[,latentkeep]
  
  
  devOut[i] <- dev_calc(Y1,Y2,y,X=Xall,beta,o1,o2,s2)
  
  if(i>burnin){
    latMean <- latMean*((i-(burnin+1))/(i-burnin)) + y*(1/(i-burnin))
  }   
  
  print(i)
}

# Density dependent, no interactions
for (i in 1:iter){
  
  beta<-sampleBeta(y,X=Xall_dd,beta.mn_dd,beta.var,s2,tmax,dd=T)
  betaOut_dd[i,]<-beta
  
  s2<-sampleSigma(y,X=Xall_dd,beta,a,b,tmax,dd=T)
  procVout[i,]<-s2
  
  o1 <- sampleObS(y,Y=Y1,a=qo1,b=ro,tmin,tmax)
  o2 <- sampleObS(y,Y=Y2,a=qo2,b=ro,tmin=17,tmax=33)
  sampVout[i,1]<-o1
  sampVout[i,2]<-o2
  
  for (t in 1:tmax){
    Vi=0
    v=0
    y[t,]<-sampleLatent(Y1,Y2,y,X=X,Xall=Xall_dd,Xdense=Xdense,s2,beta,o1,o2,Ndatasets,tmax,dd=T,ddint=F,ddbeta=ddbeta,missing=missing_list)
    
    if (t<tmax){Xall_dd[t,,ddbeta]<-y[t,]}
    
    #if(i>burnin & t>1){
    #r <- y[t,] - Xall_dd[t-1,,]%*%beta
    #resid[t-1,] <- resid[t-1,]*((i-(burnin+1))/(i-burnin)) + r*(1/(i-burnin))
    #} 
    
  }
  
  latOut[,,i]<-y[,latentkeep]
  

  devOut[i] <- dev_calc(Y1,Y2,y,X=Xall_dd,beta,o1,o2,s2,dd=T)
  
  if(i>burnin){
    latMean <- latMean*((i-(burnin+1))/(i-burnin)) + y*(1/(i-burnin))
  }   
  
  print(i)
}

# Density dependent, interactions
for (i in 1:iter){
  
  beta<-sampleBeta(y,X=Xall_ddint,beta.mn_ddint,beta.var,s2,tmax,dd=T,ddint=T)
  betaOut_ddint[i,]<-beta
  
  s2<-sampleSigma(y,X=Xall_ddint,beta,a,b,tmax,dd=T,ddint=T)
  procVout[i,]<-s2
  
  o1 <- sampleObS(y,Y=Y1,a=qo1,b=ro,tmin,tmax)
  o2 <- sampleObS(y,Y=Y2,a=qo2,b=ro,tmin=17,tmax=33)
  sampVout[i,1]<-o1
  sampVout[i,2]<-o2
  
  for (t in 1:tmax){
    
    y[t,]<-sampleLatent(Y1,Y2,y,X=X,Xall=Xall_ddint,Xdense=Xdense,s2,beta,o1,o2,Ndatasets,tmax,dd=T,ddint=T,ddbeta=ddbeta_int,missing=missing_list)
    
    if (t<tmax){Xall_ddint[t,,ddbeta_int]<-y[t,]*Xdense[t,,]}
    
    #if(i>burnin & t>1){
    #r <- y[t,] - Xall_ddint[t-1,,]%*%beta
    #resid[t-1,] <- resid[t-1,]*((i-(burnin+1))/(i-burnin)) + r*(1/(i-burnin))
    #}    
  }
  
  latOut[,,i]<-y[,latentkeep]
  
 
  
  
  devOut[i] <- dev_calc(Y1,Y2,y,X=Xall_ddint,beta,o1,o2,s2,dd=T,ddint=T)
  
  if(i>burnin){
    latMean <- latMean*((i-(burnin+1))/(i-burnin)) + y*(1/(i-burnin))
  }   
  
  print(i)
}


