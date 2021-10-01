library(mvtnorm)
library(abind)

sampleBeta<-function(y,X,beta.mn,beta.var,s2,tmax,dd=F,ddint=F){  ###This function is based on Hooton and Heffly 
##y=latent states
##X=Xall including all interactions with density   
X=apply(X,3,c)
if(dd==T|ddint==T){y=c(y[-1,])} else {y=c(y[-1,])-c(y[-tmax,])}
p=dim(X)[2]
Sig.beta.inv=diag(p)/beta.var
tmp.var<-solve(t(X)%*%X/s2+Sig.beta.inv)
tmp.mn<-tmp.var%*%(t(X)%*%y/s2+Sig.beta.inv%*%beta.mn)

beta=as.vector(rmvnorm(1,tmp.mn,tmp.var,method='chol'))
return(beta) 
}

sampleSigma<-function(y,X,beta,q,r,tmax,dd=F,ddint=F){  ###This function is based on Hooton and Heffly 
  ##y=latent states
  ##X=Xall including all interactions with density   
  X=apply(X,3,c)
  if(dd==T|ddint==T){y=c(y[-1,])} else {y=c(y[-1,])-c(y[-tmax,])}
  n<-dim(X)[1]
  tmp.r<-(1/r+.5*t(y-X%*%beta)%*%(y-X%*%beta))^(-1)
  tmp.q<-n/2+q
  s2=1/rgamma(1,tmp.q,tmp.r)
  
}

sampleLatent<-function(Y1,Y2,y,X,Xall,Xdense=NA,s2,beta,o1,o2,Ndatasets,tmax,dd=F,ddint=F,ddbeta=NA) {  #This function is tricky. I changes for first and last years and years for which we have one or two datasets
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
    # Add if loop to fill v with Y1 values if Y2 values are missing
  }
  
  
  if(t>1 & dd==F & ddint==F){
    mu1<-y[t-1,]+Xall[t-1,,]%*%beta ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu1/s2
  }
  
  if(t>1 & (dd==T | ddint==T)){
    mu1<-Xall[t-1,,]%*%beta ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu1/s2
  }
    
  if(t<tmax & dd==F & ddint==F){
    mu2<-y[t+1,]-Xall[t,,]%*%beta ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu2/s2
  }
  
  
  if(t<tmax & dd==T & ddint==F){
    mu2<-(y[t+1,]-X[t,,]%*%beta[-ddbeta])/(beta[ddbeta]) ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu2/s2
  }
  
  
  if(t<tmax & dd==T & ddint==T){
    mu2<-(y[t+1,]-X[t,,]%*%beta[-ddbeta])/(beta[ddbeta[1]]+Xdense[t,,-1]%*%beta[ddbeta[-1]]) ####Here y is arranged year by pixel and X is year by pixel by covariate 
    Vi=Vi+1/s2
    v=v+mu2/s2
  }
  
  
  
V<-1/Vi
yt<-rnorm(dim(v)[1],c(V*v),sqrt(V))
return(yt)
    
}  

load("SamplerData_1000.rda")

#X<- Raw covariates without density
#Xdense<-raw covariates that have interactions with density
#Xall includes density 
Y1<- log(pc_mat_RAP)#RAP data logged
Y2<- log(pc_mat)#PJ data logged
Ndatasets<- c(rep(1,20),rep(2,17))#Vector of length tmax that specifies whether 1 or 2 datasets are available for a given year
tmax<- 37 #total number of year
o1<- log(1 + (8.77/mean(pc_mat_RAP))) #observation variance for RAP data
o2<- log(1 + (9.6/mean(pc_mat,na.rm=T))) #observation variance for PJ data
ddbeta <- c(21) # no interaction
ddbeta_int <- c(21:28) # no interaction

######Starting values########
#beta<-solve for ML estimate 
s2<- 0.02 #good guess
y<- Y1 #RAP data + small noise? 

Xall <- X # climate
Xall_dd <- abind(X,y[-tmax,]) # climate, density, no interactions
#Xall_ddint <- abind(X,y[-tmax,],Xdense*(array(y[-tmax,],dim=dim(Xdense)))) # climate, density, interactions
Xall_ddint <- abind(X,Xdense*(array(y[-tmax,],dim=dim(Xdense)))) # climate, density, interactions

#### 3enst starting values ####

###Prior Parameters###
beta.mn<-rep(0,dim(Xall)[3])
beta.mn_dd<-rep(0,dim(Xall_dd)[3])
beta.mn_ddint<-rep(0,dim(Xall_ddint)[3])
beta.var<-10^2
q=1
r=1
#### end priors####


iter<- 10000 #number of sampler iterations
######storage for output#######
betaOut<-matrix(NA,iter,dim(Xall)[3]) #beta output
betaOut_dd<-matrix(NA,iter,dim(Xall_dd)[3]) #beta output
betaOut_ddint<-matrix(NA,iter,dim(Xall_ddint)[3]) #beta output

procVout<-matrix(NA,iter,1) #process variance output 
latentkeep<-sample(1:ncol(y),300) #indexes for some random latent values to save.
latOut<-array(NA,c(tmax,300,iter))
#####End Storage##############

for (i in 1:iter){
  
  beta<-sampleBeta(y,X=Xall,beta.mn,beta.var,s2,tmax)
  betaOut[i,]<-beta
  
  s2<-sampleSigma(y,X=Xall,beta,q,r,tmax)
  procVout[i,]<-s2
  
  for (t in 1:tmax){
    
    y[t,]<-sampleLatent(Y1,Y2,y,X=X,Xall=Xall,Xdense=Xdense,s2,beta,o1,o2,Ndatasets,tmax,dd=F,ddint=F,ddbeta=NA)
    
  }
  
  latOut[,,i]<-y[,latentkeep]
  
  print(i)
}


for (i in 1:iter){
  
  beta<-sampleBeta(y,X=Xall_dd,beta.mn_dd,beta.var,s2,tmax,dd=T,ddint=F)
  betaOut_dd[i,]<-beta
  
  s2<-sampleSigma(y,X=Xall_dd,beta,q,r,tmax,dd=T,ddint=F)
  procVout[i,]<-s2
  
  for (t in 1:tmax){
    Vi=0
    v=0
    y[t,]<-sampleLatent(Y1,Y2,y,X=X,Xall=Xall_dd,Xdense=Xdense,s2,beta,o1,o2,Ndatasets,tmax,dd=T,ddint=F,ddbeta=ddbeta)
    
    if (t<tmax){Xall_dd[t,,ddbeta]<-y[t,]}
    
  }
  
  latOut[,,i]<-y[,latentkeep]
  
  print(i)
}

for (i in 1:iter){
  
  beta<-sampleBeta(y,X=Xall_ddint,beta.mn_ddint,beta.var,s2,tmax,dd=T,ddint=T)
  betaOut_ddint[i,]<-beta
  
  s2<-sampleSigma(y,X=Xall_ddint,beta,q,r,tmax,dd=T,ddint=T)
  procVout[i,]<-s2
  
  for (t in 1:tmax){
    
    y[t,]<-sampleLatent(Y1,Y2,y,X=X,Xall=Xall_ddint,Xdense=Xdense,s2,beta,o1,o2,Ndatasets,tmax,dd=T,ddint=T,ddbeta=ddbeta_int)
    
    if (t<tmax){Xall_ddint[t,,ddbeta_int]<-y[t,]*Xdense[t,,]}
    
  }
  
  latOut[,,i]<-y[,latentkeep]
  
  print(i)
}

