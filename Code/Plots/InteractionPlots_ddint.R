library(ggplot2)

predict_fun <- function(coef=betaOut_ddint,heatload=heatload_mean,ppt=ppt_mean,tmin=tmin_mean,tmax=tmax_mean,ppt_dev=ppt_dev_mean,tmin_dev=tmin_dev_mean,tmax_dev=tmax_dev_mean,dens=dens_mean){
  coef[i,1] + coef[i,2]*heatload + coef[i,3]*ppt + coef[i,4]*tmin +
    coef[i,5]*tmax + coef[i,6]*ppt_dev + coef[i,7]*tmin_dev + coef[i,8]*tmax_dev + coef[i,9]*heatload*ppt +
    coef[i,10]*heatload*tmin + coef[i,11]*heatload*tmax + coef[i,12]*heatload*ppt_dev + coef[i,13]*heatload*tmin_dev +
    coef[i,14]*heatload*tmax_dev + coef[i,15]*ppt*tmin + coef[i,16]*ppt*tmax + coef[i,17]*tmin*tmax + 
    coef[i,18]*ppt*ppt_dev + coef[i,19]*tmin*tmin_dev + coef[i,20]*tmax*tmax_dev + coef[i,21]*dens +
    coef[i,22]*dens*heatload + coef[i,23]*dens*ppt + coef[i,24]*dens*tmin + coef[i,25]*dens*tmax +
    coef[i,26]*dens*ppt_dev + coef[i,27]*dens*tmin_dev + coef[i,28]*dens*tmax_dev
}
#INTERACTION PLOTS
samp <- sample(seq((burnin+1),iter),1000)
meanLatent <- apply(latOut,c(1,2),mean,na.rm=T)

dens_mean <- mean(meanLatent)
heatload_mean <- mean(Xall[,,2])
ppt_mean <- mean(Xall[,,3])
tmin_mean <- mean(Xall[,,4])
tmax_mean <- mean(Xall[,,5])
ppt_dev_mean <- mean(Xall[,,6])
tmin_dev_mean <- mean(Xall[,,7])
tmax_dev_mean <- mean(Xall[,,8])

densrng <- range(meanLatent,na.rm = TRUE) #setting range for heatload
dens_seq <- seq(densrng[1], densrng[2], by = 0.01)
dens_quant <- quantile(meanLatent, c(0.2, 0.8))

heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload_seq <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
heatload_quant <- quantile(Xall[,,2], c(0.2, 0.8))

pptrng <- range(Xall[,,3],na.rm = TRUE) #setting range for heatload
ppt_seq <- seq(pptrng[1], pptrng[2], by = 0.01)
ppt_quant <- quantile(Xall[,,3], c(0.2, 0.8))

tminrng <- range(Xall[,,4],na.rm = TRUE) #setting range for heatload
tmin_seq <- seq(tminrng[1], tminrng[2], by = 0.01)
tmin_quant <- quantile(Xall[,,4], c(0.2, 0.8))

tmaxrng <- range(Xall[,,5],na.rm = TRUE) #setting range for heatload
tmax_seq <- seq(tmaxrng[1], tmaxrng[2], by = 0.01)
tmax_quant <- quantile(Xall[,,5], c(0.2, 0.8))

ppt_devrng <- range(Xall[,,6],na.rm = TRUE) #setting range for heatload
ppt_dev_seq <- seq(ppt_devrng[1], ppt_devrng[2], by = 0.01)
ppt_dev_quant <- quantile(Xall[,,6], c(0.2, 0.8))

tmin_devrng <- range(Xall[,,7],na.rm = TRUE) #setting range for heatload
tmin_dev_seq <- seq(tmin_devrng[1], tmin_devrng[2], by = 0.01)
tmin_dev_quant <- quantile(Xall[,,7], c(0.2, 0.8))

tmax_devrng <- range(Xall[,,8],na.rm = TRUE) #setting range for heatload
tmax_dev_seq <- seq(tmax_devrng[1], tmax_devrng[2], by = 0.01)
tmax_dev_quant <- quantile(Xall[,,8], c(0.2, 0.8))


#Heatload and PPT
predictionHeatload_highPPT <- predictionHeatload_midPPT <- predictionHeatload_lowPPT <- matrix(NA, length(samp), length(heatload_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highPPT[s,] <- predict_fun(heatload=heatload_seq,ppt=ppt_quant[2])
  predictionHeatload_midPPT[s,] <- predict_fun(heatload=heatload_seq,ppt=ppt_mean)
  predictionHeatload_lowPPT[s,] <- predict_fun(heatload=heatload_seq,ppt=ppt_quant[1])
}

predictionHeatload_highPPT_exp <- exp(predictionHeatload_highPPT)
predictionHeatload_midPPT_exp <- exp(predictionHeatload_midPPT)
predictionHeatload_lowPPT_exp <- exp(predictionHeatload_lowPPT)
ci.Heatload_highPPT <- apply(predictionHeatload_highPPT_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midPPT <- apply(predictionHeatload_midPPT_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowPPT <- apply(predictionHeatload_lowPPT_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highPPT.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highPPT[2,], ci.low = ci.Heatload_highPPT[1,], ci.high = ci.Heatload_highPPT[3,], ci.group = "High PPT")
ci.Heatload_midPPT.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midPPT[2,], ci.low = ci.Heatload_midPPT[1,], ci.high = ci.Heatload_midPPT[3,], ci.group = "Mid PPT")
ci.Heatload_lowPPT.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowPPT[2,], ci.low = ci.Heatload_lowPPT[1,], ci.high = ci.Heatload_lowPPT[3,], ci.group = "Low PPT")
Heatload_PPT <- rbind(ci.Heatload_highPPT.df, ci.Heatload_midPPT.df, ci.Heatload_lowPPT.df)
ggplot(data = Heatload_PPT, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmin
predictionHeatload_highTmin <- predictionHeatload_midTmin <- predictionHeatload_lowTmin <- matrix(NA, length(samp), length(heatload_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmin[s,] <- predict_fun(heatload=heatload_seq,tmin=tmin_quant[2])
  
  predictionHeatload_midTmin[s,] <- predict_fun(heatload=heatload_seq,tmin=tmin_mean)
  
  predictionHeatload_lowTmin[s,] <- predict_fun(heatload=heatload_seq,tmin=tmin_quant[1])
}

predictionHeatload_highTmin_exp <- exp(predictionHeatload_highTmin)
predictionHeatload_midTmin_exp <- exp(predictionHeatload_midTmin)
predictionHeatload_lowTmin_exp <- exp(predictionHeatload_lowTmin)
ci.Heatload_highTmin <- apply(predictionHeatload_highTmin_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmin <- apply(predictionHeatload_midTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmin <- apply(predictionHeatload_lowTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmin.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highTmin[2,], ci.low = ci.Heatload_highTmin[1,], ci.high = ci.Heatload_highTmin[3,], ci.group = "High Tmin")
ci.Heatload_midTmin.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midTmin[2,], ci.low = ci.Heatload_midTmin[1,], ci.high = ci.Heatload_midTmin[3,], ci.group = "Mid Tmin")
ci.Heatload_lowTmin.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowTmin[2,], ci.low = ci.Heatload_lowTmin[1,], ci.high = ci.Heatload_lowTmin[3,], ci.group = "Low Tmin")
Heatload_Tmin <- rbind(ci.Heatload_highTmin.df, ci.Heatload_midTmin.df, ci.Heatload_lowTmin.df)
ggplot(data = Heatload_Tmin, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmax
predictionHeatload_highTmax <- predictionHeatload_midTmax <- predictionHeatload_lowTmax <- matrix(NA, length(samp), length(heatload_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmax[s,] <- predict_fun(heatload=heatload_seq,tmax=tmax_quant[2])
  
  predictionHeatload_midTmax[s,] <- predict_fun(heatload=heatload_seq,tmax=tmax_mean)
  
  predictionHeatload_lowTmax[s,] <- predict_fun(heatload=heatload_seq,tmax=tmax_quant[1])
}

predictionHeatload_highTmax_exp <- exp(predictionHeatload_highTmax)
predictionHeatload_midTmax_exp <- exp(predictionHeatload_midTmax)
predictionHeatload_lowTmax_exp <- exp(predictionHeatload_lowTmax)
ci.Heatload_highTmax <- apply(predictionHeatload_highTmax_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmax <- apply(predictionHeatload_midTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmax <- apply(predictionHeatload_lowTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmax.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highTmax[2,], ci.low = ci.Heatload_highTmax[1,], ci.high = ci.Heatload_highTmax[3,], ci.group = "High Tmax")
ci.Heatload_midTmax.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midTmax[2,], ci.low = ci.Heatload_midTmax[1,], ci.high = ci.Heatload_midTmax[3,], ci.group = "Mid Tmax")
ci.Heatload_lowTmax.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowTmax[2,], ci.low = ci.Heatload_lowTmax[1,], ci.high = ci.Heatload_lowTmax[3,], ci.group = "Low Tmax")
Heatload_Tmax <- rbind(ci.Heatload_highTmax.df, ci.Heatload_midTmax.df, ci.Heatload_lowTmax.df)
ggplot(data = Heatload_Tmax, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and PPT dev
predictionHeatload_highPPTdev <- predictionHeatload_midPPTdev <- predictionHeatload_lowPPTdev <- matrix(NA, length(samp), length(heatload_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highPPTdev[s,] <- predict_fun(heatload=heatload_seq,ppt_dev=ppt_dev_quant[2])
  
  predictionHeatload_midPPTdev[s,] <- predict_fun(heatload=heatload_seq,ppt_dev=ppt_dev_mean)
  
  predictionHeatload_lowPPTdev[s,] <- predict_fun(heatload=heatload_seq,ppt_dev=ppt_dev_quant[1])
}
predictionHeatload_highPPTdev_exp <- exp(predictionHeatload_highPPTdev)
predictionHeatload_midPPTdev_exp <- exp(predictionHeatload_midPPTdev)
predictionHeatload_lowPPTdev_exp <- exp(predictionHeatload_lowPPTdev)
ci.Heatload_highPPTdev <- apply(predictionHeatload_highPPTdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midPPTdev <- apply(predictionHeatload_midPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowPPTdev <- apply(predictionHeatload_lowPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highPPTdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highPPTdev[2,], ci.low = ci.Heatload_highPPTdev[1,], ci.high = ci.Heatload_highPPTdev[3,], ci.group = "High PPTdev")
ci.Heatload_midPPTdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midPPTdev[2,], ci.low = ci.Heatload_midPPTdev[1,], ci.high = ci.Heatload_midPPTdev[3,], ci.group = "Mid PPTdev")
ci.Heatload_lowPPTdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowPPTdev[2,], ci.low = ci.Heatload_lowPPTdev[1,], ci.high = ci.Heatload_lowPPTdev[3,], ci.group = "Low PPTdev")
Heatload_PPTdev <- rbind(ci.Heatload_highPPTdev.df, ci.Heatload_midPPTdev.df, ci.Heatload_lowPPTdev.df)
ggplot(data = Heatload_PPTdev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmin dev
predictionHeatload_highTmindev <- predictionHeatload_midTmindev <- predictionHeatload_lowTmindev <- matrix(NA, length(samp), length(heatload_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmindev[s,] <- predict_fun(heatload=heatload_seq,tmin_dev=tmin_dev_quant[2])
  
  predictionHeatload_midTmindev[s,] <- predict_fun(heatload=heatload_seq,tmin_dev=tmin_dev_mean)
  
  predictionHeatload_lowTmindev[s,] <- predict_fun(heatload=heatload_seq,tmin_dev=tmin_dev_quant[1])
}

predictionHeatload_highTmindev_exp <- exp(predictionHeatload_highTmindev)
predictionHeatload_midTmindev_exp <- exp(predictionHeatload_midTmindev)
predictionHeatload_lowTmindev_exp <- exp(predictionHeatload_lowTmindev)
ci.Heatload_highTmindev <- apply(predictionHeatload_highTmindev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmindev <- apply(predictionHeatload_midTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmindev <- apply(predictionHeatload_lowTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmindev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highTmindev[2,], ci.low = ci.Heatload_highTmindev[1,], ci.high = ci.Heatload_highTmindev[3,], ci.group = "High Tmindev")
ci.Heatload_midTmindev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midTmindev[2,], ci.low = ci.Heatload_midTmindev[1,], ci.high = ci.Heatload_midTmindev[3,], ci.group = "Mid Tmindev")
ci.Heatload_lowTmindev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowTmindev[2,], ci.low = ci.Heatload_lowTmindev[1,], ci.high = ci.Heatload_lowTmindev[3,], ci.group = "Low Tmindev")
Heatload_Tmindev <- rbind(ci.Heatload_highTmindev.df, ci.Heatload_midTmindev.df, ci.Heatload_lowTmindev.df)
ggplot(data = Heatload_Tmindev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmax dev
predictionHeatload_highTmaxdev <- predictionHeatload_midTmaxdev <- predictionHeatload_lowTmaxdev <- matrix(NA, length(samp), length(heatload_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmaxdev[s,] <- predict_fun(heatload=heatload_seq,tmax_dev=tmax_dev_quant[2])
  
  predictionHeatload_midTmaxdev[s,] <- predict_fun(heatload=heatload_seq,tmax_dev=tmax_dev_mean)
  
  predictionHeatload_lowTmaxdev[s,] <- predict_fun(heatload=heatload_seq,tmax_dev=tmax_dev_quant[1])
}

predictionHeatload_highTmaxdev_exp <- exp(predictionHeatload_highTmaxdev)
predictionHeatload_midTmaxdev_exp <- exp(predictionHeatload_midTmaxdev)
predictionHeatload_lowTmaxdev_exp <- exp(predictionHeatload_lowTmaxdev)
ci.Heatload_highTmaxdev <- apply(predictionHeatload_highTmaxdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmaxdev <- apply(predictionHeatload_midTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmaxdev <- apply(predictionHeatload_lowTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmaxdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highTmaxdev[2,], ci.low = ci.Heatload_highTmaxdev[1,], ci.high = ci.Heatload_highTmaxdev[3,], ci.group = "High Tmaxdev")
ci.Heatload_midTmaxdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midTmaxdev[2,], ci.low = ci.Heatload_midTmaxdev[1,], ci.high = ci.Heatload_midTmaxdev[3,], ci.group = "Mid Tmaxdev")
ci.Heatload_lowTmaxdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowTmaxdev[2,], ci.low = ci.Heatload_lowTmaxdev[1,], ci.high = ci.Heatload_lowTmaxdev[3,], ci.group = "Low Tmaxdev")
Heatload_Tmaxdev <- rbind(ci.Heatload_highTmaxdev.df, ci.Heatload_midTmaxdev.df, ci.Heatload_lowTmaxdev.df)
ggplot(data = Heatload_Tmaxdev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#PPT and Tmin
predictionPPT_highTmin <- predictionPPT_midTmin <- predictionPPT_lowTmin <- matrix(NA, length(samp), length(ppt_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highTmin[s,] <- predict_fun(ppt=ppt_seq,tmin=tmin_quant[2])
  
  predictionPPT_midTmin[s,] <- predict_fun(ppt=ppt_seq,tmin=tmin_mean)
  
  predictionPPT_lowTmin[s,] <- predict_fun(ppt=ppt_seq,tmin=tmin_quant[1])
}

predictionPPT_highTmin_exp <- exp(predictionPPT_highTmin)
predictionPPT_midTmin_exp <- exp(predictionPPT_midTmin)
predictionPPT_lowTmin_exp <- exp(predictionPPT_lowTmin)
ci.PPT_highTmin <- apply(predictionPPT_highTmin_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midTmin <- apply(predictionPPT_midTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowTmin <- apply(predictionPPT_lowTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highTmin.df <- data.frame(ppt = ppt_seq, median = ci.PPT_highTmin[2,], ci.low = ci.PPT_highTmin[1,], ci.high = ci.PPT_highTmin[3,], ci.group = "High Tmin")
ci.PPT_midTmin.df <- data.frame(ppt = ppt_seq, median = ci.PPT_midTmin[2,], ci.low = ci.PPT_midTmin[1,], ci.high = ci.PPT_midTmin[3,], ci.group = "Mid Tmin")
ci.PPT_lowTmin.df <- data.frame(ppt = ppt_seq, median = ci.PPT_lowTmin[2,], ci.low = ci.PPT_lowTmin[1,], ci.high = ci.PPT_lowTmin[3,], ci.group = "Low Tmin")
PPT_Tmin <- rbind(ci.PPT_highTmin.df, ci.PPT_midTmin.df, ci.PPT_lowTmin.df)
ggplot(data = PPT_Tmin, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#PPT and Tmax
predictionPPT_highTmax <- predictionPPT_midTmax <- predictionPPT_lowTmax <- matrix(NA, length(samp), length(ppt_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highTmax[s,] <- predict_fun(ppt=ppt_seq,tmax=tmax_quant[2])
  
  predictionPPT_midTmax[s,] <- predict_fun(ppt=ppt_seq,tmax=tmax_mean)
  
  predictionPPT_lowTmax[s,] <- predict_fun(ppt=ppt_seq,tmax=tmax_quant[1])
}

predictionPPT_highTmax_exp <- exp(predictionPPT_highTmax)
predictionPPT_midTmax_exp <- exp(predictionPPT_midTmax)
predictionPPT_lowTmax_exp <- exp(predictionPPT_lowTmax)
ci.PPT_highTmax <- apply(predictionPPT_highTmax_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midTmax <- apply(predictionPPT_midTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowTmax <- apply(predictionPPT_lowTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highTmax.df <- data.frame(ppt = ppt_seq, median = ci.PPT_highTmax[2,], ci.low = ci.PPT_highTmax[1,], ci.high = ci.PPT_highTmax[3,], ci.group = "High Tmax")
ci.PPT_midTmax.df <- data.frame(ppt = ppt_seq, median = ci.PPT_midTmax[2,], ci.low = ci.PPT_midTmax[1,], ci.high = ci.PPT_midTmax[3,], ci.group = "Mid Tmax")
ci.PPT_lowTmax.df <- data.frame(ppt = ppt_seq, median = ci.PPT_lowTmax[2,], ci.low = ci.PPT_lowTmax[1,], ci.high = ci.PPT_lowTmax[3,], ci.group = "Low Tmax")
PPT_Tmax <- rbind(ci.PPT_highTmax.df, ci.PPT_midTmax.df, ci.PPT_lowTmax.df)
ggplot(data = PPT_Tmax, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmin and Tmax
predictionTmin_highTmax <- predictionTmin_midTmax <- predictionTmin_lowTmax <- matrix(NA, length(samp), length(tmin_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmin_highTmax[s,] <- predict_fun(tmin=tmin_seq,tmax=tmax_quant[2])
  
  predictionTmin_midTmax[s,] <- predict_fun(tmin=tmin_seq,tmax=tmax_mean)
  
  predictionTmin_lowTmax[s,] <- predict_fun(tmin=tmin_seq,tmax=tmax_quant[1])
}

predictionTmin_highTmax_exp <- exp(predictionTmin_highTmax)
predictionTmin_midTmax_exp <- exp(predictionTmin_midTmax)
predictionTmin_lowTmax_exp <- exp(predictionTmin_lowTmax)
ci.Tmin_highTmax <- apply(predictionTmin_highTmax_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmin_midTmax <- apply(predictionTmin_midTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_lowTmax <- apply(predictionTmin_lowTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_highTmax.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_highTmax[2,], ci.low = ci.Tmin_highTmax[1,], ci.high = ci.Tmin_highTmax[3,], ci.group = "High Tmax")
ci.Tmin_midTmax.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_midTmax[2,], ci.low = ci.Tmin_midTmax[1,], ci.high = ci.Tmin_midTmax[3,], ci.group = "Mid Tmax")
ci.Tmin_lowTmax.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_lowTmax[2,], ci.low = ci.Tmin_lowTmax[1,], ci.high = ci.Tmin_lowTmax[3,], ci.group = "Low Tmax")
Tmin_Tmax <- rbind(ci.Tmin_highTmax.df, ci.Tmin_midTmax.df, ci.Tmin_lowTmax.df)
ggplot(data = Tmin_Tmax, aes(x = tmin, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmin, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#PPT and PPT dev
predictionPPT_highPPTdev <- predictionPPT_midPPTdev <- predictionPPT_lowPPTdev <- matrix(NA, length(samp), length(ppt_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highPPTdev[s,] <- predict_fun(ppt=ppt_seq,ppt_dev=ppt_dev_quant[2])
  
  predictionPPT_midPPTdev[s,] <- predict_fun(ppt=ppt_seq,ppt_dev=ppt_dev_mean)
  
  predictionPPT_lowPPTdev[s,] <- predict_fun(ppt=ppt_seq,ppt_dev=ppt_dev_quant[1])
}

predictionPPT_highPPTdev_exp <- exp(predictionPPT_highPPTdev)
predictionPPT_midPPTdev_exp <- exp(predictionPPT_midPPTdev)
predictionPPT_lowPPTdev_exp <- exp(predictionPPT_lowPPTdev)
ci.PPT_highPPTdev <- apply(predictionPPT_highPPTdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midPPTdev <- apply(predictionPPT_midPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowPPTdev <- apply(predictionPPT_lowPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highPPTdev.df <- data.frame(ppt = ppt_seq, median = ci.PPT_highPPTdev[2,], ci.low = ci.PPT_highPPTdev[1,], ci.high = ci.PPT_highPPTdev[3,], ci.group = "High PPTdev")
ci.PPT_midPPTdev.df <- data.frame(ppt = ppt_seq, median = ci.PPT_midPPTdev[2,], ci.low = ci.PPT_midPPTdev[1,], ci.high = ci.PPT_midPPTdev[3,], ci.group = "Mid PPTdev")
ci.PPT_lowPPTdev.df <- data.frame(ppt = ppt_seq, median = ci.PPT_lowPPTdev[2,], ci.low = ci.PPT_lowPPTdev[1,], ci.high = ci.PPT_lowPPTdev[3,], ci.group = "Low PPTdev")
PPT_PPTdev <- rbind(ci.PPT_highPPTdev.df, ci.PPT_midPPTdev.df, ci.PPT_lowPPTdev.df)
ggplot(data = PPT_PPTdev, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmin and Tmin dev
predictionTmin_highTmindev <- predictionTmin_midTmindev <- predictionTmin_lowTmindev <- matrix(NA, length(samp), length(tmin_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmin_highTmindev[s,] <- predict_fun(tmin=tmin_seq,tmin_dev=tmin_dev_quant[2])
  
  predictionTmin_midTmindev[s,] <- predict_fun(tmin=tmin_seq,tmin_dev=tmin_dev_mean)
  
  predictionTmin_lowTmindev[s,] <- predict_fun(tmin=tmin_seq,tmin_dev=tmin_dev_quant[1])
}

predictionTmin_highTmindev_exp <- exp(predictionTmin_highTmindev)
predictionTmin_midTmindev_exp <- exp(predictionTmin_midTmindev)
predictionTmin_lowTmindev_exp <- exp(predictionTmin_lowTmindev)
ci.Tmin_highTmindev <- apply(predictionTmin_highTmindev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmin_midTmindev <- apply(predictionTmin_midTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_lowTmindev <- apply(predictionTmin_lowTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_highTmindev.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_highTmindev[2,], ci.low = ci.Tmin_highTmindev[1,], ci.high = ci.Tmin_highTmindev[3,], ci.group = "High Tmindev")
ci.Tmin_midTmindev.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_midTmindev[2,], ci.low = ci.Tmin_midTmindev[1,], ci.high = ci.Tmin_midTmindev[3,], ci.group = "Mid Tmindev")
ci.Tmin_lowTmindev.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_lowTmindev[2,], ci.low = ci.Tmin_lowTmindev[1,], ci.high = ci.Tmin_lowTmindev[3,], ci.group = "Low Tmindev")
Tmin_Tmindev <- rbind(ci.Tmin_highTmindev.df, ci.Tmin_midTmindev.df, ci.Tmin_lowTmindev.df)
ggplot(data = Tmin_Tmindev, aes(x = tmin, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmin, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmax and Tmax dev
predictionTmax_highTmaxdev <- predictionTmax_midTmaxdev <- predictionTmax_lowTmaxdev <- matrix(NA, length(samp), length(tmax_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmax_highTmaxdev[s,] <- predict_fun(tmax=tmax_seq,tmax_dev=tmax_dev_quant[2])
  
  predictionTmax_midTmaxdev[s,] <- predict_fun(tmax=tmax_seq,tmax_dev=tmax_dev_mean)
  
  predictionTmax_lowTmaxdev[s,] <- predict_fun(tmax=tmax_seq,tmax_dev=tmax_dev_quant[1])
}

predictionTmax_highTmaxdev_exp <- exp(predictionTmax_highTmaxdev)
predictionTmax_midTmaxdev_exp <- exp(predictionTmax_midTmaxdev)
predictionTmax_lowTmaxdev_exp <- exp(predictionTmax_lowTmaxdev)
ci.Tmax_highTmaxdev <- apply(predictionTmax_highTmaxdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmax_midTmaxdev <- apply(predictionTmax_midTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_lowTmaxdev <- apply(predictionTmax_lowTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_highTmaxdev.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_highTmaxdev[2,], ci.low = ci.Tmax_highTmaxdev[1,], ci.high = ci.Tmax_highTmaxdev[3,], ci.group = "High Tmaxdev")
ci.Tmax_midTmaxdev.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_midTmaxdev[2,], ci.low = ci.Tmax_midTmaxdev[1,], ci.high = ci.Tmax_midTmaxdev[3,], ci.group = "Mid Tmaxdev")
ci.Tmax_lowTmaxdev.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_lowTmaxdev[2,], ci.low = ci.Tmax_lowTmaxdev[1,], ci.high = ci.Tmax_lowTmaxdev[3,], ci.group = "Low Tmaxdev")
Tmax_Tmaxdev <- rbind(ci.Tmax_highTmaxdev.df, ci.Tmax_midTmaxdev.df, ci.Tmax_lowTmaxdev.df)
ggplot(data = Tmax_Tmaxdev, aes(x = tmax, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmax, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 


#Heatload and density
predictionHeatload_highDens <- predictionHeatload_midDens <- predictionHeatload_lowDens <- matrix(NA, length(samp), length(heatload_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highDens[s,] <- predict_fun(heatload=heatload_seq,dens=dens_quant[2])
  
  predictionHeatload_midDens[s,] <- predict_fun(heatload=heatload_seq,dens=dens_mean)
  
  predictionHeatload_lowDens[s,] <- predict_fun(heatload=heatload_seq,dens=dens_quant[1])
}
predictionHeatload_highDens_exp <- exp(predictionHeatload_highDens)
predictionHeatload_midDens_exp <- exp(predictionHeatload_midDens)
predictionHeatload_lowDens_exp <- exp(predictionHeatload_lowDens)
ci.Heatload_highDens <- apply(predictionHeatload_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midDens <- apply(predictionHeatload_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowDens <- apply(predictionHeatload_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highDens.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highDens[2,], ci.low = ci.Heatload_highDens[1,], ci.high = ci.Heatload_highDens[3,], ci.group = "High Dens")
ci.Heatload_midDens.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midDens[2,], ci.low = ci.Heatload_midDens[1,], ci.high = ci.Heatload_midDens[3,], ci.group = "Mid Dens")
ci.Heatload_lowDens.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowDens[2,], ci.low = ci.Heatload_lowDens[1,], ci.high = ci.Heatload_lowDens[3,], ci.group = "Low Dens")
Heatload_Dens <- rbind(ci.Heatload_highDens.df, ci.Heatload_midDens.df, ci.Heatload_lowDens.df)
ggplot(data = Heatload_Dens, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#PPT and density
predictionPPT_highDens <- predictionPPT_midDens <- predictionPPT_lowDens <- matrix(NA, length(samp), length(ppt_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highDens[s,] <- predict_fun(ppt=ppt_seq,dens=dens_quant[2])
  
  predictionPPT_midDens[s,] <- predict_fun(ppt=ppt_seq,dens=dens_mean)
  
  predictionPPT_lowDens[s,] <- predict_fun(ppt=ppt_seq,dens=dens_quant[1])
}
predictionPPT_highDens_exp <- exp(predictionPPT_highDens)
predictionPPT_midDens_exp <- exp(predictionPPT_midDens)
predictionPPT_lowDens_exp <- exp(predictionPPT_lowDens)
ci.PPT_highDens <- apply(predictionPPT_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midDens <- apply(predictionPPT_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowDens <- apply(predictionPPT_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highDens.df <- data.frame(ppt = ppt_seq, median = ci.PPT_highDens[2,], ci.low = ci.PPT_highDens[1,], ci.high = ci.PPT_highDens[3,], ci.group = "High Dens")
ci.PPT_midDens.df <- data.frame(ppt = ppt_seq, median = ci.PPT_midDens[2,], ci.low = ci.PPT_midDens[1,], ci.high = ci.PPT_midDens[3,], ci.group = "Mid Dens")
ci.PPT_lowDens.df <- data.frame(ppt = ppt_seq, median = ci.PPT_lowDens[2,], ci.low = ci.PPT_lowDens[1,], ci.high = ci.PPT_lowDens[3,], ci.group = "Low Dens")
PPT_Dens <- rbind(ci.PPT_highDens.df, ci.PPT_midDens.df, ci.PPT_lowDens.df)
ggplot(data = PPT_Dens, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmin and density
predictionTmin_highDens <- predictionTmin_midDens <- predictionTmin_lowDens <- matrix(NA, length(samp), length(tmin_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmin_highDens[s,] <- predict_fun(tmin=tmin_seq,dens=dens_quant[2])
  
  predictionTmin_midDens[s,] <- predict_fun(tmin=tmin_seq,dens=dens_mean)
  
  predictionTmin_lowDens[s,] <- predict_fun(tmin=tmin_seq,dens=dens_quant[1])
}
predictionTmin_highDens_exp <- exp(predictionTmin_highDens)
predictionTmin_midDens_exp <- exp(predictionTmin_midDens)
predictionTmin_lowDens_exp <- exp(predictionTmin_lowDens)
ci.Tmin_highDens <- apply(predictionTmin_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmin_midDens <- apply(predictionTmin_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_lowDens <- apply(predictionTmin_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_highDens.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_highDens[2,], ci.low = ci.Tmin_highDens[1,], ci.high = ci.Tmin_highDens[3,], ci.group = "High Dens")
ci.Tmin_midDens.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_midDens[2,], ci.low = ci.Tmin_midDens[1,], ci.high = ci.Tmin_midDens[3,], ci.group = "Mid Dens")
ci.Tmin_lowDens.df <- data.frame(tmin = tmin_seq, median = ci.Tmin_lowDens[2,], ci.low = ci.Tmin_lowDens[1,], ci.high = ci.Tmin_lowDens[3,], ci.group = "Low Dens")
Tmin_Dens <- rbind(ci.Tmin_highDens.df, ci.Tmin_midDens.df, ci.Tmin_lowDens.df)
ggplot(data = Tmin_Dens, aes(x = tmin, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmin, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmax and density
predictionTmax_highDens <- predictionTmax_midDens <- predictionTmax_lowDens <- matrix(NA, length(samp), length(tmax_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmax_highDens[s,] <- predict_fun(tmax=tmax_seq,dens=dens_quant[2])
  
  predictionTmax_midDens[s,] <- predict_fun(tmax=tmax_seq,dens=dens_mean)
  
  predictionTmax_lowDens[s,] <- predict_fun(tmax=tmax_seq,dens=dens_quant[1])
}
predictionTmax_highDens_exp <- exp(predictionTmax_highDens)
predictionTmax_midDens_exp <- exp(predictionTmax_midDens)
predictionTmax_lowDens_exp <- exp(predictionTmax_lowDens)
ci.Tmax_highDens <- apply(predictionTmax_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmax_midDens <- apply(predictionTmax_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_lowDens <- apply(predictionTmax_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_highDens.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_highDens[2,], ci.low = ci.Tmax_highDens[1,], ci.high = ci.Tmax_highDens[3,], ci.group = "High Dens")
ci.Tmax_midDens.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_midDens[2,], ci.low = ci.Tmax_midDens[1,], ci.high = ci.Tmax_midDens[3,], ci.group = "Mid Dens")
ci.Tmax_lowDens.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_lowDens[2,], ci.low = ci.Tmax_lowDens[1,], ci.high = ci.Tmax_lowDens[3,], ci.group = "Low Dens")
Tmax_Dens <- rbind(ci.Tmax_highDens.df, ci.Tmax_midDens.df, ci.Tmax_lowDens.df)
ggplot(data = Tmax_Dens, aes(x = tmax, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmax, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 


#PPT dev and density
predictionPPTdev_highDens <- predictionPPTdev_midDens <- predictionPPTdev_lowDens <- matrix(NA, length(samp), length(ppt_dev_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPTdev_highDens[s,] <- predict_fun(ppt_dev=ppt_dev_seq,dens=dens_quant[2])
  
  predictionPPTdev_midDens[s,] <- predict_fun(ppt_dev=ppt_dev_seq,dens=dens_mean)
  
  predictionPPTdev_lowDens[s,] <- predict_fun(ppt_dev=ppt_dev_seq,dens=dens_quant[1])
}
predictionPPTdev_highDens_exp <- exp(predictionPPTdev_highDens)
predictionPPTdev_midDens_exp <- exp(predictionPPTdev_midDens)
predictionPPTdev_lowDens_exp <- exp(predictionPPTdev_lowDens)
ci.PPTdev_highDens <- apply(predictionPPTdev_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPTdev_midDens <- apply(predictionPPTdev_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPTdev_lowDens <- apply(predictionPPTdev_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPTdev_highDens.df <- data.frame(ppt_dev = ppt_dev_seq, median = ci.PPTdev_highDens[2,], ci.low = ci.PPTdev_highDens[1,], ci.high = ci.PPTdev_highDens[3,], ci.group = "High Dens")
ci.PPTdev_midDens.df <- data.frame(ppt_dev = ppt_dev_seq, median = ci.PPTdev_midDens[2,], ci.low = ci.PPTdev_midDens[1,], ci.high = ci.PPTdev_midDens[3,], ci.group = "Mid Dens")
ci.PPTdev_lowDens.df <- data.frame(ppt_dev = ppt_dev_seq, median = ci.PPTdev_lowDens[2,], ci.low = ci.PPTdev_lowDens[1,], ci.high = ci.PPTdev_lowDens[3,], ci.group = "Low Dens")
PPTdev_Dens <- rbind(ci.PPTdev_highDens.df, ci.PPTdev_midDens.df, ci.PPTdev_lowDens.df)
ggplot(data = PPTdev_Dens, aes(x = ppt_dev, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt_dev, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmin dev and density
predictionTmindev_highDens <- predictionTmindev_midDens <- predictionTmindev_lowDens <- matrix(NA, length(samp), length(tmin_dev_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmindev_highDens[s,] <- predict_fun(tmin_dev=tmin_dev_seq,dens=dens_quant[2])
  
  predictionTmindev_midDens[s,] <- predict_fun(tmin_dev=tmin_dev_seq,dens=dens_mean)
  
  predictionTmindev_lowDens[s,] <- predict_fun(tmin_dev=tmin_dev_seq,dens=dens_quant[1])
}
predictionTmindev_highDens_exp <- exp(predictionTmindev_highDens)
predictionTmindev_midDens_exp <- exp(predictionTmindev_midDens)
predictionTmindev_lowDens_exp <- exp(predictionTmindev_lowDens)
ci.Tmindev_highDens <- apply(predictionTmindev_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmindev_midDens <- apply(predictionTmindev_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmindev_lowDens <- apply(predictionTmindev_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmindev_highDens.df <- data.frame(tmin_dev_seq = tmin_dev, median = ci.Tmindev_highDens[2,], ci.low = ci.Tmindev_highDens[1,], ci.high = ci.Tmindev_highDens[3,], ci.group = "High Dens")
ci.Tmindev_midDens.df <- data.frame(tmin_dev_seq = tmin_dev, median = ci.Tmindev_midDens[2,], ci.low = ci.Tmindev_midDens[1,], ci.high = ci.Tmindev_midDens[3,], ci.group = "Mid Dens")
ci.Tmindev_lowDens.df <- data.frame(tmin_dev_seq = tmin_dev, median = ci.Tmindev_lowDens[2,], ci.low = ci.Tmindev_lowDens[1,], ci.high = ci.Tmindev_lowDens[3,], ci.group = "Low Dens")
Tmindev_Dens <- rbind(ci.Tmindev_highDens.df, ci.Tmindev_midDens.df, ci.Tmindev_lowDens.df)
ggplot(data = Tmindev_Dens, aes(x = tmin_dev, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmin_dev, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmax dev and density
predictionTmaxdev_highDens <- predictionTmaxdev_midDens <- predictionTmaxdev_lowDens <- matrix(NA, length(samp), length(tmax_dev_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmaxdev_highDens[s,] <- predict_fun(tmax_dev=tmax_dev_seq,dens=dens_quant[2])
  
  predictionTmaxdev_midDens[s,] <- predict_fun(tmax_dev=tmax_dev_seq,dens=dens_mean)
  
  predictionTmaxdev_lowDens[s,] <- predict_fun(tmax_dev=tmax_dev_seq,dens=dens_quant[1])
}
predictionTmaxdev_highDens_exp <- exp(predictionTmaxdev_highDens)
predictionTmaxdev_midDens_exp <- exp(predictionTmaxdev_midDens)
predictionTmaxdev_lowDens_exp <- exp(predictionTmaxdev_lowDens)
ci.Tmaxdev_highDens <- apply(predictionTmaxdev_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmaxdev_midDens <- apply(predictionTmaxdev_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmaxdev_lowDens <- apply(predictionTmaxdev_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmaxdev_highDens.df <- data.frame(tmax_dev = tmax_dev_seq, median = ci.Tmaxdev_highDens[2,], ci.low = ci.Tmaxdev_highDens[1,], ci.high = ci.Tmaxdev_highDens[3,], ci.group = "High Dens")
ci.Tmaxdev_midDens.df <- data.frame(tmax_dev = tmax_dev_seq, median = ci.Tmaxdev_midDens[2,], ci.low = ci.Tmaxdev_midDens[1,], ci.high = ci.Tmaxdev_midDens[3,], ci.group = "Mid Dens")
ci.Tmaxdev_lowDens.df <- data.frame(tmax_dev = tmax_dev_seq, median = ci.Tmaxdev_lowDens[2,], ci.low = ci.Tmaxdev_lowDens[1,], ci.high = ci.Tmaxdev_lowDens[3,], ci.group = "Low Dens")
Tmaxdev_Dens <- rbind(ci.Tmaxdev_highDens.df, ci.Tmaxdev_midDens.df, ci.Tmaxdev_lowDens.df)
ggplot(data = Tmaxdev_Dens, aes(x = tmax_dev, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmax_dev, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 



#INDIVIDUAL RESPONSE PLOTS
grow.monsoon$LONbin <- ifelse(grow.monsoon$LON > -109, "-109 to -104", "-114 to -109")
grow.monsoon$LATbin <- ifelse(grow.monsoon$LAT > 37, "37 to 41", "32 to 37")
grow.monsoon$LATLONbin <- paste(grow.monsoon$LONbin, grow.monsoon$LATbin)
ind.samples <- unique(grow.monsoon[,c("LATLONbin", "treeCD")]) %>% group_by(LATLONbin)

get.ind.tmp.response<- function(j){
  tree.subset <- ind.samples[j,]
  tree.grow <- grow.monsoon %>% filter(LATLONbin == tree.subset$LATLONbin & treeCD == tree.subset$treeCD)
  
  Tmean_AprMayJunrng <- range(tree.grow$Tmean_AprMayJun,na.rm = TRUE) #setting range for tmp_normrng
  Tmean_AprMayJun <- seq(Tmean_AprMayJunrng[1], Tmean_AprMayJunrng[2], by = 0.1)
  x <- mean(tree.grow$DIA_prev)
  ppt_norm <- mean(tree.grow$ppt_norm)
  tmp_norm <- mean(tree.grow$tmp_norm)
  Tmean_SepOct <- mean(tree.grow$Tmean_SepOct)
  Precip_JulAug <- mean(tree.grow$Precip_JulAug)
  Precip_NovDecJanFebMar <- mean(tree.grow$Precip_NovDecJanFebMar)
  tmp_norm_range <- quantile(tree.grow$tmp_norm, c(0.2, 0.8))
  growthpredictionTmeanAprMayJun_tnorm <- matrix(NA, length(plotdatainterval$u_beta_Tmean_AprMayJun), length(Tmean_AprMayJun)) 
  
  for(i in 1:length(plotdatainterval$u_beta_Tmean_AprMayJun)){
    growthpredictionTmeanAprMayJun_tnorm[i,] <- plotdatainterval[i,"u_beta_ppt_norm"]*ppt_norm + plotdatainterval[i,"u_beta_tmp_norm"]*tmp_norm +
      plotdatainterval[i,"u_beta_Precip_JulAug"]*Precip_JulAug + plotdatainterval[i,"u_beta_Precip_NovDecJanFebMar"]*Precip_NovDecJanFebMar +
      plotdatainterval[i,"u_beta_Tmean_AprMayJun"]*Tmean_AprMayJun + plotdatainterval[i,"u_beta_Tmean_SepOct"]*Tmean_SepOct +
      plotdatainterval[i,"u_beta_DIA_prev"]*x + plotdatainterval[i,"u_beta_ppt_norm_tmp_norm"]*ppt_norm*tmp_norm +
      plotdatainterval[i,"u_beta_ppt_norm_Precip_JulAug"]*ppt_norm*Precip_JulAug + plotdatainterval[i,"u_beta_ppt_norm_DIA_prev"]*ppt_norm*x +
      plotdatainterval[i,"u_beta_ppt_norm_Precip_NovDecJanFebMar"]*ppt_norm*Precip_NovDecJanFebMar + 
      plotdatainterval[i,"u_beta_ppt_norm_Tmean_AprMayJun"]*ppt_norm*Tmean_AprMayJun +  plotdatainterval[i,"u_beta_ppt_norm_Tmean_SepOct"]*ppt_norm*Tmean_SepOct +
      plotdatainterval[i,"u_beta_tmp_norm_DIA_prev"]*tmp_norm*x + plotdatainterval[i,"u_beta_tmp_norm_Precip_JulAug"]*tmp_norm*Precip_JulAug +
      plotdatainterval[i,"u_beta_tmp_norm_Precip_NovDecJanFebMar"]*tmp_norm*Precip_NovDecJanFebMar + 
      plotdatainterval[i,"u_beta_tmp_norm_Tmean_AprMayJun"]*tmp_norm*Tmean_AprMayJun + plotdatainterval[i,"u_beta_tmp_norm_Tmean_SepOct"]*tmp_norm*Tmean_SepOct + 
      plotdatainterval[i,"u_beta_DIA_prev_Precip_JulAug"]*x*Precip_JulAug + plotdatainterval[i,"u_beta_DIA_prev_Precip_NovDecJanFebMar"]*x*Precip_NovDecJanFebMar + 
      plotdatainterval[i,"u_beta_DIA_prev_Tmean_AprMayJun"]*x*Tmean_AprMayJun + plotdatainterval[i,"u_beta_DIA_prev_Tmean_SepOct"]*x*Tmean_SepOct + 
      plotdatainterval[i,"u_beta_Precip_JulAug_Precip_NovDecJanFebMar"]*Precip_JulAug*Precip_NovDecJanFebMar +
      plotdatainterval[i,"u_beta_Precip_JulAug_Tmean_AprMayJun"]*Precip_JulAug*Tmean_AprMayJun + 
      plotdatainterval[i,"u_beta_Precip_JulAug_Tmean_SepOct"]*Precip_JulAug*Tmean_SepOct + 
      plotdatainterval[i,"u_beta_Precip_NovDecJanFebMar_Tmean_AprMayJun"]*Precip_NovDecJanFebMar*Tmean_AprMayJun +  
      plotdatainterval[i,"u_beta_Precip_NovDecJanFebMar_Tmean_SepOct"]*Precip_NovDecJanFebMar*Tmean_SepOct + 
      plotdatainterval[i,"u_beta_Tmean_AprMayJun_Tmean_SepOct"]*Tmean_AprMayJun*Tmean_SepOct
  }
  Tmean_AprMayJun_prediction_trtnorm <- exp(growthpredictionTmeanAprMayJun_tnorm)
  ci.Tmean_AprMayJuntnorm <- apply(Tmean_AprMayJun_prediction_trtnorm, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Tmean_AprMayJuntnorm.df <- data.frame(Tmean_AprMayJun = Tmean_AprMayJun, tmp_norm = tmp_norm, median = ci.Tmean_AprMayJuntnorm[2,], ci.low = ci.Tmean_AprMayJuntnorm[1,], ci.high = ci.Tmean_AprMayJuntnorm[3,], ci.group = tree.subset$treeCD)
  Tmean_AprMayJun_tnormint <- rbind(ci.Tmean_AprMayJuntnorm.df)
  print(ind.samples[j,])
  Tmean_AprMayJun_tnormint  
}
#get.ind.tmp.response(i = 6)
Tmean_AprMayJun_tree_response <- list()
Tmean_AprMayJun_tree_response <- lapply(1:length(ind.samples$treeCD), FUN = get.ind.tmp.response)
Tmean_AprMayJun_tree_response.df <- do.call(rbind, Tmean_AprMayJun_tree_response)
merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)
#color by group
ggplot(data = merged.response.samples, aes(x = Tmean_AprMayJun, y = median, color = ci.group)) + geom_ribbon(aes(x = Tmean_AprMayJun, ymin = ci.low, ymax = ci.high, fill = ci.group),color = NA, alpha = 0.5) + 
  geom_line() + mytheme + ylab("Predicted Growth") + ylim(0, 2)
#color by LATLONbin
ggplot(data = merged.response.samples, aes(x = Tmean_AprMayJun, y = median, color = LATLONbin, group = ci.group)) + geom_ribbon(aes(x = Tmean_AprMayJun, ymin = ci.low, ymax = ci.high, fill = LATLONbin, group = ci.group),color = NA, alpha = 0.5) + 
  geom_line() + mytheme + ylab("Predicted Growth") + ylim(0, 2)
#color by tmp_norm
ggplot(data = merged.response.samples, aes(x = Tmean_AprMayJun, y = median, color = tmp_norm, group = ci.group)) + geom_line(alpha = 0.5) + 
  mytheme + ylab("Predicted Growth") + ylim(0, 2) + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")
#map of LATLONbin
all_states <- map_data("state")
states <- subset(all_states, region %in% c("arizona", "colorado", "utah"))
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-states
mapdata<-data.frame(mapdata)
ggplot() + geom_polygon(data=mapdata, aes(x=long, y=lat, group = group), color ="darkgray", fill = "darkgray")+
  geom_point(data = grow.monsoon, aes(x = LON, y = LAT, color = LATLONbin)) + theme_bw()