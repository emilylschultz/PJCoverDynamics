### Code to create interaction plots for PJ cover predictors (density interaction model)
### Created by: Emily Schultz
### Created on: 6 Jun 2022

library(ggplot2)
library(grid)

load("R:/Shriver_Lab/PJcover/DensityInt.RData")

mytheme<-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"),
               legend.text=element_text(size=11),legend.title=element_text(size=12),
               legend.key = element_rect(fill = "white"),legend.position = "top",
               axis.text=element_text(size=12),
               axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
               axis.line.x = element_line(color="black", size = 0.3),
               axis.line.y = element_line(color="black", size = 0.3))

predict_fun <- function(coef=betaOut_ddint,heatload=heatload_mean,ppt=ppt_mean,tmax=tmax_mean,ppt_dev=ppt_dev_mean,tmax_dev=tmax_dev_mean,dens=dens_mean){
  coef[i,1] + coef[i,2]*heatload + coef[i,3]*ppt + coef[i,4]*tmax + coef[i,5]*ppt_dev + coef[i,6]*tmax_dev + 
    coef[i,7]*ppt*ppt_dev +coef[i,8]*tmax*tmax_dev + 
    coef[i,9]*ppt*ppt + coef[i,10]*tmax*tmax + 
    coef[i,11]*dens + coef[i,12]*dens*heatload + coef[i,13]*dens*ppt + coef[i,14]*dens*tmax
}
#INTERACTION PLOTS
samp <- sample(seq((burnin+1),iter),1000)
meanLatent <- apply(latOut,c(1,2),mean,na.rm=T)

dens_mean <- mean(meanLatent)
heatload_mean <- mean(Xall[,,2])
ppt_mean <- mean(Xall[,,3])
tmax_mean <- mean(Xall[,,4])
ppt_dev_mean <- mean(Xall[,,5])
tmax_dev_mean <- mean(Xall[,,6])

densrng <- range(meanLatent,na.rm = TRUE) #setting range for heatload
dens_seq <- seq(densrng[1], densrng[2], by = 0.01)
dens_quant <- quantile(meanLatent, c(0.2, 0.8))

heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload_seq <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
heatload_quant <- quantile(Xall[,,2], c(0.2, 0.8))

pptrng <- range(Xall[,,3],na.rm = TRUE) #setting range for heatload
ppt_seq <- seq(pptrng[1], pptrng[2], by = 0.01)
ppt_quant <- quantile(Xall[,,3], c(0.2, 0.8))

tmaxrng <- range(Xall[,,4],na.rm = TRUE) #setting range for heatload
tmax_seq <- seq(tmaxrng[1], tmaxrng[2], by = 0.01)
tmax_quant <- quantile(Xall[,,4], c(0.2, 0.8))

ppt_devrng <- range(Xall[,,5],na.rm = TRUE) #setting range for heatload
ppt_dev_seq <- seq(ppt_devrng[1], ppt_devrng[2], by = 0.01)
ppt_dev_quant <- quantile(Xall[,,5], c(0.2, 0.8))

tmax_devrng <- range(Xall[,,6],na.rm = TRUE) #setting range for heatload
tmax_dev_seq <- seq(tmax_devrng[1], tmax_devrng[2], by = 0.01)
tmax_dev_quant <- quantile(Xall[,,6], c(0.2, 0.8))


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
ci.Heatload_highPPT.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highPPT[2,], ci.low = ci.Heatload_highPPT[1,], ci.high = ci.Heatload_highPPT[3,], ci.group = "High")
ci.Heatload_midPPT.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midPPT[2,], ci.low = ci.Heatload_midPPT[1,], ci.high = ci.Heatload_midPPT[3,], ci.group = "Mid")
ci.Heatload_lowPPT.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowPPT[2,], ci.low = ci.Heatload_lowPPT[1,], ci.high = ci.Heatload_lowPPT[3,], ci.group = "Low")
Heatload_PPT <- rbind(ci.Heatload_highPPT.df, ci.Heatload_midPPT.df, ci.Heatload_lowPPT.df)
ggplot(data = Heatload_PPT, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT ") + 
  geom_line(size=1) + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT ") +
  ylab("Predicted percent cover") + xlab("Heatload") +
  mytheme 

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
ci.Heatload_highTmax.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highTmax[2,], ci.low = ci.Heatload_highTmax[1,], ci.high = ci.Heatload_highTmax[3,], ci.group = "High")
ci.Heatload_midTmax.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midTmax[2,], ci.low = ci.Heatload_midTmax[1,], ci.high = ci.Heatload_midTmax[3,], ci.group = "Mid")
ci.Heatload_lowTmax.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowTmax[2,], ci.low = ci.Heatload_lowTmax[1,], ci.high = ci.Heatload_lowTmax[3,], ci.group = "Low")
Heatload_Tmax <- rbind(ci.Heatload_highTmax.df, ci.Heatload_midTmax.df, ci.Heatload_lowTmax.df)
ggplot(data = Heatload_Tmax, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax ") + 
  geom_line(size=1) + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax ") +
  ylab("Predicted percent cover") + xlab("Heatload") +
  mytheme

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
ci.Heatload_highPPTdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highPPTdev[2,], ci.low = ci.Heatload_highPPTdev[1,], ci.high = ci.Heatload_highPPTdev[3,], ci.group = "High")
ci.Heatload_midPPTdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midPPTdev[2,], ci.low = ci.Heatload_midPPTdev[1,], ci.high = ci.Heatload_midPPTdev[3,], ci.group = "Mid")
ci.Heatload_lowPPTdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowPPTdev[2,], ci.low = ci.Heatload_lowPPTdev[1,], ci.high = ci.Heatload_lowPPTdev[3,], ci.group = "Low")
Heatload_PPTdev <- rbind(ci.Heatload_highPPTdev.df, ci.Heatload_midPPTdev.df, ci.Heatload_lowPPTdev.df)
ggplot(data = Heatload_PPTdev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT Deviation ") + 
  geom_line(size=1) + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT Deviation ") +
  ylab("Predicted percent cover") + xlab("Heatload") +
  mytheme

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
ci.Heatload_highTmaxdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_highTmaxdev[2,], ci.low = ci.Heatload_highTmaxdev[1,], ci.high = ci.Heatload_highTmaxdev[3,], ci.group = "High")
ci.Heatload_midTmaxdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_midTmaxdev[2,], ci.low = ci.Heatload_midTmaxdev[1,], ci.high = ci.Heatload_midTmaxdev[3,], ci.group = "Mid")
ci.Heatload_lowTmaxdev.df <- data.frame(heatload = heatload_seq, median = ci.Heatload_lowTmaxdev[2,], ci.low = ci.Heatload_lowTmaxdev[1,], ci.high = ci.Heatload_lowTmaxdev[3,], ci.group = "Low")
Heatload_Tmaxdev <- rbind(ci.Heatload_highTmaxdev.df, ci.Heatload_midTmaxdev.df, ci.Heatload_lowTmaxdev.df)
ggplot(data = Heatload_Tmaxdev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax Deviation ") + 
  geom_line(size=1) + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax Deviation ") +
  ylab("Predicted percent cover") + xlab("Heatload") +
  mytheme

#PPT and PPT dev
predictionPPT_highPPTdev <- predictionPPT_midPPTdev <- predictionPPT_lowPPTdev <- matrix(NA, length(samp), length(ppt_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highPPTdev[s,] <- exp(predict_fun(ppt=ppt_seq,ppt_dev=ppt_dev_quant[2]))-exp(dens_mean)
  
  predictionPPT_midPPTdev[s,] <- exp(predict_fun(ppt=ppt_seq,ppt_dev=ppt_dev_mean))-exp(dens_mean)
  
  predictionPPT_lowPPTdev[s,] <- exp(predict_fun(ppt=ppt_seq,ppt_dev=ppt_dev_quant[1]))-exp(dens_mean)
}

predictionPPT_highPPTdev_exp <- (predictionPPT_highPPTdev)
predictionPPT_midPPTdev_exp <- (predictionPPT_midPPTdev)
predictionPPT_lowPPTdev_exp <- (predictionPPT_lowPPTdev)
ci.PPT_highPPTdev <- apply(predictionPPT_highPPTdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midPPTdev <- apply(predictionPPT_midPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowPPTdev <- apply(predictionPPT_lowPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highPPTdev.df <- data.frame(ppt = ppt_seq, median = ci.PPT_highPPTdev[2,], ci.low = ci.PPT_highPPTdev[1,], ci.high = ci.PPT_highPPTdev[3,], ci.group = "High")
ci.PPT_midPPTdev.df <- data.frame(ppt = ppt_seq, median = ci.PPT_midPPTdev[2,], ci.low = ci.PPT_midPPTdev[1,], ci.high = ci.PPT_midPPTdev[3,], ci.group = "Mid")
ci.PPT_lowPPTdev.df <- data.frame(ppt = ppt_seq, median = ci.PPT_lowPPTdev[2,], ci.low = ci.PPT_lowPPTdev[1,], ci.high = ci.PPT_lowPPTdev[3,], ci.group = "Low")
PPT_PPTdev <- rbind(ci.PPT_highPPTdev.df, ci.PPT_midPPTdev.df, ci.PPT_lowPPTdev.df)
PPT_PPTdev_plot <- ggplot(data = PPT_PPTdev, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT Deviation ") + 
  geom_line(size=1) + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT Deviation ") +
  ylab("Predicted change in tree cover") + xlab("PPT") +
  mytheme + labs(tag="A")


#Tmax and Tmax dev
predictionTmax_highTmaxdev <- predictionTmax_midTmaxdev <- predictionTmax_lowTmaxdev <- matrix(NA, length(samp), length(tmax_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmax_highTmaxdev[s,] <- exp(predict_fun(tmax=tmax_seq,tmax_dev=tmax_dev_quant[2]))-exp(dens_mean)
  
  predictionTmax_midTmaxdev[s,] <- exp(predict_fun(tmax=tmax_seq,tmax_dev=tmax_dev_mean))-exp(dens_mean)
  
  predictionTmax_lowTmaxdev[s,] <- exp(predict_fun(tmax=tmax_seq,tmax_dev=tmax_dev_quant[1]))-exp(dens_mean)
}

predictionTmax_highTmaxdev_exp <- (predictionTmax_highTmaxdev)
predictionTmax_midTmaxdev_exp <- (predictionTmax_midTmaxdev)
predictionTmax_lowTmaxdev_exp <- (predictionTmax_lowTmaxdev)
ci.Tmax_highTmaxdev <- apply(predictionTmax_highTmaxdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmax_midTmaxdev <- apply(predictionTmax_midTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_lowTmaxdev <- apply(predictionTmax_lowTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_highTmaxdev.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_highTmaxdev[2,], ci.low = ci.Tmax_highTmaxdev[1,], ci.high = ci.Tmax_highTmaxdev[3,], ci.group = "High")
ci.Tmax_midTmaxdev.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_midTmaxdev[2,], ci.low = ci.Tmax_midTmaxdev[1,], ci.high = ci.Tmax_midTmaxdev[3,], ci.group = "Mid")
ci.Tmax_lowTmaxdev.df <- data.frame(tmax = tmax_seq, median = ci.Tmax_lowTmaxdev[2,], ci.low = ci.Tmax_lowTmaxdev[1,], ci.high = ci.Tmax_lowTmaxdev[3,], ci.group = "Low")
Tmax_Tmaxdev <- rbind(ci.Tmax_highTmaxdev.df, ci.Tmax_midTmaxdev.df, ci.Tmax_lowTmaxdev.df)
Tmax_Tmaxdev_plot <- ggplot(data = Tmax_Tmaxdev, aes(x = tmax, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmax, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax Deviation ") + 
  geom_line(size=1) + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax Deviation ") +
  ylab("Predicted change in tree cover") + xlab("Tmax") +
  mytheme + labs(tag="B")


#Heatload and density
predictionHeatload_highDens <- predictionHeatload_midDens <- predictionHeatload_lowDens <- matrix(NA, length(samp), length(dens_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highDens[s,] <- exp(predict_fun(heatload=heatload_quant[2],dens=dens_seq))-exp(dens_seq)
  
  predictionHeatload_midDens[s,] <- exp(predict_fun(heatload=heatload_mean,dens=dens_seq))-exp(dens_seq)
  
  predictionHeatload_lowDens[s,] <- exp(predict_fun(heatload=heatload_quant[1],dens=dens_seq))-exp(dens_seq)
}
predictionHeatload_highDens_exp <- (predictionHeatload_highDens)
predictionHeatload_midDens_exp <- (predictionHeatload_midDens)
predictionHeatload_lowDens_exp <- (predictionHeatload_lowDens)
ci.Heatload_highDens <- apply(predictionHeatload_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midDens <- apply(predictionHeatload_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowDens <- apply(predictionHeatload_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highDens.df <- data.frame(dens = exp(dens_seq), median = ci.Heatload_highDens[2,], ci.low = ci.Heatload_highDens[1,], ci.high = ci.Heatload_highDens[3,], ci.group = "High")
ci.Heatload_midDens.df <- data.frame(dens = exp(dens_seq), median = ci.Heatload_midDens[2,], ci.low = ci.Heatload_midDens[1,], ci.high = ci.Heatload_midDens[3,], ci.group = "Mid")
ci.Heatload_lowDens.df <- data.frame(dens = exp(dens_seq), median = ci.Heatload_lowDens[2,], ci.low = ci.Heatload_lowDens[1,], ci.high = ci.Heatload_lowDens[3,], ci.group = "Low")
Heatload_Dens <- rbind(ci.Heatload_highDens.df, ci.Heatload_midDens.df, ci.Heatload_lowDens.df)
Heatload_Dens_plot <- ggplot(data = Heatload_Dens, aes(x = dens, y = median, color = ci.group)) +
  geom_ribbon(aes(x = dens, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Heatload ") + 
  geom_line() + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Heatload ") +
  ylab("Predicted change in tree cover") + xlab("Tree cover") +
  mytheme


#PPT and density
predictionPPT_highDens <- predictionPPT_midDens <- predictionPPT_lowDens <- matrix(NA, length(samp), length(dens_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highDens[s,] <- exp(predict_fun(ppt=ppt_quant[2],dens=dens_seq))-exp(dens_seq)
  
  predictionPPT_midDens[s,] <- exp(predict_fun(ppt=ppt_mean,dens=dens_seq))-exp(dens_seq)
  
  predictionPPT_lowDens[s,] <- exp(predict_fun(ppt=ppt_quant[1],dens=dens_seq))-exp(dens_seq)
}
predictionPPT_highDens_exp <- (predictionPPT_highDens)
predictionPPT_midDens_exp <- (predictionPPT_midDens)
predictionPPT_lowDens_exp <- (predictionPPT_lowDens)
ci.PPT_highDens <- apply(predictionPPT_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midDens <- apply(predictionPPT_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowDens <- apply(predictionPPT_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highDens.df <- data.frame(dens = exp(dens_seq), median = ci.PPT_highDens[2,], ci.low = ci.PPT_highDens[1,], ci.high = ci.PPT_highDens[3,], ci.group = "High")
ci.PPT_midDens.df <- data.frame(dens = exp(dens_seq), median = ci.PPT_midDens[2,], ci.low = ci.PPT_midDens[1,], ci.high = ci.PPT_midDens[3,], ci.group = "Mid")
ci.PPT_lowDens.df <- data.frame(dens = exp(dens_seq), median = ci.PPT_lowDens[2,], ci.low = ci.PPT_lowDens[1,], ci.high = ci.PPT_lowDens[3,], ci.group = "Low")
PPT_Dens <- rbind(ci.PPT_highDens.df, ci.PPT_midDens.df, ci.PPT_lowDens.df)
PPT_Dens_plot <- ggplot(data = PPT_Dens, aes(x = dens, y = median, color = ci.group)) +
  geom_ribbon(aes(x = dens, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT ") + 
  geom_line() + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="PPT ") +
  ylab("Predicted change in tree cover") + xlab("Tree cover") +
  mytheme + labs(tag="C")


#Tmax and density
predictionTmax_highDens <- predictionTmax_midDens <- predictionTmax_lowDens <- matrix(NA, length(samp), length(dens_seq)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmax_highDens[s,] <- exp(predict_fun(tmax=tmax_quant[2],dens=dens_seq))-exp(dens_seq)
  
  predictionTmax_midDens[s,] <- exp(predict_fun(tmax=tmax_mean,dens=dens_seq))-exp(dens_seq)
  
  predictionTmax_lowDens[s,] <- exp(predict_fun(tmax=tmax_quant[1],dens=dens_seq))-exp(dens_seq)
}
predictionTmax_highDens_exp <- (predictionTmax_highDens)
predictionTmax_midDens_exp <- (predictionTmax_midDens)
predictionTmax_lowDens_exp <- (predictionTmax_lowDens)
ci.Tmax_highDens <- apply(predictionTmax_highDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmax_midDens <- apply(predictionTmax_midDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_lowDens <- apply(predictionTmax_lowDens_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_highDens.df <- data.frame(dens = exp(dens_seq), median = ci.Tmax_highDens[2,], ci.low = ci.Tmax_highDens[1,], ci.high = ci.Tmax_highDens[3,], ci.group = "High")
ci.Tmax_midDens.df <- data.frame(dens = exp(dens_seq), median = ci.Tmax_midDens[2,], ci.low = ci.Tmax_midDens[1,], ci.high = ci.Tmax_midDens[3,], ci.group = "Mid")
ci.Tmax_lowDens.df <- data.frame(dens = exp(dens_seq), median = ci.Tmax_lowDens[2,], ci.low = ci.Tmax_lowDens[1,], ci.high = ci.Tmax_lowDens[3,], ci.group = "Low")
Tmax_Dens <- rbind(ci.Tmax_highDens.df, ci.Tmax_midDens.df, ci.Tmax_lowDens.df)
Tmax_Dens_plot <- ggplot(data = Tmax_Dens, aes(x = dens, y = median, color = ci.group)) +
  geom_ribbon(aes(x = dens, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax ") + 
  geom_line() + 
  scale_color_manual(values=c("High"="#054544", "Mid"="#ce9642", "Low"="#898e9f"), name="Tmax ") +
  ylab("Predicted change in tree cover") + xlab("Tree cover") +
  mytheme + labs(tag="D")


png(file="./Output/Interactions_ddint.png",9,7,units="in",type="cairo",res=600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
  nrow = 2,
  ncol = 2)))

print(PPT_PPTdev_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(Tmax_Tmaxdev_plot, vp = viewport(layout.pos.row = 1,layout.pos.col = 2))
print(PPT_Dens_plot, vp = viewport(layout.pos.row = 2,layout.pos.col = 1))
print(Tmax_Dens_plot, vp = viewport(layout.pos.row = 2,layout.pos.col = 2))

dev.off()


##INDIVIDUAL RESPONSE PLOTS
pixel_samp <- sample(seq(1,dim(Xall)[2]),500)

#Heatload and PPT dev 
get.ind.heatppt.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  ppt_devrng_pixel <- range(pixel_dat[,6]) #setting range for tmp_normrng
  ppt_dev_pixel <- seq(ppt_devrng_pixel[1], ppt_devrng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  tmin_dev_pixel <- mean(pixel_dat[,7])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  dens_pixel <- mean(meanLatent)
  predictionPPTdev_heat <- matrix(NA, length(samp), length(ppt_dev_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionPPTdev_heat[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                             ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                             dens=dens_pixel)
  }
  PPTdev_prediction <- exp(predictionPPTdev_heat)
  ci.PPTdev_heat <- apply(PPTdev_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.PPTdev_heat.df <- data.frame(ppt_dev = ppt_dev_pixel, heatload = heatload_pixel, median = ci.PPTdev_heat[2,], ci.low = ci.PPTdev_heat[1,], ci.high = ci.PPTdev_heat[3,], pixel = subset)
  PPTdev_heat_int <- rbind(ci.PPTdev_heat.df)
  print(j)
  PPTdev_heat_int  
}
#get.ind.tmp.response(i = 6)
PPTdev_heat_pixel_response <- list()
PPTdev_heat_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.heatppt.response)
PPTdev_heat_pixel_response.df <- do.call(rbind, PPTdev_heat_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by heatload
ggplot(data = PPTdev_heat_pixel_response.df, aes(x = ppt_dev, y = median, color = heatload, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#Heatload and Tmin dev 
get.ind.heatTmin.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  tmin_devrng_pixel <- range(pixel_dat[,7]) #setting range for tmp_normrng
  tmin_dev_pixel <- seq(tmin_devrng_pixel[1], tmin_devrng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  dens_pixel <- mean(meanLatent)
  predictionTmindev_heat <- matrix(NA, length(samp), length(tmin_dev_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionTmindev_heat[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                              ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                              dens=dens_pixel)
  }
  Tmindev_prediction <- exp(predictionTmindev_heat)
  ci.Tmindev_heat <- apply(Tmindev_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Tmindev_heat.df <- data.frame(tmin_dev = tmin_dev_pixel, heatload = heatload_pixel, median = ci.Tmindev_heat[2,], ci.low = ci.Tmindev_heat[1,], ci.high = ci.Tmindev_heat[3,], pixel = subset)
  Tmindev_heat_int <- rbind(ci.Tmindev_heat.df)
  print(j)
  Tmindev_heat_int  
}
#get.ind.tmp.response(i = 6)
Tmindev_heat_pixel_response <- list()
Tmindev_heat_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.heatTmin.response)
Tmindev_heat_pixel_response.df <- do.call(rbind, Tmindev_heat_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by heatload
ggplot(data = Tmindev_heat_pixel_response.df, aes(x = tmin_dev, y = median, color = heatload, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#Heatload and Tmin dev 
get.ind.heatTmax.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  tmax_devrng_pixel <- range(pixel_dat[,8]) #setting range for tmp_normrng
  tmax_dev_pixel <- seq(tmax_devrng_pixel[1], tmax_devrng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmin_dev_pixel <- mean(pixel_dat[,7])  
  dens_pixel <- mean(meanLatent)
  predictionTmaxdev_heat <- matrix(NA, length(samp), length(tmax_dev_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionTmaxdev_heat[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                              ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                              dens=dens_pixel)
  }
  Tmaxdev_prediction <- exp(predictionTmaxdev_heat)
  ci.Tmaxdev_heat <- apply(Tmaxdev_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Tmaxdev_heat.df <- data.frame(tmax_dev = tmax_dev_pixel, heatload = heatload_pixel, median = ci.Tmaxdev_heat[2,], ci.low = ci.Tmaxdev_heat[1,], ci.high = ci.Tmaxdev_heat[3,], pixel = subset)
  Tmaxdev_heat_int <- rbind(ci.Tmaxdev_heat.df)
  print(j)
  Tmaxdev_heat_int  
}
#get.ind.tmp.response(i = 6)
Tmaxdev_heat_pixel_response <- list()
Tmaxdev_heat_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.heatTmax.response)
Tmaxdev_heat_pixel_response.df <- do.call(rbind, Tmaxdev_heat_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by heatload
ggplot(data = Tmaxdev_heat_pixel_response.df, aes(x = tmax_dev, y = median, color = heatload, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#PPT and PPT dev 
get.ind.ppt.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  ppt_devrng_pixel <- range(pixel_dat[,6]) #setting range for tmp_normrng
  ppt_dev_pixel <- seq(ppt_devrng_pixel[1], ppt_devrng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  tmin_dev_pixel <- mean(pixel_dat[,7])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  dens_pixel <- mean(meanLatent)
  predictionPPTdev_ppt <- matrix(NA, length(samp), length(ppt_dev_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionPPTdev_ppt[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                            ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                            dens=dens_pixel)
  }
  PPTdev_prediction <- exp(predictionPPTdev_ppt)
  ci.PPTdev_ppt <- apply(PPTdev_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.PPTdev_ppt.df <- data.frame(ppt_dev = ppt_dev_pixel, ppt = ppt_pixel, median = ci.PPTdev_ppt[2,], ci.low = ci.PPTdev_ppt[1,], ci.high = ci.PPTdev_ppt[3,], pixel = subset)
  PPTdev_ppt_int <- rbind(ci.PPTdev_ppt.df)
  print(j)
  PPTdev_ppt_int  
}
#get.ind.tmp.response(i = 6)
PPTdev_ppt_pixel_response <- list()
PPTdev_ppt_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.ppt.response)
PPTdev_ppt_pixel_response.df <- do.call(rbind, PPTdev_ppt_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by ppt
ggplot(data = PPTdev_ppt_pixel_response.df, aes(x = ppt_dev, y = median, color = ppt, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#Tmin and Tmin dev 
get.ind.tmin.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  tmin_devrng_pixel <- range(pixel_dat[,7]) #setting range for tmp_normrng
  tmin_dev_pixel <- seq(tmin_devrng_pixel[1], tmin_devrng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  dens_pixel <- mean(meanLatent)
  predictionTmindev_tmin <- matrix(NA, length(samp), length(tmin_dev_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionTmindev_tmin[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                              ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                              dens=dens_pixel)
  }
  Tmindev_prediction <- exp(predictionTmindev_tmin)
  ci.Tmindev_tmin <- apply(Tmindev_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Tmindev_tmin.df <- data.frame(tmin_dev = tmin_dev_pixel, tmin = tmin_pixel, median = ci.Tmindev_tmin[2,], ci.low = ci.Tmindev_tmin[1,], ci.high = ci.Tmindev_tmin[3,], pixel = subset)
  Tmindev_tmin_int <- rbind(ci.Tmindev_tmin.df)
  print(j)
  Tmindev_tmin_int  
}
#get.ind.tmp.response(i = 6)
Tmindev_tmin_pixel_response <- list()
Tmindev_tmin_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.tmin.response)
Tmindev_tmin_pixel_response.df <- do.call(rbind, Tmindev_tmin_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by tmin
ggplot(data = Tmindev_tmin_pixel_response.df, aes(x = tmin_dev, y = median, color = tmin, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#Tmax and Tmax dev 
get.ind.tmax.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  tmax_devrng_pixel <- range(pixel_dat[,8]) #setting range for tmp_normrng
  tmax_dev_pixel <- seq(tmax_devrng_pixel[1], tmax_devrng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmin_dev_pixel <- mean(pixel_dat[,7])  
  dens_pixel <- mean(meanLatent)
  predictionTmaxdev_tmax <- matrix(NA, length(samp), length(tmax_dev_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionTmaxdev_tmax[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                              ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                              dens=dens_pixel)
  }
  Tmaxdev_prediction <- exp(predictionTmaxdev_tmax)
  ci.Tmaxdev_tmax <- apply(Tmaxdev_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Tmaxdev_tmax.df <- data.frame(tmax_dev = tmax_dev_pixel, tmax = tmax_pixel, median = ci.Tmaxdev_tmax[2,], ci.low = ci.Tmaxdev_tmax[1,], ci.high = ci.Tmaxdev_tmax[3,], pixel = subset)
  Tmaxdev_tmax_int <- rbind(ci.Tmaxdev_tmax.df)
  print(j)
  Tmaxdev_tmax_int  
}
#get.ind.tmp.response(i = 6)
Tmaxdev_tmax_pixel_response <- list()
Tmaxdev_tmax_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.tmax.response)
Tmaxdev_tmax_pixel_response.df <- do.call(rbind, Tmaxdev_tmax_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by tmax
ggplot(data = Tmaxdev_tmax_pixel_response.df, aes(x = tmax_dev, y = median, color = tmax, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#Heatload and density 
pixel_samp <- sample(seq(1,dim(Xall)[2]),500)

get.ind.heatdens.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  dens_rng_pixel <- range(latMean[,subset]) #setting range for tmp_normrng
  dens_pixel <- seq(dens_rng_pixel[1], dens_rng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmin_dev_pixel <- mean(pixel_dat[,7])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  predictionDens_heat <- matrix(NA, length(samp), length(dens_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionDens_heat[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                             ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                             dens=dens_pixel)
  }
  Dens_prediction <- exp(predictionDens_heat)
  ci.Dens_heat <- apply(Dens_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Dens_heat <- ci.Dens_heat - rbind(exp(dens_pixel),exp(dens_pixel),exp(dens_pixel))
  ci.Dens_heat.df <- data.frame(dens = exp(dens_pixel), heatload = heatload_pixel, median = ci.Dens_heat[2,], ci.low = ci.Dens_heat[1,], ci.high = ci.Dens_heat[3,], pixel = subset)
  Dens_heat_int <- rbind(ci.Dens_heat.df)
  print(j)
  Dens_heat_int  
}
#get.ind.tmp.response(i = 6)
Dens_heat_pixel_response <- list()
Dens_heat_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.heatdens.response)
Dens_heat_pixel_response.df <- do.call(rbind, Dens_heat_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by ppt
ggplot(data = Dens_heat_pixel_response.df, aes(x = dens, y = median, color = heatload, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted change in percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#PPT and density
get.ind.pptdens.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  dens_rng_pixel <- range(latMean[,subset]) #setting range for tmp_normrng
  dens_pixel <- seq(dens_rng_pixel[1], dens_rng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmin_dev_pixel <- mean(pixel_dat[,7])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  predictionDens_ppt <- matrix(NA, length(samp), length(dens_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionDens_ppt[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                           ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                           dens=dens_pixel)
  }
  Dens_prediction <- exp(predictionDens_ppt)
  ci.Dens_ppt <- apply(Dens_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Dens_ppt <- ci.Dens_ppt - rbind(exp(dens_pixel),exp(dens_pixel),exp(dens_pixel))
  ci.Dens_ppt.df <- data.frame(dens = exp(dens_pixel), ppt = ppt_pixel, median = ci.Dens_ppt[2,], ci.low = ci.Dens_ppt[1,], ci.high = ci.Dens_ppt[3,], pixel = subset)
  Dens_ppt_int <- rbind(ci.Dens_ppt.df)
  print(j)
  Dens_ppt_int  
}
#get.ind.tmp.response(i = 6)
Dens_ppt_pixel_response <- list()
Dens_ppt_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.pptdens.response)
Dens_ppt_pixel_response.df <- do.call(rbind, Dens_ppt_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by ppt
ggplot(data = Dens_ppt_pixel_response.df, aes(x = dens, y = median, color = ppt, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted change in percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#Tmin and density
get.ind.tmindens.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  dens_rng_pixel <- range(latMean[,subset]) #setting range for tmp_normrng
  dens_pixel <- seq(dens_rng_pixel[1], dens_rng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmin_dev_pixel <- mean(pixel_dat[,7])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  predictionDens_tmin <- matrix(NA, length(samp), length(dens_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionDens_tmin[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                          ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                          dens=dens_pixel)
  }
  Dens_prediction <- exp(predictionDens_tmin)
  ci.Dens_tmin <- apply(Dens_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Dens_tmin <- ci.Dens_tmin - rbind(exp(dens_pixel),exp(dens_pixel),exp(dens_pixel))
  ci.Dens_tmin.df <- data.frame(dens = exp(dens_pixel), tmin = tmin_pixel, median = ci.Dens_tmin[2,], ci.low = ci.Dens_tmin[1,], ci.high = ci.Dens_tmin[3,], pixel = subset)
  Dens_tmin_int <- rbind(ci.Dens_tmin.df)
  print(j)
  Dens_tmin_int  
}
#get.ind.tmp.response(i = 6)
Dens_tmin_pixel_response <- list()
Dens_tmin_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.tmindens.response)
Dens_tmin_pixel_response.df <- do.call(rbind, Dens_tmin_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by heatload
ggplot(data = Dens_tmin_pixel_response.df, aes(x = dens, y = median, color = tmin, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted change in percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")


#Tmax and density
get.ind.tmaxdens.response<- function(j,sample){
  subset <- pixel_samp[j]
  pixel_dat <- Xall[,subset,]
  #pixel_dens <- meanLatent
  
  dens_rng_pixel <- range(latMean[,subset]) #setting range for tmp_normrng
  dens_pixel <- seq(dens_rng_pixel[1], dens_rng_pixel[2], by = 0.1)
  heatload_pixel <- mean(pixel_dat[,2])
  ppt_pixel <- mean(pixel_dat[,3])
  tmin_pixel <- mean(pixel_dat[,4])
  tmax_pixel <- mean(pixel_dat[,5])
  ppt_dev_pixel <- mean(pixel_dat[,6])
  tmin_dev_pixel <- mean(pixel_dat[,7])
  tmax_dev_pixel <- mean(pixel_dat[,8])  
  predictionDens_tmax <- matrix(NA, length(samp), length(dens_pixel)) 
  
  for(s in 1:length(samp)){
    i <- samp[s]
    predictionDens_tmax[s,] <- predict_fun(heatload=heatload_pixel,ppt=ppt_pixel,tmin=tmin_pixel,tmax=tmax_pixel,
                                           ppt_dev=ppt_dev_pixel,tmin_dev=tmin_dev_pixel,tmax_dev=tmax_dev_pixel,
                                           dens=dens_pixel)
  }
  Dens_prediction <- exp(predictionDens_tmax)
  ci.Dens_tmax <- apply(Dens_prediction, 2, quantile, c(0.025, 0.5, 0.975))
  ci.Dens_tmax <- ci.Dens_tmax - rbind(exp(dens_pixel),exp(dens_pixel),exp(dens_pixel))
  ci.Dens_tmax.df <- data.frame(dens = exp(dens_pixel), tmax = tmax_pixel, median = ci.Dens_tmax[2,], ci.low = ci.Dens_tmax[1,], ci.high = ci.Dens_tmax[3,], pixel = subset)
  Dens_tmax_int <- rbind(ci.Dens_tmax.df)
  print(j)
  Dens_tmax_int  
}
#get.ind.tmp.response(i = 6)
Dens_tmax_pixel_response <- list()
Dens_tmax_pixel_response <- lapply(1:length(pixel_samp), FUN = get.ind.tmaxdens.response)
Dens_tmax_pixel_response.df <- do.call(rbind, Dens_tmax_pixel_response)
#merged.response.samples <- merge(Tmean_AprMayJun_tree_response.df, ind.samples, by.x = "ci.group", by.y = "treeCD")
#merged.response.samples$ci.group <- as.character(merged.response.samples$ci.group)

#color by heatload
ggplot(data = Dens_tmax_pixel_response.df, aes(x = dens, y = median, color = tmax, group = pixel)) + 
  geom_line(alpha = 0.5) + 
  ylab("Predicted change in percent cover") + scale_color_gradient2(low = "#4575b4", mid = "#fddbc7", high = "#b2182b")
