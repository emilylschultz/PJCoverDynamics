#INTERACTION PLOTS
samp <- sample(seq((burnin+1),iter),1000)

#Heatload and PPT
heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
x <- mean(meanLatent)
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
ppt_rng <- quantile(Xall[,,3], c(0.2, 0.8))
predictionHeatload_highPPT <- predictionHeatload_midPPT <- predictionHeatload_lowPPT <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highPPT[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt_rng[2] + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt_rng[2] +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt_rng[2]*tmin + betaOut[i,16]*ppt_rng[2]*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt_rng[2]*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_midPPT[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_lowPPT[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt_rng[1] + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt_rng[1] +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt_rng[1]*tmin + betaOut[i,16]*ppt_rng[1]*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt_rng[1]*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
}
predictionHeatload_highPPT_exp <- exp(predictionHeatload_highPPT)
predictionHeatload_midPPT_exp <- exp(predictionHeatload_midPPT)
predictionHeatload_lowPPT_exp <- exp(predictionHeatload_lowPPT)
ci.Heatload_highPPT <- apply(predictionHeatload_highPPT_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midPPT <- apply(predictionHeatload_midPPT_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowPPT <- apply(predictionHeatload_lowPPT_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highPPT.df <- data.frame(heatload = heatload, median = ci.Heatload_highPPT[2,], ci.low = ci.Heatload_highPPT[1,], ci.high = ci.Heatload_highPPT[3,], ci.group = "High PPT")
ci.Heatload_midPPT.df <- data.frame(heatload = heatload, median = ci.Heatload_midPPT[2,], ci.low = ci.Heatload_midPPT[1,], ci.high = ci.Heatload_midPPT[3,], ci.group = "Mid PPT")
ci.Heatload_lowPPT.df <- data.frame(heatload = heatload, median = ci.Heatload_lowPPT[2,], ci.low = ci.Heatload_lowPPT[1,], ci.high = ci.Heatload_lowPPT[3,], ci.group = "Low PPT")
Heatload_PPT <- rbind(ci.Heatload_highPPT.df, ci.Heatload_midPPT.df, ci.Heatload_lowPPT.df)
ggplot(data = Heatload_PPT, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmin
heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
x <- mean(meanLatent)
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmin_rng <- quantile(Xall[,,4], c(0.2, 0.8))
predictionHeatload_highTmin <- predictionHeatload_midTmin <- predictionHeatload_lowTmin <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmin[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin_rng[2] +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin_rng[2] + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin_rng[2] + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin_rng[2]*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin_rng[2]*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_midTmin[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_lowTmin[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin_rng[1] +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin_rng[1] + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin_rng[1] + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin_rng[1]*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin_rng[1]*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
}
predictionHeatload_highTmin_exp <- exp(predictionHeatload_highTmin)
predictionHeatload_midTmin_exp <- exp(predictionHeatload_midTmin)
predictionHeatload_lowTmin_exp <- exp(predictionHeatload_lowTmin)
ci.Heatload_highTmin <- apply(predictionHeatload_highTmin_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmin <- apply(predictionHeatload_midTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmin <- apply(predictionHeatload_lowTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmin.df <- data.frame(heatload = heatload, median = ci.Heatload_highTmin[2,], ci.low = ci.Heatload_highTmin[1,], ci.high = ci.Heatload_highTmin[3,], ci.group = "High Tmin")
ci.Heatload_midTmin.df <- data.frame(heatload = heatload, median = ci.Heatload_midTmin[2,], ci.low = ci.Heatload_midTmin[1,], ci.high = ci.Heatload_midTmin[3,], ci.group = "Mid Tmin")
ci.Heatload_lowTmin.df <- data.frame(heatload = heatload, median = ci.Heatload_lowTmin[2,], ci.low = ci.Heatload_lowTmin[1,], ci.high = ci.Heatload_lowTmin[3,], ci.group = "Low Tmin")
Heatload_Tmin <- rbind(ci.Heatload_highTmin.df, ci.Heatload_midTmin.df, ci.Heatload_lowTmin.df)
ggplot(data = Heatload_Tmin, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmax
heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
x <- mean(meanLatent)
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmax_rng <- quantile(Xall[,,5], c(0.2, 0.8))
predictionHeatload_highTmax <- predictionHeatload_midTmax <- predictionHeatload_lowTmax <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax_rng[2] + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax_rng[2] + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax_rng[2] + betaOut[i,17]*tmin*tmax_rng[2] + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax_rng[2]*tmax_dev + x
  
  predictionHeatload_midTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_lowTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax_rng[1] + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax_rng[1] + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax_rng[1] + betaOut[i,17]*tmin*tmax_rng[1] + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax_rng[1]*tmax_dev + x
}
predictionHeatload_highTmax_exp <- exp(predictionHeatload_highTmax)
predictionHeatload_midTmax_exp <- exp(predictionHeatload_midTmax)
predictionHeatload_lowTmax_exp <- exp(predictionHeatload_lowTmax)
ci.Heatload_highTmax <- apply(predictionHeatload_highTmax_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmax <- apply(predictionHeatload_midTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmax <- apply(predictionHeatload_lowTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmax.df <- data.frame(heatload = heatload, median = ci.Heatload_highTmax[2,], ci.low = ci.Heatload_highTmax[1,], ci.high = ci.Heatload_highTmax[3,], ci.group = "High Tmax")
ci.Heatload_midTmax.df <- data.frame(heatload = heatload, median = ci.Heatload_midTmax[2,], ci.low = ci.Heatload_midTmax[1,], ci.high = ci.Heatload_midTmax[3,], ci.group = "Mid Tmax")
ci.Heatload_lowTmax.df <- data.frame(heatload = heatload, median = ci.Heatload_lowTmax[2,], ci.low = ci.Heatload_lowTmax[1,], ci.high = ci.Heatload_lowTmax[3,], ci.group = "Low Tmax")
Heatload_Tmax <- rbind(ci.Heatload_highTmax.df, ci.Heatload_midTmax.df, ci.Heatload_lowTmax.df)
ggplot(data = Heatload_Tmax, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and PPT dev
heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
x <- mean(meanLatent)
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
ppt_dev_rng <- quantile(Xall[,,6], c(0.2, 0.8))
predictionHeatload_highPPTdev <- predictionHeatload_midPPTdev <- predictionHeatload_lowPPTdev <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highPPTdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev_rng[2] + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev_rng[2] + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev_rng[2] + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_midPPTdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_lowPPTdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev_rng[1] + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev_rng[1] + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev_rng[1] + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
}
predictionHeatload_highPPTdev_exp <- exp(predictionHeatload_highPPTdev)
predictionHeatload_midPPTdev_exp <- exp(predictionHeatload_midPPTdev)
predictionHeatload_lowPPTdev_exp <- exp(predictionHeatload_lowPPTdev)
ci.Heatload_highPPTdev <- apply(predictionHeatload_highPPTdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midPPTdev <- apply(predictionHeatload_midPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowPPTdev <- apply(predictionHeatload_lowPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highPPTdev.df <- data.frame(heatload = heatload, median = ci.Heatload_highPPTdev[2,], ci.low = ci.Heatload_highPPTdev[1,], ci.high = ci.Heatload_highPPTdev[3,], ci.group = "High PPTdev")
ci.Heatload_midPPTdev.df <- data.frame(heatload = heatload, median = ci.Heatload_midPPTdev[2,], ci.low = ci.Heatload_midPPTdev[1,], ci.high = ci.Heatload_midPPTdev[3,], ci.group = "Mid PPTdev")
ci.Heatload_lowPPTdev.df <- data.frame(heatload = heatload, median = ci.Heatload_lowPPTdev[2,], ci.low = ci.Heatload_lowPPTdev[1,], ci.high = ci.Heatload_lowPPTdev[3,], ci.group = "Low PPTdev")
Heatload_PPTdev <- rbind(ci.Heatload_highPPTdev.df, ci.Heatload_midPPTdev.df, ci.Heatload_lowPPTdev.df)
ggplot(data = Heatload_PPTdev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmin dev
heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
x <- mean(meanLatent)
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmin_dev_rng <- quantile(Xall[,,7], c(0.2, 0.8))
predictionHeatload_highTmindev <- predictionHeatload_midTmindev <- predictionHeatload_lowTmindev <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmindev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev_rng[2] + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev_rng[2] +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev_rng[2] + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_midTmindev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_lowTmindev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev_rng[1] + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev_rng[1] +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev_rng[1] + betaOut[i,20]*tmax*tmax_dev + x
}
predictionHeatload_highTmindev_exp <- exp(predictionHeatload_highTmindev)
predictionHeatload_midTmindev_exp <- exp(predictionHeatload_midTmindev)
predictionHeatload_lowTmindev_exp <- exp(predictionHeatload_lowTmindev)
ci.Heatload_highTmindev <- apply(predictionHeatload_highTmindev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmindev <- apply(predictionHeatload_midTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmindev <- apply(predictionHeatload_lowTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmindev.df <- data.frame(heatload = heatload, median = ci.Heatload_highTmindev[2,], ci.low = ci.Heatload_highTmindev[1,], ci.high = ci.Heatload_highTmindev[3,], ci.group = "High Tmindev")
ci.Heatload_midTmindev.df <- data.frame(heatload = heatload, median = ci.Heatload_midTmindev[2,], ci.low = ci.Heatload_midTmindev[1,], ci.high = ci.Heatload_midTmindev[3,], ci.group = "Mid Tmindev")
ci.Heatload_lowTmindev.df <- data.frame(heatload = heatload, median = ci.Heatload_lowTmindev[2,], ci.low = ci.Heatload_lowTmindev[1,], ci.high = ci.Heatload_lowTmindev[3,], ci.group = "Low Tmindev")
Heatload_Tmindev <- rbind(ci.Heatload_highTmindev.df, ci.Heatload_midTmindev.df, ci.Heatload_lowTmindev.df)
ggplot(data = Heatload_Tmindev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Heatload and Tmax dev
heatloadrng <- range(Xall[,,2],na.rm = TRUE) #setting range for heatload
heatload <- seq(heatloadrng[1], heatloadrng[2], by = 0.01)
x <- mean(meanLatent)
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmax_dev_rng <- quantile(Xall[,,8], c(0.2, 0.8))
predictionHeatload_highTmaxdev <- predictionHeatload_midTmaxdev <- predictionHeatload_lowTmaxdev <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionHeatload_highTmaxdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev_rng[2] + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev_rng[2] + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev_rng[2] + x
  
  predictionHeatload_midTmaxdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionHeatload_lowTmaxdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev_rng[1] + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev_rng[1] + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev_rng[1] + x
}
predictionHeatload_highTmaxdev_exp <- exp(predictionHeatload_highTmaxdev)
predictionHeatload_midTmaxdev_exp <- exp(predictionHeatload_midTmaxdev)
predictionHeatload_lowTmaxdev_exp <- exp(predictionHeatload_lowTmaxdev)
ci.Heatload_highTmaxdev <- apply(predictionHeatload_highTmaxdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Heatload_midTmaxdev <- apply(predictionHeatload_midTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_lowTmaxdev <- apply(predictionHeatload_lowTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Heatload_highTmaxdev.df <- data.frame(heatload = heatload, median = ci.Heatload_highTmaxdev[2,], ci.low = ci.Heatload_highTmaxdev[1,], ci.high = ci.Heatload_highTmaxdev[3,], ci.group = "High Tmaxdev")
ci.Heatload_midTmaxdev.df <- data.frame(heatload = heatload, median = ci.Heatload_midTmaxdev[2,], ci.low = ci.Heatload_midTmaxdev[1,], ci.high = ci.Heatload_midTmaxdev[3,], ci.group = "Mid Tmaxdev")
ci.Heatload_lowTmaxdev.df <- data.frame(heatload = heatload, median = ci.Heatload_lowTmaxdev[2,], ci.low = ci.Heatload_lowTmaxdev[1,], ci.high = ci.Heatload_lowTmaxdev[3,], ci.group = "Low Tmaxdev")
Heatload_Tmaxdev <- rbind(ci.Heatload_highTmaxdev.df, ci.Heatload_midTmaxdev.df, ci.Heatload_lowTmaxdev.df)
ggplot(data = Heatload_Tmaxdev, aes(x = heatload, y = median, color = ci.group)) +
  geom_ribbon(aes(x = heatload, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#PPT and Tmin
pptrng <- range(Xall[,,3],na.rm = TRUE) #setting range for heatload
ppt <- seq(pptrng[1], pptrng[2], by = 0.01)
x <- mean(meanLatent)
heatload <- mean(Xall[,,2])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmin_rng <- quantile(Xall[,,4], c(0.2, 0.8))
predictionPPT_highTmin <- predictionPPT_midTmin <- predictionPPT_lowTmin <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highTmin[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin_rng[2] +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin_rng[2] + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin_rng[2] + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin_rng[2]*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin_rng[2]*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionPPT_midTmin[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionPPT_lowTmin[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin_rng[1] +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin_rng[1] + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin_rng[1] + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin_rng[1]*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin_rng[1]*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
}
predictionPPT_highTmin_exp <- exp(predictionPPT_highTmin)
predictionPPT_midTmin_exp <- exp(predictionPPT_midTmin)
predictionPPT_lowTmin_exp <- exp(predictionPPT_lowTmin)
ci.PPT_highTmin <- apply(predictionPPT_highTmin_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midTmin <- apply(predictionPPT_midTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowTmin <- apply(predictionPPT_lowTmin_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highTmin.df <- data.frame(ppt = ppt, median = ci.PPT_highTmin[2,], ci.low = ci.PPT_highTmin[1,], ci.high = ci.PPT_highTmin[3,], ci.group = "High Tmin")
ci.PPT_midTmin.df <- data.frame(ppt = ppt, median = ci.PPT_midTmin[2,], ci.low = ci.PPT_midTmin[1,], ci.high = ci.PPT_midTmin[3,], ci.group = "Mid Tmin")
ci.PPT_lowTmin.df <- data.frame(ppt = ppt, median = ci.PPT_lowTmin[2,], ci.low = ci.PPT_lowTmin[1,], ci.high = ci.PPT_lowTmin[3,], ci.group = "Low Tmin")
PPT_Tmin <- rbind(ci.PPT_highTmin.df, ci.PPT_midTmin.df, ci.PPT_lowTmin.df)
ggplot(data = PPT_Tmin, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#PPT and Tmax
pptrng <- range(Xall[,,3],na.rm = TRUE) #setting range for heatload
ppt <- seq(pptrng[1], pptrng[2], by = 0.01)
x <- mean(meanLatent)
heatload <- mean(Xall[,,2])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmax_rng <- quantile(Xall[,,5], c(0.2, 0.8))
predictionPPT_highTmax <- predictionPPT_midTmax <- predictionPPT_lowTmax <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax_rng[2] + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax_rng[2] + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax_rng[2] + betaOut[i,17]*tmin*tmax_rng[2] + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax_rng[2]*tmax_dev + x
  
  predictionPPT_midTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionPPT_lowTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax_rng[1] + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax_rng[1] + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax_rng[1] + betaOut[i,17]*tmin*tmax_rng[1] + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax_rng[1]*tmax_dev + x
}
predictionPPT_highTmax_exp <- exp(predictionPPT_highTmax)
predictionPPT_midTmax_exp <- exp(predictionPPT_midTmax)
predictionPPT_lowTmax_exp <- exp(predictionPPT_lowTmax)
ci.PPT_highTmax <- apply(predictionPPT_highTmax_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midTmax <- apply(predictionPPT_midTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowTmax <- apply(predictionPPT_lowTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highTmax.df <- data.frame(ppt = ppt, median = ci.PPT_highTmax[2,], ci.low = ci.PPT_highTmax[1,], ci.high = ci.PPT_highTmax[3,], ci.group = "High Tmax")
ci.PPT_midTmax.df <- data.frame(ppt = ppt, median = ci.PPT_midTmax[2,], ci.low = ci.PPT_midTmax[1,], ci.high = ci.PPT_midTmax[3,], ci.group = "Mid Tmax")
ci.PPT_lowTmax.df <- data.frame(ppt = ppt, median = ci.PPT_lowTmax[2,], ci.low = ci.PPT_lowTmax[1,], ci.high = ci.PPT_lowTmax[3,], ci.group = "Low Tmax")
PPT_Tmax <- rbind(ci.PPT_highTmax.df, ci.PPT_midTmax.df, ci.PPT_lowTmax.df)
ggplot(data = PPT_Tmax, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmin and Tmax
tminrng <- range(Xall[,,4],na.rm = TRUE) #setting range for heatload
tmin <- seq(pptrng[1], pptrng[2], by = 0.01)
x <- mean(meanLatent)
heatload <- mean(Xall[,,2])
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmax_rng <- quantile(Xall[,,5], c(0.2, 0.8))
predictionTmin_highTmax <- predictionTmin_midTmax <- predictionTmin_lowTmax <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmin_highTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax_rng[2] + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax_rng[2] + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax_rng[2] + betaOut[i,17]*tmin*tmax_rng[2] + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax_rng[2]*tmax_dev + x
  
  predictionTmin_midTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionTmin_lowTmax[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax_rng[1] + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax_rng[1] + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax_rng[1] + betaOut[i,17]*tmin*tmax_rng[1] + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax_rng[1]*tmax_dev + x
}
predictionTmin_highTmax_exp <- exp(predictionTmin_highTmax)
predictionTmin_midTmax_exp <- exp(predictionTmin_midTmax)
predictionTmin_lowTmax_exp <- exp(predictionTmin_lowTmax)
ci.Tmin_highTmax <- apply(predictionTmin_highTmax_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmin_midTmax <- apply(predictionTmin_midTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_lowTmax <- apply(predictionTmin_lowTmax_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_highTmax.df <- data.frame(tmin = tmin, median = ci.Tmin_highTmax[2,], ci.low = ci.Tmin_highTmax[1,], ci.high = ci.Tmin_highTmax[3,], ci.group = "High Tmax")
ci.Tmin_midTmax.df <- data.frame(tmin = tmin, median = ci.Tmin_midTmax[2,], ci.low = ci.Tmin_midTmax[1,], ci.high = ci.Tmin_midTmax[3,], ci.group = "Mid Tmax")
ci.Tmin_lowTmax.df <- data.frame(tmin = tmin, median = ci.Tmin_lowTmax[2,], ci.low = ci.Tmin_lowTmax[1,], ci.high = ci.Tmin_lowTmax[3,], ci.group = "Low Tmax")
Tmin_Tmax <- rbind(ci.Tmin_highTmax.df, ci.Tmin_midTmax.df, ci.Tmin_lowTmax.df)
ggplot(data = Tmin_Tmax, aes(x = tmin, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmin, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#PPT and PPT dev
pptrng <- range(Xall[,,3],na.rm = TRUE) #setting range for heatload
ppt <- seq(pptrng[1], pptrng[2], by = 0.01)
x <- mean(meanLatent)
heatload <- mean(Xall[,,2])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
ppt_dev_rng <- quantile(Xall[,,6], c(0.2, 0.8))
predictionPPT_highPPTdev <- predictionPPT_midPPTdev <- predictionPPT_lowPPTdev <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionPPT_highPPTdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev_rng[2] + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev_rng[2] + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev_rng[2] + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionPPT_midPPTdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionPPT_lowPPTdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev_rng[1] + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev_rng[1] + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev_rng[1] + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
}
predictionPPT_highPPTdev_exp <- exp(predictionPPT_highPPTdev)
predictionPPT_midPPTdev_exp <- exp(predictionPPT_midPPTdev)
predictionPPT_lowPPTdev_exp <- exp(predictionPPT_lowPPTdev)
ci.PPT_highPPTdev <- apply(predictionPPT_highPPTdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.PPT_midPPTdev <- apply(predictionPPT_midPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_lowPPTdev <- apply(predictionPPT_lowPPTdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.PPT_highPPTdev.df <- data.frame(ppt = ppt, median = ci.PPT_highPPTdev[2,], ci.low = ci.PPT_highPPTdev[1,], ci.high = ci.PPT_highPPTdev[3,], ci.group = "High PPTdev")
ci.PPT_midPPTdev.df <- data.frame(ppt = ppt, median = ci.PPT_midPPTdev[2,], ci.low = ci.PPT_midPPTdev[1,], ci.high = ci.PPT_midPPTdev[3,], ci.group = "Mid PPTdev")
ci.PPT_lowPPTdev.df <- data.frame(ppt = ppt, median = ci.PPT_lowPPTdev[2,], ci.low = ci.PPT_lowPPTdev[1,], ci.high = ci.PPT_lowPPTdev[3,], ci.group = "Low PPTdev")
PPT_PPTdev <- rbind(ci.PPT_highPPTdev.df, ci.PPT_midPPTdev.df, ci.PPT_lowPPTdev.df)
ggplot(data = PPT_PPTdev, aes(x = ppt, y = median, color = ci.group)) +
  geom_ribbon(aes(x = ppt, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmin and Tmin dev
tminrng <- range(Xall[,,4],na.rm = TRUE) #setting range for heatload
tmin <- seq(pptrng[1], pptrng[2], by = 0.01)
x <- mean(meanLatent)
heatload <- mean(Xall[,,2])
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin_dev <- mean(Xall[,,7])
tmax <- mean(Xall[,,5])
tmax_dev <- mean(Xall[,,8])
tmin_dev_rng <- quantile(Xall[,,7], c(0.2, 0.8))
predictionTmin_highTmindev <- predictionTmin_midTmindev <- predictionTmin_lowTmindev <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmin_highTmindev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev_rng[2] + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev_rng[2] +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev_rng[2] + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionTmin_midTmindev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionTmin_lowTmindev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev_rng[1] + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev_rng[1] +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev_rng[1] + betaOut[i,20]*tmax*tmax_dev + x
}
predictionTmin_highTmindev_exp <- exp(predictionTmin_highTmindev)
predictionTmin_midTmindev_exp <- exp(predictionTmin_midTmindev)
predictionTmin_lowTmindev_exp <- exp(predictionTmin_lowTmindev)
ci.Tmin_highTmindev <- apply(predictionTmin_highTmindev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmin_midTmindev <- apply(predictionTmin_midTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_lowTmindev <- apply(predictionTmin_lowTmindev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmin_highTmindev.df <- data.frame(tmin = tmin, median = ci.Tmin_highTmindev[2,], ci.low = ci.Tmin_highTmindev[1,], ci.high = ci.Tmin_highTmindev[3,], ci.group = "High Tmindev")
ci.Tmin_midTmindev.df <- data.frame(tmin = tmin, median = ci.Tmin_midTmindev[2,], ci.low = ci.Tmin_midTmindev[1,], ci.high = ci.Tmin_midTmindev[3,], ci.group = "Mid Tmindev")
ci.Tmin_lowTmindev.df <- data.frame(tmin = tmin, median = ci.Tmin_lowTmindev[2,], ci.low = ci.Tmin_lowTmindev[1,], ci.high = ci.Tmin_lowTmindev[3,], ci.group = "Low Tmindev")
Tmin_Tmindev <- rbind(ci.Tmin_highTmindev.df, ci.Tmin_midTmindev.df, ci.Tmin_lowTmindev.df)
ggplot(data = Tmin_Tmindev, aes(x = tmin, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmin, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) + 
  geom_line() + 
  scale_color_manual(values=c("#4575B4", "#FDDBC7", "#B2182B")) +
  ylab("Predicted percent cover") 

#Tmax and Tmax dev
tmaxrng <- range(Xall[,,5],na.rm = TRUE) #setting range for heatload
tmax <- seq(pptrng[1], pptrng[2], by = 0.01)
x <- mean(meanLatent)
heatload <- mean(Xall[,,2])
ppt <- mean(Xall[,,3])
ppt_dev <- mean(Xall[,,6])
tmin <- mean(Xall[,,4])
tmin_dev <- mean(Xall[,,7])
tmax_dev <- mean(Xall[,,8])
tmax_dev_rng <- quantile(Xall[,,8], c(0.2, 0.8))
predictionTmax_highTmaxdev <- predictionTmax_midTmaxdev <- predictionTmax_lowTmaxdev <- matrix(NA, length(samp), length(heatload)) 

for(s in 1:length(samp)){
  i <- samp[s]
  predictionTmax_highTmaxdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev_rng[2] + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev_rng[2] + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev_rng[2] + x
  
  predictionTmax_midTmaxdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev + x
  
  predictionTmax_lowTmaxdev[s,] <- betaOut[i,1] + betaOut[i,2]*heatload + betaOut[i,3]*ppt + betaOut[i,4]*tmin +
    betaOut[i,5]*tmax + betaOut[i,6]*ppt_dev + betaOut[i,7]*tmin_dev + betaOut[i,8]*tmax_dev_rng[1] + betaOut[i,9]*heatload*ppt +
    betaOut[i,10]*heatload*tmin + betaOut[i,11]*heatload*tmax + betaOut[i,12]*heatload*ppt_dev + betaOut[i,13]*heatload*tmin_dev +
    betaOut[i,14]*heatload*tmax_dev_rng[1] + betaOut[i,15]*ppt*tmin + betaOut[i,16]*ppt*tmax + betaOut[i,17]*tmin*tmax + 
    betaOut[i,18]*ppt*ppt_dev + betaOut[i,19]*tmin*tmin_dev + betaOut[i,20]*tmax*tmax_dev_rng[1] + x
}
predictionTmax_highTmaxdev_exp <- exp(predictionTmax_highTmaxdev)
predictionTmax_midTmaxdev_exp <- exp(predictionTmax_midTmaxdev)
predictionTmax_lowTmaxdev_exp <- exp(predictionTmax_lowTmaxdev)
ci.Tmax_highTmaxdev <- apply(predictionTmax_highTmaxdev_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
ci.Tmax_midTmaxdev <- apply(predictionTmax_midTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_lowTmaxdev <- apply(predictionTmax_lowTmaxdev_exp, 2, quantile, c(0.025, 0.5, 0.975))
ci.Tmax_highTmaxdev.df <- data.frame(tmax = tmax, median = ci.Tmax_highTmaxdev[2,], ci.low = ci.Tmax_highTmaxdev[1,], ci.high = ci.Tmax_highTmaxdev[3,], ci.group = "High Tmaxdev")
ci.Tmax_midTmaxdev.df <- data.frame(tmax = tmax, median = ci.Tmax_midTmaxdev[2,], ci.low = ci.Tmax_midTmaxdev[1,], ci.high = ci.Tmax_midTmaxdev[3,], ci.group = "Mid Tmaxdev")
ci.Tmax_lowTmaxdev.df <- data.frame(tmax = tmax, median = ci.Tmax_lowTmaxdev[2,], ci.low = ci.Tmax_lowTmaxdev[1,], ci.high = ci.Tmax_lowTmaxdev[3,], ci.group = "Low Tmaxdev")
Tmax_Tmaxdev <- rbind(ci.Tmax_highTmaxdev.df, ci.Tmax_midTmaxdev.df, ci.Tmax_lowTmaxdev.df)
ggplot(data = Tmax_Tmaxdev, aes(x = tmax, y = median, color = ci.group)) +
  geom_ribbon(aes(x = tmax, ymin = ci.low, ymax = ci.high, fill = ci.group), color = NA, alpha = 0.5) +
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
