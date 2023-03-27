library(ggplot2)

predict_fun <- function(coef=betaOut_dd,heatload=heatload_mean,ppt=ppt_mean,tmin=tmin_mean,tmax=tmax_mean,ppt_dev=ppt_dev_mean,tmin_dev=tmin_dev_mean,tmax_dev=tmax_dev_mean,dens=dens_mean){
	coef[i,1] + coef[i,2]*heatload + coef[i,3]*ppt + coef[i,4]*tmin +
		coef[i,5]*tmax + coef[i,6]*ppt_dev + coef[i,7]*tmin_dev + coef[i,8]*tmax_dev + coef[i,9]*heatload*ppt +
		coef[i,10]*heatload*tmin + coef[i,11]*heatload*tmax + coef[i,12]*heatload*ppt_dev + coef[i,13]*heatload*tmin_dev +
		coef[i,14]*heatload*tmax_dev + coef[i,15]*ppt*tmin + coef[i,16]*ppt*tmax + coef[i,17]*tmin*tmax + 
		coef[i,18]*ppt*ppt_dev + coef[i,19]*tmin*tmin_dev + coef[i,20]*tmax*tmax_dev + coef[i,21]*dens 
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
predictionDens<- matrix(NA, length(samp), length(dens_seq)) 

for(s in 1:length(samp)){
	i <- samp[s]
	predictionDens[s,] <- predict_fun(dens=dens_seq)
}
predictionDens_exp <- exp(predictionDens)
ci.Dens <- apply(predictionDens_exp, 2, quantile, c(0.025,0.5,0.975)) #confidence intervals
Dens.df <- data.frame(dens = exp(dens_seq), median = ci.Dens[2,], ci.low = ci.Dens[1,], ci.high = ci.Dens[3,])
ggplot(data = Dens.df, aes(x = dens, y = median)) +
	geom_ribbon(aes(x = dens, ymin = ci.low, ymax = ci.high), fill = "#4575B4", alpha = 0.5) +
	geom_line(color = "#4575B4") + 
	ylab("Predicted percent cover") 
