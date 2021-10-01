
meanBeta <- colMeans(betaOut_ddint)
sdBeta <- apply(betaOut,2,sd)

meanLatent <- apply(latOut,c(1,2),mean)

plot(1:37,meanLatent[,1],type="l")
lines(1:37,Y1[,latentkeep[1]],type="l",col="lightblue")
lines(1:37,Y2[,latentkeep[1]],type="l",col="blue")

plot(1:37,meanLatent[,2],type="l",ylim=c(-20,5))
lines(1:37,Y1[,latentkeep[2]],type="l",col="lightblue")
lines(1:37,Y2[,latentkeep[2]],type="l",col="blue")

plot(1:37,meanLatent[,3],type="l",ylim=c(-20,5))
lines(1:37,Y1[,latentkeep[3]],type="l",col="lightblue")
lines(1:37,Y2[,latentkeep[3]],type="l",col="blue")

plot(1:37,meanLatent[,4],type="l",ylim=c(0,5))
lines(1:37,Y1[,latentkeep[4]],type="l",col="lightblue")
lines(1:37,Y2[,latentkeep[4]],type="l",col="blue")

plot(1:37,meanLatent[,5],type="l",ylim=c(0,5))
lines(1:37,Y1[,latentkeep[5]],type="l",col="lightblue")
lines(1:37,Y2[,latentkeep[5]],type="l",col="blue")
