### Code to create variograms for PJ cover data
### Created by: Emily Schultz
### Created on: 4 Feb 2022

library(sp)
library(gstat)

load("locations.rda")

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[1,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[2,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[3,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[4,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[5,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[6,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[7,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[8,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[9,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[10,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[11,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[12,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[13,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[14,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[15,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[16,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[17,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[18,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[19,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[20,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[21,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[22,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[23,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[24,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[25,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[26,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[27,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[28,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[29,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[30,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[31,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[32,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[33,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[34,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[35,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)

Var.Data <- data.frame(x=location.x[sample],y=location.y[sample],resid=resid[36,])
coordinates(Var.Data)= ~ x+y
TheVariogram=variogram(resid~1, data=Var.Data)
plot(TheVariogram)
