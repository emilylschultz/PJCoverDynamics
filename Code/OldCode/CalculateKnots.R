####
#### Generate knots and plot
####
library(sp)
library(raster)
library(rgeos)
library(reshape2)
library(ggplot2)
library(Matrix)

mask <- raster("./PJCover/PJmask.tif")

Coords<-data.frame(Lon=location.x,Lat=location.y)

x.min=min(location.x)
x.max=max(location.x)
y.min=min(location.y)
y.max=max(location.y)

splits.x <- round((x.max-x.min)/4000)
splits.y <- round((y.max-y.min)/4000)
X=x.min+(x.max-x.min)/splits.x*c(0:splits.x)
Y=y.min+(y.max-y.min)/splits.y*c(splits.y:0)
XY=expand.grid(x=X,y=Y)
XY=XY[-which(XY$x==min(XY$x)),]

Knots=SpatialPoints(coords=XY,proj4string=CRS(proj4string(mask)))
rasValue=extract(mask,XY)
Knots=SpatialPoints(coords=XY[which(rasValue==1),],proj4string=CRS(proj4string(mask))) #
obsPts=SpatialPoints(coords=Coords, proj4string=CRS(proj4string(mask)))
Distances=gDistance(Knots,obsPts,byid=TRUE)
Distances=apply(Distances,2,'min')
my.buffer=150000
Which.include=which(Distances<my.buffer)

Knot.cell.distances=gDistance(Knots[Which.include,],obsPts,byid=TRUE)
diff.x=(x.max-x.min)/splits.x 
diff.y=(y.max-y.min)/splits.y 
test=(diff.x+diff.y)/2

range_parameter = 4000/3
sigma=range_parameter

#plot knot distances
Knot.distances=gDistance(Knots[Which.include,],Knots[Which.include,],byid=TRUE)
m <- melt(Knot.distances)
ggplot(data=m, aes(x=Var1, y=Var2))+
  geom_raster(aes(z=value, fill=value))

source("./Code/Conn_util_funcs.R")
Knot.Adj=rect_adj(splits.x+1,splits.y+1)
Knot.Adj=Knot.Adj[Which.include,Which.include]
Q.knot=-Knot.Adj
diag(Q.knot)=apply(Knot.Adj,2,'sum')
Q.knot=Matrix(Q.knot)

w=exp(-Knot.cell.distances/sigma) #exponential covariance structure

plot(Knot.cell.distances[,1], w[,1], xlab="Euclidean Distance (meters)", ylab="Covariance")
abline(v = test, col="red", lwd=2)
text(6000 , 0.8, "Distance btwn knots", col="red")

K=w/apply(w,1,'sum')
K.data=list(K=K,Q.knot=Q.knot)
save(K.data,file="./Output/Knot_cell_distances_subset.rda")


####
####  Plot the data with knots overlaid
####
library(plyr)
avgD <- apply(pc_mat,1,mean)
avgD <- data.frame(Lon=location.x,Lat=location.y,cover=avgD)
knotsD <- as.data.frame(Knots)

library(RColorBrewer)
myPalette <- colorRampPalette(brewer.pal(9, "Greens"))
tmp.theme=theme(axis.ticks = element_blank(), axis.text = element_blank(),
                strip.text=element_text(face="bold"),
                axis.title=element_text(size=14),text=element_text(size=16),
                legend.text=element_text(size=12), legend.title=element_text(size=16))

g <- ggplot()+
  geom_raster(data=avgD, aes(x=Lon, y=Lat, z=cover, fill=cover))+
  geom_point(data=knotsD, aes(x,y), size=2, color="white")+
  geom_point(data=knotsD, aes(x,y), size=1.5, color="black")+
  scale_fill_gradientn(colours=myPalette(100), name="Percent \nCover")+
  tmp.theme+
  theme(strip.background=element_rect(fill="white"))+
  coord_equal()+
  xlab("Longitude")+
  ylab("Latitude")
print(g)

Knots=Knots[Which.include,]


