#### PJ Cover Dynamics: Code to rasters of fire occurrence and add to percent cover dataframe
## Created by: Emily Schultz
## Created on: 23 Jul 2021

library(raster)
library(RColorBrewer)

# Load percent cover rasters
PC.path <-  "./PJCover/"

PCFiles <- list.files(path = PC.path, pattern = glob2rx("Clipped*.tif"), full.names = TRUE)

PJcover <- stack(PCFiles)

# Make rasters with change in percent cover
d_PJcover<-PJcover[[1:16]]
for(i in 2:length(PCFiles)){
	values(d_PJcover[[(i-1)]]) <- values(PJcover[[i]])-values(PJcover[[(i-1)]])
}


pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))
cuts<-c(seq(min(na.omit(values(d_PJcover))),0,length=6),
				seq(0,max(na.omit(values(d_PJcover))),length=6)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(d_PJcover, breaks=cuts, col=pal_div(11))

for (i in 1:16){
	plot(d_PJcover[[i]], breaks=cuts, col=pal_div(11)) 
}

# Large negative change in percent cover in north-central part of study extent from 2002 to 2003 - matches fire perimeter in MODIS data