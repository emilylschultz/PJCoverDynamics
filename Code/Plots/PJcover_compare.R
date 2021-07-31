#### PJ Cover Dynamics: Code to compare Landsat (?) and RAP percent cover rasters
## Created by: Emily Schultz
## Created on: 30 Jul 2021

library(raster)
library(RColorBrewer)

# Set RAP percent cover data file pathway
PC.path <-  "./PJCover/"

# Get percent cover raster data files
PCFiles <- list.files(path = PC.path, pattern = glob2rx("Clipped*.tif"), full.names = TRUE) # Do one decade at a time because of memory limitations
RAPFiles <- list.files(path = PC.path, pattern = glob2rx("RAP*.tif"), full.names = TRUE) # Do one decade at a time because of memory limitations

PC <- stack(PCFiles)
RAP <- stack(RAPFiles)

# Select years of overlap
names(PC) <- as.character(2000:2016)
names(RAP) <- as.character(1984:2019)

YearStart <- "X2000"
YearEnd <- "X2016"

RAP_subset <- RAP[[(which(names(RAP)==YearStart)):which(names(RAP)==YearEnd)]] 

# Create rasters of difference between percent cover files
d_PJcover<-PC
for(i in 1:length(PCFiles)){
	values(d_PJcover[[i]]) <- values(PC[[i]])-values(RAP_subset[[(i)]])
}

# Plot rasters of difference between percent cover files
pal_div <- colorRampPalette(brewer.pal(n=11, name = "BrBG"))
cuts<-c(seq(min(na.omit(values(d_PJcover))),0,length=6),
				seq(0,max(na.omit(values(d_PJcover))),length=6)[2:31])

cuts_round<-round(cuts,2)
legend_text<-(c(cuts_round[1],cuts_round[11],cuts_round[21],cuts_round[31],cuts_round[41],cuts_round[51],cuts_round[61]))

plot(d_PJcover, breaks=cuts, col=pal_div(11))

for (i in 1:16){
	plot(d_PJcover[[i]], breaks=cuts, col=pal_div(11)) 
}
