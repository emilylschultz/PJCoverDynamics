#### PJ Cover Dynamics: Code to compare Landsat (?) and RAP percent cover rasters
## Created by: Emily Schultz
## Created on: 30 Jul 2021

library(raster)
library(RColorBrewer)
library(tidyverse)

# Set RAP percent cover data file pathway
PC.path <-  "./PJCover/"

# Get percent cover raster data files
PCFiles <- list.files(path = PC.path, pattern = glob2rx("Clipped*.tif"), full.names = TRUE) # Do one decade at a time because of memory limitations
RAPFiles <- list.files(path = PC.path, pattern = glob2rx("RAP*.tif"), full.names = TRUE) # Do one decade at a time because of memory limitations

PC <- stack(PCFiles)
RAP <- stack(RAPFiles)

# Select years of overlap
names(PC) <- as.character(2000:2016)
names(RAP) <- as.character(1984:2020)

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


# Compare individual pixels over time
n <- 3
max_y <- ncol(PC[[1]])
max_x <- nrow(PC[[1]])

y <- sample(1:max_y,n)
x <- sample(1:max_x,n)

PC_sample <- as.data.frame(PC[x,y])
RAP_sample <- as.data.frame(RAP_subset[x,y])

PC_sample$Sample<-rownames(PC_sample)
RAP_sample$Sample<-rownames(RAP_sample)

PC_sample_long <- pivot_longer(PC_sample,names(PC_sample)[-18],names_to = "Year", values_to = "PC")
RAP_sample_long <- pivot_longer(RAP_sample,names(RAP_sample)[-18],names_to = "Year", values_to = "RAP")

Sample <- merge(PC_sample_long, RAP_sample_long)

Sample$Year <- as.numeric(str_sub(Sample$Year, 2,5))

Sample <- pivot_longer(Sample,c(PC,RAP), names_to = "Dataset", values_to = "PC")

mytheme<-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
							 panel.background = element_blank(), axis.line = element_line(colour = "black"),
							 legend.text=element_text(size=11),legend.title=element_text(size=12),
							 legend.key = element_rect(fill = "white"),axis.text=element_text(size=12),
							 axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
							 axis.line.x = element_line(color="black", size = 0.3),
							 axis.line.y = element_line(color="black", size = 0.3))

sample1 <- ggplot(subset(Sample,Sample==1),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample2 <- ggplot(subset(Sample,Sample==2),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample3 <- ggplot(subset(Sample,Sample==3),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample4 <- ggplot(subset(Sample,Sample==4),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample5 <- ggplot(subset(Sample,Sample==5),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample6 <- ggplot(subset(Sample,Sample==6),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample7 <- ggplot(subset(Sample,Sample==7),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample8 <- ggplot(subset(Sample,Sample==8),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample9 <- ggplot(subset(Sample,Sample==9),aes(x=Year,y=PC,colour=Dataset)) +
	geom_line(size=1.2) + mytheme

sample1
sample2
sample3
sample4
sample5
sample6
sample7
sample8
sample9

	