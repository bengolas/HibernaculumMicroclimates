rm(list=ls())


library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
library(schoolmath)
library(plyr)
library(gridExtra)
library(ghibli)
library(cowplot)
# library(raster)
# library(rgdal)
# library(plothelper)
# library(dplyr)
# library(plotly)
library(akima)
# library(zoo)
# library(raster)
# library(sp)
library(sf)
library(rjags)
library(lemon)

setwd("C:/Users/bengo/Documents/WNS/2017Barton")

load("Output/FullHierarchicalWorking.jags.rda")
load("Output/FullHierarchicalWorking.coda.rda")


###################################################################
# Set up data
###################################################################


#Set the working directory for the folder of interest on your computer
setwd("C:/Users/bengo/Documents/WNS/2017Barton/Barton2017")
getwd()
#Identify your iButton files in RHreads
#Not including faulty loggers 28,33,80

RHreads <- c("003_RH.csv","004_RH.csv","005_RH.csv","006_RH.csv","007_RH.csv",
             "013_RH.csv","020_RH.csv","022_RH.csv","023_RH.csv","026_RH.csv","027_RH.csv",
             "031_RH.csv","038_RH.csv","040_RH.csv","045_RH.csv",
             "046_RH.csv","050_RH.csv","058_RH.csv","064_RH.csv","065_RH.csv","067_RH.csv",
             "068_RH.csv","072_RH.csv","074_RH.csv","075_RH.csv","078_RH.csv",
             "088_RH.csv","094_RH.csv","096_RH.csv","101_RH.csv","103_RH.csv","110_RH.csv",
             "116_RH.csv","117_RH.csv")

Treads <- c("003_T.csv","004_T.csv","005_T.csv","006_T.csv","007_T.csv",
            "013_T.csv","020_T.csv","022_T.csv","023_T.csv","026_T.csv","027_T.csv",
            "031_T.csv","038_T.csv","040_T.csv","045_T.csv",
            "046_T.csv","050_RH.csv","058_T.csv","064_T.csv","065_T.csv","067_T.csv",
            "068_T.csv","072_T.csv","074_T.csv","075_T.csv","078_T.csv",
            "088_T.csv","094_T.csv","096_T.csv","101_T.csv","103_T.csv","110_T.csv",
            "116_T.csv","117_T.csv")



IDs <- as.numeric(gsub("_RH.csv","",RHreads))

setwd("C:/Users/bengo/Documents/WNS/2017Barton")

#Load data that is cleaned in DataCleaning file
setwd("C:/Users/bengo/Documents/WNS/2017Barton/DataCleaning")
load("df.trim") #Data for 2017-18 data loggers
load("dwt") #Data files for 2017-18 averaged outside data
# load("dwh")
# load("dww")
# load("df.pred.trim") #Data for 2018-19 data loggers
# load("dwt.18") #Data for 2018-19 averaged outside data
# load("dwh.18")
# load("dww.18")
load("Temp14_19")
load("Outside.mean.daily")

# Make sure all time points are present and equally spaced
test <- as.data.frame(cbind(unique(df.trim$Time),1:length(unique(df.trim$Time))))
# ggplot(test,aes(as_datetime(V1, format="%Y-%m-%d %H"),as.numeric(as.character(V2)),group=1)) + geom_line()
ID.drum <- c("020","022","023","026","027","028","031","033")
ID.upper <- c("003","004","005","006","007","008","013","038","094","096","101","103","110","116","117")
ID.lower <- c("040","045","046","050","058","064","065","067","068","072","074","075","078","080","088")
df.trim$Room <- NA
df.trim$Room[which(match(df.trim$ID,ID.drum)!="NA")] <- "drum"
df.trim$Room[which(match(df.trim$ID,ID.upper)!="NA")] <- "upper"
df.trim$Room[which(match(df.trim$ID,ID.lower)!="NA")] <- "lower"
df.trim$Drum <- 0
df.trim$Drum[which(match(df.trim$ID,ID.drum)!="NA")] <- 1
df.trim$Upper <- 0
df.trim$Upper[which(match(df.trim$ID,ID.upper)!="NA")] <- 1
df.trim$Lower <- 0
df.trim$Lower[which(match(df.trim$ID,ID.lower)!="NA")] <- 1

# ggplot(df.drum,aes(Time,Temp))+geom_line(aes(group=iButton, color=Depth)) + 
# theme_minimal() + scale_color_viridis_c()



RHreads <- c("020_RH.csv", "028_RH.csv", "040_RH.csv")

Treads <- c("020_T.csv", "028_T.csv", "040_T.csv")

IDs.18 <- as.numeric(gsub("_RH.csv","",RHreads))

#Below will plot line graphs for each iButton separately.
# ggplot(df.pred.trim,aes(Time,Temp))+geom_line(aes(group=iButton,color=ID)) + theme_minimal()
# ggplot(df.trim,aes(Time,Temp))+geom_line(aes(group=iButton,color=Depth)) +
#   theme_minimal() + ylim(-10,25)
# ggplot(df.trim,aes(Time,RH))+geom_line(aes(group=iButton,color=Depth)) + theme_minimal()

df.RH.test <- df.trim[df.trim$Time>="2017-12-01 00" & df.trim$Time<="2018-04-15 22",]
# ggplot(df.RH.test, aes(Time,RH)) + geom_line(aes(group=iButton,color=Depth)) +
# theme_minimal() + ylim(40,120)


# ggplot(df.RH.test,aes(Time,(RH/100)*0.611*exp(17.503*Temp/(Temp+240.97))))+geom_line(aes(group=iButton,color=Depth)) + theme_minimal() + ylim(-0.5,1.4)
# #Make sure all time points are present and equally spaced
# test <- as.data.frame(cbind(unique(df.pred.trim$Time),1:length(unique(df.pred.trim$Time))))
# ggplot(test,aes(as_datetime(V1),as.numeric(as.character(V2)),group=1)) + geom_line()  


setwd("C:/Users/bengo/Documents/WNS/2017Barton")
df.location <- read.csv("LocationInfo.csv")
df.location <- df.location[match(IDs,df.location$iButton),]
df.location <- subset(separate(df.location,"Approx..Elevation.Above.Lake.Champlain",into=c("a1","a2"),sep="/"))
df.location$a1 <- as.numeric(df.location$a1)
df.location$a2 <- as.numeric(df.location$a2)
df.location$Elevation <- rowMeans(df.location[,4:5],na.rm=TRUE)

df.location$is.MYLU <- ifelse(grepl("MYLU",df.location$Bats),1,0)
df.location$is.MYSO <- ifelse(grepl("MYSO",df.location$Bats),1,0)
df.location$is.MYLE <- ifelse(grepl("MYLE",df.location$Bats),1,0)

df.location$Elevation[which(df.location$iButton==88)] <- 1325
df.location$Elevation[which(df.location$iButton==20)] <- 1321


xycoords <- cbind(IDs,subset(df.location, select=c(x_coord,y_coord,Elevation)))

df.location <- subset(df.location, select = -c(a1,a2,Bats,Region))

#Suspect some logger depths are incorrectly recorded



df.trim <- df.trim[which(as.numeric(as.character(df.trim$ID)) %in% IDs),]
df.trim$Elevation <- rep(df.location$Elevation, length(df.trim[,1])/length(df.location[,1]))

# #Remove problematic loggers
# 
# rejects <- c("28","33","80")
# df.location <- df.location[-which(as.numeric(df.location$iButton %in% rejects)==1),]


#Clean data to remove unusable values
#Reset values associated with temperature errors
for(i in 1:5){
  df.trim$RH[which(df.trim$Temp>=20)] <- df.trim$RH[(which(df.trim$Temp>=20)-length(unique(df.trim$ID)))] 
  df.trim$RH[which(df.trim$Temp<=-6)] <- df.trim$RH[(which(df.trim$Temp<=-6)-length(unique(df.trim$ID)))]
}
#Reset erroneous values
df.trim$RH <- df.trim$RH/100
df.trim$RH[which(df.trim$Time <= "2017-11-30 22")] <- 1 #100% RH in warmer seasons
df.trim$RH[which(df.trim$Time >= "2018-04-20 00")] <- 1
df.trim$RH[which(df.trim$RH>=1)] <- 1 #Set truncate measurements to be realistic
df.trim$RH[which(df.trim$RH<=0)] <- 0 #i.e. >100% = 100%, <0% = 0%

#Remove errored temperature values
for(i in 1:50){
  df.trim$Temp[which(df.trim$Temp>=20)] <- df.trim$Temp[(which(df.trim$Temp>=20)-length(unique(df.trim$ID)))] 
  df.trim$Temp[which(df.trim$Temp<=-6)] <- df.trim$Temp[(which(df.trim$Temp<=-6)-length(unique(df.trim$ID)))] 
  
  df.trim$RH[which(df.trim$RH<=.4)] <- df.trim$RH[(which(df.trim$RH<=.4)-length(unique(df.trim$ID)))] 
}


df.trim$RH[which(df.trim$Time >= "2017-12-15 22" & df.trim$Time <= "2018-03-15" & df.trim$Temp>=8)] <- df.trim$RH[which(df.trim$Time >= "2017-11-30 22" & df.trim$Time <= "2018-03-31" & df.trim$Temp>=9)-length(unique(df.trim$ID))]
df.trim$Temp[which(df.trim$Time >= "2017-12-15 22" & df.trim$Time <= "2018-03-15" & df.trim$Temp>=8)] <- df.trim$Temp[which(df.trim$Time >= "2017-12-15 22" & df.trim$Time <= "2018-03-15" & df.trim$Temp>=8)-length(unique(df.trim$ID))]

T.tormin <- 2

df.microclim <- df.trim[which(df.trim$Time >= "2017-08-16 00" & df.trim$Time <= "2018-05-14 22"),]

df.microclim$d.WVP.bat <- 0.611*exp(17.503*pmax(df.microclim$Temp,T.tormin)/(pmax(df.microclim$Temp,T.tormin)+240.97)) - df.microclim$RH*0.611*exp((17.503*df.microclim$Temp)/(df.microclim$Temp+240.97))

df.microclim$d.WVP.a <- 0.611*exp((17.503*df.microclim$Temp)/(df.microclim$Temp+240.97)) - df.microclim$RH*0.611*exp((17.503*df.microclim$Temp)/(df.microclim$Temp+240.97))

# ggplot(df.microclim,aes(Time,Temp))+geom_line(aes(group=iButton,color=Depth)) +
# theme_minimal() 
# ggplot(df.microclim,aes(Time,RH))+geom_line(aes(group=iButton,color=Depth)) + theme_minimal()



df.location$T.mean <- with(df.microclim,tapply(Temp,list(ID=ID),mean))
df.location$T.sd <- with(df.microclim,tapply(Temp,list(ID=ID),sd))
df.location$T.max <- with(df.microclim,tapply(Temp,list(ID=ID),max))
df.location$T.min <- with(df.microclim,tapply(Temp,list(ID=ID),min))
df.location$T.median <- with(df.microclim,tapply(Temp,list(ID=ID),median))
# df.location$RH.mean <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),mean))
# df.location$RH.sd <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),sd))
# df.location$RH.min <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),min))
# df.location$RH.median <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),median))
df.location$T.Sept.mean <- with(df.microclim[which(df.microclim$Time >= "2017-09-01 00" & df.microclim$Time <= "2017-09-30 22"),],tapply(Temp,list(ID=ID),mean))
df.location$T.Sept.sd <- with(df.microclim[which(df.microclim$Time >= "2017-09-01 00" & df.microclim$Time <= "2017-09-30 22"),],tapply(Temp,list(ID=ID),sd))
df.location$T.Oct.mean <- with(df.microclim[which(df.microclim$Time >= "2017-10-01 00" & df.microclim$Time <= "2017-10-31 22"),],tapply(Temp,list(ID=ID),mean))
df.location$T.Oct.sd <- with(df.microclim[which(df.microclim$Time >= "2017-10-01 00" & df.microclim$Time <= "2017-10-31 22"),],tapply(Temp,list(ID=ID),sd))
df.location$T.Nov.mean <- with(df.microclim[which(df.microclim$Time >= "2017-11-01 00" & df.microclim$Time <= "2017-11-30 22"),],tapply(Temp,list(ID=ID),mean))
df.location$T.Nov.sd <- with(df.microclim[which(df.microclim$Time >= "2017-11-01 00" & df.microclim$Time <= "2017-11-30 22"),],tapply(Temp,list(ID=ID),sd))
# df.location$d.WVP.a.mean <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),mean))
# df.location$d.WVP.a.sd <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),sd))
# df.location$d.WVP.a.median <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),median))
# df.location$d.WVP.a.min <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),min))
df.location$d.WVP.a.winter.mean <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),mean))
df.location$d.WVP.a.winter.sd <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),sd))
df.location$d.WVP.a.winter.min <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),min))
df.location$d.WVP.bat.mean <- with(df.microclim,tapply(d.WVP.bat,list(ID=ID),mean))
df.location$d.WVP.a.winter.median <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),median))

df.location$Elevation.scaled <- (df.location$Elevation-mean(df.location$Elevation))/sd(df.location$Elevation)



df.location$Room <- "NA"
df.location$Room[which(match(df.trim$ID[1:length(unique(df.location$iButton))],ID.drum)!="NA")] <- "Side"
df.location$Room[which(match(df.trim$ID[1:length(unique(df.location$iButton))],ID.upper)!="NA")] <- "Upper"
df.location$Room[which(match(df.trim$ID[1:length(unique(df.location$iButton))],ID.lower)!="NA")] <- "Lower"

xycoords.midpoint <- rbind(xycoords, 
                           c("midpoint",2660,2430, (xycoords$Elevation[which(xycoords$IDs==94)] + xycoords$Elevation[which(xycoords$IDs==88)])/2),
                           c("upper", mean(subset(xycoords$x_coord, subset = IDs %in% c(101, 103, 110, 116, 117))), mean(subset(xycoords$y_coord, subset = IDs %in% c(101, 103, 110, 116, 117))), mean(subset(xycoords$Elevation, subset = IDs %in% c(101, 103, 110, 116, 117)))),
                           c("lower", mean(subset(xycoords$x_coord, subset = IDs %in% c(72, 74, 75, 78, 88))), mean(subset(xycoords$y_coord, subset = IDs %in% c(72, 74, 75, 78, 88))), mean(subset(xycoords$Elevation, subset = IDs %in% c(72, 74, 75, 78, 88)))),
                           c("drum", mean(subset(xycoords$x_coord, subset = IDs %in% c(27, 31))), mean(subset(xycoords$y_coord, subset = IDs %in% c(27, 31))), mean(subset(xycoords$Elevation, subset = IDs %in% c(27, 31)))),
                           c("out.warm",3450,1500,1670),
                           c("out.cold",1600,2300, xycoords$Elevation[which(xycoords$IDs==40)])
)


xycoords.midpoint$x_coord <- as.numeric(as.character(xycoords.midpoint$x_coord))
xycoords.midpoint$y_coord <- as.numeric(as.character(xycoords.midpoint$y_coord))
xycoords.midpoint$Elevation <- as.numeric(as.character(xycoords.midpoint$Elevation))


contacts <- read.csv("LoggerNetwork.csv")
rownames(contacts) <- contacts[,1]
contacts <- contacts[,-1]
colnames(contacts) <- rownames(contacts)

logger.net <- NA

for(i in 1:length(contacts[1,])){
  for(j in 1:length(contacts[,1])){
    contacts[i,j] <- ifelse(contacts[i,j]=="NA","NA", sqrt((xycoords.midpoint$x_coord[j]-xycoords.midpoint$x_coord[i])^2 + (xycoords.midpoint$y_coord[j]-xycoords.midpoint$y_coord[i])^2 + (xycoords.midpoint$Elevation[j]-xycoords.midpoint$Elevation[i])^2))
    
    logger.net <- rbind(logger.net,c(rownames(contacts)[i],colnames(contacts)[j],contacts[i,j]))
  }
}
logger.net <- as.data.frame(logger.net)
colnames(logger.net) <- c("from","to","weight")
logger.net$weight <- as.numeric(as.character(logger.net$weight))

logger.net <- logger.net[-which(is.na(logger.net$weight)),]


library(igraph)
xycoords.midpoint$IDs <- rownames(contacts)
xycoords.midpoint$is.MYLU <- append(df.location$is.MYLU,c(2,3,3,3,4,4))
net <- graph.data.frame(logger.net,xycoords.midpoint,directed=F)
xycoords.midpoint$color <- "#D98594"
xycoords.midpoint$color[which(xycoords.midpoint$is.MYLU==1)] <- "#86C2DA"
xycoords.midpoint$color[which(xycoords.midpoint$is.MYLU==2)] <- "red"
xycoords.midpoint$color[which(xycoords.midpoint$is.MYLU==3)] <- "blue"
xycoords.midpoint$color[which(xycoords.midpoint$is.MYLU==4)] <- "dark green"
V(net)$color <- xycoords.midpoint$color

plot(net, layout=as.matrix(-xycoords.midpoint[,c("x_coord","y_coord")]), 
     edge.curved=0,
     vertex.color=V(net)$color,
     vertex.size=10,
     vertex.label=NA)

dist.path <- matrix(NA,ncol=ncol(contacts),nrow=nrow(contacts))
colnames(dist.path) <- colnames(contacts)
rownames(dist.path) <- colnames(dist.path)


for(i in 1:nrow(dist.path)){
  for(j in 1:ncol(dist.path)){
    dist.path[i,j] <- distances(net,v=rownames(dist.path)[i],to=colnames(dist.path)[j])
  }
}

for(i in 1:length(df.location$d.warm)){
  df.location$d.warm[i] <- distances(net,v=rownames(dist.path)[i],to="out.warm")
  df.location$d.cold[i] <- distances(net,v=rownames(dist.path)[i],to="out.cold")
}

dist.path.bats <- dist.path
dist.path.bats <- dist.path.bats[,-which(xycoords.midpoint$is.MYLU!=3)]
df.location$path.bats <- rep(NA, length(df.location$is.MYLU))
for(i in 1:length(df.location$path.bats)){
  df.location$path.bats[i] <- sum(1/dist.path.bats[i,])
}
plot(df.location$path.bats,df.location$is.MYLU)
plot(df.location$d.warm,df.location$is.MYLU)
plot(df.location$d.cold,df.location$is.MYLU)

df.Sept <- df.microclim[which(df.microclim$Time >= "2017-08-22" & df.microclim$Time <= "2017-09-30 22"),]
T.out.daily <- Temp.Daily[which(Temp.Daily$Day >= "2017-08-22" & Temp.Daily$Day <= "2017-09-30"),]

# T.out.daily <- cbind(T.out.daily, Daily.mean.Barton)

ggplot(df.Sept, aes(Time, Temp)) + geom_line(aes(color=Depth, group=ID)) +
  theme_classic() 

df.location$Airflow.High <- c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0)
df.location$Airflow.Medium <- c(0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,0,0,1,1,0,1,1)
df.location$Airflow.Low <- c(0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)

df.location$phi <- summary(zj$phi, mean)$stat






###############################################################################
# Create data outside data logger bounds by extrapolating with model parameters
###############################################################################

#Identify new sites to plot

df.pred <- as.data.frame(rbind(c("X1", 3450, 1550, 1580, 0, 0, 1, 0, 0, 1, 0, 0, 0.001, NA, NA, NA),
                               c("X3", 4000, 1600, 1580, 0, 0, 1, 0, 0, 1, 0, 0, 0.001, NA, NA, NA),
                               c("X4", 4000, 1500, 1580, 0, 0, 1, 0, 0, 1, 0, 0, 0.001, NA, NA, NA),
                               c("X5", 3200, 1200, 1580, 0, 0, 1, 0, 0, 1, 0, 0, 0.001, NA, NA, NA),
                               c("X2", 3600, 2200, 
                                  df.location$Elevation[which(df.location$iButton==38)], 
                                  NA, NA, 1, 0, 0, 1, 0, 0, NA, NA, NA, NA),
                               c("X8", 3400, 2400, df.location$Elevation[which(df.location$iButton==20)], NA, NA, 0, 0, 1, 0, 1, 0, NA, NA, NA, NA),
                               c("X9", 3300, 2600, df.location$Elevation[which(df.location$iButton==27)], NA, NA, 0, 0, 1, 0, 0, 1, NA, NA, NA, NA),
                               c("X10", 3400, 2400, df.location$Elevation[which(df.location$iButton==31)], NA, NA, 0, 0, 1, 0, 0, 1, NA, NA, NA, NA),
                               c("X11", 3000, 3000, 1100, NA, NA, 0, 0, 1, 0, 0, 1, NA, NA, NA, NA),
                               c("X12", 2200, 3300, 1100, NA, NA, 0, 1, 0, 0, 1, 0, NA, NA, NA, NA),
                               c("X13", 1600, 2900, 1200, NA, NA, 0, 1, 0, 1, 0, 0, NA, NA, NA, NA),
                               c("X14", 1400, 1900, 1300, NA, NA, 0, 1, 0, 1, 0, 0, NA, NA, NA, NA),
                               c("X15", 2200, 1700, 1450, NA, NA, 1, 0, 0, 1, 0, 0, NA, NA, NA, NA),
                               c("X16", 3200, 1700, 1550, NA, NA, 1, 0, 0, 1, 0, 0, NA, NA, NA, NA),
                               c("X17", 3400, 1700, 1500, NA, NA, 1, 0, 0, 1, 0, 0, NA, NA, NA, NA),
                               c("X18", 3100, 1800, 1500, NA, NA, 1, 0, 0, 1, 0, 0, NA, NA, NA, NA),
                               c("X19", 3300, 1750, 1500, NA, NA, 1, 0, 0, 1, 0, 0, NA, NA, NA, NA)))
colnames(df.pred) <- c("ID",
                        "X",
                        "Y",
                        "Elevation",
                        "d.warm",
                        "d.cold",
                        "Upper",
                        "Lower",
                        "Drum",
                        "Airflow.High",
                        "Airflow.Medium",
                        "Airflow.Low",
                        "path.bats",
                        "T.mean.pred",
                        "T.sd.Sept.pred",
                        "phi.pred")
df.pred$X <- as.numeric(as.character(df.pred$X))
df.pred$Y <- as.numeric(as.character(df.pred$Y))
df.pred$Elevation <- as.numeric(as.character(df.pred$Elevation))


# # Populate input data for predictions
for(i in 5:nrow(df.pred)){
  closest.node <- which(sqrt((df.pred$X[i]-xycoords.midpoint$x_coord)^2 + (df.pred$Y[i]-xycoords.midpoint$y_coord)^2 + (df.pred$Elevation[i]-xycoords.midpoint$Elevation)^2)==min(sqrt((df.pred$X[i]-xycoords.midpoint$x_coord)^2 + (df.pred$Y[i]-xycoords.midpoint$y_coord)^2 + (df.pred$Elevation[i]-xycoords.midpoint$Elevation)^2)))
  closest.node.dist <- sqrt((df.pred$X[i]-xycoords.midpoint$x_coord[closest.node])^2 + (df.pred$Y[i]-xycoords.midpoint$y_coord[closest.node])^2 + (df.pred$Elevation[i]-xycoords.midpoint$Elevation[closest.node])^2)
  df.pred$d.warm[i] <- closest.node.dist + dist.path[closest.node,39]
  df.pred$d.cold[i] <- closest.node.dist + dist.path[closest.node,40]
  df.pred$path.bats[i] <- 1/(closest.node.dist + dist.path.bats[closest.node,1]) + 1/(closest.node.dist + dist.path.bats[closest.node,2]) + 1/(closest.node.dist + dist.path.bats[closest.node,3])
}
df.pred$d.warm <- as.numeric(as.character(df.pred$d.warm))
df.pred$d.cold<- as.numeric(as.character(df.pred$d.cold))
df.pred$path.bats <- as.numeric(as.character(df.pred$path.bats))
df.pred$Upper <- as.numeric(as.character(df.pred$Upper))
df.pred$Lower <- as.numeric(as.character(df.pred$Lower))
df.pred$Drum <- as.numeric(as.character(df.pred$Drum))
df.pred$Airflow.High <- as.numeric(as.character(df.pred$Airflow.High))
df.pred$Airflow.Medium <- as.numeric(as.character(df.pred$Airflow.Medium))
df.pred$Airflow.Low <- as.numeric(as.character(df.pred$Airflow.Low))


# # Run through hierarchical model equations to predict T.mean, T.sd, and phi

mean.T.Sept.sd <- mean(summary(zj$T.Sept.sd.pred, mean)$stat)
sd.T.Sept.sd <- sd(summary(zj$T.Sept.sd.pred, mean)$stat)
mean.T.mean <- mean(summary(zj$T.mean.pred, mean)$stat)
sd.T.mean <- sd(summary(zj$T.mean.pred, mean)$stat)
mean.path.bats <- mean(df.location$path.bats)
sd.path.bats <- sd(df.location$path.bats)

start.Date <- which(Temp.Daily$Day==Temp.Daily$Day[which(Temp.Daily$Day=="2017-08-15") + which(Temp.Daily$T.AVG[Temp.Daily$Day >= "2017-08-16" & Temp.Daily$Day <= "2017-08-31"]==max(Temp.Daily$T.AVG[Temp.Daily$Day >= "2017-08-16" & Temp.Daily$Day <= "2017-08-31"]))])
end.Date <- which(Temp.Daily$Day=="2018-04-30")
# end.Date <- which(Temp.Daily$Day=="2017-09-30")

df.input <- df.microclim[which(df.microclim$Time >= "2017-08-22" & df.microclim$Time <= "2018-04-30 22"),]
# df.input <- df.Sept


start.MAST = start.Date #Day to start MAST
end.MAST = end.Date #Day to end MAST
T.history = Temp.Daily$T.AVG #Historical temps to calculate MAST analogue
# id.total = length(unique(df.input$ID)) #Number of data loggers
days = length(unique(df.input$day.ind)) #Number of days to evaluate
elev.drum.entry = (df.location$Elevation[which(df.location$iButton==38)]-1100)/1600 #Elev. of drum entrance
elev.midpoint = ((max(df.location$Elevation[which(df.location$Room=="Lower")]) + min(df.location$Elevation[which(df.location$Room=="Upper")]))/2-1100)/1600 #elevation of temp settling midpoint
# elevation = df.location$Elevation, #Data logger elevation
Window.size = 365 #Period of time to average to determine MAST
elev.upper.entry = (1580-1100)/1600
elev.lower.entry = (max(df.location$Elevation[which(df.location$Room=="Lower")])-1100)/1600
Sept.date.index = which(T.out.daily$Day>="2017-09-01" & T.out.daily$Day<="2017-09-30")
n.logistic = 5
Outside.mean.daily = as.numeric(as.character(Outside.mean.daily$Temp))
i.drument = which(df.location$iButton==38)




for(i in 1:nrow(df.pred)){
  
elevation.scaled = (df.pred$Elevation[i]-1100)/1600

lambda.1.warm <- summary(zj$lambda.1.warm, mean)$stat
lambda.2.warm <- summary(zj$lambda.2.warm, mean)$stat
lambda.3.warm <- summary(zj$lambda.3.warm, mean)$stat
lambda.4.warm <- summary(zj$lambda.4.warm, mean)$stat
lambda.1.cold <- summary(zj$lambda.1.cold, mean)$stat
lambda.2.cold <- summary(zj$lambda.2.cold, mean)$stat
lambda.3.cold <- summary(zj$lambda.3.cold, mean)$stat
lambda.4.cold <- summary(zj$lambda.4.cold, mean)$stat

lambda.wall.warm <- summary(zj$lambda.wall.warm, mean)$stat
lambda.wall.cold <- summary(zj$lambda.wall.cold, mean)$stat

nu.warm <- summary(zj$nu.warm, mean)$stat
nu.cold <- summary(zj$nu.cold, mean)$stat

alpha.1.warm <- lambda.1.warm[1] + lambda.1.warm[2]*df.pred$d.warm[i] + lambda.1.warm[3]*df.pred$d.cold[i] + lambda.1.warm[4]*(df.pred$Elevation[i]) + lambda.1.warm[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.1.warm[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.1.warm[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.1.warm[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.1.warm[9]*df.pred$Upper[i] + lambda.1.warm[10]*df.pred$Lower[i] + lambda.1.warm[11]*df.pred$Drum[i]
alpha.2.warm <- lambda.2.warm[1] + lambda.2.warm[2]*df.pred$d.warm[i] + lambda.2.warm[3]*df.pred$d.cold[i] + lambda.2.warm[4]*(df.pred$Elevation[i]) + lambda.2.warm[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.2.warm[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.2.warm[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.2.warm[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.2.warm[9]*df.pred$Upper[i] + lambda.2.warm[10]*df.pred$Lower[i] + lambda.2.warm[11]*df.pred$Drum[i]
alpha.3.warm <- lambda.3.warm[1] + lambda.3.warm[2]*df.pred$d.warm[i] + lambda.3.warm[3]*df.pred$d.cold[i] + lambda.3.warm[4]*(df.pred$Elevation[i]) + lambda.3.warm[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.3.warm[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.3.warm[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.3.warm[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.3.warm[9]*df.pred$Upper[i] + lambda.3.warm[10]*df.pred$Lower[i] + lambda.3.warm[11]*df.pred$Drum[i]
alpha.4.warm <- lambda.4.warm[1] + lambda.4.warm[2]*df.pred$d.warm[i] + lambda.4.warm[3]*df.pred$d.cold[i] + lambda.4.warm[4]*(df.pred$Elevation[i]) + lambda.4.warm[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.4.warm[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.4.warm[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.4.warm[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.4.warm[9]*df.pred$Upper[i] + lambda.4.warm[10]*df.pred$Lower[i] + lambda.4.warm[11]*df.pred$Drum[i]
alpha.1.cold <- lambda.1.cold[1] + lambda.1.cold[2]*df.pred$d.warm[i] + lambda.1.cold[3]*df.pred$d.cold[i] + lambda.1.cold[4]*(df.pred$Elevation[i]) + lambda.1.cold[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.1.cold[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.1.cold[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.1.cold[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.1.cold[9]*df.pred$Upper[i] + lambda.1.cold[10]*df.pred$Lower[i] + lambda.1.cold[11]*df.pred$Drum[i]
alpha.2.cold <- lambda.2.cold[1] + lambda.2.cold[2]*df.pred$d.warm[i] + lambda.2.cold[3]*df.pred$d.cold[i] + lambda.2.cold[4]*(df.pred$Elevation[i]) + lambda.2.cold[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.2.cold[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.2.cold[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.2.cold[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.2.cold[9]*df.pred$Upper[i] + lambda.2.cold[10]*df.pred$Lower[i] + lambda.2.cold[11]*df.pred$Drum[i]
alpha.3.cold <- lambda.3.cold[1] + lambda.3.cold[2]*df.pred$d.warm[i] + lambda.3.cold[3]*df.pred$d.cold[i] + lambda.3.cold[4]*(df.pred$Elevation[i]) + lambda.3.cold[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.3.cold[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.3.cold[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.3.cold[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.3.cold[9]*df.pred$Upper[i] + lambda.3.cold[10]*df.pred$Lower[i] + lambda.3.cold[11]*df.pred$Drum[i]
alpha.4.cold <- lambda.4.cold[1] + lambda.4.cold[2]*df.pred$d.warm[i] + lambda.4.cold[3]*df.pred$d.cold[i] + lambda.4.cold[4]*(df.pred$Elevation[i]) + lambda.4.cold[5]*df.pred$d.warm[i]*df.pred$d.cold[i] + lambda.4.cold[6]*df.pred$d.warm[i]*df.pred$Elevation[i] + lambda.4.cold[7]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.4.cold[8]*df.pred$d.warm[i]*df.pred$d.cold[i]*df.pred$Elevation[i] + lambda.4.cold[9]*df.pred$Upper[i] + lambda.4.cold[10]*df.pred$Lower[i] + lambda.4.cold[11]*df.pred$Drum[i]

MAST <- rep(NA, days)
for(d in start.MAST:end.MAST){
  MAST[d-start.MAST+1] <- mean(T.history[(d-Window.size+1):d])
}
MAST.adjust <- summary(zj$MAST.adjust, mean)$stat
MAST.analog <- MAST-MAST.adjust
mu.temp.out <- rep(NA, days)
mu.temp.out.lag <- rep(NA, days)

for(d in 1:days){
  MAST.analog[d] <- MAST[d] - MAST.adjust #define local MAST equivalent
  mu.temp.out[d] <- Outside.mean.daily[d+3] #define outdoor conditions
  mu.temp.out.lag[d] <- mean(Outside.mean.daily[d:d+3])
}

mu.temp.out <- as.numeric(as.character(mu.temp.out))

#A couple prior days to get lag rolling
mu.temp.out.neg1 <- Outside.mean.daily[3]
mu.temp.out.neg2 <- Outside.mean.daily[2]

temp.midpoint <- summary(zj$temp.midpoint, mean)$stat
beta.lower <- summary(zj$beta.lower, mean)$stat
lambda.upper <- summary(zj$lambda.upper, mean)$stat
lambda.drum <- summary(zj$lambda.drum, mean)$stat
gamma.warm <- summary(zj$gamma.warm, mean)$stat
gamma.cold <- summary(zj$gamma.cold, mean)$stat

mu.temp <- rep(NA,days)
mu.temp[1] <- (df.pred$Upper[i]*(1-df.pred$Drum[i])*(1-df.pred$Lower[i])) * ((mu.temp.out.lag[1]-temp.midpoint)*exp(-lambda.upper*(elev.upper.entry-elevation.scaled))+temp.midpoint) + (df.pred$Drum[i]*(1-df.pred$Upper[i])*(1-df.pred$Lower[i])) * ((temp.midpoint-MAST.analog[1])*exp(-lambda.drum*(elev.drum.entry-elevation.scaled))+MAST.analog[1]) + (df.pred$Lower[i]*(1-df.pred$Drum[i])*(1-df.pred$Upper[i])) * (temp.midpoint+beta.lower*(elevation.scaled-elev.lower.entry))
mu.temp[2] <- (nu.warm[1]*df.pred$Airflow.High[i]+nu.warm[2]*df.pred$Airflow.Medium[i]+nu.warm[3]*df.pred$Airflow.Low[i])*(mu.temp[1] + alpha.1.warm*(mu.temp.out[2]-mu.temp.out[1]) + alpha.2.warm*(mu.temp.out[1]-mu.temp.out.neg1) + alpha.3.warm*(mu.temp.out.neg1-mu.temp.out.neg2) + alpha.4.warm*(MAST.analog[2]-MAST.analog[1]) + gamma.warm[1]*df.pred$d.warm[i] + gamma.warm[2]*df.pred$d.cold[i] + gamma.warm[3]*df.pred$Elevation[i] + gamma.warm[4]*mu.temp.out[2] + gamma.warm[5]*mu.temp.out[1] + gamma.warm[6]*mu.temp.out.neg1 + gamma.warm[7]*mu.temp.out.neg2 + gamma.warm[8]*MAST.analog[1] + gamma.warm[9]*MAST.analog[2]) + (1-(nu.warm[1]*df.pred$Airflow.High[i]+nu.warm[2]*df.pred$Airflow.Medium[i]+nu.warm[3]*df.pred$Airflow.Low[i]))*((mu.temp[1]-MAST.analog[2])*exp(-lambda.wall.warm)+MAST.analog[2])
mu.temp[3] <- (nu.warm[1]*df.pred$Airflow.High[i]+nu.warm[2]*df.pred$Airflow.Medium[i]+nu.warm[3]*df.pred$Airflow.Low[i])*(mu.temp[2] + alpha.1.warm*(mu.temp.out[3]-mu.temp.out[2]) + alpha.2.warm*(mu.temp.out[2]-mu.temp.out[1]) + alpha.3.warm*(mu.temp.out[1]-mu.temp.out.neg1) + alpha.4.warm*(MAST.analog[3]-MAST.analog[2]) + gamma.warm[1]*df.pred$d.warm[i] + gamma.warm[2]*df.pred$d.cold[i] + gamma.warm[3]*df.pred$Elevation[i] + gamma.warm[4]*mu.temp.out[3] + gamma.warm[5]*mu.temp.out[2] + gamma.warm[6]*mu.temp.out[1] + gamma.warm[7]*mu.temp.out.neg1 + gamma.warm[8]*MAST.analog[2] + gamma.warm[9]*MAST.analog[3]) + (1-(nu.warm[1]*df.pred$Airflow.High[i]+nu.warm[2]*df.pred$Airflow.Medium[i]+nu.warm[3]*df.pred$Airflow.Low[i]))*((mu.temp[2]-MAST.analog[3])*exp(-lambda.wall.warm)+MAST.analog[3])

for(d in 4:days){
  mu.temp[d] <- ifelse(mu.temp.out[d]>=MAST.analog[d], 
                       (nu.warm[1]*df.pred$Airflow.High[i]+nu.warm[2]*df.pred$Airflow.Medium[i]+nu.warm[3]*df.pred$Airflow.Low[i])*(mu.temp[(d-1)] + alpha.1.warm*(mu.temp.out[d]-mu.temp.out[(d-1)]) + alpha.2.warm*(mu.temp.out[(d-1)]-mu.temp.out[(d-2)]) + alpha.3.warm*(mu.temp.out[(d-2)]-mu.temp.out[(d-3)]) + alpha.4.warm*(MAST.analog[d]-MAST.analog[d-1]) + gamma.warm[1]*df.pred$d.warm[i] + gamma.warm[2]*df.pred$d.cold[i] + gamma.warm[3]*df.pred$Elevation[i] + gamma.warm[4]*mu.temp.out[d] + gamma.warm[5]*mu.temp.out[d-1] + gamma.warm[6]*mu.temp.out[d-2] + gamma.warm[7]*mu.temp.out[d-3] + gamma.warm[8]*MAST.analog[d-1] + gamma.warm[9]*MAST.analog[d]) + (1-(nu.warm[1]*df.pred$Airflow.High[i]+nu.warm[2]*df.pred$Airflow.Medium[i]+nu.warm[3]*df.pred$Airflow.Low[i]))*((mu.temp[d-1]-MAST.analog[d])*exp(-lambda.wall.warm)+MAST.analog[d]), 
                       (nu.cold[1]*df.pred$Airflow.High[i]+nu.cold[2]*df.pred$Airflow.Medium[i]+nu.cold[3]*df.pred$Airflow.Low[i])*(mu.temp[(d-1)] + alpha.1.cold*(mu.temp.out[d]-mu.temp.out[(d-1)]) + alpha.2.cold*(mu.temp.out[(d-1)]-mu.temp.out[(d-2)]) + alpha.3.cold*(mu.temp.out[(d-2)]-mu.temp.out[(d-3)]) + alpha.4.cold*(MAST.analog[d]-MAST.analog[d-1]) + gamma.cold[1]*df.pred$d.warm[i] + gamma.cold[2]*df.pred$d.cold[i] + gamma.cold[3]*df.pred$Elevation[i] + gamma.cold[4]*mu.temp.out[d] + gamma.cold[5]*mu.temp.out[d-1] + gamma.cold[6]*mu.temp.out[d-2] + gamma.cold[7]*mu.temp.out[d-3] + gamma.cold[8]*MAST.analog[d-1] + gamma.cold[9]*MAST.analog[d]) + (1-(nu.cold[1]*df.pred$Airflow.High[i]+nu.cold[2]*df.pred$Airflow.Medium[i]+nu.cold[3]*df.pred$Airflow.Low[i]))*((mu.temp[d-1]-MAST.analog[d])*exp(-lambda.wall.cold)+MAST.analog[d]))
}

df.pred$T.mean.pred[i] <- mean(mu.temp)
df.pred$T.sd.Sept.pred[i] <- ifelse(sd(mu.temp[Sept.date.index])==0, 0.000001, sd(mu.temp[Sept.date.index]))


# # Logistic prediction
B <- summary(zj$B, mean)$stat
d.warm.scale <- (df.pred$d.warm[i]-mean(df.location$d.warm))/sd(df.location$d.warm)
T.mean.pred.scale <- (as.numeric(as.character(df.pred$T.mean.pred[i]))-mean.T.mean)/sd.T.mean
T.sd.Sept.pred.scale <- (as.numeric(as.character(df.pred$T.sd.Sept.pred[i]))-mean.T.Sept.sd)/sd.T.Sept.sd
path.bats.scale <- (as.numeric(as.character(df.pred$path.bats[i]))-mean.path.bats)/sd.path.bats

df.pred$phi.pred[i] <- 1/(1+exp(-(B[1] + B[2]*d.warm.scale + B[3]*T.mean.pred.scale + B[4]*T.sd.Sept.pred.scale + B[5]*path.bats.scale)))

}


######################################
# Create spatial file
#######################################


Barton.hole1 <- as.data.frame(read.csv("BartonSpatialFiles/BartonHole1.csv"))
Barton.hole2 <- as.data.frame(read.csv("BartonSpatialFiles/BartonHole2.csv"))
Barton.hole3 <- as.data.frame(read.csv("BartonSpatialFiles/BartonHole3.csv"))
Barton.hole4 <- as.data.frame(read.csv("BartonSpatialFiles/BartonHole4.csv"))
colnames(Barton.hole1) <- c("X","Y")
Barton.coords <- as.data.frame(read.csv("BartonSpatialFiles/BartonEdgeCoords.csv"))
colnames(Barton.coords) <- c("X","Y")
Barton.coords$X <- as.numeric(as.character(Barton.coords$X))
Barton.coords$Y <- as.numeric(as.character(Barton.coords$Y))
Barton.coords$Value <- 1
outer <- as.data.frame(cbind(c(min(Barton.coords$X)-200,
                                 min(Barton.coords$X)-200,
                                 max(Barton.coords$X)+200,
                                 max(Barton.coords$X)+200,
                                 min(Barton.coords$X)-200),
                               c(max(Barton.coords$Y)+200,
                                 min(Barton.coords$Y)-200,
                                 min(Barton.coords$Y)-200,
                                 max(Barton.coords$Y)+200,
                                 max(Barton.coords$Y)+200)))
colnames(outer) <- c("X","Y")

hole <- as.data.frame(Barton.coords[,c("X","Y")])

outer$subid <- 1L
hole$subid <- 2L
poly.Barton <- rbind(outer,hole)


ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  xlab("") + ylab("") +
  coord_fixed(ratio=1)





# # Plot cave map with points for loggers and insets of temperature traces


chosen.logger <- c(which(df.location$iButton==5), which(df.location$iButton==27), which(df.location$iButton==74))
choose <- 1
  i <- chosen.logger[choose]
  
  mean.temp <- rep(NA,length(unique(df.input$day.ind)))
  for(d in unique(df.input$day.ind)[1]:unique(df.input$day.ind)[length(unique(df.input$day.ind))]){
    mean.temp[d-unique(df.input$day.ind)[1]+1] <- mean(df.input$Temp[which(df.input$day.ind==d & df.input$ID==unique(df.input$ID)[i])])
  }
  
  df.seq <- cbind(summary(zj$mu.temp,mean)$stat[,i],
                  t(summary(zj$mu.temp,quantile,c(0.025,0.975))$stat[,,i]),
                  summary(zj$temp.hat,mean, na.rm=TRUE)$stat[,i],
                  t(summary(zj$temp.hat,quantile,c(0.025,0.975), na.rm=TRUE)$stat[,,i]),
                  mean.temp)
  colnames(df.seq) <- c("mu.mean","mu.low","mu.high","temp.hat.mean","temp.hat.low","temp.hat.high","data")
  Date <- as.Date(Temp.Daily[start.Date:end.Date,1])
  
  df.seq <- as.data.frame(df.seq)
  df.seq$Date <- Date
  
  p1 <- ggplot(df.seq) + theme_classic() + ylab("Temperature (Celsius)") + xlab("Date") + 
    # ggtitle(IDs[i]) + 
    # geom_line(aes(Date,mu.mean),color="red") + 
    # geom_ribbon(aes(Date,ymin=mu.low,ymax=mu.high),color="red",alpha=0.3) + 
    geom_line(aes(Date,temp.hat.mean),
              color=ghibli_palettes$LaputaMedium[4],
              size=1) + 
    geom_ribbon(aes(Date,ymin=temp.hat.low,ymax=temp.hat.high),
                color=ghibli_palettes$LaputaMedium[4],
                alpha=0.3) +
    geom_point(aes(Date,data), size=0.5) +
    theme(plot.background = element_rect(colour =ghibli_palettes$LaputaMedium[4], fill="white", size=1),
          panel.background = element_rect(fill="white")) +
    geom_text(aes(as.Date("2018-3-15"), 14, label="1"), color="black", size=5)
  
  p2 <- ggplot(df.seq) + theme_classic() + ylab("Temperature (Celsius)") + xlab("Date") + 
    # ggtitle(IDs[i]) + 
    # geom_line(aes(Date,mu.mean),color="red") + 
    # geom_ribbon(aes(Date,ymin=mu.low,ymax=mu.high),color="red",alpha=0.3) + 
    geom_line(aes(Date,temp.hat.mean),
              color=ghibli_palettes$LaputaMedium[4],
              size=1) + 
    geom_ribbon(aes(Date,ymin=temp.hat.low,ymax=temp.hat.high),
                color=ghibli_palettes$LaputaMedium[4],
                alpha=0.3) +
    geom_point(aes(Date,data), size=0.5) +
    theme(plot.background = element_rect(colour =ghibli_palettes$LaputaMedium[4], fill="white", size=1),
          panel.background = element_rect(fill="white")) +
    geom_text(aes(as.Date("2018-3-15"), 6, label="2"), color="black", size=5)

  p3 <- ggplot(df.seq) + theme_classic() + ylab("Temperature (Celsius)") + xlab("Date") + 
    # ggtitle(IDs[i]) + 
    # geom_line(aes(Date,mu.mean),color="red") + 
    # geom_ribbon(aes(Date,ymin=mu.low,ymax=mu.high),color="red",alpha=0.3) + 
    geom_line(aes(Date,temp.hat.mean),
              color=ghibli_palettes$LaputaMedium[4],
              size=1) + 
    geom_ribbon(aes(Date,ymin=temp.hat.low,ymax=temp.hat.high),
                color=ghibli_palettes$LaputaMedium[4],
                alpha=0.3) +
    geom_point(aes(Date,data), size=0.5) +
    theme(plot.background = element_rect(colour =ghibli_palettes$LaputaMedium[4], fill="white", size=1),
          panel.background = element_rect(fill="white")) +
    geom_text(aes(as.Date("2018-3-15"), 7.5, label="3"), color="black", size=5)




main <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  geom_point(data=df.location, 
             aes(x_coord, y_coord, color=as.factor(is.MYLU)),
             size=3) +
  scale_color_manual(values=c(ghibli_palettes$MarnieMedium1[5],ghibli_palettes$MarnieMedium1[3]),
                     name="M. lucifugus\nwinter roost",
                     labels=c("No","Yes")) +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  geom_point(data=df.location[chosen.logger,], 
           aes(x_coord, y_coord), 
           color=ghibli_palettes$LaputaMedium[4],
           size=5,
           pch=0,
           stroke=2) +
  geom_point(data=df.location[chosen.logger,], 
             aes(x_coord+90, y_coord+90), 
             color="white",
             size=5,
             pch=15,
             stroke=2) +
  geom_point(data=df.location[chosen.logger,], 
             aes(x_coord+90, y_coord+90), 
             color=ghibli_palettes$LaputaMedium[4],
             size=5,
             pch=0,
             stroke=2) +
  geom_text(data=df.location[chosen.logger,], 
            aes(x_coord+90, y_coord+90, label=c(1,2,3)), 
            color="black",
            size=5)


plot.with.inset <-
  ggdraw() +
  draw_plot(main) +
  draw_plot(p1, x = 0.5, y = 0.65, width = .4, height = .32) +
  draw_plot(p2, x = 0.0, y = 0.0, width = .4, height = .32) +
  draw_plot(p3, x = 0.55, y = 0.0, width = .4, height = .32)





# # Dataframe for variable of interest to plot

df.plot <- as.data.frame(rbind(cbind(df.location$x_coord, df.location$y_coord, df.location$phi),
                               cbind(df.pred$X, df.pred$Y, df.pred$phi.pred)))
colnames(df.plot) <- c("x_coord","y_coord","phi")
df.plot$x_coord <- as.numeric(as.character(df.plot$x_coord))
df.plot$y_coord <- as.numeric(as.character(df.plot$y_coord))
df.plot$phi <- as.numeric(as.character(df.plot$phi))

z <- akima::interp(df.plot$x_coord, df.plot$y_coord, df.plot$phi, 
            extrap = TRUE, duplicate = "mean")

df.z <- subset(data.frame(x = rep(z$x, nrow(z$z)),
                  y = rep(z$y, each = ncol(z$z)),
                  z = as.numeric(z$z)),
       !is.na(z))
col.phi <- colorRampPalette(ghibli_palettes$LaputaMedium[2:7])(10)
col.phi <- c("#1D2645", "#302D59", "#43376D", "#524C84", "#6E659B", "#9B86B4", "#B0AACB",
             "#B3D2E0", "#CED8B5", "#F0D77B")

p3 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  # geom_point(data=df.plot, aes(x=x, y=y, color=phi)) +
  geom_contour_filled(data=df.z, aes(x=x, y=y, z=z)) +
  scale_fill_gradientn(colors = col.phi,
                       super = metR::ScaleDiscretised) +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="Hierarchical\nmodel roosting\nprobability") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) 


df.z <- as.data.frame(read.csv("2047roost.csv"))
col.phi <- colorRampPalette(ghibli_palettes$LaputaMedium[2:7])(10)
col.phi <- c("#1D2645", "#302D59", "#43376D", "#524C84", "#6E659B", "#9B86B4", "#B0AACB",
             "#B3D2E0", "#CED8B5", "#F0D77B")

p4 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  # geom_point(data=df.plot, aes(x=x, y=y, color=phi)) +
  geom_contour_filled(data=df.z, aes(x=x, y=y, z=z)) +
  scale_fill_gradientn(colors = col.phi,
                       super = metR::ScaleDiscretised) +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="Hierarchical\nmodel roosting\nprobability") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) 




# # Plot phi output standard deviation for data and prediction model
load("LogisticDataOutput211120.rda")
outsampledata <- read.csv("LogisticOutSamplePreds.csv")
outsampledata$iButton <- as.numeric(as.character(outsampledata$iButton))

df.location$phi.insampmean <- summary(zj.data$mu, mean)$stat
df.location$phi.insamplow <- summary(zj.data$mu, quantile, 0.025)$stat
df.location$phi.insamphigh <- summary(zj.data$mu, quantile, 0.975)$stat

df.location$philow <- summary(zj$phi, quantile, 0.025)$stat
df.location$phihigh <- summary(zj$phi, quantile, 0.975)$stat

df.location$phi.outsampmean <- NA
df.location$phi.outsamplow <- NA
df.location$phi.outsamphigh <- NA

for(i in 1:nrow(df.location)){
  df.site <- outsampledata[which(outsampledata$iButton==df.location$iButton[i]),]
  print(unique(df.site$iButton))
  df.location$phi.outsampmean[i] <- mean(df.site$mu.pred.mean)
  df.location$phi.outsamplow[i] <- confint(lm(mu.pred.mean ~ 1, df.site), level=0.95)[1]
  df.location$phi.outsamphigh[i] <- confint(lm(mu.pred.mean ~ 1, df.site), level=0.95)[2]
}

df.figures <- as.data.frame(rbind(cbind(df.location$iButton, df.location$phi, df.location$philow,df.location$phihigh,1, df.location$Room),
                                cbind(df.location$iButton, df.location$phi.insampmean, df.location$phi.insamplow,df.location$phi.insamphigh,2, df.location$Room),
                                cbind(df.location$iButton, df.location$phi.outsampmean,df.location$phi.outsamplow,df.location$phi.outsamphigh,3, df.location$Room)))
colnames(df.figures) <- c("iButton","phi","low","high","pred.type","room")
df.figures$pred.type <- as.factor(df.figures$pred.type)
df.figures$iButton <- factor(df.figures$iButton, levels=c("101", "103", "110", "116", "117", "72", "74", "75", "78", "88", "27", "31","3","4","5","6","7","13","38","40","94","96","45","46","50","58","64","65","67","68","20","22","23","26"))
df.figures$room <- factor(df.figures$room, levels=c("Upper","Lower","Side"))
df.figures$high <- as.numeric(as.character(df.figures$high))
df.figures$low <- as.numeric(as.character(df.figures$low))
df.figures$phi <- as.numeric(as.character(df.figures$phi))
df.figures$high[df.figures$high>1] <- 1
df.figures$low[df.figures$low<0] <- 0

df.figures.roost <- df.figures[df.figures$iButton %in% c("101", "103", "110", "116", "117", "72", "74", "75", "78", "88", "27", "31"),]
df.figures.noroost<- df.figures[df.figures$iButton %in% c("3","4","5","6","7","13","38","40","94","96","45","46","50","58","64","65","67","68","20","22","23","26"),]

col.mon <- ghibli_palettes$MononokeMedium[c(2,3,6)]

p1 <- ggplot(data=df.figures.roost) + theme_classic() + 
  ylab("P(Roost)\nMYLU present") + xlab("Microsite") +
  ylim(0,1) +
  geom_point(aes(iButton, phi, group=pred.type, color=pred.type, shape=pred.type), 
             position=position_dodge(width=0.75)) + 
  scale_color_manual(values=col.mon,
                     labels=c("Logistic in sample","Logistic out of sample","Hierarchical"),
                     name="Prediction method") + 
  scale_shape_manual(values=c(16,17,15),
                     labels=c("Logistic in sample","Logistic out of sample","Hierarchical"),
                     name="Prediction method") +
  geom_errorbar(aes(x=iButton, ymin=low, ymax=high, group=pred.type, color=pred.type), 
             position=position_dodge(width=0.75), width=0.5) +
  facet_grid(. ~ room, scales = "free", space='free')



p2 <- ggplot(data=df.figures.noroost) + theme_classic() + 
  ylab("P(Roost)\nMYLU absent") + xlab("Microsite") +
  ylim(0,1) +
  geom_point(aes(iButton, phi, group=pred.type, color=pred.type, shape=pred.type), 
             position=position_dodge(width=0.75)) + 
  scale_color_manual(values=col.mon,
                     labels=c("Logistic in sample","Logistic out of sample","Hierarchical"),
                     name="Prediction method") + 
  scale_shape_manual(values=c(16,17,15),
                     labels=c("Logistic in sample","Logistic out of sample","Hierarchical"),
                     name="Prediction method") +
  geom_errorbar(aes(x=iButton, ymin=low, ymax=high, group=pred.type, color=pred.type), 
                position=position_dodge(width=0.75), width=0.5) +
  facet_grid(. ~ room, scales = "free", space='free') +
  theme(legend.position = "none") 


p34 <- plot_grid(p3 +theme(legend.position = "none") + xlab("2017 Prediction"),
                 p4 +theme(legend.position = "none") + xlab("2047 Forecast"),
                 get_legend(p3),
                 ncol=3,
                 labels=c("c","d",""))

plot_grid(p1,p2,p34, ncol=1, labels=c("a","b",""), rel_heights=c(0.4,0.4,1))








# # Plot measured/predicted temperature mean/Sept SD

df.plot <- as.data.frame(rbind(cbind(df.location$x_coord, df.location$y_coord,
                               df.location$T.mean, df.location$T.Sept.sd,
                               summary(zj$T.mean.pred, mean)$stat,
                               summary(zj$T.Sept.sd.pred, mean)$stat),
                               cbind(df.pred$X, df.pred$Y, NA, NA, df.pred$T.mean.pred, df.pred$T.sd.Sept.pred)))
colnames(df.plot) <- c("x_coord","y_coord","T.mean","T.sd.Sept", "T.mean.pred","T.sd.Sept.pred")

df.plot$x_coord <- as.numeric(as.character(df.plot$x_coord))
df.plot$y_coord <- as.numeric(as.character(df.plot$y_coord))
df.plot$phi <- as.numeric(as.character(df.plot$T.mean))
df.plot.2 <- as.data.frame(df.plot[1:34,])

z <- akima::interp(df.plot$x_coord[which(is.na(df.plot$T.mean)==FALSE)], 
                   df.plot$y_coord[which(is.na(df.plot$T.mean)==FALSE)], 
                   df.plot$phi[which(is.na(df.plot$T.mean)==FALSE)], 
                   extrap = TRUE, duplicate = "mean")

df.z <- subset(data.frame(x = rep(z$x, nrow(z$z)),
                          y = rep(z$y, each = ncol(z$z)),
                          z = as.numeric(z$z)),
               !is.na(z))
col.phi <- colorRampPalette(rev(ghibli_palettes$SpiritedMedium[3:5]))(10)

p1 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  geom_point(data=df.plot.2, aes(x=as.numeric(as.character(x_coord)), 
                                      y=as.numeric(as.character(y_coord)), 
                                      color=as.numeric(as.character(T.mean))),
             size=3)+
  scale_color_gradient2(low=ghibli_palettes$SpiritedMedium[5],
                        mid=ghibli_palettes$SpiritedMedium[4],
                        high=ghibli_palettes$SpiritedMedium[3],
                        midpoint=mean(c(min(as.numeric(as.character(df.plot$T.mean.pred))),
                                        max(as.numeric(as.character(df.plot$T.mean.pred))))),
                       limits=c(-2, 13)) +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="Mean hibernation\ntemperature") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) 




df.plot$x_coord <- as.numeric(as.character(df.plot$x_coord))
df.plot$y_coord <- as.numeric(as.character(df.plot$y_coord))
df.plot$phi <- as.numeric(as.character(df.plot$T.mean.pred))

z <- akima::interp(df.plot$x_coord[which(is.na(df.plot$T.mean.pred)==FALSE)], 
                   df.plot$y_coord[which(is.na(df.plot$T.mean.pred)==FALSE)], 
                   df.plot$phi[which(is.na(df.plot$T.mean.pred)==FALSE)], 
                   extrap = TRUE, duplicate = "mean")

df.z <- subset(data.frame(x = rep(z$x, nrow(z$z)),
                          y = rep(z$y, each = ncol(z$z)),
                          z = as.numeric(z$z)),
               !is.na(z))
col.phi <- colorRampPalette(rev(ghibli_palettes$SpiritedMedium[3:5]))(10)

p2 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  geom_contour_filled(data=df.z, aes(x=x, y=y, z=z)) +
  scale_fill_gradientn(colours = col.phi,
                       super = metR::ScaleDiscretised,
                       limits=c(-2, 13)) +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="Mean hibernation\ntemperature") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) 




df.plot$x_coord <- as.numeric(as.character(df.plot$x_coord))
df.plot$y_coord <- as.numeric(as.character(df.plot$y_coord))
df.plot$phi <- as.numeric(as.character(df.plot$T.sd.Sept))

z <- akima::interp(df.plot$x_coord[which(is.na(df.plot$T.sd.Sept)==FALSE)], 
                   df.plot$y_coord[which(is.na(df.plot$T.sd.Sept)==FALSE)], 
                   df.plot$phi[which(is.na(df.plot$T.sd.Sept)==FALSE)], 
                   extrap = TRUE, duplicate = "mean")

df.z <- subset(data.frame(x = rep(z$x, nrow(z$z)),
                          y = rep(z$y, each = ncol(z$z)),
                          z = as.numeric(z$z)),
               !is.na(z))
col.phi <- colorRampPalette(ghibli_palettes$MarnieMedium2[1:7])(10)

p3 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  geom_point(data=df.plot.2, aes(x=as.numeric(as.character(x_coord)), 
                                 y=as.numeric(as.character(y_coord)), 
                                 color=as.numeric(as.character(T.sd.Sept))),
             size=3)+
  scale_color_gradientn(colors=col.phi,
                     limits=c(0, 1.9)) +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="September\ntemperature\nstandard deviation") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) 



df.plot$x_coord <- as.numeric(as.character(df.plot$x_coord))
df.plot$y_coord <- as.numeric(as.character(df.plot$y_coord))
df.plot$phi <- as.numeric(as.character(df.plot$T.sd.Sept.pred))

z <- akima::interp(df.plot$x_coord[which(is.na(df.plot$T.sd.Sept.pred)==FALSE)], 
                   df.plot$y_coord[which(is.na(df.plot$T.sd.Sept.pred)==FALSE)], 
                   df.plot$phi[which(is.na(df.plot$T.sd.Sept.pred)==FALSE)], 
                   extrap = TRUE, duplicate = "mean")

df.z <- subset(data.frame(x = rep(z$x, nrow(z$z)),
                          y = rep(z$y, each = ncol(z$z)),
                          z = as.numeric(z$z)),
               !is.na(z))
col.phi <- colorRampPalette(ghibli_palettes$MarnieMedium2[1:7])(10)

p4 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  geom_contour_filled(data=df.z, aes(x=x, y=y, z=z)) +
  scale_fill_gradientn(colours = col.phi,
                       super = metR::ScaleDiscretised,
                       limits=c(0, 1.9)) +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="September\ntemperature\nstandard deviation") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) 



df.z <- as.data.frame(read.csv("2047mean.csv"))
colnames(df.z) <- c("","x","y","z")
col.phi <- colorRampPalette(rev(ghibli_palettes$SpiritedMedium[3:5]))(10)

p5 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  geom_contour_filled(data=df.z, aes(x=x, y=y, z=z)) +
  scale_fill_gradientn(colours = col.phi,
                       super = metR::ScaleDiscretised,
                       limits=c(-2, 13)) +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="Mean hibernation\ntemperature") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

df.z <- read.csv("2047sd.csv")
colnames(df.z) <- c("","x","y","z")
col.phi <- colorRampPalette(ghibli_palettes$MarnieMedium2[1:7])(10)

p6 <- ggplot(poly.Barton, aes(x = X, y = Y)) +
  geom_polygon(data=outer, aes(X,Y), fill="light gray") +
  geom_contour_filled(data=df.z, aes(x=x, y=y, z=z)) +
  scale_fill_gradientn(colours = col.phi,
                       super = metR::ScaleDiscretised,
                       limits=c(0, 1.9)) +
  geom_polygon(aes(subgroup = subid), fill="black") +
  geom_polygon(data=Barton.hole1, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole2, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole3, aes(X,Y), fill="black") +
  geom_polygon(data=Barton.hole4, aes(X,Y), fill="black") +
  theme_classic() +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  labs(fill="September\ntemperature\nstandard deviation") +
  coord_fixed(ratio=1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) 




legend.mean <- get_legend(p2)
legend.sd <- get_legend(p4)
p1 <- p1+theme(legend.position="none") + xlab("Observed")
p2 <- p2+theme(legend.position="none") + xlab("2017 Prediction")
p3 <- p3+theme(legend.position="none") + xlab("Observed")
p4 <- p4+theme(legend.position="none") + xlab("2017 Prediction")
p5 <- p5+theme(legend.position="none") + xlab("2047 Forecast")
p6 <- p6+theme(legend.position="none") + xlab("2047 Forecast")

plot_grid(p1,p2,p5,legend.mean,p3,p4,p6, legend.sd, ncol=2,
          labels=c("a","b","c","","d","e","f",""))


# 
# # # remove unnecessary databases as needed
# rm(df.microclim,
#    df.RH.test,
#    df.trim,
#    dwt,
#    test)
# gc()



###############################################
# Appendix figures
###############################################

# pred vs. observation plots
df.location$T.mean.pred <- summary(zj$T.mean.pred, mean)$stat
df.location$T.Sept.sd.pred <- summary(zj$T.Sept.sd.pred, mean)$stat

x <- lm(df.location$T.mean.pred ~ df.location$T.mean)
summary(x)
x <- lm(df.location$T.Sept.sd.pred ~ df.location$T.Sept.sd)
summary(x)

col_pal <- ghibli_palettes$MononokeMedium[5:7]

ggplot(df.location, aes(T.mean, T.mean.pred)) + theme_classic() +
  ylab("Predicted mean hibernation temperature") + xlab("Observed mean hibernation temperature") + 
  geom_abline(slope=1, intercept=0) +
  geom_smooth(method='lm') +
  geom_point(aes(color=Room, shape=as.factor(is.MYLU)), size=3) + 
  geom_text(aes(x=3.5, y=7.5), label="y=0.925x+0.365, R squared=0.94") +
  scale_shape_manual(labels=c("No","Yes"),
                     name="Roost present?",
                     values=c(16,17)) +
  scale_color_manual(name="Chamber",
                     values=col_pal)

ggplot(df.location, aes(T.Sept.sd, T.Sept.sd.pred)) + theme_classic() +
  ylab("Predicted September standard deviation in temperature") + xlab("Observed September standard deviation in temperature") + 
  geom_abline(slope=1, intercept=0) +
  geom_smooth(method='lm') +
  geom_point(aes(color=Room, shape=as.factor(is.MYLU)), size=3) + 
  geom_text(aes(x=0.4, y=1), label="y=0.470x+0.259, R squared=0.23") +
  scale_shape_manual(labels=c("No","Yes"),
                     name="Roost present?",
                     values=c(16,17)) +
  scale_color_manual(name="Chamber",
                     values=col_pal)



#####################################################
# From Data Cleaning
#####################################################
df.microclim$Time <- as_datetime(df.microclim$Time, format="%Y-%m-%d %H")
ggplot(df.microclim,aes(Time,Temp))+ geom_line(aes(group=iButton,color=Depth)) + 
  theme_classic() + xlab("") + ylab("Temperature (degrees C)") +
  scale_color_gradientn(colors=ghibli_palettes$PonyoMedium,
                        name="Elevation") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))
ggplot(df.microclim,aes(Time,RH*100))+geom_line(aes(group=iButton,color=Depth)) + 
  theme_classic() + xlab("Time") + ylab("Relative Humidity (%)") +
  scale_color_gradientn(colors=ghibli_palettes$PonyoMedium,
                        name="Elevation") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))


