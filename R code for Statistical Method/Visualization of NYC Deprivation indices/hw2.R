############################
#HW 2: [Minxia Ji]-[1:15]  #
############################


#################
#   Question 1  #
#################
#load data DP03 and DP04
x<-read.table("ACS_13_5YR_DP03_with_ann.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
y<-read.table("ACS_13_5YR_DP04_with_ann.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)

#missing values are "-" instead of NA
tail(x)
tail(y)

#for these four variables,replace "-" with NA
x$HC03_VC07[x$HC03_VC07=="-"]<-NA
y$HC03_VC65[y$HC03_VC65=="-"]<-NA
y$HC03_VC84[y$HC03_VC84=="-"]<-NA
y$HC03_VC112[y$HC03_VC112=="-"]<-NA

#x.clean<-subset(x, !is.na(x$HC03_VC07))
#y.clean<-subset(y,!is.na(y$HC03_VC65)&!is.na(y$HC03_VC84)
               # &!is.na(y$HC03_VC112))

#merge by census tracts
temp<-merge(x,y,by="GEO.display.label")

#clean the last row,they are names instead of values
temp<-subset(temp,temp$GEO.display.label!="Geography")

#transfer the data to "overcrowded households de???ned as homes with 
#more than one person per room"
temp$HC03_VC112.y<-100-as.numeric(temp$HC03_VC112.y)

#creat data.frame townsend
townsend<-data.frame(temp$GEO.id2.x,temp$GEO.display.label,as.numeric(temp$HC03_VC07),
                     as.numeric(temp$HC03_VC65),as.numeric(temp$HC03_VC84.y),
                     as.numeric(temp$HC03_VC112.y))
rm(temp)


#rename colunm names
colnames(townsend)<-c("GEOID","Geography","unemployment","housingTenure",
                     "noVehicles","lowOccupancy")
#################
#   Question 2  #
#################
#see solution 

#################
#   Question 3  #
#################
# figure with 4 plots: (2 row, 2 columns; fill by row)
par(mfrow=c(2,2))

#construct histogram for each variable
hist(townsend$unemployment,las=TRUE,
     col="deeppink",border = "white",
     main="Unemployment Rate\n Civilian labor force",
     xlab="Unemployment Rate %")

hist(townsend$housingTenure,las=TRUE,
     col="deeppink2",border = "white",
     main="Renter occupied\n Occupied housing units",
     xlab="Renter occupied rate %")

hist(townsend$noVehicles,las=TRUE,
     col="deeppink3",border = "white",
     main="No Vehicles\n No vehicles available",
     xlab="No VehiclesRate %")

hist(townsend$lowOccupancy,las=TRUE,
     col="deeppink4",border = "white",
     main="More than oneOCCUPANTS PER ROOM\n Occupied housing units",
     xlab="Low Occupancy Rate %")

#use apply function to calculate maximum,minumum,
#mean,median and standard deviation of each variable
apply(townsend[,c("unemployment", "housingTenure", "noVehicles", 
           "lowOccupancy")], 2, mean, na.rm=TRUE)

apply(townsend[,c("unemployment", "housingTenure", "noVehicles", 
                  "lowOccupancy")], 2, median, na.rm=TRUE)

apply(townsend[,c("unemployment", "housingTenure", "noVehicles", 
                  "lowOccupancy")], 2, sd, na.rm=TRUE)

apply(townsend[,c("unemployment", "housingTenure", "noVehicles", 
                  "lowOccupancy")], 2, max, na.rm=TRUE)

apply(townsend[,c("unemployment", "housingTenure", "noVehicles", 
                  "lowOccupancy")], 2, min, na.rm=TRUE)

#################
#   Question 4  #
#################

#Calculate how many variables missing for each variables
unemployment.missing<-subset(x,is.na(x))
housingTenure.missing<-subset(y,is.na(y$HC03_VC65))
noVehicles.missing<-subset(y,is.na(y$HC03_VC84))
lowOccupancy.missing<-subset(y,is.na(y$HC03_VC112))

x.rectangle<-x[complete.cases(x),]
y.rectangle<-y[complete.cases(y),]

#fraction missing
1-nrow(x.rectangle)/nrow(x)  
1-nrow(y.rectangle)/nrow(y) 

#################
#   Question 5  #
#################
#make a scartterplot
pairs(townsend[,c("unemployment", "housingTenure", "noVehicles", 
                  "lowOccupancy")])

#transform variables
t.unemp<-log(townsend$unemployment+1)
t.oc<-log(townsend$lowOccupancy+1)
t.rent<-log(townsend$housingTenure+1)
t.car<-(townsend$noVehicles)^(1/2)

transform<-data.frame(t.unemp,t.rent,t.oc,t.car)

#scattor plot after transforming
pairs(transform[,c("t.unemp","t.rent","t.oc","t.car")])

#dev.off()
# Construct a correlation matrix of the transformed variables
cor(transform[complete.cases(transform),])

#################
#   Question 6  #
#################

#calculate z1,z2,z3,z4 to get the sum of z.ij
z1<-(t.unemp-mean(t.unemp,na.rm = TRUE))/sd(t.unemp,na.rm = TRUE)
z2<-(t.rent-mean(t.rent,na.rm = TRUE))/sd(t.rent,na.rm = TRUE)
z3<-(t.car-mean(t.car,na.rm = TRUE))/sd(t.car,na.rm = TRUE)
z4<-(t.oc-mean(t.oc,na.rm = TRUE))/sd(t.oc,na.rm = TRUE)

z.ij<-z1+z2+z3+z4

#add z.ij to the townsend and rename z.ij as ¡°index¡±
townsend <- data.frame(townsend,"index"=z.ij)

#find the census tract of max and its value
townsend$Geography[which.max(townsend$index)]
townsend$index[which.max(townsend$index)]

#find the census tract of min and its value
townsend$Geography[which.min(townsend$index)]
townsend$index[which.min(townsend$index)]

#################
#   Question 7  #
#################
#see solutions

#################
#   Question 8  #
#################
#load packages
library(sp)
library(RColorBrewer)
library(rgdal)

# read map file
ny.map <- readOGR(dsn="tl_2013_36_tract",layer = "tl_2013_36_tract")

# set GEOID as row names for ny.map
row.names(ny.map) <- as.character(ny.map$GEOID)
# extract tracts for the New York County only
ny.map <- ny.map[is.element(ny.map$GEOID,townsend$GEOID),]
# set GEO.id as row names for townsend
row.names(townsend) <- as.character(townsend$GEOID)
# match the row names in townsend with those in ny.map
townsend <- townsend[row.names(ny.map),]

#range(townsend$townsend.index, na.rm = TRUE)
# convert the numeric vector into factor vector, break up into 8 intervals
breaks.factor <- cut(townsend$index, breaks=seq(from=-13.1, to=5.3, length=9))

color.palette <- rev(brewer.pal(n=length(levels(breaks.factor)),"Spectral"))
color.coding <- color.palette[as.numeric(breaks.factor)]

#pdf("ny_map.pdf",width=8,length=8)
plot(ny.map, col=color.coding)
legend("bottomleft", legend=c(levels(breaks.factor), "no data","Lowenstein"), 
       fill=c(color.palette, "white","black"), cex=0.8, bty="n",
       y.intersp=1, ncol=2)

#add a line to discribe the graph
text(-74.12524,40.86051, cex=1.2,
     labels="Deprivation Ranks\nfrom red(most deprived)to blue(least deprived)")


#################
#   Question 9  #
#################
# point out the census tract where contains Lowenstein locates
points(coordinates(ny.map["36061014500",]), cex=1.2, pch=20)
arrows(x0=-74.04717, y0=40.80498, x1=-73.98426, y1=40.76981, length=0.1, lwd=1.8)
#townsend$index.rank["36061014500",]
text(-74.04126, 40.78706,cex = 0.9, labels="Lowenstein\n rank 20th in New York County")
#dev.off()

#################
#   Question10  #
#################
#see solution