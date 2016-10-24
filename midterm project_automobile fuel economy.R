library(tidyr)
library(dplyr)
library(ggplot2)
library(alr4)
options(max.print = 4000000)

#read raw data
vehicles <- read.csv("tidy_tables/vehicles.csv")
View(vehicles)

#remove data of dual fuel vehicles
fuel2 <- which(vehicles$barrelsA08!=0)
length(vehicles$barrelsA08[vehicles$barrelsA08!=0])
vehicles <- vehicles[-fuel2,]

#remove uneeded and inconsistent variables
vehicles$barrelsA08 <- NULL
vehicles$charge120 <- NULL
vehicles$charge240 <- NULL
vehicles$city08U <- NULL
vehicles$cityA08 <- NULL
vehicles$cityA08U <- NULL
vehicles$cityCD <- NULL
vehicles$cityE <- NULL
vehicles$cityUF <- NULL
vehicles$co2A <- NULL
vehicles$co2TailpipeAGpm <- NULL
vehicles$comb08U <- NULL
vehicles$combA08 <- NULL
vehicles$combA08U <- NULL
vehicles$combE <- NULL
vehicles$combinedCD <- NULL
vehicles$combinedUF <- NULL
vehicles$evMotor <- NULL
vehicles$fuelCostA08 <- NULL
vehicles$fuelType <- NULL
vehicles$ghgScoreA <- NULL
vehicles$guzzler <- NULL
vehicles$highwayE <- NULL
vehicles$highwayA08 <- NULL
vehicles$highwayA08U <- NULL
vehicles$highwayCD <- NULL
vehicles$highwayUF <- NULL
vehicles$highwayE <- NULL
vehicles$phevBlended <- NULL
vehicles$rangeA <- NULL
vehicles$rangeCityA <- NULL
vehicles$rangeHwyA <- NULL
vehicles$mfrCode <- NULL
vehicles$UCityA <- NULL
vehicles$UHighwayA <- NULL
vehicles$sCharger <- NULL
vehicles$tCharger <- NULL
vehicles$atvType <- NULL
vehicles$c240Dscr <- NULL
vehicles$c240bDscr <- NULL
vehicles$charge240b <- NULL
vehicles$phevCity <- NULL
vehicles$phevHwy <- NULL
vehicles$phevComb <- NULL
vehicles$startStop <- NULL
vehicles$fuelType2 <- NULL
vehicles$highway08U <- NULL
vehicles$range <- NULL
vehicles$rangeCity <- NULL
vehicles$rangeHwy <- NULL
vehicles$createdOn <- NULL
vehicles$modifiedOn <- NULL
vehicles$trans_dscr <- NULL

#deal with the missing data
vehicles$co2[vehicles$co2==-1] <- NA
vehicles$feScore[vehicles$feScore==-1] <- NA
vehicles$eng_dscr[vehicles$eng_dscr==""] <- NA
vehicles$ghgScore[vehicles$ghgScore==-1] <- NA
vehicles[vehicles==""] <- NA

#turn the long strings into letters
vehicles$drive <- gsub("Rear-Wheel Drive",replacement = "R",vehicles$drive)
vehicles$drive <- gsub("Front-Wheel Drive",replacement = "F",vehicles$drive)
vehicles$drive <- gsub("4-Wheel or All-Wheel Drive",replacement = "4A",vehicles$drive)
vehicles$drive <- gsub("4-Wheel Drive",replacement = "4",vehicles$drive)
vehicles$drive <- gsub("2-Wheel Drive",replacement = "2",vehicles$drive)
vehicles$drive <- gsub("All-Wheel Drive",replacement = "A",vehicles$drive)
vehicles$drive <- gsub("Part-time 4-Wheel Drive",replacement = "P4",vehicles$drive)

unique(vehicles$fuelType1)
vehicles$fuelType1 <- gsub("Regular Gasoline",replacement = "RG",vehicles$fuelType1)
vehicles$fuelType1 <- gsub("Premium Gasoline",replacement = "PG",vehicles$fuelType1)
vehicles$fuelType1 <- gsub("Natural Gas",replacement = "NG",vehicles$fuelType1)
vehicles$fuelType1 <- gsub("Diesel",replacement = "D",vehicles$fuelType1)
vehicles$fuelType1 <- gsub("Electricity",replacement = "E",vehicles$fuelType1)
vehicles$fuelType1 <- gsub("Midgrade Gasoline",replacement = "MG",vehicles$fuelType1)


#sort by years and create cleaning data set "vehicles1" which contains the variables we need.
vehicles1 <- vehicles[,c("year","make","model","trany","drive","VClass","eng_dscr","fuelType1","id","engId","displ","cylinders","youSaveSpend","UCity","UHighway","pv2","pv4","lv2","lv4","hlv","hpv","barrels08","city08","highway08","comb08","fuelCost08","co2","co2TailpipeGpm","feScore","ghgScore","mpgData")]
vehicles1 <- vehicles1[order(vehicles1$year),]
write.csv(vehicles1,"vehicles1.csv")

#Tidy data by seperating vehicles1 into some small data sets and each data set focuses on some ralative data
vehicles_engine <- vehicles1[,c(1:12)]
vehicles_engine <- vehicles_engine[order(vehicles_engine$year,vehicles_engine$displ,vehicles_engine$cylinders),]
write.csv(vehicles_engine,"vehicles_engine information")

vehicles_score <- vehicles1[,c(1,2,3,4,29,30)]
vehicles_score <- vehicles_score[order(vehicles_score$year,-vehicles_score$feScore,-vehicles_score$ghgScore),]
write.csv(vehicles_score,"vehicles mpg score")

vehicles_co2 <- vehicles1[,c(1,2,3,4,27,28)]
vehicles_co2 <- vehicles_co2[order(vehicles_co2$year,vehicles_co2$co2,vehicles_co2$co2TailpipeGpm),]
write.csv(vehicles_co2,"vehicles tailpipe CO2")

vehicles_fuelcost <- vehicles1[,c(1,2,3,4,26,22,23,24,25,14,15)]
vehicles_fuelcost <- vehicles_fuelcost[order(vehicles_fuelcost$year,vehicles_fuelcost$fuelCost08),]
write.csv(vehicles_fuelcost,"vehicles annual fuel cost")

vehicles_volume <- vehicles1[,c(1:4,16:21)]
vehicles_volume <- vehicles_volume[order(vehicles_volume$year,vehicles_volume$pv2,vehicles_volume$pv4,vehicles_volume$lv2,vehicles_volume$lv4,vehicles_volume$hlv,vehicles_volume$hpv),]
write.csv(vehicles_volume,"vehicles volume information")

#some analysis
#linear regression analysis of fuelcost and mpg data
pairs(~fuelCost08+barrels08+city08+highway08+UCity+UHighway+comb08,data = vehicles_fuelcost)

#relationship between fuel cost and spend
scatterplot(vehicles$youSaveSpend~vehicles$fuelCost08)
result <- lm(vehicles$youSaveSpend~vehicles$fuelCost08)
summary(result)

#relationship between fuel cost and engine
scatterplot(vehicles$fuelCost08~vehicles$displ)
result1 <- lm(vehicles$fuelCost08~vehicles$displ)
summary(result1)

#data change in years
fuelcostmean <- rep(0,34)
combmean <- rep(0,34)
displmean <- rep(0,34)
co2mean <- rep(0,34)
x <- c(1984:2017)

for (i in 1984:2017) {
  fuelcostmean[i-1983] <- mean(vehicles1$fuelCost08[vehicles1$year==i])
  combmean[i-1983] <- mean(vehicles1$comb08[vehicles1$year==i])
  displmean[i-1983] <- mean(vehicles1$displ[vehicles1$year==i])
  co2mean[i-1983] <- mean(vehicles1$co2TailpipeGpm[vehicles1$year==i])
}

plotloess <- function(x,y){
  plot(x,y)
  model_loess <- loess(y~x)
  fit <- fitted(model_loess)
  ord <- order(x)
  lines(x[ord],fit[ord],lwd=2,lty=2,col="red")
}

plotloess(x,fuelcostmean)
plotloess(x,combmean)
plotloess(x,displmean)
plotloess(x,co2mean)
