"
R script to read CSV CFT output file and calculate means of overall data,
regional data, density states and do a visualisation on change over time
"


library("dplyr")
library(ggplot2)

workingdir <- "/Users/Christopher/Desktop/ZSL"
setwd(workingdir) #Set working directory

CFTData <- read.csv("CO2Final.csv") #Reads data file
Locations <- read.csv("Locations.csv")
DensityStates <- read.csv("FieldRegions.csv")

CO2T <- data.frame(CFTData$CO2.Total) #Extracts CO2 Total

CO2P <- data.frame(CFTData$CO2.Pest)

CO2F <- data.frame(CFTData$Fuel)


CO2TMean <- sapply(CO2T, mean)
CO2PMean <- sapply(CO2P, mean)
CO2FMean <- sapply(CO2F, mean)

TotalMeans = data.frame()
TotalMeans <- rbind(c(CO2TMean, CO2PMean, CO2FMean),TotalMeans)
names(TotalMeans) <- c("CO2 Mean","Pest", "Fuel")

o4 = data.frame()
o5 = data.frame()
o6 = data.frame()
o7 = data.frame()
o8 = data.frame()
o9 = data.frame()
o10 = data.frame()
o11 = data.frame()
o12 = data.frame()
o13 = data.frame()
o14 = data.frame()

o4 <- rbind(filter(CFTData, grepl("2004",CFTData$Field.ID) == TRUE), o4)
o5 <- rbind(filter(CFTData, grepl("2005",CFTData$Field.ID) == TRUE), o5)
o6 <- rbind(filter(CFTData, grepl("2006",CFTData$Field.ID) == TRUE), o6)
o7 <- rbind(filter(CFTData, grepl("2007",CFTData$Field.ID) == TRUE), o7)
o8 <- rbind(filter(CFTData, grepl("2008",CFTData$Field.ID) == TRUE), o8)
o9 <- rbind(filter(CFTData, grepl("2009",CFTData$Field.ID) == TRUE), o9)
o10 <- rbind(filter(CFTData, grepl("2010",CFTData$Field.ID) == TRUE), o10)
o11 <- rbind(filter(CFTData, grepl("2011",CFTData$Field.ID) == TRUE), o11)
o12 <- rbind(filter(CFTData, grepl("2012",CFTData$Field.ID) == TRUE), o12)
o13 <- rbind(filter(CFTData, grepl("2013",CFTData$Field.ID) == TRUE), o13)
o14 <- rbind(filter(CFTData, grepl("2014",CFTData$Field.ID) == TRUE), o14)


Means = data.frame()
years <- c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)
Means <- rbind(sapply(o4[,4:7],mean),Means)
Means <- rbind(sapply(o5[,4:7],mean),Means)
Means <- rbind(sapply(o6[,4:7],mean),Means)
Means <- rbind(sapply(o7[,4:7],mean),Means)
Means <- rbind(sapply(o8[,4:7],mean),Means)
Means <- rbind(sapply(o9[,4:7],mean),Means)
Means <- rbind(sapply(o10[,4:7],mean),Means)
Means <- rbind(sapply(o11[,4:7],mean),Means)
Means <- rbind(sapply(o12[,4:7],mean),Means)
Means <- rbind(sapply(o13[,4:7],mean),Means)
Means <- rbind(sapply(o14[,4:7],mean),Means)
Means <- cbind(rev(years), Means)
names(Means) <- c("Year","CO2 Total","CO2 Pest","N Fert","CO2 Fuel")

p <- ggplot(Means, aes(x = Year, y= value, color = variable)) + 
  geom_point(aes(y = Means$`CO2 Total`, col = "CO2 Total")) + 
  geom_point(aes(y = Means$`CO2 Pest`, col = "CO2 Pest")) +
  geom_point(aes(y = Means$`CO2 Fuel`, col = "CO2 Fuel"))
p + labs(x = "Year", y = "CO2 (kg/ha)")
Total <- ggplot(Means, aes(x = Year, y= value, color = variable)) + 
  geom_point(aes(y = Means$`CO2 Total`, col = "CO2 Total"))
Total + labs(x = "Year", y = "CO2 (kg/ha)", title = "Total CO2 emissions per hectare")
ggsave("TotalCO2.png")

EE_farms = data_frame()
EM_farms = data_frame()
SE_farms = data_frame()
WM_farms = data_frame()
YH_farms = data_frame()

EE_farms <- rbind(filter(Locations, grepl("EE", Locations$region) == TRUE), EE_farms)
EM_farms <- rbind(filter(Locations, grepl("EM", Locations$region) == TRUE), EM_farms)
SE_farms <- rbind(filter(Locations, grepl("SE", Locations$region) == TRUE), SE_farms)
WM_farms <- rbind(filter(Locations, grepl("WM", Locations$region) == TRUE), WM_farms)
YH_farms <- rbind(filter(Locations, grepl("YH", Locations$region) == TRUE), YH_farms)

EE = data_frame()
EM = data_frame()
SE = data_frame()
WM = data_frame()
YH = data_frame()

EE <- rbind(filter(CFTData, CFTData$Field.Name %in% EE_farms$field == TRUE), EE)
EM <- rbind(filter(CFTData, CFTData$Field.Name %in% EM_farms$field == TRUE), EM)
SE <- rbind(filter(CFTData, CFTData$Field.Name %in% SE_farms$field == TRUE), SE)
WM <- rbind(filter(CFTData, CFTData$Field.Name %in% WM_farms$field == TRUE), WM)
YH <- rbind(filter(CFTData, CFTData$Field.Name %in% YH_farms$field == TRUE), YH)

RegionMean = data_frame()

RegionMean <- rbind(sapply(EE[,4:7], mean),RegionMean)
RegionMean <- rbind(sapply(EM[,4:7], mean),RegionMean)
RegionMean <- rbind(sapply(SE[,4:7], mean),RegionMean)
RegionMean <- rbind(sapply(WM[,4:7], mean),RegionMean)
RegionMean <- rbind(sapply(YH[,4:7], mean),RegionMean)
Regions = c("EE","EM","SE","WM","YH")
RegionMean <- cbind(Regions, RegionMean)
names(RegionMean) <- c("Region","CO2 Total","CO2 Pest","N Fert","CO2 Fuel")

Low_farms = data_frame()
Med_farms = data_frame()
High_farms = data_frame()
VHigh_farms = data_frame()


Low_farms <- rbind(filter(DensityStates, grepl("low", DensityStates$den) == TRUE), Low_farms)
Med_farms <- rbind(filter(DensityStates, grepl("medium", DensityStates$den) == TRUE), Med_farms)
High_farms <- rbind(filter(DensityStates, grepl("high", DensityStates$den) == TRUE), High_farms)
VHigh_farms <- rbind(filter(DensityStates, grepl("veryhigh", DensityStates$den) == TRUE), VHigh_farms)


Low = data_frame()
Med = data_frame()
High = data_frame()
VHigh = data_frame()


Low <- rbind(filter(CFTData, CFTData$Field.Name %in% Low_farms$field == TRUE), Low)
Med <- rbind(filter(CFTData, CFTData$Field.Name %in% Med_farms$field == TRUE), Med)
High <- rbind(filter(CFTData, CFTData$Field.Name %in% High_farms$field == TRUE), High)
VHigh <- rbind(filter(CFTData, CFTData$Field.Name %in% VHigh_farms$field == TRUE), VHigh)

DenMean = data_frame()

DenMean <- rbind(sapply(Low[,4:7], mean),DenMean)
DenMean <- rbind(sapply(Med[,4:7], mean),DenMean)
DenMean <- rbind(sapply(High[,4:7], mean),DenMean)
DenMean <- rbind(sapply(VHigh[,4:7], mean),DenMean)

Densities = c("Low","Medium","High","Very High")
DenMean <- cbind(Densities, DenMean)
names(DenMean) <- c("Density","CO2 Total","CO2 Pest","N Fert","CO2 Fuel")
