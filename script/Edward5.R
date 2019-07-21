# R script uder to evaluate and present results of "Meeting SDGs 2 & 13 within global constraints" study
# ---- START ----
# clear memory
rm(list=ls()) 

# Load Libraries
#library(reshape);
library(reshape2);
library(ggplot2);
library(plyr);
library(dplyr)
library(data.table);
library(tidyr)
library(stringr)
library(xlsx)
library(openxlsx)
library(ggpubr)
library(gridExtra)
library(grid)
library(lattice)
library(gdata)


# ---- INPUTS: IMAGE ----
ppi <- 300
# set directory path 
#setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - documents/SDG/Edward/R-Scripts/")
setwd("C:/Users/Asus/Documents/Github/Edward/")
# Read Data File
SSP1.26=read.csv("data/SSP1_450.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
SSP1.26[] <- lapply(SSP1.26, as.character)
SSP1.26=melt(SSP1.26, id.vars=c("Model","Scenario","Region","Variable","Unit"), na.rm=TRUE)
colnames(SSP1.26)[6] <-"Year"
SSP1.26$Year = as.numeric(substr(SSP1.26$Year, start=2, stop=5))
SSP1.26$value = as.numeric(substr(SSP1.26$value, start=1, stop=5))

SSP2.26=read.csv("data/SSP2_450.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
SSP2.26[] <- lapply(SSP2.26, as.character)
SSP2.26=melt(SSP2.26, id.vars=c("Model","Scenario","Region","Variable","Unit"), na.rm=TRUE)
colnames(SSP2.26)[6] <-"Year"
SSP2.26$Year = as.numeric(substr(SSP2.26$Year, start=2, stop=5))
SSP2.26$value = as.numeric(substr(SSP2.26$value, start=1, stop=5))

SSP1.19=read.csv("data/SSP1_20.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
SSP1.19[] <- lapply(SSP1.19, as.character)
SSP1.19=melt(SSP1.19, id.vars=c("Model","Scenario","Region","Variable","Unit"), na.rm=TRUE)
colnames(SSP1.19)[6] <-"Year"
SSP1.19$Year = as.numeric(substr(SSP1.19$Year, start=2, stop=5))
SSP1.19$value = as.numeric(substr(SSP1.19$value, start=1, stop=5))

SSP2.19=read.csv("data/SSP2_20.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
SSP2.19[] <- lapply(SSP2.19, as.character)
SSP2.19=melt(SSP2.19, id.vars=c("Model","Scenario","Region","Variable","Unit"), na.rm=TRUE)
colnames(SSP2.19)[6] <-"Year"
SSP2.19$Year = as.numeric(substr(SSP2.19$Year, start=2, stop=5))
SSP2.19$value = as.numeric(substr(SSP2.19$value, start=1, stop=5))

LivestockProd_SSP1_20 = read.xlsx("data/Livestockprod.xlsx", sheet = 1, startRow=5)
LivestockProd_SSP1_450 = read.xlsx("data/Livestockprod.xlsx", sheet = 2, startRow=5)
LivestockProd_SSP2_20 = read.xlsx("data/Livestockprod.xlsx", sheet = 3, startRow=5)
LivestockProd_SSP2_450 = read.xlsx("data/Livestockprod.xlsx", sheet = 4, startRow=5)
LivestockProd_SSP1_20$SCENARIO <- "SSP1_20"
LivestockProd_SSP1_450$SCENARIO <- "SSP1_450"
LivestockProd_SSP2_20$SCENARIO <- "SSP2_20"
LivestockProd_SSP2_450$SCENARIO <- "SSP2_450"
LivestockProd = rbind(LivestockProd_SSP1_20,LivestockProd_SSP1_450,LivestockProd_SSP2_20,LivestockProd_SSP2_450)
rm(LivestockProd_SSP1_20,LivestockProd_SSP1_450,LivestockProd_SSP2_20,LivestockProd_SSP2_450)
LivestockProd=melt(LivestockProd, id.vars=c("t","NAPT","SCENARIO"), na.rm=TRUE)
colnames(LivestockProd)[1:5] <-c("YEAR","VARIABLE","SCENARIO","REGION","value1")
LivestockProd$value <- LivestockProd$value1 / 1000 
LivestockProd$UNIT <- "MtDM/yr"
LivestockProd$value1 <- NULL
LivestockProd  = subset(LivestockProd, YEAR==1981|YEAR==1990|YEAR==2000|YEAR==2010|YEAR==2020|YEAR==2030|YEAR==2040|YEAR==2050|YEAR==2060|YEAR==2070|YEAR==2080|YEAR==2090|YEAR==2100)

# Livestock Consumption
LivestockCons_SSP1_20 = read.xlsx("data/LivestockCons.xlsx", sheet = 1, startRow=5)
LivestockCons_SSP1_450 = read.xlsx("data/LivestockCons.xlsx", sheet = 2, startRow=5)
LivestockCons_SSP2_20 = read.xlsx("data/LivestockCons.xlsx", sheet = 3, startRow=5)
LivestockCons_SSP2_450 = read.xlsx("data/LivestockCons.xlsx", sheet = 4, startRow=5)
LivestockCons_SSP1_20$SCENARIO <- "SSP1_20"
LivestockCons_SSP1_450$SCENARIO <- "SSP1_450"
LivestockCons_SSP2_20$SCENARIO <- "SSP2_20"
LivestockCons_SSP2_450$SCENARIO <- "SSP2_450"
LivestockCons = rbind(LivestockCons_SSP1_20,LivestockCons_SSP1_450,LivestockCons_SSP2_20,LivestockCons_SSP2_450)
rm(LivestockCons_SSP1_20,LivestockCons_SSP1_450,LivestockCons_SSP2_20,LivestockCons_SSP2_450)
LivestockCons = subset(LivestockCons, NUFPST==4)
LivestockCons$NUFPST <- NULL
LivestockCons=melt(LivestockCons, id.vars=c("t","NAPT","SCENARIO"), na.rm=TRUE)
colnames(LivestockCons)[1:5] <-c("YEAR","VARIABLE","SCENARIO","REGION","value1")
LivestockCons$value <- LivestockCons$value1 / 1000 
LivestockCons$UNIT <- "MtDM/yr"
LivestockCons$value1 <- NULL
LivestockCons  = subset(LivestockCons, YEAR==1981|YEAR==1990|YEAR==2000|YEAR==2010|YEAR==2020|YEAR==2030|YEAR==2040|YEAR==2050|YEAR==2060|YEAR==2070|YEAR==2080|YEAR==2090|YEAR==2100)

# ---- INPUTS: DATA ----
# Population
WEU_pop=read.csv("data/Eurostat_pop.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
WEU_pop=melt(WEU_pop, id.vars="COUNTRY", na.rm=TRUE)
colnames(WEU_pop)[2] <-"Year"
WEU_pop$Year = as.numeric(substr(WEU_pop$Year, start=2, stop=5))
WEU_pop$value = as.numeric(substr(WEU_pop$value, start=1, stop=9))
NL_pop=subset(WEU_pop, COUNTRY=="Netherlands")
#Emissions
WEU_Emis=read.csv("data/EU_Emis.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character") # This data is "As reported by member states"
WEU_Emis=melt(WEU_Emis, id.vars=c("COUNTRY","VARIABLE"), na.rm=TRUE)
colnames(WEU_Emis)[3] <-"Year"
WEU_Emis$Year = as.numeric(substr(WEU_Emis$Year, start=2, stop=5))
WEU_Emis$value = as.numeric(substr(WEU_Emis$value, start=1, stop=5))
NL_Emis=subset(WEU_Emis, COUNTRY=="Netherlands")
NL_Emis.IPCC <- data.frame(c("CO2","CH4","N2O","total"),c(0.1,13,6.1,19.2))
colnames(NL_Emis.IPCC)[1:2] <- c("Gas","MtCO2")

EuroStat_Emis = read.csv("data/env_air_gge_1_Data.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character") # This data is from DATASET: Greenhouse gas emissions by source sector (source: EEA) [env_air_gge]
EuroStat_Emis$ AIRPOL <- NULL
EuroStat_Emis=subset(EuroStat_Emis, AIREMSECT=="Agriculture")
colnames(EuroStat_Emis)[1:4] <- c("Year","Country","Unit","Sector")
EuroStat_Emis=subset(EuroStat_Emis, Country=="Netherlands"|
                       Country=="Belgium"|
                       Country=="Germany (until 1990 former territory of the FRG)"|
                       Country=="Ireland"|
                       Country=="Spain"|
                       Country=="Sweden"|
                       Country=="Iceland"|
                       Country=="Norway"|
                       Country=="Denmark"|
                       Country=="Greece"|
                       Country=="France"|
                       Country=="Italy"|
                       Country=="Luxembourg"|
                       Country=="Malta"|
                       Country=="Austria"|
                       Country=="Portugal"|
                       Country=="Finland"|
                       Country=="United Kingdom"|
                       Country=="Liechtenstein"|
                       Country=="Switzerland")
EuroStat_Emis$Unit <- "MtCO2-eq/yr"
EuroStat_Emis$Value = as.numeric(substr(EuroStat_Emis$Value, start=1, stop=5))
EuroStat_Emis$Year = as.numeric(substr(EuroStat_Emis$Year, start=1, stop=4))
EuroStat_Emis=spread(EuroStat_Emis,Country,Value)
EuroStat_Emis$WEU <- rowSums(EuroStat_Emis[,4:23])
EuroStat_Emis = EuroStat_Emis %>% mutate(NLFrac=Netherlands/WEU) 
EuroStat_Emis = subset(EuroStat_Emis, select =c(Year,Unit,Sector,Netherlands,WEU,NLFrac))
EuroStat_Emis=melt(EuroStat_Emis, id.vars=c("Year","Unit","Sector"), na.rm=TRUE)
colnames(EuroStat_Emis)[4]<-"Country"
# Livestock Emission Factors for the Netherlands
NLAgHist=read.csv("data/NL_Ag_Hist.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character") # This data is (mostly) From a PBL report (for livestock) and FAO (for crops)
colnames(NLAgHist)[1] <-"Variable"
NLAgHist=melt(NLAgHist, id.vars=c("Variable","Source"), na.rm=TRUE)
colnames(NLAgHist)[3] <-"Year"
NLAgHist$Year = as.numeric(substr(NLAgHist$Year, start=2, stop=5))
NLAgHist$value = as.numeric(substr(NLAgHist$value, start=1, stop=5))
NLAgHist$VarOrder = factor(NLAgHist$Variable, levels=c("Crop_EIAdm","Crop_EIAha","Milk_per_animal","LU_per_animal","CH4_per_animal","Milk_EI_perDM","Milk_EI_perHa",
                                                       "Crop_Yield","Cropland","N_AppRate"))

# Agricultural Production
WEU_Prod.in=read.csv("data/FAO_EU_Production.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
WEU_Prod.in$Year = as.numeric(substr(WEU_Prod.in$Year, start=1, stop=4))
WEU_Prod.in$Value = as.numeric(substr(WEU_Prod.in$Value, start=1, stop=5))
WEU_Prod.in=subset(WEU_Prod.in, select=-Element)

WEU_Prod.meat=subset(WEU_Prod.in, Item=="Bovine Meat"|Item=="Mutton & Goat Meat"|Item=="Pigmeat"|Item=="Poultry Meat"|Item=="Meat, Other"|Item=="Eggs")
WEU_Prod.crop=subset(WEU_Prod.in, !(Item=="Bovine Meat"|Item=="Mutton & Goat Meat"|Item=="Pigmeat"|Item=="Poultry Meat"|Item=="Meat, Other"|Item=="Eggs"))

WEU_Prod.meat <- aggregate(WEU_Prod.meat$Value, by=list(Country=WEU_Prod.meat$Country, Year=WEU_Prod.meat$Year, Unit=WEU_Prod.meat$Unit), FUN=sum, na.rm=TRUE)
WEU_Prod.crop <- aggregate(WEU_Prod.crop$Value, by=list(Country=WEU_Prod.crop$Country, Year=WEU_Prod.crop$Year, Unit=WEU_Prod.crop$Unit), FUN=sum, na.rm=TRUE)

WEU_Prod.meat1 <- aggregate(WEU_Prod.meat$x, by=list(Year=WEU_Prod.meat$Year, Unit=WEU_Prod.meat$Unit), FUN=sum, na.rm=TRUE)
WEU_Prod.crop1 <- aggregate(WEU_Prod.crop$x, by=list(Year=WEU_Prod.crop$Year, Unit=WEU_Prod.crop$Unit), FUN=sum, na.rm=TRUE)

WEU_Prod.meat1$Country <- "WEU"
WEU_Prod.crop1$Country <- "WEU"

WEU_Prod.meat2 = rbind(WEU_Prod.meat,WEU_Prod.meat1)
WEU_Prod.crop2 = rbind(WEU_Prod.crop,WEU_Prod.crop1)

WEU_Prod.meat2$Product <- "Meat"
WEU_Prod.crop2$Product <- "Crops"

WEU_Prod=rbind(WEU_Prod.meat2,WEU_Prod.crop2)
WEU_Prod = WEU_Prod %>% mutate(value=x/1000)
WEU_Prod$Unit <- "MtDM/yr"
WEU_Prod$x <- NULL

WEU_Prod=spread(WEU_Prod,Product,value)
WEU_Prod=WEU_Prod %>% mutate(Total=Crops+Meat) 
WEU_Prod=melt(WEU_Prod, id.vars=c("Country","Year","Unit"),na.rm=TRUE)  
WEU_Prod=spread(WEU_Prod,Country,value)
WEU_Prod=WEU_Prod %>% mutate(NLShare=Netherlands/WEU)

NL_Prod=subset(WEU_Prod, select=c(Year,Unit,variable,Netherlands,NLShare))
NL_Prod$COUNTRY <- "Netherlands"
NL_Prod$Unit <- NULL
colnames(NL_Prod)[3] <- "value"
colnames(NL_Prod)[4] <- "Share"
NL_Prod=melt(NL_Prod, id.vars=c("COUNTRY","Year","variable"), na.rm=TRUE)
colnames(NL_Prod)[3] <-"Group"
colnames(NL_Prod)[4] <-"Type"
# Land Use
WEU_Land.in=read.csv("data/FAO_EU_LandCover1.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
WEU_Land.in$Year = as.numeric(substr(WEU_Land.in$Year, start=1, stop=4))
WEU_Land.in$Value = as.numeric(substr(WEU_Land.in$Value, start=1, stop=6))
WEU_Land.in$Element <- NULL
WEU_Land.in$Item[WEU_Land.in$Item=="Arable land and Permanent crops"] <- "Cropland"
WEU_Land.in$Item[WEU_Land.in$Item=="Temporary meadows and pastures"] <- "Pasture1"
WEU_Land.in$Item[WEU_Land.in$Item=="Permanent meadows and pastures"] <- "Pasture2"
WEU_Land.in=spread(WEU_Land.in,Item,Value)
WEU_Land.in=WEU_Land.in %>% mutate(Pasture = Pasture1 + Pasture2)
WEU_Land.in=subset(WEU_Land.in, select=-c(Pasture1,Pasture2))
WEU_Land.in=melt(WEU_Land.in, id.vars=c("Country","Year","Unit"), na.rm=TRUE)
WEU_Land = aggregate(WEU_Land.in$value, by=list(variable=WEU_Land.in$variable, Year=WEU_Land.in$Year, Unit=WEU_Land.in$Unit), FUN=sum, na.rm = TRUE)
WEU_Land$Country <- "WEU"
colnames(WEU_Land)[4] <- "value"
WEU_Land=rbind(WEU_Land.in,WEU_Land)

NL_Land = subset(WEU_Land, Country=="Netherlands"|Country=="WEU")
NL_Land=spread(NL_Land,Country,value)
NL_Land=NL_Land %>% mutate(Share=Netherlands/WEU)
names(NL_Land)[names(NL_Land) == "Netherlands"] = "value1"
NL_Land=NL_Land %>% mutate(value=value1/1000)
NL_Land=subset(NL_Land, select=-c(WEU,value1,Unit))
NL_Land$COUNTRY <- "Netherlands"
NL_Land=melt(NL_Land, id.vars=c("COUNTRY","Year","variable"), na.rm=TRUE)
colnames(NL_Land)[3] <- "Group"
colnames(NL_Land)[4] <- "Type"

# Projections of share of Dutch GDP in WEU
# Based on EU legislation on effort sharing and Eurostat (see spreadsheet "GDP projections_6 EU countries" - Worksheet "Method 2")
NL_ReducShare = data.frame(c("2010","2020","2030","2040","2050"),c(0.040059835,0.040059835,0.040059835,0.040059835,0.040059835))
colnames(NL_ReducShare)[1] <- "Year"
colnames(NL_ReducShare)[2] <- "ReducShare"

#
# ---- BASE DF ----
# Make single DF and correct variables of interest
SSP_all=rbind(SSP1.26,SSP2.26,SSP1.19,SSP2.19)
SSP_all=subset(SSP_all, Year=="1981"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
SSP_all$Variable <-gsub( "[[:punct:]]","",SSP_all$Variable,fixed=F)
SSP_all$Variable <-gsub( "[[:space:]]","",SSP_all$Variable,fixed=F)
SSP_all$Variable <-gsub( "Agricultural","Agri",SSP_all$Variable,fixed=F)
SSP_all$Variable <-gsub( "Agriculture","Agri",SSP_all$Variable,fixed=F)
SSP_all$Variable <-gsub( "Emissions","Emis",SSP_all$Variable,fixed=F)
SSP_all$Variable <-gsub( "LandCover","Land",SSP_all$Variable,fixed=F)
SSP_all$Variable <-gsub( "Demand","Dem",SSP_all$Variable,fixed=F)
SSP_all$Variable <-gsub( "Production","Prod",SSP_all$Variable,fixed=F)
SSP_all$VarID = substr(SSP_all$Variable,start=1,stop=8)
SSP_all$VarID2 = substr(SSP_all$Variable,start=1,stop=4)
SSP_all$Year <- as.numeric(SSP_all$Year, start=1, stop=4)

SSP_all.R=SSP_all

# ---- REGIONAL FIX: EU ----
# Correct for EU=WUE+CEU
SSP_all=spread(SSP_all,Region,value)
SSP_all=SSP_all %>% mutate(EU=CEU+WEU)
SSP_all=melt(SSP_all, id.vars=c("Model","Scenario","Variable","Unit","Year","VarID","VarID2"))
colnames(SSP_all)[8] <-"Region"

# For ratios, assume Get population weighted average
SSP_all.temp = subset(SSP_all, Variable=="YieldCereal"|Variable=="PriceCarbon"|Variable=="FoodDem"|Variable=="FoodDemCrops"|Variable=="FoodDemLivestock"|Variable=="Population")
SSP_all.temp$value[SSP_all.temp$Region=="EU"] <- 
  (
  (SSP_all.temp$value[SSP_all.temp$Region=="WEU"]*
     (SSP_all.temp$value[SSP_all.temp$Region=="WEU"&SSP_all.temp$Variable=="Population"]/
        (SSP_all.temp$value[SSP_all.temp$Region=="WEU"&SSP_all.temp$Variable=="Population"]+SSP_all.temp$value[SSP_all.temp$Region=="CEU"&SSP_all.temp$Variable=="Population"]))) +
  (SSP_all.temp$value[SSP_all.temp$Region=="CEU"] *
     (SSP_all.temp$value[SSP_all.temp$Region=="CEU"&SSP_all.temp$Variable=="Population"]/
        (SSP_all.temp$value[SSP_all.temp$Region=="WEU"&SSP_all.temp$Variable=="Population"]+SSP_all.temp$value[SSP_all.temp$Region=="CEU"&SSP_all.temp$Variable=="Population"])))
  )
SSP_all.temp=subset(SSP_all.temp, !(Variable=="Population"))
SSP_all.temp1 = subset(SSP_all, !(Variable=="YieldCereal"|Variable=="PriceCarbon"|Variable=="FoodDem"|Variable=="FoodDemCrops"|Variable=="FoodDemLivestock"))

SSP_all=rbind(SSP_all.temp,SSP_all.temp1)
rm(SSP_all.temp,SSP_all.temp1)

# ---- REGIONAL FIX: MEX-CAM ----
# Correct for MEXCAM=MEX+RCAM
SSP_all=spread(SSP_all,Region,value)
SSP_all=SSP_all %>% mutate(MEXCAM=MEX+RCAM)
SSP_all=melt(SSP_all, id.vars=c("Model","Scenario","Variable","Unit","Year","VarID","VarID2"))
colnames(SSP_all)[8] <-"Region"

# For ratios, assume Get population weighted average
SSP_all.temp = subset(SSP_all, Variable=="YieldCereal"|Variable=="PriceCarbon"|Variable=="FoodDem"|Variable=="FoodDemCrops"|Variable=="FoodDemLivestock"|Variable=="Population")
SSP_all.temp$value[SSP_all.temp$Region=="MEXCAM"] <- 
  (
    (SSP_all.temp$value[SSP_all.temp$Region=="MEX"]*
       (SSP_all.temp$value[SSP_all.temp$Region=="MEX"&SSP_all.temp$Variable=="Population"]/
          (SSP_all.temp$value[SSP_all.temp$Region=="MEX"&SSP_all.temp$Variable=="Population"]+SSP_all.temp$value[SSP_all.temp$Region=="RCAM"&SSP_all.temp$Variable=="Population"]))) +
      (SSP_all.temp$value[SSP_all.temp$Region=="RCAM"] *
         (SSP_all.temp$value[SSP_all.temp$Region=="RCAM"&SSP_all.temp$Variable=="Population"]/
            (SSP_all.temp$value[SSP_all.temp$Region=="MEX"&SSP_all.temp$Variable=="Population"]+SSP_all.temp$value[SSP_all.temp$Region=="RCAM"&SSP_all.temp$Variable=="Population"])))
  )
SSP_all.temp=subset(SSP_all.temp, !(Variable=="Population"))
SSP_all.temp1 = subset(SSP_all, !(Variable=="YieldCereal"|Variable=="PriceCarbon"|Variable=="FoodDem"|Variable=="FoodDemCrops"|Variable=="FoodDemLivestock"))

SSP_all=rbind(SSP_all.temp,SSP_all.temp1)
rm(SSP_all.temp,SSP_all.temp1)

# ---- REGIONAL FIX: RCP5 ----
SSP_all.RCP=spread(SSP_all,Region,value)
SSP_all.RCP = SSP_all.RCP %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
SSP_all.RCP = SSP_all.RCP %>% mutate(REF=CEU+RUS+STAN+UKR)
SSP_all.RCP = SSP_all.RCP %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
SSP_all.RCP = SSP_all.RCP %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
SSP_all.RCP = SSP_all.RCP %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
SSP_all.RCP = melt(SSP_all.RCP, id.vars=c("Model","Scenario","Variable","Unit","Year","VarID","VarID2"), na.rm=TRUE)
colnames(SSP_all.RCP)[8] <- "Region"
SSP_all.RCP = subset(SSP_all.RCP, Region=="OECD90"|Region=="REF"|Region=="ASIA"|Region=="MAF"|Region=="LAM")


# ---- DOWNSCALING FOR NL ----
# In order to get results relevant for the Netherlands we downscale
# IMAGE projections accourding to different downscaling methods: 
#
#   (i) Assume the share of WEU’s production and GHG emissions represented by the 
#       Netherlands remains the same as in 2015  throughout the study’s timeframe;
#
#   (ii) Assume the trends in the share of GHG emissions and agricultural production 
#        in the WEU represented by the Netherlands are extrapolated until 2050
#
#   (iii) Dutch agricultural emissions reduce in line with EU policies on effort sharing, 
#         with the share of agricultural production levels remaining constant
#
# FIRST FIX DATASETS
# Have to correct for VarID2= Agri,Emis,Fert,Land,Popu
SSP_all.NL=subset(SSP_all, Region=="WEU")
SSP_all.NL$Region <- "NL"
SSP_all.temp=SSP_all
# Remove variables we do not want/have
SSP_all.NL=subset(SSP_all.NL, !(VarID2=="Food"|VarID2=="Pric"))
SSP_all.NL=subset(SSP_all.NL, !(VarID=="AgriDemC"|VarID=="AgriDemL"|VarID=="Fertiliz"))
SSP_all.NL=subset(SSP_all.NL, !(VarID2=="Emis"&!(Variable=="EmisCO2LandUse"|Variable=="EmisCH4LandUse"|Variable=="EmisN2OLandUse")))
SSP_all.NL=subset(SSP_all.NL, !(VarID2=="Agri"&!(Variable=="AgriProdLivestock"|Variable=="AgriProdCropsEnergy"|Variable=="AgriProdCropsNonEnergy")))
SSP_all.NL=subset(SSP_all.NL, !(VarID2=="Land"&!(Variable=="LandCropland"|Variable=="LandOtherArableLand"|Variable=="LandPasture")))
SSP_all.NL = spread(SSP_all.NL,Variable,value)
# Use population (and projections from Eurostat)
SSP_all.NL1 = subset(SSP_all.NL, VarID2=="Popu")
SSP_all.NL1$Population <- NL_pop[match(SSP_all.NL1$Year, NL_pop$Year),3]
SSP_all.NL = subset(SSP_all.NL, !(VarID2=="Popu"))
SSP_all.NL = rbind(SSP_all.NL,SSP_all.NL1)
rm(SSP_all.NL1)
# ----*** Downscaling (i) ----
#
#   (i) Assume the share of WEU’s production and GHG emissions represented by the 
#       Netherlands remains the same as in 2015  throughout the study’s timeframe;
#
SSP_all.NLa = SSP_all.NL
# Correct emissions based on share of NL emissions in WEU for 2015 (EU report?)
SSP_all.NLa = SSP_all.NLa %>% mutate(EmisCH4LandUse_NL = EmisCH4LandUse * NL_Emis$value[NL_Emis$VARIABLE=="Share_Agriculture"&NL_Emis$Year=="2015"])
SSP_all.NLa = SSP_all.NLa %>% mutate(EmisN2OLandUse_NL = EmisN2OLandUse * NL_Emis$value[NL_Emis$VARIABLE=="Share_Agriculture"&NL_Emis$Year=="2015"])
SSP_all.NLa = SSP_all.NLa %>% mutate(EmisCO2LandUse_NL = EmisCO2LandUse * NL_Emis$value[NL_Emis$VARIABLE=="Share_LULUCF"&NL_Emis$Year=="2015"])
SSP_all.NLa = subset(SSP_all.NLa, select=-c(EmisCH4LandUse,EmisN2OLandUse,EmisCO2LandUse))
names(SSP_all.NLa)[names(SSP_all.NLa) == "EmisCH4LandUse_NL"] = "EmisCH4LandUse"
names(SSP_all.NLa)[names(SSP_all.NLa) == "EmisN2OLandUse_NL"] = "EmisN2OLandUse"
names(SSP_all.NLa)[names(SSP_all.NLa) == "EmisCO2LandUse_NL"] = "EmisCO2LandUse"
# Scale 2020 emission to what is reported in IPCC for the Netherlands
# Do this iteratively over all scenarios as they may have different starting points
for(i in unique(SSP_all.NLa$Scenario)){
  NLScale.CH41 = subset(SSP_all.NLa, Year=="2020"&VarID=="EmisCH4L"&Scenario==i)
  NLScale.N2O1 = subset(SSP_all.NLa, Year=="2020"&VarID=="EmisN2OL"&Scenario==i&Unit=="Mt CO2eq/yr")
  NLScale.CH4 = NL_Emis.IPCC$MtCO2[NL_Emis.IPCC$Gas=="CH4"]/NLScale.CH41[,16]
  NLScale.N2O = NL_Emis.IPCC$MtCO2[NL_Emis.IPCC$Gas=="N2O"]/NLScale.N2O1[,17]
  SSP_all.NLa$EmisCH4LandUse2[SSP_all.NLa$Scenario==i] = SSP_all.NLa$EmisCH4LandUse[SSP_all.NLa$Scenario==i] * NLScale.CH4
  SSP_all.NLa$EmisN2OLandUse2[SSP_all.NLa$Scenario==i] = SSP_all.NLa$EmisN2OLandUse[SSP_all.NLa$Scenario==i] * NLScale.N2O
  rm(NLScale.CH41,NLScale.N2O1,NLScale.CH4,NLScale.N2O)
}

SSP_all.NLa$EmisCH4LandUse <- NULL
SSP_all.NLa$EmisN2OLandUse <- NULL
colnames(SSP_all.NLa)[colnames(SSP_all.NLa) == 'EmisCH4LandUse2'] <- 'EmisCH4LandUse'
colnames(SSP_all.NLa)[colnames(SSP_all.NLa) == 'EmisN2OLandUse2'] <- 'EmisN2OLandUse'

# Correct agricultural production based on share of NL production in WEU for 2015 (FAO Data)
SSP_all.NLa = SSP_all.NLa %>% mutate(AgriProdCropsEnergy_NL = AgriProdCropsEnergy * NL_Prod$value[NL_Prod$Group=="Crops"&NL_Prod$Type=="Share"&NL_Prod$Year=="2013"])
SSP_all.NLa = SSP_all.NLa %>% mutate(AgriProdCropsNonEnergy_NL = AgriProdCropsNonEnergy * NL_Prod$value[NL_Prod$Group=="Crops"&NL_Prod$Type=="Share"&NL_Prod$Year=="2013"])
SSP_all.NLa = SSP_all.NLa %>% mutate(AgriProdLivestock_NL = AgriProdLivestock * NL_Prod$value[NL_Prod$Group=="Meat"&NL_Prod$Type=="Share"&NL_Prod$Year=="2013"])
SSP_all.NLa = subset(SSP_all.NLa, select=-c(AgriProdCropsEnergy,AgriProdCropsNonEnergy,AgriProdLivestock))
names(SSP_all.NLa)[names(SSP_all.NLa) == "AgriProdCropsEnergy_NL"] = "AgriProdCropsEnergy"
names(SSP_all.NLa)[names(SSP_all.NLa) == "AgriProdCropsNonEnergy_NL"] = "AgriProdCropsNonEnergy"
names(SSP_all.NLa)[names(SSP_all.NLa) == "AgriProdLivestock_NL"] = "AgriProdLivestock"

# Correct land use
SSP_all.NLa = SSP_all.NLa %>% mutate(LandCropland_NL = LandCropland * NL_Land$value[NL_Land$Group=="Cropland"&NL_Land$Type=="Share"&NL_Land$Year=="2013"])
SSP_all.NLa = SSP_all.NLa %>% mutate(LandOtherArableLand_NL = LandOtherArableLand * NL_Land$value[NL_Land$Group=="Cropland"&NL_Land$Type=="Share"&NL_Land$Year=="2013"])
SSP_all.NLa = SSP_all.NLa %>% mutate(LandPasture_NL = LandPasture * NL_Land$value[NL_Land$Group=="Pasture"&NL_Land$Type=="Share"&NL_Land$Year=="2013"])
SSP_all.NLa = subset(SSP_all.NLa, select=-c(LandCropland,LandOtherArableLand,LandPasture))
names(SSP_all.NLa)[names(SSP_all.NLa) == "LandCropland_NL"] = "LandCropland"
names(SSP_all.NLa)[names(SSP_all.NLa) == "LandOtherArableLand_NL"] = "LandOtherArableLand"
names(SSP_all.NLa)[names(SSP_all.NLa) == "LandPasture_NL"] = "LandPasture"

SSP_all.NLa=subset(SSP_all.NLa, select=-c(VarID,VarID2))
SSP_all.NLa=melt(SSP_all.NLa, id.vars=c("Model","Scenario","Unit","Year","Region"), na.rm=TRUE)
names(SSP_all.NLa)[names(SSP_all.NLa) == "variable"] = "Variable"
SSP_all.NLa$VarID = substr(SSP_all.NLa$Variable,start=1,stop=8)
SSP_all.NLa$VarID2 = substr(SSP_all.NLa$Variable,start=1,stop=4)

# Make required columns so that other downscaling methods can be scaled to this
SSP_all.NLa$CorVar=paste(SSP_all.NLa$Scenario,SSP_all.NLa$Variable, SSP_all.NLa$Unit)
SSP_all.NLa$CorVar2=paste(SSP_all.NLa$Scenario,SSP_all.NLa$Variable, SSP_all.NLa$Unit,SSP_all.NLa$Year)
SSP_all.NLa2010=subset(SSP_all.NLa,Year=="2010")
SSP_all.NLa$value2010 <- SSP_all.NLa2010[match(SSP_all.NLa$CorVar,SSP_all.NLa2010$CorVar),7]
#
# ----*** Downscaling (ii) ----
#
#   (ii) Assume the trends in the share of GHG emissions and agricultural production 
#        in the WEU represented by the Netherlands are extrapolated until 2050
#
SSP_all.NLb = SSP_all.NL
# First Determine trends of shares of NL in WEU Emissions 
      plot(EuroStat_Emis$Year[EuroStat_Emis$Country=="NLFrac"],EuroStat_Emis$value[EuroStat_Emis$Country=="NLFrac"])
      abline(lm(value ~ Year, data=subset(EuroStat_Emis, Country=="NLFrac")))

EuroStat_Emis.Cor=subset(EuroStat_Emis, Year>"2005")      
fit <- lm(value ~ Year, data=subset(EuroStat_Emis.Cor, Country=="NLFrac"))
Coef <- c(coef(summary(fit))["Year","Estimate"],coef(summary(fit))["(Intercept)","Estimate"])
      
      jpeg("output/Diagnostic/NL_DS2_Emis.jpg")
        plot(EuroStat_Emis.Cor$Year[EuroStat_Emis.Cor$Country=="NLFrac"],EuroStat_Emis.Cor$value[EuroStat_Emis.Cor$Country=="NLFrac"])
        abline(lm(value ~ Year, data=subset(EuroStat_Emis.Cor, Country=="NLFrac")))
      dev.off()

# Correct emissions based on trend of share of NL emissions in WEU for 2000:2015 (EU report?)
SSP_all.NLb = SSP_all.NLb %>% mutate(EmisCH4LandUse_NL = EmisCH4LandUse * (Coef[1]*Year+Coef[2]))
SSP_all.NLb = SSP_all.NLb %>% mutate(EmisN2OLandUse_NL = EmisN2OLandUse * (Coef[1]*Year+Coef[2]))
SSP_all.NLb = SSP_all.NLb %>% mutate(EmisCO2LandUse_NL = EmisCO2LandUse * (Coef[1]*Year+Coef[2]))
SSP_all.NLb = subset(SSP_all.NLb, select=-c(EmisCH4LandUse,EmisN2OLandUse,EmisCO2LandUse))
names(SSP_all.NLb)[names(SSP_all.NLb) == "EmisCH4LandUse_NL"] = "EmisCH4LandUse"
names(SSP_all.NLb)[names(SSP_all.NLb) == "EmisN2OLandUse_NL"] = "EmisN2OLandUse"
names(SSP_all.NLb)[names(SSP_all.NLb) == "EmisCO2LandUse_NL"] = "EmisCO2LandUse"
# Scale 2020 emission to what is reported in IPCC for the Netherlands
for(i in unique(SSP_all.NLb$Scenario)){
  NLScale.CH41 = subset(SSP_all.NLb, Year=="2020"&VarID=="EmisCH4L"&Scenario==i)
  NLScale.N2O1 = subset(SSP_all.NLb, Year=="2020"&VarID=="EmisN2OL"&Scenario==i&Unit=="Mt CO2eq/yr")
  NLScale.CH4 = NL_Emis.IPCC$MtCO2[NL_Emis.IPCC$Gas=="CH4"]/NLScale.CH41[,16]
  NLScale.N2O = NL_Emis.IPCC$MtCO2[NL_Emis.IPCC$Gas=="N2O"]/NLScale.N2O1[,17]
  SSP_all.NLb$EmisCH4LandUse2[SSP_all.NLb$Scenario==i] = SSP_all.NLb$EmisCH4LandUse[SSP_all.NLb$Scenario==i] * NLScale.CH4
  SSP_all.NLb$EmisN2OLandUse2[SSP_all.NLb$Scenario==i] = SSP_all.NLb$EmisN2OLandUse[SSP_all.NLb$Scenario==i] * NLScale.N2O
  rm(NLScale.CH41,NLScale.N2O1,NLScale.CH4,NLScale.N2O)
}

SSP_all.NLb$EmisCH4LandUse <- NULL
SSP_all.NLb$EmisN2OLandUse <- NULL
colnames(SSP_all.NLb)[colnames(SSP_all.NLb) == 'EmisCH4LandUse2'] <- 'EmisCH4LandUse'
colnames(SSP_all.NLb)[colnames(SSP_all.NLb) == 'EmisN2OLandUse2'] <- 'EmisN2OLandUse'
rm(NLScale.CH41,NLScale.N2O1,NLScale.CH4,NLScale.N2O,Coef,fit)
# Correct agricultural production based on share of NL production in WEU for 2015 (FAO Data)
      plot(NL_Prod$Year[NL_Prod$Type=="Share"&NL_Prod$Group=="Crops"],NL_Prod$value[NL_Prod$Type=="Share"&NL_Prod$Group=="Crops"])
      abline(lm(value ~ Year, data=subset(NL_Prod, Group=="Crops"&Type=="Share")))
      
      plot(NL_Prod$Year[NL_Prod$Type=="Share"&NL_Prod$Group=="Meat"],NL_Prod$value[NL_Prod$Type=="Share"&NL_Prod$Group=="Meat"])
      abline(lm(value ~ Year, data=subset(NL_Prod, Group=="Meat"&Type=="Share")))

NL_Prod.Cor = subset(NL_Prod, Year>"2005")      
Coef_Prod=matrix(ncol=3, nrow=length(unique(NL_Prod.Cor$Group)))
l=0
for(i in unique(NL_Prod.Cor$Group)){
  l=l+1
  fit <- lm(value ~ Year, data=subset(NL_Prod.Cor, Group==i&Type=="Share"))
  Coef_Prod[l,1] <- i
  Coef_Prod[l,2] = coef(summary(fit))["Year","Estimate"]
  Coef_Prod[l,3] = coef(summary(fit))["(Intercept)","Estimate"]
}
Coef_Prod <- as.data.frame(Coef_Prod)
colnames(Coef_Prod) <- c("Group","Gradient","Intercept")
Coef_Prod$Gradient = as.numeric(substr(Coef_Prod$Gradient, start=1, stop=30))
Coef_Prod$Intercept = as.numeric(substr(Coef_Prod$Intercept, start=1, stop=5))

      jpeg("output/Diagnostic/NL_DS2_ProdCrops.jpg")
        plot(NL_Prod.Cor$Year[NL_Prod.Cor$Type=="Share"&NL_Prod.Cor$Group=="Crops"],NL_Prod.Cor$value[NL_Prod.Cor$Type=="Share"&NL_Prod.Cor$Group=="Crops"])
        abline(lm(value ~ Year, data=subset(NL_Prod.Cor, Group=="Crops"&Type=="Share")))
      dev.off()
        
      jpeg("output/Diagnostic/NL_DS2_ProdMeat.jpg")
        plot(NL_Prod.Cor$Year[NL_Prod.Cor$Type=="Share"&NL_Prod.Cor$Group=="Meat"],NL_Prod.Cor$value[NL_Prod.Cor$Type=="Share"&NL_Prod.Cor$Group=="Meat"])
        abline(lm(value ~ Year, data=subset(NL_Prod.Cor, Group=="Meat"&Type=="Share")))
      dev.off()

SSP_all.NLb = SSP_all.NLb %>% mutate(AgriProdCropsEnergy_NL = AgriProdCropsEnergy * (Coef_Prod$Gradient[Coef_Prod$Group=="Crops"] * Year + Coef_Prod$Intercept[Coef_Prod$Group=="Crops"]))
SSP_all.NLb = SSP_all.NLb %>% mutate(AgriProdCropsNonEnergy_NL = AgriProdCropsNonEnergy * (Coef_Prod$Gradient[Coef_Prod$Group=="Crops"] * Year + Coef_Prod$Intercept[Coef_Prod$Group=="Crops"]))
SSP_all.NLb = SSP_all.NLb %>% mutate(AgriProdLivestock_NL = AgriProdLivestock * (Coef_Prod$Gradient[Coef_Prod$Group=="Meat"] * Year + Coef_Prod$Intercept[Coef_Prod$Group=="Meat"]))
SSP_all.NLb = subset(SSP_all.NLb, select=-c(AgriProdCropsEnergy,AgriProdCropsNonEnergy,AgriProdLivestock))
names(SSP_all.NLb)[names(SSP_all.NLb) == "AgriProdCropsEnergy_NL"] = "AgriProdCropsEnergy"
names(SSP_all.NLb)[names(SSP_all.NLb) == "AgriProdCropsNonEnergy_NL"] = "AgriProdCropsNonEnergy"
names(SSP_all.NLb)[names(SSP_all.NLb) == "AgriProdLivestock_NL"] = "AgriProdLivestock"
rm(Coef_Prod,fit)

# Correct land use
      plot(NL_Land$Year[NL_Land$Type=="Share"&NL_Land$Group=="Cropland"],NL_Land$value[NL_Land$Type=="Share"&NL_Land$Group=="Cropland"])
      abline(lm(value ~ Year, data=subset(NL_Land, Group=="Cropland"&Type=="Share")))
      
      plot(NL_Land$Year[NL_Land$Type=="Share"&NL_Land$Group=="Pasture"],NL_Land$value[NL_Land$Type=="Share"&NL_Land$Group=="Pasture"])
      abline(lm(value ~ Year, data=subset(NL_Land, Group=="Pasture"&Type=="Share")))

NL_Land.Cor = subset(NL_Land, Year>"2005")      

Coef_Prod=matrix(ncol=3, nrow=length(unique(NL_Land.Cor$Group)))
l=0
for(i in unique(NL_Land.Cor$Group)){
  l=l+1
  fit <- lm(value ~ Year, data=subset(NL_Land.Cor, Group==i&Type=="Share"))
  Coef_Prod[l,1] <- i
  Coef_Prod[l,2] = coef(summary(fit))["Year","Estimate"]
  Coef_Prod[l,3] = coef(summary(fit))["(Intercept)","Estimate"]
}
Coef_Prod <- as.data.frame(Coef_Prod)
colnames(Coef_Prod) <- c("Group","Gradient","Intercept")
Coef_Prod$Gradient = as.numeric(substr(Coef_Prod$Gradient, start=1, stop=30))
Coef_Prod$Intercept = as.numeric(substr(Coef_Prod$Intercept, start=1, stop=5))

      jpeg("output/Diagnostic/NL_DS2_LandCrops.jpg")
        plot(NL_Land.Cor$Year[NL_Land.Cor$Type=="Share"&NL_Land.Cor$Group=="Cropland"],NL_Land.Cor$value[NL_Land.Cor$Type=="Share"&NL_Land.Cor$Group=="Cropland"])
        abline(lm(value ~ Year, data=subset(NL_Land.Cor, Group=="Cropland"&Type=="Share")))
      dev.off()
      jpeg("output/Diagnostic/NL_DS2_LandPasture.jpg")
        plot(NL_Land.Cor$Year[NL_Land.Cor$Type=="Share"&NL_Land.Cor$Group=="Pasture"],NL_Land.Cor$value[NL_Land.Cor$Type=="Share"&NL_Land.Cor$Group=="Pasture"])
        abline(lm(value ~ Year, data=subset(NL_Land.Cor, Group=="Pasture"&Type=="Share")))
      dev.off()

SSP_all.NLb = SSP_all.NLb %>% mutate(LandCropland_NL = LandCropland * (Coef_Prod$Gradient[Coef_Prod$Group=="Cropland"] * Year + Coef_Prod$Intercept[Coef_Prod$Group=="Cropland"]))
SSP_all.NLb = SSP_all.NLb %>% mutate(LandOtherArableLand_NL = LandOtherArableLand * (Coef_Prod$Gradient[Coef_Prod$Group=="Cropland"] * Year + Coef_Prod$Intercept[Coef_Prod$Group=="Cropland"]))
SSP_all.NLb = SSP_all.NLb %>% mutate(LandPasture_NL = LandPasture * (Coef_Prod$Gradient[Coef_Prod$Group=="Pasture"] * Year + Coef_Prod$Intercept[Coef_Prod$Group=="Pasture"]))
SSP_all.NLb = subset(SSP_all.NLb, select=-c(LandCropland,LandOtherArableLand,LandPasture))
names(SSP_all.NLb)[names(SSP_all.NLb) == "LandCropland_NL"] = "LandCropland"
names(SSP_all.NLb)[names(SSP_all.NLb) == "LandOtherArableLand_NL"] = "LandOtherArableLand"
names(SSP_all.NLb)[names(SSP_all.NLb) == "LandPasture_NL"] = "LandPasture"
rm(Coef_Prod,fit)

SSP_all.NLb=subset(SSP_all.NLb, select=-c(VarID,VarID2))
SSP_all.NLb=melt(SSP_all.NLb, id.vars=c("Model","Scenario","Unit","Year","Region"), na.rm=TRUE)
names(SSP_all.NLb)[names(SSP_all.NLb) == "variable"] = "Variable"

# Get trends and harmonize to 2010 value of SSP_all.NLa
SSP_all.NLb$CorVar=paste(SSP_all.NLb$Scenario,SSP_all.NLb$Variable, SSP_all.NLb$Unit)
SSP_all.NLb$CorVar2=paste(SSP_all.NLb$Scenario,SSP_all.NLb$Variable, SSP_all.NLb$Unit,SSP_all.NLb$Year)
SSP_all.NLb2010=subset(SSP_all.NLb,Year=="2010")
SSP_all.NLb$value2010<- SSP_all.NLb2010[match(SSP_all.NLb$CorVar,SSP_all.NLb2010$CorVar),7]
SSP_all.NLb = SSP_all.NLb %>% mutate(Index2010 = value / value2010)

SSP_all.NLb2 = SSP_all.NLa
SSP_all.NLb2$Index2010 <- SSP_all.NLb[match(SSP_all.NLb2$CorVar2,SSP_all.NLb$CorVar2),11]
SSP_all.NLb2 = SSP_all.NLb2 %>% mutate (valueCor = value2010*Index2010)
SSP_all.NLb2 = subset(SSP_all.NLb2, select=-c(value2010,Index2010,value))
colnames(SSP_all.NLb2)[11] <- "value"
SSP_all.NLb2$Region <- "NL_1"

# For Variable==AgriProdCropsEnergy, use downscaled value as trends dont work since 2010 value=0
SSP_all.NLbEne = subset(SSP_all.NLb, Variable=="AgriProdCropsEnergy")
for(i in unique(SSP_all.NLbEne$CorVar2)){
SSP_all.NLb2$value[SSP_all.NLb2$CorVar2==i] <- SSP_all.NLbEne$value[SSP_all.NLbEne$CorVar2==i]
}

# Have to correct historic production down downscale method (i) since use of trend creates mistakes
test = subset(SSP_all.NLb2, (Variable=="AgriProdCropsNonEnergy"|Variable=="AgriProdLivestock"|Variable=="AgriProdCropsEnergy")&(Year==1981|Year==1990|Year==2000|Year==2010|Year==2020))
SSP_all.NLb2 = subset(SSP_all.NLb2, !((Variable=="AgriProdCropsNonEnergy"|Variable=="AgriProdLivestock"|Variable=="AgriProdCropsEnergy")&(Year==1981|Year==1990|Year==2000|Year==2010|Year==2020)))
test$value <- SSP_all.NLa[match(test$CorVar2,SSP_all.NLa$CorVar2),7] 
SSP_all.NLb2 = rbind(SSP_all.NLb2,test)

rm(test)
#
# ----*** Downscaling (iii) ----
#
#   (iii) Dutch agricultural emissions reduce in line with EU policies on effort sharing, 
#         with agricultural production levels remaining constant
#
SSP_all.NLc = SSP_all.NLa
# Scale emission reduction based on the GDP share
# First determine actual reduction on the WEU level
EmReduc = subset(SSP_all, Region=="WEU"&VarID2=="Emis"&(Variable=="EmisCH4LandUse"|Variable=="EmisN2OLandUse"|Variable=="EmisCO2LandUse")&!(Unit=="kt N2O/yr"))  
EmReduc$IDVar=paste(EmReduc$Scenario,EmReduc$Variable)
EmReduc.2010 = subset(EmReduc, Year=="2010")
EmReduc$value2010=EmReduc.2010[match(EmReduc$IDVar,EmReduc.2010$IDVar),9]
rm(EmReduc.2010)
EmReduc = EmReduc %>% mutate(Reduction=value2010-value)
EmReduc = subset(EmReduc, select=-c(IDVar, value2010))
EmReduc = subset(EmReduc, !(Year=="1981"|Year=="1990"|Year=="2000"))
# Determine NL reduction based on GDP share
EmReduc$ReducShare = NL_ReducShare[match(EmReduc$Year,NL_ReducShare$Year),2] 
EmReduc$ReducShare[is.na(EmReduc$ReducShare)] <- 0.040059835                 #Set post 2050 values to 2050 level  
EmReduc = EmReduc %>% mutate (NL = Reduction * ReducShare)
# Thus get emission projections for NL
NLcEmis=matrix(ncol=12, nrow = length(unique(EmReduc$Scenario)) * length(unique(EmReduc$Variable)) * length(unique(EmReduc$Year)))
l=0
for(i in unique(EmReduc$Scenario)){
  for(j in unique(EmReduc$Variable)){
    for(k in unique(EmReduc$Year)){
      l=l+1
      NLReduction=subset(EmReduc, Scenario==i&Variable==j&Year==k)
      temp=subset(SSP_all.NLc, Scenario==i&Variable==j&Year==2010&!(Unit=="kt N2O/yr"))
      temp2 =  temp$value2010 - NLReduction$NL
      NLcEmis[l,1:12] <- c("IMAGE",i,"Mt CO2eq/yr",k,"NL_2",j,temp2,temp$VarID,temp$VarID2,temp$CorVar,temp$CorVar2,temp$value2010)
    }
  }
}
NLcEmis <- as.data.frame(NLcEmis)
colnames(NLcEmis) <- c("Model","Scenario","Unit","Year","Region","Variable","value","VarID","VarID2","CorVar","CorVar2","value2010")
NLcEmis$Year=as.numeric(substr(NLcEmis$Year, start=1, stop=4))
NLcEmis$value=as.numeric(substr(NLcEmis$value, start=1, stop=6))
NLcEmis$value2010=as.numeric(substr(NLcEmis$value2010, start=1, stop=6))
# Attach corrected emissions to main NLc dataset
SSP_all.NLc=subset(SSP_all.NLc, !VarID2=="Emis")
SSP_all.NLc=rbind(SSP_all.NLc,NLcEmis)
rm(NLcEmis,temp,temp2,i,j,k,l,EmReduc)
# In order to avoid errors when smoothing (later on, add historic emissions for 1981,1990 and 2000)
HistEmis=subset(SSP_all.NLa, VarID2=="Emis"&!(Unit=="kt N2O/yr")&(Year=="1981"|Year=="1990"|Year=="2000"))
SSP_all.NLc=rbind(SSP_all.NLc,HistEmis)
# Scale Agricultural Production to GDP share

# Scale Land Use to GDP Share

SSP_all.NLc$Region <- "NL_2"

#
# ----*** Final Dataset ----
SSP_all.NLa2 = subset(SSP_all.NLa, select=-c(CorVar,CorVar2,value2010))
SSP_all.NLb2 = subset(SSP_all.NLb2, select=-c(CorVar,CorVar2))
SSP_all.NLc2 = subset(SSP_all.NLc, select=-c(CorVar,CorVar2,value2010))

SSP_all = rbind(SSP_all.temp,SSP_all.NLa2,SSP_all.NLb2,SSP_all.NLc2,SSP_all.RCP)

rm(SSP_all.NLa,SSP_all.NLa2,
   SSP_all.NLb,SSP_all.NLb2,EuroStat_Emis.Cor,NL_Prod.Cor,NL_Land.Cor,
   SSP_all.NLc,SSP_all.NLc2)

Population = subset(SSP_all, Variable=="Population")
Population$VarID3 = paste(Population$Scenario,Population$Year,Population$Region)

# ---- MAKE RELEVANT DFs ----
# Make DF of Agricultural Propduction
SSP_all.AP = subset(SSP_all, VarID=="AgriProd")
SSP_all.AP = subset(SSP_all.AP, select=-c(VarID,VarID2))
SSP_all.AP = spread(SSP_all.AP,Variable,value)
# Make DF of Emissions
SSP_all.EM = subset(SSP_all, VarID2=="Emis"&(Unit=="Mt CO2/yr"|Unit=="Mt CO2eq/yr"))
SSP_all.EM=na.omit(SSP_all.EM)
SSP_all.EM = subset(SSP_all.EM, select=-c(VarID,VarID2,Unit))
SSP_all.EM = spread(SSP_all.EM,Variable,value)
# Make DF of Land Cover
SSP_all.LC = subset(SSP_all, VarID2=="Land")
SSP_all.LC = subset(SSP_all.LC, select=-c(VarID,VarID2))
SSP_all.LC = spread(SSP_all.LC,Variable,value)

#
# ---- CALCULATIONS LAND COVER ----
# LANDCOVER
SSP_all.LC = SSP_all.LC %>% mutate(TotalLandUse=LandCropland+LandOtherArableLand+LandPasture)
SSP_all.LC = subset(SSP_all.LC, select=c(Model,Scenario,Year,Region,LandCropland,LandOtherArableLand,LandPasture,TotalLandUse))
SSP_all.LC = melt(SSP_all.LC, id.vars=c("Model","Scenario","Year","Region"))

#
# ---- CALCULATIONS EMISSIONS ----
SSP_all.EM = subset(SSP_all.EM, select=c(Model,Scenario,Year,Region,EmisCO2LandUse,EmisCH4LandUse,EmisN2OLandUse,EmisCO2FossilFuelsandIndustry))
SSP_all.EM = melt(SSP_all.EM, id.vars=c("Model","Scenario","Year","Region"))
SSP_all.EM = spread(SSP_all.EM,Year,value)

# Emissions have to be averaged over 20 years 
SSP_all.EM2=matrix(ncol=17, nrow = length(unique(SSP_all.EM$variable)) * length(unique(SSP_all.EM$Region)) * length(unique(SSP_all.EM$Scenario)))
YearCount=7:17
l=0
# have to find mean between columns 7:5,8:6,9:7...17:15
for (i in unique(SSP_all.EM$variable)){
  for(j in unique(SSP_all.EM$Region)){
    for(k in unique(SSP_all.EM$Scenario)){
      l=l+1
        for(m in YearCount){
  if(length(subset(SSP_all.EM, variable==i & Region==j & Scenario==k)[,1]) >0)
  { 
    temp=subset(SSP_all.EM, variable==i&Region==j&Scenario==k)
    Mean=(temp[,m] + temp[,m-2])/2
    SSP_all.EM2[l,1:4] <- c("IMAGE",k,j,i)
    SSP_all.EM2[l,m] <- Mean
  }  
}}}}
SSP_all.EM2 <- as.data.frame(SSP_all.EM2)
colnames(SSP_all.EM2) <- c("Model","Scenario", "Region","variable",
                         "1980","1990","2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")
for(i in 5:17) {SSP_all.EM2[,i] <- as.numeric(as.character(SSP_all.EM2[,i]))}
SSP_all.EM2 = melt(SSP_all.EM2, id.vars=c("Model","Scenario","Region","variable"))
colnames(SSP_all.EM2) <- c("Model","Scenario", "Region","variable","Year","value")
SSP_all.EM2$Year = as.numeric(substr(SSP_all.EM2$Year, start=1, stop=4))
# add values for 1990
SSP_all.EM3 = subset(SSP_all.EM2, Year=="1990")
SSP_all.EM3$VarID = paste(SSP_all.EM3$Scenario,SSP_all.EM3$Region,SSP_all.EM3$variable,SSP_all.EM3$Year)

SSP_all.EM4 = melt(SSP_all.EM,id.vars=c("Model","Scenario","variable","Region"))
colnames(SSP_all.EM4)[5] <-"Year"
SSP_all.EM4$VarID = paste(SSP_all.EM4$Scenario,SSP_all.EM4$Region,SSP_all.EM4$variable,SSP_all.EM4$Year)
SSP_all.EM4 = subset(SSP_all.EM4, Year=="1990")
SSP_all.EM3$value <- SSP_all.EM4[match(SSP_all.EM3$VarID,SSP_all.EM4$VarID),6]
SSP_all.EM3$VarID <- NULL
SSP_all.EM2 = subset(SSP_all.EM2, !(Year=="1990"))
SSP_all.EM2 = rbind(SSP_all.EM2,SSP_all.EM3)
rm(l,temp,i,j,k,m,Mean,YearCount,SSP_all.EM3,SSP_all.EM4)
# Calibrate LULUCF emissions
NLCorLULUCF = NL_Emis$value[NL_Emis$VARIABLE=="LULUCF"&NL_Emis$Year=="2015"] - SSP_all.EM2$value[SSP_all.EM2$Year=="2010"&SSP_all.EM2$variable=="EmisCO2LandUse"&SSP_all.EM2$Scenario=="SSP1_450"&SSP_all.EM2$Region=="NL"]
SSP_all.EMtemp = subset(SSP_all.EM2, Region=="NL"&variable=="EmisCO2LandUse")
SSP_all.EMtemp1 = subset(SSP_all.EM2, !(Region=="NL"&variable=="EmisCO2LandUse"))
SSP_all.EMtemp$value1 = SSP_all.EMtemp$value + NLCorLULUCF
SSP_all.EMtemp$value <- NULL
names(SSP_all.EMtemp)[names(SSP_all.EMtemp) == "value1"] = "value"
SSP_all.EM2 = rbind(SSP_all.EMtemp,SSP_all.EMtemp1)
# Have to Calibrate historic CH4 and N2O Emissions for NL, which have been affected by the smoothing
  # First Isolate NL
SSP_all.EM2_NL = subset(SSP_all.EM2, (Region=="NL"|Region=="NL_1"|Region=="NL_2"))
SSP_all.EM2 = subset(SSP_all.EM2, !(Region=="NL"|Region=="NL_1"|Region=="NL_2"))
  # Determine required Correction Factors (historic)
test=subset(SSP_all.EM2_NL, variable=="EmisCH4LandUse"|variable=="EmisN2OLandUse")
test$Historic[test$variable=="EmisCH4LandUse" & test$Year==2020] = NL_Emis.IPCC$MtCO2[NL_Emis.IPCC$Gas=="CH4"]
test$Historic[test$variable=="EmisN2OLandUse" & test$Year==2020] = NL_Emis.IPCC$MtCO2[NL_Emis.IPCC$Gas=="N2O"]
test$Historic[test$variable=="EmisCH4LandUse" & test$Year==1990] = 15
test$Historic[test$variable=="EmisN2OLandUse" & test$Year==1990] = 10
test$Historic[test$variable=="EmisCH4LandUse" & test$Year==2000] = 13.79
test$Historic[test$variable=="EmisN2OLandUse" & test$Year==2000] = 9.506
test$Historic[test$variable=="EmisCH4LandUse" & test$Year==2010] = 13.46
test$Historic[test$variable=="EmisN2OLandUse" & test$Year==2010] = 9.342
test = test %>% mutate(Correction = Historic/value)
  # Future correction values based on 2020
test$AgrID1 = paste(test$Scenario,test$Region,test$variable)
test$AgrID = paste(test$Scenario,test$Region,test$variable,test$Year)
test1 = subset(test, Year > 2020)
test2 = subset(test, Year == 2020)
test = subset(test, Year <= 2020)
test1$Correction <- test2[match(test1$AgrID1,test2$AgrID1),8] 
test=rbind(test,test1)
test$AgrID1 <-NULL
rm(test1,test2)
  # Make correction to historic and future values
SSP_all.EM2_NL$AgrID <- paste(SSP_all.EM2_NL$Scenario,SSP_all.EM2_NL$Region,SSP_all.EM2_NL$variable,SSP_all.EM2_NL$Year)
SSP_all.EM2_NL$Correction <- test[match(SSP_all.EM2_NL$AgrID,test$AgrID),8] 
SSP_all.EM2_NL$Correction[is.na(SSP_all.EM2_NL$Correction)]<- 1
SSP_all.EM2_NL = SSP_all.EM2_NL %>% mutate(value_cor = value * Correction)
SSP_all.EM2_NL = subset(SSP_all.EM2_NL, select=-c(value,AgrID,Correction))
colnames(SSP_all.EM2_NL)[6] <- "value"
  # Merge corrected NL values
SSP_all.EM2 = rbind(SSP_all.EM2,SSP_all.EM2_NL)
rm(test,SSP_all.EM2_NL)
#Total Emissions (have to correct due to NL calibration)
SSP_all.EM2 = spread(SSP_all.EM2,variable,value)
SSP_all.EM2 = SSP_all.EM2 %>% mutate(TotalAgrEmissions=EmisCH4LandUse+EmisN2OLandUse)
SSP_all.EM2 = melt(SSP_all.EM2, id.vars=c("Model","Scenario","Year","Region"))

#
# ---- CALCULATIONS AGRI. PROD. ----
SSP_all.AP = SSP_all.AP %>% mutate(TotalProduction=AgriProdLivestock+AgriProdCropsEnergy+AgriProdCropsNonEnergy)
SSP_all.AP = subset(SSP_all.AP, select=c(Model,Scenario,Year,Region,AgriProdLivestock,AgriProdCropsEnergy,AgriProdCropsNonEnergy,TotalProduction))
SSP_all.AP = melt(SSP_all.AP, id.vars=c("Model","Scenario","Year","Region"))
SSP_all.AP = spread(SSP_all.AP,Year,value)

# Production has to be averaged over past 20 years 
SSP_all.AP2=matrix(ncol=17, nrow = length(unique(SSP_all.AP$variable)) * length(unique(SSP_all.AP$Region)) * length(unique(SSP_all.AP$Scenario)))
YearCount=7:17
l=0
# have to find mean between columns 7:5,8:6,9:7...17:15
for (i in unique(SSP_all.AP$variable)){
  for(j in unique(SSP_all.AP$Region)){
    for(k in unique(SSP_all.AP$Scenario)){
      l=l+1
      for(m in YearCount){
        if(length(subset(SSP_all.AP, variable==i & Region==j & Scenario==k)[,1]) >0)
        { 
          temp=subset(SSP_all.AP, variable==i&Region==j&Scenario==k)
          Mean=(temp[,m]+ temp[,m-1] + temp[,m-2])/3
          SSP_all.AP2[l,1:4] <- c("IMAGE",k,j,i)
          SSP_all.AP2[l,m] <- Mean
        }  
      }}}}
SSP_all.AP2 <- as.data.frame(SSP_all.AP2)
colnames(SSP_all.AP2) <- c("Model","Scenario", "Region","variable",
                         "1980","1990","2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")
for(i in 5:17) {SSP_all.AP2[,i] <- as.numeric(as.character(SSP_all.AP2[,i]))}
SSP_all.AP2 = melt(SSP_all.AP2, id.vars=c("Model","Scenario","Region","variable"))
colnames(SSP_all.AP2) <- c("Model","Scenario", "Region","variable","Year","value")
SSP_all.AP2$Year = as.numeric(substr(SSP_all.AP2$Year, start=1, stop=4))
rm(l,i,j,k,m,YearCount,temp,Mean)
# ---- CALCULATIONS EIA ----
# Consolidated DF
EIA = rbind(SSP_all.LC,SSP_all.EM2,SSP_all.AP2)
EIA = spread(EIA,variable,value)

EIA = EIA %>% mutate(EIA_DM=TotalAgrEmissions/TotalProduction) # MtCO2e/t-DM
EIA = EIA %>% mutate(EIA_Ha=TotalAgrEmissions/TotalLandUse)    # tCO2e/Ha (MtCO2e/MHa)
EIA = melt(EIA, id.vars=c("Model","Scenario","Year","Region"))
EIA$Year[EIA$Year=="1981"]<-"1980"
EIA$Year = as.numeric(substr(EIA$Year, start=1, stop=4))
EIA$RegOrder = factor(EIA$Region, levels=c("CAN","USA","MEXCAM","MEX","RCAM","BRA","RSAM","EU","WEU","CEU","NL","NL_1","NL_2","TUR","UKR","RUS","STAN","CHN","KOR","JAP","INDIA","RSAS","INDO","SEAS","NAF","ME","WAF","EAF","RSAF","SAF","OCE","World",
                                           "OECD90","REF","ASIA","MAF","LAM"))
EIA$RegOrder2 = EIA$RegOrder
EIA$RegOrder2[EIA$RegOrder=="NL_1"|EIA$RegOrder=="NL_2"] <- "NL" 
EIA$ScenOrder = factor(EIA$Scenario, levels=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"))

#
# ---- GLOBAL DATA ----
RegAggr <- data.frame(c(unique(SSP_all.R$Region)),
                      #"RCAM" "UKR" "SAF" "World" "OCE" "RUS" "TUR" "JAP" "USA" "INDIA" "RSAF"  "STAN""NAF" "RSAS" "MEX" "RSAM""SEAS" "WAF" "BRA" "CHN"  "WEU""CAN" "EAF""CEU" "INDO" "ME"  "KOR"  
                      c("LAM","FSU","AFR","World","OCE","FSU","ASIA","ASIA","NAME","ASIA","AFR","FSU","NAF","ASIA","LAM","LAM","ASIA","AFR","LAM","ASIA","EU","NAM","AFR","EU","ASIA","ME","ASIA"))
colnames(RegAggr)<-c("IMAGE","MegaRegions")

# TRADE BALANCE
TradBal = subset(SSP_all.R, Variable=="AgriProdCropsTotal"|Variable=="AgriDemCropsTotal")
TradBal = subset(TradBal, select=-c(VarID, VarID2))
TradBal=spread(TradBal,Region,value)
TradBal = TradBal %>% mutate(NAM=CAN+USA)
TradBal = TradBal %>% mutate(LAM=MEX+RCAM+BRA+RSAM)
TradBal = TradBal %>% mutate(EU=WEU+CEU)
TradBal = TradBal %>% mutate(AFR=NAF+WAF+EAF+RSAF+SAF)
TradBal = TradBal %>% mutate(FSU=UKR+RUS+STAN)
TradBal = TradBal %>% mutate(ASIA=CHN+JAP+INDIA+SEAS+RSAS+TUR+INDO+KOR)
TradBal=melt(TradBal, id.vars=c("Model","Scenario","Variable","Unit","Year"))
colnames(TradBal)[6] <- "Region2"
TradBal=subset(TradBal, Region2=="NAM"|Region2=="LAM"|Region2=="EU"|Region2=="AFR"|Region2=="FSU"|Region2=="ASIA"|Region2=="ME"|Region2=="OCE"|Region2=="World")
TradBal=spread(TradBal,Variable,value)
TradBal=TradBal %>% mutate(NetCropTrade=AgriProdCropsTotal-AgriDemCropsTotal)
TradBal$ScenOrder=factor(TradBal$Scenario, levels=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"))
# AGRICULTURAL PRODUCTION
AgriProd = subset(SSP_all.AP2)
AgriProd = subset(AgriProd, variable=="TotalProduction")
AgriProd=spread(AgriProd,Region,value)
AgriProd = AgriProd %>% mutate(NAM=CAN+USA)
AgriProd = AgriProd %>% mutate(LAM=MEX+RCAM+BRA+RSAM)
AgriProd = AgriProd %>% mutate(EU=WEU+CEU)
AgriProd = AgriProd %>% mutate(AFR=NAF+WAF+EAF+RSAF+SAF)
AgriProd = AgriProd %>% mutate(FSU=UKR+RUS+STAN)
AgriProd = AgriProd %>% mutate(ASIA=CHN+JAP+INDIA+SEAS+RSAS+TUR+INDO+KOR)
AgriProd=melt(AgriProd, id.vars=c("Model","Scenario","variable","Year"))
colnames(AgriProd)[5] <- "Region2"

# Fraction of global production
AgriProd.frac=matrix(ncol=6, nrow = length(unique(AgriProd$Year)) * length(unique(AgriProd$Region2)) * length(unique(AgriProd$Scenario)))
#YearCount=7:17
l=0
for (i in unique(AgriProd$Year)){
  for(j in unique(AgriProd$Region2)){
    for(k in unique(AgriProd$Scenario)){
      l=l+1
        if(length(subset(AgriProd, Year==i & Region2==j & Scenario==k)[,1]) >0)
        { 
          temp=subset(AgriProd, Year==i&Region2==j&Scenario==k)
          Frac=temp$value / AgriProd$value[AgriProd$Year==i&AgriProd$Scenario==k&AgriProd$Region2=="World"]
          AgriProd.frac[l,] <- c("IMAGE",k,"AgriProdFrac",i,j,Frac)
        }  
      }}}
AgriProd.frac <- as.data.frame(AgriProd.frac)
colnames(AgriProd.frac) <- c("Model","Scenario", "variable","Year","Region2","value")

AgriProd=rbind(AgriProd,AgriProd.frac)
AgriProd$value=as.numeric(substr(AgriProd$value, start=1, stop=5))
AgriProd$Year=as.numeric(substr(AgriProd$Year, start=1, stop=4))
AgriProd$ScenOrder=factor(AgriProd$Scenario, levels=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"))
rm(l,i,j,k,temp,Frac,AgriProd.frac)

AgriProd2=subset(AgriProd, select=-c(Model))
AgriProd.temp=AgriProd2
AgriProd2=spread(AgriProd2,Year,value)
AgriProd=subset(AgriProd, Region2=="NAM"|Region2=="LAM"|Region2=="EU"|Region2=="AFR"|Region2=="FSU"|Region2=="ASIA"|Region2=="ME"|Region2=="OCE"|Region2=="World")

#
# ---- RESULTS FOR PAPER ----
# Evolution of Agricultural Emissions
EmisDat = spread(SSP_all.EM2,Year,value)
for(i in 5:17) {colnames(EmisDat)[i] <- paste("x",colnames(EmisDat[i]),sep="")}
EmisDat = EmisDat %>% mutate(x50_10_Change=((x2050-x2010)/x2010)*100)
EmisDat = EmisDat %>% mutate(x50_10_diff=x2050-x2010)
EmisDat = EmisDat %>% mutate(x100_10_diff=x2100-x2010)

# Comparison with EU regulations
EmisDat = melt(EmisDat, id.vars=c("Model","Scenario","variable","Region"))
colnames(EmisDat)[5] <- "Year"
EmisDat=spread(EmisDat,variable,value)
EmisDat=EmisDat %>% mutate(NonCO2Emis=EmisCH4LandUse+EmisN2OLandUse)

EmisDat.temp2=matrix(ncol=5, nrow = length(unique(EmisDat$Year)) * length(unique(EmisDat$Region)) * length(unique(EmisDat$Scenario)))
l=0
for(i in unique(EmisDat$Scenario)){
  for(j in unique(EmisDat$Region)){
    for(k in unique(EmisDat$Year)){
      l=l+1
      temp = subset(EmisDat, Scenario==i&Region==j&Year==k)
      Base = subset(EmisDat, Scenario==i&Region==j&Year=="x1990")
      EmisDat.temp1 <- ((temp$NonCO2Emis-Base$NonCO2Emis)/Base$NonCO2Emis)*100
      EmisDat.temp2[l,] <- c("IMAGE",i,j,k,EmisDat.temp1)
    }
  }
}
EmisDat.temp2 <- as.data.frame(EmisDat.temp2)
colnames(EmisDat.temp2) <- c("Model","Scenario", "Region","Year","NonCO2Emis_1990")
EmisDat.temp2=melt(EmisDat.temp2,id.vars=c("Model","Scenario","Region","Year"))
EmisDat=melt(EmisDat,id.vars=c("Model","Scenario","Region","Year"))
EmisDat=rbind(EmisDat,EmisDat.temp2)
EmisDat$Year=substr(EmisDat$Year, start=2,stop=15)
EmisDat$value=as.numeric(substr(EmisDat$value, start=1,stop=5))
EmisDat=spread(EmisDat,Year,value)
EmisDat=subset(EmisDat, select=-c(Model,1980))
EmisDat <- EmisDat[,c("Scenario","Region","variable","1990","2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100","50_10_Change","50_10_diff","100_10_diff")]
rm(l,i,j,k,temp,Base,EmisDat.temp1,EmisDat.temp2)
# Evolution of Agricultural Production
AgrProdDat = spread(SSP_all.AP2,Year,value)
for(i in 5:17) {colnames(AgrProdDat)[i] <- paste("x",colnames(AgrProdDat[i]),sep="")}
AgrProdDat = AgrProdDat %>% mutate(x50_10_Change=((x2050-x2010)/x2010)*100)
AgrProdDat = AgrProdDat %>% mutate(x50_10_diff=x2050-x2010)
AgrProdDat = AgrProdDat %>% mutate(x100_10_diff=x2100-x2010)

# Evolution of EIA
EIADat = subset(EIA, variable=="EIA_DM"|variable=="EIA_Ha")
EIADat = na.omit(EIADat)
EIADat = spread(EIADat,Year,value)
for(i in 8:19) {colnames(EIADat)[i] <- paste("x",colnames(EIADat[i]),sep="")}
EIADat = EIADat %>% mutate(x50_10_Change=((x2050-x2010)/x2010)*100)
EIADat = EIADat %>% mutate(x50_10_diff=x2050-x2010)
EIADat = EIADat %>% mutate(x100_10_diff=x2100-x2010)
EIADat = EIADat %>% mutate(AnnChange=((x2050/x2010)^(1/(2050-2010)))-1)

EIADat2 = subset(EIADat, select=-c(Model,RegOrder))
# EIA decomposition
EIADecomp = subset(EIA, variable=="TotalProduction"|variable=="TotalAgrEmissions"|variable=="TotalLandUse"|variable=="EIA_DM"|variable=="EIA_Ha")
EIADecomp = na.omit(EIADecomp)
EIADecomp = spread(EIADecomp,variable,value)

EIADecomp = EIADecomp %>% mutate(EIA_DM_EmisComp=TotalAgrEmissions/EIADecomp$TotalProduction[EIADecomp$Year=="2010"])
EIADecomp = EIADecomp %>% mutate(EIA_DM_DMComp=TotalAgrEmissions[EIADecomp$Year=="2010"]/EIADecomp$TotalProduction)
EIADecomp = EIADecomp %>% mutate(EIA_Ha_EmisComp=TotalAgrEmissions/EIADecomp$TotalLandUse[EIADecomp$Year=="2010"])
EIADecomp = EIADecomp %>% mutate(EIA_Ha_DMComp=TotalAgrEmissions[EIADecomp$Year=="2010"]/EIADecomp$TotalLandUse)

#
# ---- DFs FOR FIGURES ----
EIA.1 = subset(EIA, variable=="EIA_DM"|variable=="EIA_Ha")
EIA.1 = subset(EIA.1, Year>1999)
EIA.1 = subset(EIA.1, Region=="WEU"|Region=="World"|Region=="NL"|Region=="NL_1"|Region=="NL_2")

Emis = subset(SSP_all.EM2, Year>1999)
Emis = subset(Emis, Region=="EU"|Region=="World"|Region=="NL"|Region=="NL_1"|Region=="NL_2")
Emis = subset(Emis, !(variable=="EmisCO2LandUse"))

AgProd = subset(SSP_all.AP2, Year>1999)
AgProd = subset(AgProd, Region=="EU"|Region=="World"|Region=="NL"|Region=="NL_1"|Region=="NL_2")

Socio = subset(SSP_all, Variable=="Population"|
                      Variable=="AgriDemCropsFood"|Variable=="AgriDemCropsFeed"|
                      Variable=="YieldCereal"|
                      Variable=="LandCropland"|Variable=="LandCroplandEnergyCrops"|Variable=="LandOtherArableLand"|Variable=="LandPasture")
Socio = subset(Socio, select=-c(VarID,VarID2,Unit))
colnames(Socio)[3]<-"variable"
Socio = rbind(Socio,subset(SSP_all.LC, variable=="TotalLandUse"))
Socio=spread(Socio,variable,value)
Socio$YieldIndex = Socio$YieldCereal / Socio$YieldCereal[Socio$Year=="2010"]
Socio = Socio %>% mutate(AgriDemCropsFood_PerCap = AgriDemCropsFood/Population)
Socio = Socio %>% mutate(AgriDemCropsFeed_PerCap = AgriDemCropsFeed/Population)
Socio=melt(Socio, id.vars=c("Model","Scenario","Year","Region"))
Socio$VarOrder = factor(Socio$variable, levels=c("Population","AgriDemCropsFood","AgriDemCropsFeed","AgriDemLivestockFood","AgriDemCropsFood_PerCap","AgriDemCropsFeed_PerCap","AgriDemLivestockFood_PerCap","YieldCereal","YieldIndex","TotalLandUse"))
Socio$Unit[Socio$variable=="AgriDemCropsFeed"] <- "MtDM/yr"
Socio$Unit[Socio$variable=="AgriDemCropsFood"] <- "MtDM/yr"
Socio$Unit[Socio$variable=="Population"] <- "Million"
Socio$Unit[Socio$variable=="TotalLandUse"] <- "MHa"
Socio$Unit[Socio$variable=="LandCropland"] <- "MHa"
Socio$Unit[Socio$variable=="LandCroplandEnergyCrops"] <- "MHa"
Socio$Unit[Socio$variable=="LandOtherArableLand"] <- "MHa"
Socio$Unit[Socio$variable=="LandPasture"] <- "MHa"
Socio$Unit[Socio$variable=="YieldCereal"] <- "tDM/Ha/yr"
Socio$Unit[Socio$variable=="YieldIndex"] <- "2010=1"
Socio$Unit[Socio$variable=="AgriDemCropsFeed_PerCap"] <- "tDM/cap/yr"
Socio$Unit[Socio$variable=="AgriDemCropsFood_PerCap"] <- "tDM/cap/yr"

  # Bind livestock consumption (TOTAL and PER CAPITA)
  # Set Orders
  LivestockCons$ScenOrder = factor(LivestockCons$SCENARIO, levels=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"))
  LivestockCons$LiveOrder = factor(LivestockCons$VARIABLE, levels=c("beef","mutton & goat meat","pork","milk","poultry & eggs"))
  # Add population and per-cap variables
  LivestockCons$VarID3 = paste(LivestockCons$SCENARIO,LivestockCons$YEAR,LivestockCons$REGION)
  LivestockCons$Pop_M <- Population[match(LivestockCons$VarID3, Population$VarID3),9]
  LivestockCons = LivestockCons %>% mutate(kg_pcap = value / Pop_M * 1000)
  LivestockCons$VarID3 <- NULL
  # Make per-cap variable indexed to 2010
  LivestockCons$VarID4 = paste(LivestockCons$SCENARIO,LivestockCons$LiveOrder,LivestockCons$REGION)
  LivestockCons.2010 = subset(LivestockCons, YEAR==2010)
  LivestockCons$value_2010 <- LivestockCons.2010[match(LivestockCons$VarID4, LivestockCons.2010$VarID4),10]
  LivestockCons = LivestockCons %>% mutate(index_2010 = kg_pcap / value_2010)
  rm(LivestockCons.2010)
  LivestockCons$VarID4 <- NULL
  
  AgriDemLiveTotal = subset(LivestockCons, select=c(YEAR,VARIABLE,SCENARIO,REGION,value))
  AgriDemLiveTotal = subset(AgriDemLiveTotal, VARIABLE=="Total")
  AgriDemLiveTotal$Model<-"IMAGE"
  AgriDemLiveTotal$VARIABLE<-"AgriDemLivestockTotal"
  AgriDemLiveTotal$Unit<-"ktDM/yr"
  AgriDemLiveTotal$VarOrder<-""
  colnames(AgriDemLiveTotal)[1:8]<-c("Year","variable","Scenario","Region","value","Model","Unit","VarOrder")
  
  AgriDemLiveTotal_pc = subset(LivestockCons, select=c(YEAR,VARIABLE,SCENARIO,REGION,kg_pcap))
  AgriDemLiveTotal_pc = subset(AgriDemLiveTotal_pc, VARIABLE=="Total")
  AgriDemLiveTotal_pc$Model<-"IMAGE"
  AgriDemLiveTotal_pc$VARIABLE<-"AgriDemLivestockTotal_PerCap"
  AgriDemLiveTotal_pc$Unit<-"kgDM/cap/yr"
  AgriDemLiveTotal_pc$VarOrder<-""
  colnames(AgriDemLiveTotal_pc)[1:8]<-c("Year","variable","Scenario","Region","value","Model","Unit","VarOrder")

Socio=rbind(Socio,AgriDemLiveTotal,AgriDemLiveTotal_pc)
rm(AgriDemLiveTotal_pc,AgriDemLiveTotal)
#
Socio2=subset(Socio, selec=-c(Model,VarOrder))

Socio2=spread(Socio2,Year,value)

EmisTot = subset(SSP_all, VarID2=="Emis"&(Unit=="Mt CO2/yr"|Unit=="Mt CO2eq/yr"))
EmisTot = subset(EmisTot, Variable=="EmisCH4LandUse"|
                   Variable=="EmisCO2FossilFuelsandIndustry"|
                   Variable=="EmisCO2LandUse"|
                   Variable=="EmisN2OLandUse"| 
                   Variable=="EmisN2OEnergySupplyandDem")
# 
# # # ---- NUMERIC OUTPUTS ----
# write.xlsx(EmisDat, file="output/Results_v11.xlsx", sheetName="Emissions", row.names=FALSE, showNA = TRUE)
# write.xlsx(EIADat2, file="output/Results_v11.xlsx", sheetName="EIA", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(Socio2, file="output/Results_v11.xlsx", sheetName="Socio-Economics", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(AgrProdDat, file="output/Results_v11.xlsx", sheetName="Agricultural Production", append=TRUE, row.names=FALSE, showNA = TRUE)

# wb <- createWorkbook("output/Results_v11.xlsx")
# addWorksheet(wb, sheetName = "Emissions")
# writeData(wb, sheet = "Emissions", x = EmisDat)
# addWorksheet(wb, sheetName = "EIA")
# writeData(wb, sheet = "EIA", x = EIADat2)
# addWorksheet(wb, sheetName = "Socio-Economics")
# writeData(wb, sheet = "Socio-Economics", x = Socio2)
# addWorksheet(wb, sheetName = "Agricultural Production")
# writeData(wb, sheet = "Agricultural Production", x = AgrProdDat)
# saveWorkbook(wb, "output/Results_v11.xlsx")

#
# ---- LABELS ----
var_labels <- c("Population"="Population (Mil.)",
                  "PriceCarbon"="Carbon Price ($/tCO2)",
                  "AgriDemCropsFood"="Food Crops Dem. (MtDM/yr)",
                  "AgriDemCropsFeed"="Feed Crops Dem. (MtDM/yr)",
                  "AgriDemLivestockFood"="Livestock Dem. (Mt DM/yr)",
                  "YieldCereal"="Cereal Yields (tDM/ha/yr)",
                  "YieldIndex"="Cereal Yield Index (2010=1)",
                  "TotalLandUse"="Land Use (MHa)",
                  "AgriDemCropsFood_PerCap"="Food Crops Demand (tDM/Cap)",
                  "AgriDemLivestockFood_PerCap"="Livestock Demand (tDM/cap)",
                  "TotalAgrEmissions"="Agricultural Emissions (MtCO2-eq/yr)",
                  "TotalProduction"="Agricultural Production (MtDM/yr)")
scen_labels <-c("SSP1_450"="SSP1-2C",
                "SSP2_450"="SSP2-2C",
                "SSP1_20"="SSP1-1.5C",
                "SSP2_20"="SSP2-1.5C")
reg_labels <- c("BRA" = "Brazil",
                "CAN" = "Canada",
                "CEU" = "Central Europe",
                "CHN" = "China",
                "EAF" = "E. Africa",
                "INDIA" = "India",
                "INDO" = "Indonesia",
                "JAP" = "Japan",
                "KOR" = "Korea",
                "ME" = "Middle East",
                "MEX" = "Mexico",
                "NAF" = "N. Africa",
                "OCE" = "Oceania",
                "RCAM" = "Rest C. America",
                "RSAF" = "Rest S. Africa",
                "RSAM" = "Rest S. America",
                "RSAS" = "Rest S. Asia",
                "RUS" = "Russia",
                "SAF" = "S. Africa",
                "SEAS" = "S.E. Asia",
                "STAN" = "Kazakhstan",
                "TUR" = "Turkey",
                "UKR" = "Ukraine",
                "USA" = "USA",
                "WAF" = "W. Africa",
                "WEU" = "W. Europe",
                "OECD.Europe" = "W. Europe",
                "World" = "World",
                "EU" = "EU-28",
                "MEXCAM"= "Mexico and C. America",
                "NL" = "The Netherlands",
                "NL_1" = "The Netherlands(2)",
                "NL_2" = "The Netherlands(3)",
                "OECD90"="OECD",
                "REF"="Reforming Economies",
                "ASIA"="Asia",
                "MAF"="M. East & Africa",
                "LAM"="Lat. America")
live_labels <- c("beef" = "Ruminants",
                "mutton & goat meat" = "Non-Ruminants",
                "pork" = "Pork",
                "milk" = "Dairy",
                "poultry & eggs" = "Poultry")
                
FontSize=15 # For presentation
FontSize3=12 # For presentation - axes

FontSize2=8 # For draft

#
# ---- FIG: SSP projections ----
temp =subset(AgriProd.temp, select=-c(ScenOrder))
temp$MODEL <- "IMAGE"
colnames(temp) <- c("Scenario","variable","Year","Region","value","Model")
AllInd=rbind(subset(Socio, select=-c(VarOrder,Unit)),SSP_all.EM2,temp)
rm(temp)

AllIndFig=subset(AllInd, Year>1999&Region=="WEU"&(variable=="Population"|variable=="TotalAgrEmissions"|variable=="TotalProduction"|variable=="TotalLandUse"|variable=="AgriDemCropsFood_PerCap"|variable=="AgriDemLivestockFood_PerCap"))
AllIndFig$VarOrder = factor(AllIndFig$variable, levels =c("Population","TotalAgrEmissions","TotalProduction","TotalLandUse","AgriDemCropsFood_PerCap","AgriDemLivestockFood_PerCap"))

FigSSP_15 <-ggplot(data=subset(AllIndFig, Scenario=="SSP1_20"|Scenario=="SSP2_20"), aes(x=Year, y=value, colour=Scenario)) + 
  geom_line(size=1)+
  geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) + theme_bw() + ylab("")+xlab("") +
  ggtitle("a. Socioeconomic Projections for 1.5C target") + theme(plot.title = element_text(face="plain", size=FontSize)) +
  theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.title=element_blank(), legend.position="none") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(strip.background = element_blank()) + 
  scale_colour_manual(values=c("limegreen", "blue"),name ="Scenario",breaks=c("SSP1_20","SSP2_20"),labels=c("SSP1-1.5C","SSP2-1.5C")) +
  facet_wrap(~VarOrder,strip.position = "top", scales="free_y", labeller=labeller(VarOrder = var_labels))
FigSSP_15

FigSSP_2  <-ggplot(data=subset(AllIndFig, Scenario=="SSP1_450"|Scenario=="SSP2_450"), aes(x=Year, y=value, colour=Scenario)) + 
  geom_line(size=1)+
  geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) + theme_bw() + ylab("")+xlab("") +
  ggtitle("b. Socioeconomic Projections for 2C target") + theme(plot.title = element_text(face="plain", size=FontSize)) +
  theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.title=element_blank(), legend.position="bottom") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(strip.background = element_blank()) + 
  scale_colour_manual(values=c("limegreen", "blue"),name ="Scenario",breaks=c("SSP1_450","SSP2_450"),labels=c("SSP1","SSP2")) +
  facet_wrap(~VarOrder,strip.position = "top", scales="free_y", labeller=labeller(VarOrder = var_labels))
FigSSP_2

lay<-rbind(1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2)
FigS6 <-grid.arrange(FigSSP_15,FigSSP_2,layout_matrix=lay)
# ---- FIG: EIA WORLD----
# EIA World
# ---- ***SSP1-450 ----
FigEIAWorldSSP1_26<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP1_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                        aes(x=Year, y=value, colour=variable)) +
  geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
  theme_bw() + theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) + theme(plot.title = element_text(face="bold", size=7)) +
  ylab("") + xlab("") + 
  scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(tCO[2],-eq/Ha))), guide=FALSE)
FigEIAWorldSSP1_26

FigProdWorldSSP1_26 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) + ylim(0,7700) +
  scale_fill_manual(values=c("blueviolet","limegreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock"),guide=FALSE) 
FigProdWorldSSP1_26

FigEmisWorldSSP1_26 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"), legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
  scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")),guide=FALSE)
FigEmisWorldSSP1_26

# ---- ***SSP2-450 ----
FigEIAWorldSSP2_26<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP2_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
            aes(x=Year, y=value, colour=variable)) +
  geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
  theme_bw() + theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("") + xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(EIA[DM]),expression(EIA[Ha])),guide=FALSE)
FigEIAWorldSSP2_26

FigProdWorldSSP2_26 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP2_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) + ylim(0,7700) +
  scale_fill_manual(values=c("blueviolet","limegreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock"),guide=FALSE) 
FigProdWorldSSP2_26

FigEmisWorldSSP2_26 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP2_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"), legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
  scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")),guide=FALSE)
FigEmisWorldSSP2_26

#
# ---- ***SSP1-20 ----
FigEIAWorldSSP1_19<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP1_20"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                        aes(x=Year, y=value, colour=variable)) +
  geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
  theme_bw() + theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) + theme(plot.title = element_text(face="bold", size=7)) +
  ylab("") + xlab("") + 
  scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(tCO[2],-eq/Ha))), guide=FALSE)
FigEIAWorldSSP1_19

FigProdWorldSSP1_19 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP1_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) + ylim(0,7700) +
  scale_fill_manual(values=c("blueviolet","limegreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock"),guide=FALSE) 
FigProdWorldSSP1_19

FigEmisWorldSSP1_19 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP1_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"), legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
  scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")),guide=FALSE)
FigEmisWorldSSP1_19

# ---- ***SSP2-20 ----
FigEIAWorldSSP2_19<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP2_20"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                        aes(x=Year, y=value, colour=variable)) +
  geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
  theme_bw() + theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("") + xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(EIA[DM]),expression(EIA[Ha])))
FigEIAWorldSSP2_19

FigProdWorldSSP2_19 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP2_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) +
  scale_fill_manual(values=c("blueviolet","limegreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock")) 
FigProdWorldSSP2_19

FigEmisWorldSSP2_19 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP2_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain"), legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
  scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")))
FigEmisWorldSSP2_19
#
# ---- ***Consolidated ----
FigWorldSSP1_26 <- grid.arrange(FigProdWorldSSP1_26,FigEmisWorldSSP1_26,FigEIAWorldSSP1_26,ncol=3,top=textGrob("SSP1-2C", gp=gpar(fontsize=FontSize3)))
FigWorldSSP2_26 <- grid.arrange(FigProdWorldSSP2_26,FigEmisWorldSSP2_26,FigEIAWorldSSP2_26,ncol=3,top=textGrob("SSP2-2C", gp=gpar(fontsize=FontSize3)))
FigWorldSSP1_19 <- grid.arrange(FigProdWorldSSP1_19,FigEmisWorldSSP1_19,FigEIAWorldSSP1_19,ncol=3,top=textGrob("SSP1-1.5C", gp=gpar(fontsize=FontSize3)))
FigWorldSSP2_19 <- grid.arrange(FigProdWorldSSP2_19,FigEmisWorldSSP2_19,FigEIAWorldSSP2_19,ncol=3,top=textGrob("SSP2-1.5C", gp=gpar(fontsize=FontSize3)))
layout<-rbind(1,1,1,1,1,
              2,2,2,2,2,
              3,3,3,3,3,
              4,4,4,4,4,4,4,4)
FigWorld<- grid.arrange(FigWorldSSP1_26,FigWorldSSP2_26,
                        FigWorldSSP1_19,FigWorldSSP2_19,layout_matrix=layout)

rm(layout)

## SAVE MANUALLY WITH DIMENSION 1000 X 1200

# png("output/For Draft/Figure1.png", width=5*ppi, height=8*ppi, res=ppi, bg="black")
# print(plot(FigWorld))
# dev.off()
#
# ---- FIG: EIA ALL REGIONS ----
# EIA All Regions
# These will make up 2 figures, each with 2 panels
# Figure 1: SSP1, Figure 2: SSP2
# Panel A: RCP1.9, Panel B: RCP2.6
# Panel As should not have a legend
plot_listA = list()
for(i in c("SSP1_20","SSP2_20")){
Fig<-ggplot(data=subset(EIA, Year>1999&!(Region=="MEX"|Region=="RCAM"|Region=="NL"|Region=="NL_1"|Region=="NL_2"|Region=="EU"|Region=="World"|Region=="OECD90"|Region=="REF"|Region=="ASIA"|Region=="MAF"|Region=="LAM")&Scenario==i&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                     aes(x=Year, y=value, colour=variable)) + 
  geom_line(size=0.4)+  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("a.") + theme(plot.title = element_text(face="bold", size=7)) +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=8, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.position="none", legend.text = element_text(size=10, face="plain")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("") + xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),
                     name ="",
                     breaks=c("EIA_DM","EIA_Ha")) +
  facet_wrap(~RegOrder, ncol = 5, labeller=labeller(Scenario = scen_labels, RegOrder = reg_labels))
plot_listA[[i]] = Fig
}

plot_listB = list()
for(i in c("SSP1_450","SSP2_450")){
  Fig<-ggplot(data=subset(EIA, Year>1999&!(Region=="MEX"|Region=="RCAM"|Region=="NL"|Region=="NL_1"|Region=="NL_2"|Region=="EU"|Region=="World"|Region=="OECD90"|Region=="REF"|Region=="ASIA"|Region=="MAF"|Region=="LAM")&Scenario==i&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
              aes(x=Year, y=value, colour=variable)) + 
    geom_line(size=0.4)+  geom_hline(yintercept=0,size = 0.1, colour='black') +
    ggtitle("b.") + theme(plot.title = element_text(face="bold", size=7)) +
    xlim(2010,2050) +
    theme_bw() +
    theme(text= element_text(size=8, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
    theme(legend.position="bottom", legend.text = element_text(size=10, face="plain")) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
    ylab("") +  xlab("") +
    scale_colour_manual(values=c("darkorchid3", "limegreen"),
                        name ="",
                        breaks=c("EIA_DM","EIA_Ha"), 
                        labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),paste(EIA[Ha]," (",tCO[2],-eq/Ha,")")))) +
    facet_wrap(~RegOrder, ncol = 5, labeller=labeller(Scenario = scen_labels, RegOrder = reg_labels))
  plot_listB[[i]] = Fig
}

FigS4 <-grid.arrange(plot_listA[["SSP1_20"]],plot_listB[["SSP1_450"]],ncol=1)
FigS5 <-grid.arrange(plot_listA[["SSP2_20"]],plot_listB[["SSP2_450"]],ncol=1)

FigWorldWEU<-ggplot(data=subset(EIA, Year>1999&(Region=="WEU"|Region=="World")&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
            aes(x=Year, y=value, colour=variable)) + 
  geom_line(size=0.4)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=8, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.position="bottom", legend.text = element_text(size=10, face="plain")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),
                      name ="",
                      breaks=c("EIA_DM","EIA_Ha"),
                      labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
  facet_grid(ScenOrder~Region, labeller=labeller(ScenOrder = scen_labels, RegOrder = reg_labels))
FigWorldWEU

#
# ---- FIG: EIA EU+WEU+NL----
# EIA WEU and World
FigEIAWEUWo<-ggplot(data=subset(EIA, Year>1999&(Region=="World"|Region=="WEU")&(Scenario=="SSP1_450"|Scenario=="SSP2_450")&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                 aes(x=Year, y=value, colour=variable)) + 
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  xlim(2010,2050) +
  #coord_cartesian(ylim=c(0, 15)) +
  theme_bw() +
  theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
  theme(legend.position="bottom", legend.text = element_text(size=10, face="plain")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),
                      name ="",
                      breaks=c("EIA_DM","EIA_Ha"),
                      #labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(tCO[2],-eq/Ha)))) +
                      labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
  facet_grid(ScenOrder~RegOrder2, labeller=labeller(ScenOrder = scen_labels, RegOrder = reg_labels))
FigEIAWEUWo

# EIA WEU and Netherlands
FigEIAEU<-ggplot(data=subset(EIA, Year>1999&(Region=="EU"|Region=="WEU"|Region=="NL"|Region=="NL_1"|Region=="NL_2")&Scenario=="SSP1_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                    aes(x=Year, y=value, colour=variable, linetype=RegOrder)) + 
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  xlim(2010,2050) +
  #coord_cartesian(ylim=c(0, 15)) +
  theme_bw() +
  theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
  theme(legend.position=c(0.2,0.8), legend.text = element_text(size=10, face="plain")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),
                      name ="",
                      breaks=c("EIA_DM","EIA_Ha"),
                      #labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(CO[2],-eq/Ha)))) +
                      labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
  scale_linetype_manual(values=c("solid","solid","solid","twodash","dashed"), name="",breaks=c("WEU","EU","NL","NL_1","NL_2"),labels=c("WEU","EU","1","2","3"), guide=FALSE) +
  facet_wrap(~RegOrder2, labeller=labeller(Scenario = scen_labels, RegOrder2 = reg_labels))
FigEIAEU

FigEIA_WEUNL <-ggplot(data=subset(EIA.1, !Region=="World"), aes(x=Year, y=value, colour=variable, linetype=Region)) + 
  geom_line(size=0.4
  )+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),
                      name ="",
                      breaks=c("EIA_DM","EIA_Ha"),
                      #labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(tCO[2],-eq/Ha)))) +
                      labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
  scale_linetype_manual(values=c("solid","solid","twodash","dashed"), name="",breaks=c("WEU","NL","NL_1","NL_2"),labels=c("WEU","1","2","3"), guide=FALSE) +
  facet_grid(RegOrder2 ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels, Region = reg_labels))
FigEIA_WEUNL

#
# ---- FIG: EIA NL (all scen)----
FigEIANL<-ggplot(data=subset(EIA, Year>1999&RegOrder2=="NL"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                 aes(x=Year, y=value, colour=variable, linetype=Region)) +
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical",legend.text.align = 0) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("EIA") +
  xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),
                      name ="",
                      breaks=c("EIA_DM","EIA_Ha"),
                      labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),paste(EIA[Ha]," (",tCO[2],-eq/Ha,")")))) +
  scale_linetype_manual(values=c("solid","twodash","dashed"), name="Downscaling Method:",breaks=c("NL","NL_1","NL_2"),labels=c("1","2","3")) +
  facet_grid( .~ScenOrder, labeller=labeller(ScenOrder = scen_labels))
FigEIANL

FigEIAdmNL<-ggplot(data=subset(EIA, Year>1999&RegOrder2=="NL"&(variable=="EIA_DM")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                 aes(x=Year, y=value, colour=variable, linetype=Region)) +
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_blank(), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="none", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical",legend.text.align = 0) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"))) +
  xlab("") +
  scale_colour_manual(values="darkorchid3") +
  scale_linetype_manual(values=c("solid","twodash","dashed"), name="Downscaling Method:",breaks=c("NL","NL_1","NL_2"),labels=c("1","2","3")) +
  facet_grid( .~ScenOrder, labeller=labeller(ScenOrder = scen_labels))
FigEIAdmNL

FigEIAhaNL<-ggplot(data=subset(EIA, Year>1999&RegOrder2=="NL"&(variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                   aes(x=Year, y=value, colour=variable, linetype=Region)) +
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical",legend.text.align = 0) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EIA[Ha]," (",tCO[2],-eq/Ha,")"))) +
  xlab("") +
  scale_colour_manual(values="limegreen", guide=FALSE) +
  scale_linetype_manual(values=c("solid","twodash","dashed"), name="Downscaling Method:",breaks=c("NL","NL_1","NL_2"),labels=c("1","2","3")) +
  facet_grid( .~ScenOrder) +
  theme(strip.background = element_blank(),strip.text.x = element_blank())
FigEIAhaNL

lay <- rbind(1,1,1,1,2,2,2,2,2,2,2)
FigNL2 <-grid.arrange(FigEIAdmNL,FigEIAhaNL,layout_matrix=lay)
rm(lay)

#
# ---- FIG: EIA NL (+detail, SSP1_450)----
FigEIANL2<-ggplot(data=subset(EIA, Year>1999&Region=="NL"&Scenario=="SSP1_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
                    aes(x=Year, y=value, colour=variable, linetype=Region)) +
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  xlim(2010,2050) +
  #coord_cartesian(ylim=c(0, 15)) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="bottom", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values=c("darkorchid3", "limegreen"),
                      name ="",
                      breaks=c("EIA_DM","EIA_Ha"),
                      labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
  scale_linetype_manual(values=c("solid"), name="Downscaling Method",breaks=c("NL"),labels=c("1"),guide=FALSE) 
FigEIANL2

FigProdNL <- ggplot(data=subset(AgProd, Region=="NL"&!(variable=="TotalProduction")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"))
                       , mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
  theme(legend.position="bottom", legend.text = element_text(size=10, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(Mt[DM],"/yr"))) +
  xlab("") +
  xlim(2000,2060) +
  scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),
                    name="",
                    breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),
                    labels=c("Energy crops","Food and feed","Livestock")) 
FigProdNL

FigEmisNL <- ggplot(data=subset(Emis, Region=="NL"&!(variable=="TotalAgrEmissions")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
  theme(legend.position="bottom", legend.text = element_text(size=10, face="plain"), legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  xlab("") +
  xlim(2000,2060) +
  scale_fill_manual(values=c("bisque","coral4","blue","black"),
                    name="",
                    breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),
                    labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")))
FigEmisNL

FigNL <-grid.arrange(FigProdNL,FigEmisNL,FigEIANL2,ncol=3)

#
# ---- FIG: EIA ----
FigEIA <-ggplot(data=EIA.1, aes(x=Year, y=value, colour=Region, fill=Region, linetype=variable)) + 
  geom_line(size=0.4
  )+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  xlab("") +
  scale_colour_manual(values=c("black", "blue","orange","limegreen","darkorchid3"),
                      name ="Regions",
                      breaks=c("World","WEU","NL","NL_1","NL_2"),
                      labels=c("World","WEU","NL","NL_1","NL_2")
  ) +
  facet_grid(. ~ Scenario, labeller=labeller(Scenario = scen_labels))
FigEIA

#
# ---- FIG: EMISSIONS COMPONENTS----
FigEmisEU <- ggplot(data=subset(Emis, Region=="EU"&!(variable=="TotalAgrEmissions")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_line(data=subset(Emis, Region=="EU"&variable=="TotalAgrEmissions"), aes(x=Year, y = value)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  xlab("") +
  xlim(2000,2060) +
  scale_fill_manual(values=c("bisque","coral4","blue","black"),
                    name="",
                    breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),
                    labels=c("CH4","CO2","N2O","Total"),
                    guide=FALSE
  ) +
  facet_grid(Region ~ Scenario, scales="free_y", labeller=labeller(Scenario=scen_labels))
FigEmisEU

FigEmisWo <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions"|variable=="EmisCO2FossilFuelsandIndustry")&(Year==2010|Year==2020|Year==2030|Year==2040|Year==2050)), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  xlab("") +
  scale_fill_manual(values=c("bisque","coral4"),
                    name="",
                    breaks=c("EmisCH4LandUse","EmisN2OLandUse"),
                    labels=c(expression(CH[4]),expression(paste(N[2],"O")))
  ) +
  facet_grid(Region ~ Scenario, scales="free_y", labeller=labeller(Scenario=scen_labels))
FigEmisWo

FigEmisNL <- ggplot(data=subset(Emis, (Region=="NL"|Region=="NL_1"|Region=="NL_2")&!(variable=="TotalAgrEmissions")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_line(data=subset(Emis, Region=="NL"&variable=="TotalAgrEmissions"), aes(x=Year, y = value)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  xlab("") +
  xlim(2000,2060) +
  scale_fill_manual(values=c("bisque","coral4","blue","black"),
                    name="",
                    breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),
                    labels=c("CH4","CO2","N2O","Total")
  ) +
  facet_grid(Region ~ Scenario, scales="free_y", labeller=labeller(Scenario=scen_labels))
FigEmisNL

# png("output/NL_Emis.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(FigEmisNL))
# dev.off()


#
# ---- FIG: AG. PROD. COMPONENTS ----
FigProdEU <- ggplot(data=subset(AgProd, Region=="EU"&!(variable=="TotalProduction")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("Mt DM/yr") +
  xlab("") +
  xlim(2000,2060) +
  scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),
                    name="",
                    breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),
                    labels=c("Energy crops","Food and feed","Livestock"),
                    guide=FALSE
  ) +
  facet_grid(Region ~ Scenario, scales="free_y", labeller=labeller(Scenario=scen_labels))
FigProdEU

FigProdWo <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("Mt DM/yr") +
  xlab("") +
  xlim(2000,2060) +
  scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),
                    name="",
                    breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),
                    labels=c("Energy crops","Food and feed","Livestock")
  ) +
  facet_grid(Region ~ Scenario, scales="free_y", labeller=labeller(Scenario=scen_labels))
FigProdWo

FigProdNL <- ggplot(data=subset(AgProd, Region=="NL"&!(variable=="TotalProduction")), mapping=aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  ylab("Mt DM/yr") +
  xlab("") +
  xlim(2000,2060) +
  scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),
                    name="",
                    breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),
                    labels=c("Energy crops","Food and feed","Livestock")
  ) +
  facet_grid(Region ~ Scenario, scales="free_y", labeller=labeller(Scenario=scen_labels))
FigProdNL
#
# ---- FIG: CONSOLIDATED -----
layout<-rbind(c(1,1,1,2,2,2,3,3,3,3),c(4,4,4,5,5,5,6,6,6,6)) 
FigEmisProd <- grid.arrange(FigEmisWo,FigEmisEU,FigEmisNL,
                            FigProdWo,FigProdEU,FigProdNL,layout_matrix=layout)
rm(layout)
#
# ---- FIG: Ctax (all scen) ----
Ctax<-ggplot(data=subset(SSP_all.R, Year>1999&Region=="World"&Variable=="PriceCarbon"),aes(x=Year, y=value)) +
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="right", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  ylab(expression(paste(US[2010],"/tCO"[2],""))) +
  xlab("") +
  facet_grid( .~Scenario, labeller=labeller(Scenario = scen_labels))
Ctax
#
# ---- FIG: TOT GHG (all scen) ----
FigEmisTot<-ggplot(data=subset(EmisTot, Year>1999&Region=="World"),aes(x=Year, y=value, colour=Variable)) +
  # geom_area(colour="black", size=.1) +
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="right", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical",legend.text.align = 0) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
  xlab("") +
  scale_colour_manual(values=c("deepskyblue","black","limegreen","magenta","brown"),
                      name ="Emission Source",
                      breaks=c("EmisCH4LandUse","EmisCO2FossilFuelsandIndustry","EmisCO2LandUse","EmisN2OEnergySupplyandDem","EmisN2OLandUse"),
                      labels=c(expression(paste(CH[4]," Land Use"),
                                          paste(CO[2]," Fossil Fuels and Industry"),
                                          paste(CO[2]," Land Use"),
                                          paste(N[2],"O Energy Supply and Demand"),
                                          paste(N[2],"O Land Use")))) +
  facet_grid( .~Scenario, labeller=labeller(Scenario = scen_labels))
FigEmisTot
#
# ---- FIG: AGRI PROD. & TRADE ----
FigTrad <-ggplot(data=subset(TradBal, !(Region2=="World")), aes(x=Year, y=NetCropTrade, colour=Region2)) + 
  geom_line(size=0.4)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("Mt DM/yr") +
  xlab("") +
  theme(legend.position="right", legend.text=element_text(size=6), legend.title=element_text(face="bold")) +
  scale_colour_manual(values=c("darkgoldenrod1","gray50","darkorchid3","limegreen","dodgerblue","darkorchid1","chocolate","black"),
                      name ="",
                      breaks=c("ME","OCE","NAM","LAM","EU","AFR","FSU","ASIA"),
                      labels=c("ME","OCE","NAM","LAM","EU","AFR","FSU","ASIA")
  ) +
  facet_grid(. ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels))
FigTrad

FigAgProd <-ggplot(data=subset(AgriProd, !(Region2=="World")&variable=="TotalProduction"), aes(x=Year, y=value, fill=Region2)) + 
  geom_area(colour="black", size=.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=6), legend.title=element_text(face="bold")) +
  ylab("Mt DM/yr") +
  xlab("") +
  scale_fill_manual(values=c("black","dodgerblue","limegreen","darkgoldenrod1","gray50","darkorchid3","darkorchid1","chocolate"),
                      name ="",
                    breaks=c("ASIA","EU","LAM","ME","OCE","NAM","AFR","FSU"),
                    labels=c("ASIA","EU","LAM","ME","OCE","NAM","AFR","FSU")
  ) +
  facet_grid(. ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels))
FigAgProd

FigAgProdFrac <-ggplot(data=subset(AgriProd, !(Region2=="World")&variable=="AgriProdFrac"), aes(x=Year, y=value, fill=Region2)) + 
  geom_area(colour="black", size=.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=6), legend.title=element_text(face="bold")) +
  ylab("Fraction [-]") +
  xlab("") +
  scale_fill_manual(values=c("black","dodgerblue","limegreen","darkgoldenrod1","gray50","orangered","darkorchid1","chocolate"),
                    name ="",
                    breaks=c("ASIA","EU","LAM","ME","OCE","NAM","AFR","FSU"),
                    labels=c("ASIA","EU","LAM","ME","OCE","NAM","AFR","FSU")
  ) +
  facet_grid(. ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels))
FigAgProdFrac

FigAgr <- grid.arrange(FigAgProd,FigAgProdFrac,FigTrad)

#
# ---- FIG: NL Historic EF ----
NL_EIA_Liv<-ggplot() +
  geom_line(data=subset(NLAgHist, !(Source=="Agricultural Soils")&Variable=="CH4_per_animal"|Variable=="Milk_per_animal"|Variable=="LU_per_animal"|Variable=="Milk_EI_perDM"|Variable=="Milk_EI_perHa"),
            aes(x=Year, y=value, colour=VarOrder, linetype=VarOrder, size=VarOrder)) + 
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("a.") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  coord_cartesian(ylim=c(0.5, 1.6, 0.2)) + ylab("Index (1=1990)") + xlab("") + theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="right", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical",legend.text.align = 0) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  scale_colour_manual(values=c("tan","slategray3","black","darkorchid3","limegreen"),
                      name ="",breaks=c("Milk_EI_perDM","Milk_EI_perHa","Milk_per_animal","LU_per_animal","CH4_per_animal"),
                      labels=c(expression(paste(EIA[DM]," (",kgCH[4],"/",kg[Milk],")"),paste(EIA[Ha]," (",kgCH[4],"/ha",")"),paste("Productivity (",kg[Milk],"/Animal/yr)"),"Stocking Rate (Animal/ha)",paste("Emission Factor (",kgCH[4],"/Animal)"))))+
  scale_linetype_manual(values = c(2,2,2,1,1),
                        name ="",breaks=c("Milk_EI_perDM","Milk_EI_perHa","Milk_per_animal","LU_per_animal","CH4_per_animal"),
                        labels=c(expression(paste(EIA[DM]," (",kgCH[4],"/",kg[Milk],")"),paste(EIA[Ha]," (",kgCH[4],"/ha",")"),paste("Productivity (",kg[Milk],"/Animal/yr)"),"Stocking Rate (Animal/ha)",paste("Emission Factor (",kgCH[4],"/Animal)"))))+
  scale_size_manual(values = c(0.5,0.5,0.5,1,1),
                        name ="",breaks=c("Milk_EI_perDM","Milk_EI_perHa","Milk_per_animal","LU_per_animal","CH4_per_animal"),
                    labels=c(expression(paste(EIA[DM]," (",kgCH[4],"/",kg[Milk],")"),paste(EIA[Ha]," (",kgCH[4],"/ha",")"),paste("Productivity (",kg[Milk],"/Animal/yr)"),"Stocking Rate (Animal/ha)",paste("Emission Factor (",kgCH[4],"/Animal)"))))+
  facet_grid(~Source)
NL_EIA_Liv

NL_EIA_Crop<-ggplot() +
  geom_line(data=subset(NLAgHist, Source=="Agricultural Soils"&(Variable=="Crop_EIAdm"|Variable=="Crop_EIAha"|Variable=="Crop_Yield"|Variable=="N_AppRate"|Variable=="Cropland")),aes(x=Year, y=value, colour=VarOrder, linetype=VarOrder, size=VarOrder)) + 
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("b.") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  coord_cartesian(ylim=c(0.5, 1.6)) + ylab("Index (1=1990)") + xlab("") + theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.position="right", legend.text = element_text(size=FontSize2, face="plain"),legend.direction="vertical",legend.text.align = 0) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  scale_colour_manual(values=c("darkorchid3","limegreen","tan","slategray3","black"),
                      name ="",breaks=c("Crop_EIAdm","Crop_EIAha","Crop_Yield","Cropland","N_AppRate"),
                      labels=c(expression(paste(EIA[DM]," (",kgN[2],"O/",kg[Crop],")"),paste(EIA[Ha],"(",kgN[2],"O/ha",")"),"Crop Yields (t/ha)","Cropland (ha)","Nitrogen Application (kgN/ha/yr)")))+
  scale_linetype_manual(values = c(1,1,2,2,2),
                        name ="",breaks=c("Crop_EIAdm","Crop_EIAha","Crop_Yield","Cropland","N_AppRate"),
                        labels=c(expression(paste(EIA[DM]," (",kgN[2],"O/",kg[Crop],")"),paste(EIA[Ha],"(",kgN[2],"O/ha",")"),"Crop Yields (t/ha)","Cropland (ha)","Nitrogen Application (kgN/ha/yr)")))+
  scale_size_manual(values = c(1,1,0.5,0.5,0.5),
                    name ="",breaks=c("Crop_EIAdm","Crop_EIAha","Crop_Yield","Cropland","N_AppRate"),
                    labels=c(expression(paste(EIA[DM]," (",kgN[2],"O/",kg[Crop],")"),paste(EIA[Ha],"(",kgN[2],"O/ha",")"),"Crop Yields (t/ha)","Cropland (ha)","Nitrogen Application (kgN/ha/yr)")))+
  facet_grid(~Source)
NL_EIA_Crop

layout<-rbind(1,2)
FigNLHist <- grid.arrange(NL_EIA_Liv,NL_EIA_Crop,layout_matrix=layout)
rm(layout)
# #
# ---- FIG: Livestock Prod per Type ----
# Set Orders
LivestockProd$ScenOrder = factor(LivestockProd$SCENARIO, levels=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"))
LivestockProd$LiveOrder = factor(LivestockProd$VARIABLE, levels=c("beef","mutton & goat meat","pork","milk","poultry & eggs"))
# Add population and per-cap variables
LivestockProd$VarID3 = paste(LivestockProd$SCENARIO,LivestockProd$YEAR,LivestockProd$REGION)
LivestockProd$Pop_M <- Population[match(LivestockProd$VarID3, Population$VarID3),9]
LivestockProd = LivestockProd %>% mutate(kg_pcap = value / Pop_M * 1000)
LivestockProd$VarID3 <- NULL
# Make per-cap variable indexed to 2010
LivestockProd$VarID4 = paste(LivestockProd$SCENARIO,LivestockProd$LiveOrder,LivestockProd$REGION)
LivestockProd.2010 = subset(LivestockProd, YEAR==2010)
LivestockProd$value_2010 <- LivestockProd.2010[match(LivestockProd$VarID4, LivestockProd.2010$VarID4),10]
LivestockProd = LivestockProd %>% mutate(index_2010 = kg_pcap / value_2010)
rm(LivestockProd.2010)
LivestockProd$VarID4 <- NULL

FigLivestock <-ggplot(data=subset(LivestockProd, (REGION=="WEU"|REGION=="World")&(YEAR>=2010|YEAR<2050)&!(VARIABLE=="Total")), 
                      aes(x=YEAR, y=index_2010, colour=LiveOrder, linetype=LiveOrder)) + 
  geom_line(size=0.5) +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=FontSize2), legend.title=element_text(face="bold")) +
  ylab("kg DM/cap/yr (2010=1.0)") +
  xlab("") +
  ylim(0.5,1.5) +
  scale_color_manual(values=c("navy","olivedrab4","magenta","mistyrose4","orangered"),
                    name ="",
                    breaks=c("beef","mutton & goat meat","pork","milk","poultry & eggs"),
                    labels=c("Beef","Non-Ruminant","Pork","Milk","Poultry")
  ) +
  scale_linetype_manual(values=c("twodash","solid","solid","dotdash","longdash"),
                        name ="",
                        breaks=c("beef","mutton & goat meat","pork","milk","poultry & eggs"),
                        labels=c("Beef","Non-Ruminant","Pork","Milk","Poultry")
  ) +                      
  facet_grid(REGION ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels, REGION=reg_labels), scale="free_y")
FigLivestock

##
# ---- FIG: Livestock Cons per Type ----
FigLivestockCons <-ggplot(data=subset(LivestockCons, (REGION=="WEU"|REGION=="World")&(YEAR>=2010|YEAR<2050)&!(VARIABLE=="Total")), 
                      aes(x=YEAR, y=index_2010, colour=LiveOrder, linetype=LiveOrder)) + 
  geom_line(size=0.5) +
  xlim(2010,2050) +
  theme_bw() +
  theme(text= element_text(size=FontSize2, face="plain"), axis.text.x = element_text(angle=66, size=FontSize2, hjust=1), axis.text.y = element_text(size=FontSize2)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=FontSize2), legend.title=element_text(face="bold")) +
  ylab("Per-capita Consumption (2010 = 1.00)") +
  xlab("") +
  ylim(0.5,1.5) +
  scale_color_manual(values=c("navy","olivedrab4","magenta","mistyrose4","orangered"),
                     name ="",
                     breaks=c("beef","mutton & goat meat","pork","milk","poultry & eggs"),
                     labels=c("Beef","Mutton & Goat Meat","Pork","Milk","Poultry & Eggs")
  ) +
  scale_linetype_manual(values=c("twodash","solid","solid","dotdash","longdash"),
                        name ="",
                        breaks=c("beef","mutton & goat meat","pork","milk","poultry & eggs"),
                        labels=c("Beef","Mutton & Goat Meat","Pork","Milk","Poultry & Eggs")
  ) +                      
  facet_grid(REGION ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels, REGION=reg_labels), scale="free_y")
FigLivestockCons

##

# # ---- OUTPUT: FOR DRAFT ----
# png("output/For Draft/Figure1.png", width=5*ppi, height=8*ppi, res=ppi)
# print(plot(FigWorld))
# dev.off()
# 
# png("output/For Draft/Figure2.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(FigNL2))
# dev.off()
# 
# png("output/For Draft/Figure3.png", width=7*ppi, height=5*ppi, res=ppi)
# print(plot(FigNLHist))
# dev.off()
# #
# png("output/For Draft/FigureS1.png", width=7*ppi, height=4*ppi, res=ppi)
# print(plot(FigEmisTot))
# dev.off()
# 
# png("output/For Draft/FigureS2.png", width=5*ppi, height=4*ppi, res=ppi)
# print(plot(Ctax))
# dev.off()
# 
# png("output/For Draft/FigureS3.png", width=7*ppi, height=2*ppi, res=ppi)
# print(plot(FigAgProdFrac))
# dev.off()
# 
# png("output/For Draft/FigureS4.png", width=9*ppi, height=14*ppi, res=ppi)
# print(plot(FigS4))
# dev.off()
# 
# png("output/For Draft/FigureS5.png", width=9*ppi, height=14*ppi, res=ppi)
# print(plot(FigS5))
# dev.off()
# 
# png("output/For Draft/FigureS6.png", width=10*ppi, height=12*ppi, res=ppi)
# print(plot(FigS6))
# dev.off()
# #
# png("output/For Draft/FigureS7.png", width=9*ppi, height=5*ppi, res=ppi)
# print(plot(FigLivestock))
# dev.off()
# #
# png("output/For Draft/FigureS7_cons.png", width=9*ppi, height=4.5*ppi, res=ppi)
# print(plot(FigLivestockCons))
# dev.off()
# #
# ---- OUTPUT: OTHER ----
# png("output/EIA.png", width=8*ppi, height=4*ppi, res=ppi)
# print(plot(FigEIA))
# dev.off()
# 
# png("output/EmisProd.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(FigEmisProd))
# dev.off()
# 
# png("output/SocioEconomic.png", width=6*ppi, height=8*ppi, res=ppi)
# print(plot(FigSoc))
# dev.off()
# 
# png("output/TradBalance.png", width=8*ppi, height=4*ppi, res=ppi)
# print(plot(FigTrad))
# dev.off()
# 
# png("output/AgProd.png", width=8*ppi, height=4*ppi, res=ppi)
# print(plot(FigAgProd))
# dev.off()
# 
# png("output/ProdTrade.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(FigAgr))
# dev.off()

# ---- FIGURES FOR PRESENTATION----
# # EIA World
# # ---- ***SSP1-450 ----
# FigEIAWorldSSP1_26<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP1_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                            aes(x=Year, y=value, colour=variable)) +
#   geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
#   theme_bw() + theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) + theme(plot.title = element_text(face="bold", size=FontSize)) +
#   ylab("") + xlab("") + 
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(tCO[2],-eq/Ha))), guide=FALSE)
# FigEIAWorldSSP1_26
# 
# FigProdWorldSSP1_26 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) + ylim(0,7700) +
#   scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock"),guide=FALSE) 
# FigProdWorldSSP1_26
# 
# FigEmisWorldSSP1_26 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize3)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"), legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
#   scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")),guide=FALSE)
# FigEmisWorldSSP1_26
# 
# # ---- ***SSP2-450 ----
# FigEIAWorldSSP2_26<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP2_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                            aes(x=Year, y=value, colour=variable)) +
#   geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
#   theme_bw() + theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") + xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(EIA[DM]),expression(EIA[Ha])),guide=FALSE)
# FigEIAWorldSSP2_26
# 
# FigProdWorldSSP2_26 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP2_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) + ylim(0,7700) +
#   scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock"),guide=FALSE) 
# FigProdWorldSSP2_26
# 
# FigEmisWorldSSP2_26 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP2_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"), legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
#   scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")),guide=FALSE)
# FigEmisWorldSSP2_26
# 
# #
# # ---- ***SSP1-20 ----
# FigEIAWorldSSP1_19<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP1_20"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                            aes(x=Year, y=value, colour=variable)) +
#   geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
#   theme_bw() + theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) + theme(plot.title = element_text(face="bold", size=7)) +
#   ylab("") + xlab("") + 
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(tCO[2],-eq/Ha))), guide=FALSE)
# FigEIAWorldSSP1_19
# 
# FigProdWorldSSP1_19 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP1_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) + ylim(0,7700) +
#   scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock"),guide=FALSE) 
# FigProdWorldSSP1_19
# 
# FigEmisWorldSSP1_19 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP1_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=0, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"), legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
#   scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")),guide=FALSE)
# FigEmisWorldSSP1_19
# 
# # ---- ***SSP2-20 ----
# FigEIAWorldSSP2_19<-ggplot(data=subset(EIA, Year>1999&Region=="World"&Scenario=="SSP2_20"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                            aes(x=Year, y=value, colour=variable)) +
#   geom_line(size=1) + geom_hline(yintercept=0,size = 0.1, colour='black') + xlim(2010,2050) +
#   theme_bw() + theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical", legend.text.align=0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") + xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),name ="",breaks=c("EIA_DM","EIA_Ha"),labels=c(expression(paste(EIA[DM]," (",MtCO[2],"-eq/",t[DM],")")),
#                                                                                                          expression(paste(EIA[Ha]," (",tCO[2],"-eq/Ha)"))))
# FigEIAWorldSSP2_19
# 
# FigProdWorldSSP2_19 <- ggplot(data=subset(AgProd, Region=="World"&!(variable=="TotalProduction")&Scenario=="SSP2_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") +  geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(Mt[DM],"/yr"))) +  xlab("") +  xlim(2000,2060) +
#   scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),name="",breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),labels=c("Energy crops","Food and feed","Livestock")) 
# FigProdWorldSSP2_19
# 
# FigEmisWorldSSP2_19 <- ggplot(data=subset(Emis, Region=="World"&!(variable=="TotalAgrEmissions")&Scenario=="SSP2_20"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") + geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +  theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"), legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(MtCO[2],"-eq/yr",""))) +  xlab("") +  xlim(2000,2060) + ylim(0,7900) +
#   scale_fill_manual(values=c("bisque","coral4","blue","black"),name="",breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")))
# FigEmisWorldSSP2_19
# #
# # ---- ***Consolidated ----
# FigWorldSSP1_26 <- grid.arrange(FigProdWorldSSP1_26,FigEmisWorldSSP1_26,FigEIAWorldSSP1_26,ncol=3,top=textGrob("SSP1-2C", gp=gpar(fontsize=FontSize, fontface="bold")))
# FigWorldSSP2_26 <- grid.arrange(FigProdWorldSSP2_26,FigEmisWorldSSP2_26,FigEIAWorldSSP2_26,ncol=3,top=textGrob("SSP2-2C", gp=gpar(fontsize=FontSize, fontface="bold")))
# FigWorldSSP1_19 <- grid.arrange(FigProdWorldSSP1_19,FigEmisWorldSSP1_19,FigEIAWorldSSP1_19,ncol=3,top=textGrob("SSP1-1.5C", gp=gpar(fontsize=FontSize, fontface="bold")))
# FigWorldSSP2_19 <- grid.arrange(FigProdWorldSSP2_19,FigEmisWorldSSP2_19,FigEIAWorldSSP2_19,ncol=3,top=textGrob("SSP2-1.5C", gp=gpar(fontsize=FontSize, fontface="bold")))
# layout<-rbind(1,1,1,1,1,
#               2,2,2,2,2,
#               3,3,3,3,3,
#               4,4,4,4,4,4)
# FigWorld<- grid.arrange(FigWorldSSP1_26,FigWorldSSP2_26,
#                         FigWorldSSP1_19,FigWorldSSP2_19,layout_matrix=layout)

# Width:1100, Height 1000
# jpeg("output/For Presentation/Figure1.jpeg", width=8*ppi, height=9*ppi, res=ppi, bg="black")
# print(plot(FigWorld))
# dev.off()
#
# ---- FIG: EIA ALL REGIONS ----
# # EIA All Regions
# plot_list = list()
# for(i in unique(EIA$Scenario)){
#   Fig<-ggplot(data=subset(EIA, Year>1999&!(Region=="NL"|Region=="NL_1"|Region=="NL_2"|Region=="WEU"|Region=="CEU"|Region=="World"|Region=="OECD90"|Region=="REF"|Region=="ASIA"|Region=="MAF"|Region=="LAM")&Scenario==i&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#               aes(x=Year, y=value, colour=variable)) + 
#     geom_line(size=0.4)+
#     geom_hline(yintercept=0,size = 0.1, colour='black') +
#     xlim(2010,2050) +
#     #coord_cartesian(ylim=c(0, 15)) +
#     theme_bw() +
#     theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
#     theme(legend.position="bottom", legend.text = element_text(size=FontSize3, face="plain")) +
#     theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#     ylab("") +
#     xlab("") +
#     scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                         name ="",
#                         breaks=c("EIA_DM","EIA_Ha"),
#                         #                     labels=c(expression("per t"["DM"],"per Ha"))) +
#                         labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),paste(EIA[Ha],"( ",tCO[2],-eq/Ha,")")))) +
#                         # labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#     facet_wrap(~RegOrder, ncol = 5, labeller=labeller(Scenario = scen_labels, RegOrder = reg_labels))
#   plot_list[[i]] = Fig
#   
#   
# }
# plot_list[[4]]
# 
# # ---- FIG: EIA (World, WEU) ----
# FigWorldWEU<-ggplot(data=subset(EIA, Year>1999&(Region=="WEU"|Region=="World")&(Scenario=="SSP1_450"|Scenario=="SSP2_450")&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                     aes(x=Year, y=value, colour=variable)) + 
#   geom_line(size=0.4)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   xlim(2010,2050) +
#   #coord_cartesian(ylim=c(0, 15)) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"), legend.direction="vertical", legend.text.align = 0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") +
#   xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                       name ="",
#                       breaks=c("EIA_DM","EIA_Ha"),
#                       labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),
#                                           paste(EIA[Ha],"( ",tCO[2],-eq/Ha,")")))) +
#                     #                     labels=c(expression("per t"["DM"],"per Ha"))) +
#                       # labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#   facet_grid(ScenOrder~Region, labeller=labeller(ScenOrder = scen_labels, RegOrder = reg_labels))
# FigWorldWEU
# 
# FigWorldWEU_15<-ggplot(data=subset(EIA, Year>1999&(Region=="WEU"|Region=="World")&(Scenario=="SSP1_20"|Scenario=="SSP2_20")&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                     aes(x=Year, y=value, colour=variable)) + 
#   geom_line(size=0.4)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   xlim(2010,2050) +
#   #coord_cartesian(ylim=c(0, 15)) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"), legend.direction="vertical", legend.text.align = 0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") +
#   xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                       name ="",
#                       breaks=c("EIA_DM","EIA_Ha"),
#                       labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),
#                                           paste(EIA[Ha],"( ",tCO[2],-eq/Ha,")")))) +
#   #                     labels=c(expression("per t"["DM"],"per Ha"))) +
#   # labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#   facet_grid(ScenOrder~Region, labeller=labeller(ScenOrder = scen_labels, RegOrder = reg_labels))
# FigWorldWEU_15
# 
# #
# # ---- FIG: EIA NL (all scen)----
# FigEIANL<-ggplot(data=subset(EIA, Year>1999&RegOrder2=="NL"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                  aes(x=Year, y=value, colour=variable, linetype=Region)) +
#   geom_line(size=0.5)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   #coord_cartesian(ylim=c(0, 15)) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical", legend.text.align = 0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") +
#   xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                       name ="",
#                       breaks=c("EIA_DM","EIA_Ha"),
#                       labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),paste(EIA[Ha],"( ",tCO[2],-eq/Ha,")")))) +
#                       # labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#   scale_linetype_manual(values=c("solid","twodash","dashed"), name="Downscaling Method",breaks=c("NL","NL_1","NL_2"),labels=c("1","2","3")) +
#   facet_grid( .~ScenOrder, labeller=labeller(ScenOrder = scen_labels))
# FigEIANL
# 
# FigEIANL_15<-ggplot(data=subset(EIA, Year>1999&RegOrder2=="NL"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")&(Scenario=="SSP1_20"|Scenario=="SSP2_20")),
#                  aes(x=Year, y=value, colour=variable, linetype=Region)) +
#   geom_line(size=0.5)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   #coord_cartesian(ylim=c(0, 15)) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical", legend.text.align = 0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") +
#   xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                       name ="",
#                       breaks=c("EIA_DM","EIA_Ha"),
#                       labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),paste(EIA[Ha],"( ",tCO[2],-eq/Ha,")")))) +
#   # labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#   scale_linetype_manual(values=c("solid","twodash","dashed"), name="Downscaling Method",breaks=c("NL","NL_1","NL_2"),labels=c("1","2","3")) +
#   facet_grid( .~ScenOrder, labeller=labeller(ScenOrder = scen_labels))
# FigEIANL_15
# 
# #
# # ---- FIG: EIA EU+WEU+NL----
# # EIA WEU and Netherlands
# FigEIAEU<-ggplot(data=subset(EIA, Year>1999&(Region=="EU"|Region=="WEU"|Region=="NL"|Region=="NL_1"|Region=="NL_2")&Scenario=="SSP1_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                  aes(x=Year, y=value, colour=variable, linetype=RegOrder)) + 
#   geom_line(size=0.5)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   #coord_cartesian(ylim=c(0, 15)) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position=c(0.2,0.8), legend.text = element_text(size=FontSize, face="plain")) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") +
#   xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                       name ="",
#                       breaks=c("EIA_DM","EIA_Ha"),
#                       #labels=c(expression(paste(MtCO[2],-eq/t[DM]),paste(CO[2],-eq/Ha)))) +
#                       labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#   scale_linetype_manual(values=c("solid","solid","solid","twodash","dashed"), name="",breaks=c("WEU","EU","NL","NL_1","NL_2"),labels=c("WEU","EU","1","2","3"), guide=FALSE) +
#   facet_wrap(~RegOrder2, labeller=labeller(Scenario = scen_labels, RegOrder2 = reg_labels))
# FigEIAEU
# #
# # ---- FIG: EIA NL (+detail, SSP1_450)----
# FigEIANL2<-ggplot(data=subset(EIA, Year>1999&Region=="NL"&Scenario=="SSP1_450"&(variable=="EIA_DM"|variable=="EIA_Ha")&(Year>"2000"|Year<"2060")&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                   aes(x=Year, y=value, colour=variable, linetype=Region)) +
#   geom_line(size=0.5)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   #coord_cartesian(ylim=c(0, 15)) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical", legend.text.align = 0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") +
#   xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                       name ="",
#                       breaks=c("EIA_DM","EIA_Ha"),
#                       # labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#                       labels=c(expression(paste(EIA[DM]," (",MtCO[2],-eq/t[DM],")"),paste(EIA[Ha]," (",tCO[2],-eq/Ha,")"))), guide=FALSE) +
#   scale_linetype_manual(values=c("solid"), name="Downscaling Method",breaks=c("NL"),labels=c("1"),guide=FALSE) 
# FigEIANL2
# 
# FigProdNL <- ggplot(data=subset(AgProd, Region=="NL"&!(variable=="TotalProduction")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"))
#                     , mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") +
#   geom_hline(yintercept=0,size = 0.1, color="black") +
#   theme_bw() +
#   theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(Mt[DM],"/yr"))) +
#   xlab("") +
#   xlim(2000,2060) +
#   scale_fill_manual(values=c("blueviolet","darkgreen","darkgoldenrod4"),
#                     name="",
#                     breaks=c("AgriProdCropsEnergy","AgriProdCropsNonEnergy","AgriProdLivestock"),
#                     labels=c("Energy crops","Food and feed","Livestock"), guide=FALSE) 
# FigProdNL
# 
# FigEmisNL <- ggplot(data=subset(Emis, Region=="NL"&!(variable=="TotalAgrEmissions")&Scenario=="SSP1_450"&(Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")), mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") +
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme_bw() +
#   theme(text= element_text(size=FontSize3, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"), legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(MtCO[2],"-eq/yr",""))) +
#   xlab("") +
#   xlim(2000,2060) +
#   scale_fill_manual(values=c("bisque","coral4","blue","black"),
#                     name="",
#                     breaks=c("EmisCH4LandUse","EmisCO2LandUse","EmisN2OLandUse","TotalAgrEmissions"),
#                     labels=c(expression("CH"["4"],"CO"["2"],paste(N[2],O),"Total")), guide=FALSE)
# FigEmisNL
# 
# FigNL <-grid.arrange(FigProdNL,FigEmisNL,FigEIANL2,ncol=3)
# 
# #
# # ---- FIG: TOT GHG (all scen) ----
# FigEmisTot<-ggplot(data=subset(EmisTot, Year>1999&Region=="World"),aes(x=Year, y=value/1000, colour=Variable)) +
#   # geom_area(colour="black", size=.1) +
#   geom_line(size=0.5)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   xlim(2010,2050) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical",legend.text.align = 0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(GtCO[2],"-eq/yr",""))) +
#   xlab("") +
#   scale_colour_manual(values=c("deepskyblue","black","limegreen","magenta","brown"),
#                       name ="Emission Source",
#                       breaks=c("EmisCH4LandUse","EmisCO2FossilFuelsandIndustry","EmisCO2LandUse","EmisN2OEnergySupplyandDem","EmisN2OLandUse"),
#                       labels=c(expression(paste(CH[4]," Land Use"),
#                                           paste(CO[2]," Fossil Fuels and Industry"),
#                                           paste(CO[2]," Land Use"),
#                                           paste(N[2],"O Energy Supply and Demand"),
#                                           paste(N[2],"O Land Use")))) +
#   facet_grid( .~Scenario, labeller=labeller(Scenario = scen_labels))
# FigEmisTot
# 
# FigEmisTot_15<-ggplot(data=subset(EmisTot, Year>1999&Region=="World"&(Scenario=="SSP1_20"|Scenario=="SSP2_20")),aes(x=Year, y=value/1000, colour=Variable)) +
#   # geom_area(colour="black", size=.1) +
#   geom_line(size=0.5)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   xlim(2010,2050) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.position="bottom", legend.text = element_text(size=FontSize, face="plain"),legend.direction="vertical",legend.text.align = 0) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(GtCO[2],"-eq/yr",""))) +
#   xlab("") +
#   scale_colour_manual(values=c("deepskyblue","black","limegreen","magenta","brown"),
#                       name ="Emission Source",
#                       breaks=c("EmisCH4LandUse","EmisCO2FossilFuelsandIndustry","EmisCO2LandUse","EmisN2OEnergySupplyandDem","EmisN2OLandUse"),
#                       labels=c(expression(paste(CH[4]," Land Use"),
#                                           paste(CO[2]," Fossil Fuels and Industry"),
#                                           paste(CO[2]," Land Use"),
#                                           paste(N[2],"O Energy Supply and Demand"),
#                                           paste(N[2],"O Land Use")))) +
#   facet_grid( .~Scenario, labeller=labeller(Scenario = scen_labels))
# FigEmisTot_15
# 
# #
# # ---- FIG: SOCIOECONOMIC ----
# FigSocEU <-ggplot(data=subset(Socio, Year>1999&Region=="WEU"&!(variable=="AgriDemCropsFood_PerCap"|variable=="AgriDemCropsFeed_PerCap"|variable=="AgriDemLivestockFood_PerCap")&(Scenario=="SSP1_20"|Scenario=="SSP2_20")), aes(x=Year, y=value, colour=Scenario)) + 
#   geom_line(size=1
#   )+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   xlim(2010,2050) +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize3, hjust=1), axis.text.y = element_text(size=FontSize3)) +
#   theme(legend.title=element_blank(), legend.position="bottom") +
#   theme(legend.position=c(0.4,0.1)) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(strip.background = element_blank()) +
#   ylab("")+xlab("") +
#   scale_colour_manual(values=c("limegreen", "blue"),
#                       name ="Scenario",
#                       breaks=c("SSP1_20","SSP2_20"),
#                       labels=c("SSP1","SSP2")
#   ) +
#   facet_wrap(~VarOrder,strip.position = "top", scales="free_y", labeller=labeller(VarOrder = var_labels))
# FigSocEU
# 
# FigCtax <-ggplot(data=subset(SSP_all.R, Year>1999&Variable=="PriceCarbon"&Region=="World"), aes(x=Year, y=value)) + 
#   geom_line(size=1)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   xlim(2010,2050) +
#   ylab(expression(paste("Carbon Tax (US$/t",CO[2],")"))) +
#   xlab("") +
#   theme_bw() +
#   theme(text= element_text(size=FontSize, face="plain"), axis.text.x = element_text(angle=66, size=FontSize, hjust=1), axis.text.y = element_text(size=FontSize)) +
#   theme(legend.title=element_blank(), legend.position="bottom") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(legend.position="bottom") +
#   facet_grid( .~Scenario, labeller=labeller(Scenario = scen_labels))
# FigCtax

#
#
# ---- OUTPUT PRESENTATION ----
# png("output/For Presentation/EIAAllR_SSP1-26.png", width=9*ppi, height=8*ppi, res=ppi)
# print(plot(plot_list[[2]]))
# dev.off()
# 
# png("output/For Presentation/EIAAllR_SSP2-26.png", width=9*ppi, height=8*ppi, res=ppi)
# print(plot(plot_list[[4]]))
# dev.off()
#
# png("output/For Presentation/EIAAllR_SSP1-19.png", width=9*ppi, height=8*ppi, res=ppi)
# print(plot(plot_list[[1]]))
# dev.off()
# # 
# png("output/For Presentation/EIAAllR_SSP2-19.png", width=9*ppi, height=8*ppi, res=ppi)
# print(plot(plot_list[[3]]))
# dev.off()
# 
# png("output/For Presentation/WEUWorld.png", width=7*ppi, height=6*ppi, res=ppi)
# print(plot(FigWorldWEU))
# dev.off()
# 
# png("output/For Presentation/WEUWorld_15.png", width=7*ppi, height=6*ppi, res=ppi)
# print(plot(FigWorldWEU_15))
# dev.off()
# 
# png("output/For Presentation/EIANL.png", width=9*ppi, height=5*ppi, res=ppi)
# print(plot(FigEIANL))
# dev.off()
# 
# png("output/For Presentation/EIANL_15.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(FigEIANL_15))
# dev.off()
# 
# png("output/For Presentation/EIAEU_SSP1.png", width=10*ppi, height=4*ppi, res=ppi)
# print(plot(FigEIAEU))
# dev.off()
#
# png("output/For Presentation/NL_SSP1.png", width=10*ppi, height=3.5*ppi, res=ppi)
# print(plot(FigNL))
# dev.off()
#
# png("output/For Presentation/NL_EntFer.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigNL_EntFer))
# dev.off()
# #
# png("output/For Presentation/NL_Manure.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigNL_Man))
# dev.off()
# 
# png("output/For Presentation/FigureS1.png", width=9*ppi, height=5*ppi, res=ppi)
# print(plot(FigEmisTot))
# dev.off()
# 
# png("output/For Presentation/FigureS1.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(FigEmisTot_15))
# dev.off()
# 
# png("output/For Presentation/SocEU.png", width=10*ppi, height=6*ppi, res=ppi)
# print(plot(FigSocEU))
# dev.off()
#
# png("output/For Presentation/Ctax.png", width=9*ppi, height=4*ppi, res=ppi)
# print(plot(FigCtax))
# dev.off()

# ---- OLD ----
# ---- FIG: EIA RCP5----
# # EIA World
# Dummy <- unique(EIA[,c("Scenario","Region","Year","variable")])
# Dummy = subset(Dummy, Region=="World")
# Dummy$Year <- Dummy$value <-1
# 
# FigEIARCP<-ggplot(data=subset(EIA, (Region=="World"|Region=="OECD90"|Region=="REF"|Region=="ASIA"|Region=="MAF"|Region=="LAM")&
#                                 (variable=="EIA_DM"|variable=="EIA_Ha")&
#                                 (Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050")),
#                   aes(x=Year, y=value, colour=variable)) +
#   geom_line(size=0.5)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   geom_rect(data=subset(Dummy, variable=="EIA_DM"|variable=="EIA_Ha"), aes(fill = Region),
#             # xmin = as.numeric(Glob1$ScenOrder[[1]]) -1,
#             # xmax = as.numeric(Glob1$ScenOrder[[6]]) +1,
#             xmin = -Inf, xmax = Inf,
#             ymin = -Inf, ymax = Inf, alpha=0.04) +
#   #ggtitle("Global Emission Intensity of Agriculutre (SSP1-2.6)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   #coord_cartesian(ylim=c(0, 15)) +
#   theme_bw() +
#   theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
#   theme(legend.position="bottom", legend.text = element_text(size=6, face="plain"),legend.direction="vertical") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("") +
#   xlab("") +
#   scale_colour_manual(values=c("darkorchid3", "limegreen"),
#                       name ="",
#                       breaks=c("EIA_DM","EIA_Ha"),
#                       labels=c(expression(EIA[DM]),expression(EIA[Ha]))) +
#   facet_grid(ScenOrder~Region, labeller=labeller(ScenOrder= scen_labels,Region=reg_labels))
# FigEIARCP
# 
# #

# ---- FIG: SOCIOECONOMIC ----
# FigSocEU <-ggplot(data=subset(Socio, Year>1999&Region=="WEU"&!(variable=="AgriDemCropsFood_PerCap"|variable=="AgriDemCropsFeed_PerCap"|variable=="AgriDemLivestockFood_PerCap")), aes(x=Year, y=value, colour=Scenario)) +
#   geom_line(size=1
#   )+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   ggtitle("(A) EU-27") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   theme_bw() +
#   theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
#   theme(legend.title=element_blank(), legend.position="bottom") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(strip.background = element_blank()) +
#   ylab("")+xlab("") +
#   scale_colour_manual(values=c("limegreen", "blue","darkorchid3","black"),
#                       name ="Scenario",
#                       breaks=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"),
#                       labels=c("SSP1-2.6","SSP2-2.6","SSP1-1.9","SSP2-1.9"),
#                       guide=FALSE
#   ) +
#   facet_wrap(~VarOrder,strip.position = "top", scales="free_y", labeller=labeller(VarOrder = var_labels))
# FigSocEU
# 
# FigSocWo <-ggplot(data=subset(Socio, Year>1999&Region=="World"&!(variable=="AgriDemCropsFood_PerCap"|variable=="AgriDemCropsFeed_PerCap"|variable=="AgriDemLivestockFood_PerCap")), aes(x=Year, y=value, colour=Scenario)) + 
#   geom_line(size=1
#   )+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   ggtitle("(B) World") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   theme_bw() +
#   theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
#   theme(legend.title=element_blank(), legend.position="bottom") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(strip.background = element_blank()) +
#   theme(legend.position="right") +
#   ylab("")+xlab("") +
#   scale_colour_manual(values=c("limegreen", "blue","darkorchid3","black"),
#                       name ="Scenario",
#                       breaks=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"),
#                       labels=c("SSP1-2.6","SSP2-2.6","SSP1-1.9","SSP2-1.9"),
#                       guide=FALSE
#   ) +
#   facet_wrap(~VarOrder,strip.position = "right", scales="free_y", labeller=labeller(VarOrder = var_labels))
# FigSocWo
# 
# FigCtax <-ggplot(data=subset(SSP_all, Year>1999&Variable=="PriceCarbon"&Region=="World"), aes(x=Year, y=value, colour=Scenario)) + 
#   geom_line(size=0.4  )+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   ggtitle("(C) Global Carbon Tax") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   xlim(2010,2050) +
#   ylim(0,400) +
#   ylab(expression(paste("Carbon Tax (US$/t",CO[2],")"))) +
#   xlab("") +
#   theme_bw() +
#   theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
#   theme(legend.title=element_blank(), legend.position="bottom") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(strip.background = element_blank()) +
#   theme(legend.position="bottom") +
#   scale_colour_manual(values=c("limegreen", "blue","darkorchid3","black"),
#                       name ="Scenario",
#                       breaks=c("SSP1_450","SSP2_450","SSP1_20","SSP2_20"),
#                       labels=c("SSP1-2.6","SSP2-2.6","SSP1-1.9","SSP2-1.9")
#   ) 
# FigCtax
# 
# 
# #layout<-matrix(c(1,1,2,2,2), nrow=1)
# layout<-rbind(c(1,2),c(1,2),c(3,3))
# FigSoc <- grid.arrange(FigSocEU,FigSocWo,FigCtax,layout_matrix=layout)
# rm(layout)
# 
# #layout<-rbind(c(1,2),c(1,2),c(3,3))
# FigSoc2 <- grid.arrange(FigSocEU,FigSocWo)
# rm(layout)

#


# ---- FIG: NL Livestock EF ----
# FigNL_EntFer<-ggplot(data=subset(NLLivEF, Source=="Enteric Fermentation"),aes(x=Year, y=value, colour=Variable)) +
#   geom_line(size=1)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   ggtitle("Trends for Dairy Cattle (NL)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   #xlim(2010,2050) +
#   coord_cartesian(ylim=c(0.5, 1.5)) +
#   theme_bw() +
#   theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
#   theme(legend.position=c(0.4,0.2), legend.text = element_text(size=10, face="plain"),legend.direction="horizontal") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("Index (1=1990)") +
#   xlab("") +
#   scale_colour_manual(values=c("black","skyblue", "darkorchid3"),
#                       name ="",
#                       breaks=c("CH4_per_animal","Milk_per_animal","Milk EI"),
#                       labels=c(expression(paste(kgCH[4],"/Animal"),paste(kg[Milk],"/Animal"),paste(kgCH[4],"/",kg[Milk]))))
# FigNL_EntFer
# 
# FigNL_Man<-ggplot(data=subset(NLLivEF, Source=="Manure Management"),aes(x=Year, y=value, colour=Variable)) +
#   geom_line(size=1)+
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   ggtitle("Trends for Dairy Cattle (NL)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
#   #xlim(2010,2050) +
#   coord_cartesian(ylim=c(0.5, 1.6)) +
#   theme_bw() +
#   theme(text= element_text(size=10, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=10)) +
#   theme(legend.position=c(0.35,0.2), legend.text = element_text(size=10, face="plain"),legend.direction="horizontal") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("Index (1=1990)") +
#   xlab("") +
#   scale_colour_manual(values=c("black","skyblue", "darkorchid3"),
#                       name ="",
#                       breaks=c("CH4_per_animal","Milk_per_animal","Milk EI"),
#                       labels=c(expression(paste(kgCH[4],"/Animal"),paste(kg[Milk],"/Animal"),paste(kgCH[4],"/",kg[Milk]))))
# FigNL_Man
#
