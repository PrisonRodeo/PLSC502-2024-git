# Making some Africa data for PLSC 502...
#
# setwd("/Users/cuz10/Dropbox (Personal)/PLSC 502")
#
# Begin with the lovely -wdi-, -peacesciencer-, and
# -countrycode- packages:

install.packages(c("WDI","peacesciencer","countrycode"))
library(WDI)
library(countrycode)
library(peacesciencer)

# Now create some country-years:

df<-create_stateyears(system="gw")

# Add the ISO3a codes to those data (form merging, etc.):

df$ISO3<-countrycode(df$gwcode,origin="gwn",
                     destination="iso3c")

# Add some variables...

df<-add_democracy(df)              # Democracy variables
df<-add_ucdp_onsets(df)            # Conflict onset data

# Get some WDI data...

wdi<-WDI(country="all",indicator=c("Population"="SP.POP.TOTL",
                       "PopulationDensity"="EN.POP.DNST",
                       "GINIIndex"="SI.POV.GINI",
                       "LiteracyRate"="SE.ADT.LITR.ZS",
                       "HIVPrevalence"="SH.DYN.AIDS.ZS",
                       "HomicidesPer100K"="VC.IHR.PSRC.P5",
                       "AdFertilityRate"="SP.ADO.TFRT",
                       "PropWomenInParliament"="SG.GEN.PARL.ZS"),
         extra=TRUE)

# Subset the data to the year 2015 *only*:

df<-df[df$year==2015,]
wdi<-wdi[wdi$year==2015,]

# Merge those two data frames:

wdi$ISO3<-wdi$iso3c # create ISO3 indicator
data<-merge(df,wdi,by=c("ISO3"),all=TRUE) # merge

# Limit the data to Africa *only* (this is, sadly, more 
# complex than one would hope):

data$InAfrica<-ifelse(data$region=="Sub-Saharan Africa",1,0)
data$InAfrica<-ifelse(data$country=="Algeria",1,data$InAfrica)
data$InAfrica<-ifelse(data$country=="Egypt, Arab Rep.",1,data$InAfrica)
data$InAfrica<-ifelse(data$country=="Libya",1,data$InAfrica)
data$InAfrica<-ifelse(data$country=="Morocco",1,data$InAfrica)
data$InAfrica<-ifelse(data$country=="Tunisia",1,data$InAfrica)

data<-data[data$InAfrica==1,]

# Eliminate messiness:

data<-data[is.na(data$ISO3)==FALSE,]

# Indicator for sub-saharan Africa:

NoAf<-c("DZA","EGY","LBY","MAR","TUN")
data$SubSaharan<-ifelse(data$ISO3 %in% NoAf,0,1)

# Now make the data prettier and get rid of extra variables:

Data<-with(data, data.frame(ISO3=ISO3,Country=country,Capital=capital,
                            Longitude=as.numeric(longitude),
                            Latitude=as.numeric(latitude),
                            SubSaharan=SubSaharan,
                            Population=Population,
                            PopulationDensity=PopulationDensity,
                            VDEMDemocracy=v2x_polyarchy,
                            POLITYDemocracy=polity2,
                            ConflictOnset=sumnewconf,
                            HIVPrevalence=HIVPrevalence,
                            AdFertilityRate=AdFertilityRate,
                            HomicidesPer100K=HomicidesPer100K,
                            PropWomenInParliament=PropWomenInParliament,
                            LiteracyRate=LiteracyRate,
                            GINIIndex=GINIIndex))

# Output data:

write.csv(Data,"Data/Africa2015.csv",row.names=FALSE)