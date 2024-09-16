#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction / Preliminaries                     ####
#
# PLSC 502 -- Fall 2024
#
# Day Three materials: Univariate, bivariate, and
# multivariate graphics / data visualization...
#
# Packages, etc.:                               ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("peacesciencer","readr","car","psych","lattice","fmsb",
     "akima","rgl","rnaturalearth","rnaturalearthdata","sf",
     "tmap","plotly","ggplot2")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Highlight and run that ^^^ code 10-12 times until you get 
# all smileys in the window below. :) (Running it "extra"
# times won't hurt anything.) 
#
# Next, set a working directory, if you want (it's a good idea, IMO):
#
# setwd("~/Dropbox (Personal)/PLSC 502/Notes & Slides")
#
# or use an R project, etc.
#
# Then set some R options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# OK, time to get to work.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Univariate graphics...                           ####
#
# Get Africa (2015) data:

Africa<-as.data.frame(read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/Africa2015.csv"))

# Note that if you want to recreate these data yourself,
# the code for doing so is available in the "Code" folder
# on the Github repo, in the file called "Make-Africa-Data.R".
#
# Summary statistics:

summary(Africa)

# Somewhat nicer:

describe(Africa,skew=FALSE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data plots...
#
# Dotchart, population (in millions)

pdf("PopulationDotchart15.pdf",6,5)
par(mar=c(2,2,2,2))
with(Africa, dotchart(Population/1000000,pch=19,labels=Country,
             cex=0.4,xlab="Population in Millions"))
dev.off()

# Dotchart redux (sorted):

Africa<-Africa[order(Africa$Population),]

pdf("PopulationDotchart15-2.pdf",6,5)
par(mar=c(2,2,2,2))
with(Africa, dotchart(Population/1000000,pch=19,labels=Country,
                      cex=0.4,xlab="Population in Millions"))
abline(v=c(25,50,75,100,125,150,175),lty=2,lwd=1)
dev.off()

# Barchart (just like a dotchart...):

pdf("PopulationBarchart15.pdf",6,5)
par(mar=c(4,6,2,2))
with(Africa, barplot(Population/1000000,horiz=TRUE,names.arg=Country,
             las=1,cex.names=0.4,xlab="Population in Millions"))
dev.off()

# Spider / Radar plots...
# 
# Let's subset the data... first, *just* grab Botswana:

BWA<-Africa[Africa$Country %in% c("Botswana"),]

# Subset variables too:

BWA<-BWA[,c("PropWomenInParliament","GINIIndex","AdFertilityRate","HIVPrevalence")]

# Now we have to add two additional rows with the minimum and maximum
# values of the variables, as described here:
#
# https://r-graph-gallery.com/142-basic-radar-chart.html

BWA<-rbind(c(100,100,200,50),rep(0,4),BWA)

# Now, draw the radar plot:

pdf("RadarChart15-1.pdf",6,6)
radarchart(BWA,vlcex=0.8)
dev.off()

# Add a second country... how about Namibia?

NAM<-Africa[Africa$Country %in% c("Namibia"),
            c("PropWomenInParliament","GINIIndex","AdFertilityRate","HIVPrevalence")]
DF<-rbind(BWA,NAM)

# Now a plot with the two countries:

pdf("RadarChart15-2.pdf",6,6)
radarchart(DF,vlcex=0.8)
legend("topright",col=c("black","red"),pch=19,bty="n",
       legend=c("Botswana","Namibia"))
dev.off()

# Add Ethiopia?

ETH<-Africa[Africa$Country %in% c("Ethiopia"),
            c("PropWomenInParliament","GINIIndex","AdFertilityRate","HIVPrevalence")]
DF<-rbind(DF,ETH)

# Plot:

pdf("RadarChart15-3.pdf",6,6)
radarchart(DF,vlcex=0.8)
legend("topright",col=c("black","red","green"),pch=19,bty="n",
       legend=c("Botswana","Namibia","Ethiopia"))
dev.off()

# MAPS!
#
# Get the map data:

afmap<-ne_countries(continent="Africa",scale="medium",returnclass="sf")

# Merge with the main Africa data from 2015:

mapdata<-merge(afmap,Africa,by.x="iso_a3",by.y="ISO3",all=TRUE)

# Set the viewing mode:

tmap_mode(mode="plot") # Static map (for now...)

# Draw the map (this is the simple version -- there is
# a **lot** more complexity to this...)

pdf("FertilityMap2015.pdf",6,6)
qtm(mapdata,"AdFertilityRate") +
  tm_layout(frame = FALSE)     # get rid of the box around the map
dev.off()

# AND ANOTHER BECAUSE MAPS UNIRONICALLY ROCK:

pdf("PWPMap2015.pdf",6,6)
qtm(mapdata,"PropWomenInParliament") +
  tm_layout(frame = FALSE)     # get rid of the box around the map
dev.off()

# Yet another, this time with lots of missing data:

pdf("LiteracyMap2015.pdf",6,6)
qtm(mapdata,"LiteracyRate") +
  tm_layout(frame = FALSE)     # get rid of the box around the map
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Summary plots:
#
# Histogram, POLITY Democracy score:

pdf("POLITYHistogram15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, hist(POLITYDemocracy,breaks=10,col="grey",main=" ",
             xlab="POLITY Democracy Score"))
dev.off()

# Histogram, Sub-saharan:

Africa$Region<-ifelse(Africa$SubSaharan==0,"Not Sub-Saharan","Sub-Saharan")

pdf("SubsaharanHistogram15.pdf",6,5)
par(mar=c(4,4,2,2))
xx<-with(Africa, barplot(table(Region),col="grey",main=" ",
         xlab="Region",ylim=c(0,55),
         beside=TRUE,xpd=FALSE))
# Add Ns to top of bars:
with(Africa, text(xx, table(Region),pos=3,
         labels=paste("N = ",c(table(Region)),sep="")))
dev.off()

# Kernel density:

pdf("POLITYKDensity15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa[is.na(Africa$POLITYDemocracy)==FALSE,],
     plot(density(POLITYDemocracy),t="l",lwd=2,main="",
             xlab="POLITY Democracy Score",
             xlim=c(-10,10)))
dev.off()

# Overlay histogram and density plot:

pdf("POLITYHistoDensity15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, hist(POLITYDemocracy,breaks=10,col="grey",main=" ",
                  xlab="POLITY Democracy Score",freq=FALSE))
with(Africa[is.na(Africa$POLITYDemocracy)==FALSE,],
     lines(density(POLITYDemocracy),t="l",lwd=2))
dev.off()

# Density plot, Prop. Women in Parliament:

pdf("PWPKDensity15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa[is.na(Africa$PropWomenInParliament)==FALSE,],
     plot(density(PropWomenInParliament),t="l",
                  lwd=2,main="",
                  xlab="Percentage of Women in Parliament"))
dev.off()

# Overlay a Normal distribution with the same 
# mean and variance / std. deviation:

pdf("PWPKDensity15-2.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa[is.na(Africa$PropWomenInParliament)==FALSE,],
     plot(density(PropWomenInParliament),t="l",
          lwd=2,main="",
          xlab="Percentage of Women in Parliament"))
curve(dnorm(x,mean=mean(Africa$PropWomenInParliament,na.rm=TRUE),
              sd=sd(Africa$PropWomenInParliament,na.rm=TRUE)),
      lty=2,lwd=2,col="grey",add=TRUE)
legend("topright",bty="n",lty=c(1,2),col=c("black","grey"),
       lwd=2,legend=c("Density","Theoretical Normal"))
dev.off()


# Q-Q plot, Prop. Women in Parliament:

pdf("PWP-QQ15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, qqnorm(PropWomenInParliament,main="",
                  ylab="Percentage of Women in Parliament"))
with(Africa, qqline(PropWomenInParliament,lwd=2))
dev.off()

# Boxplots, Women in Parliament:

pdf("PWPBoxplot15.pdf",6,5)
par(mar=c(2,4,2,2))
with(Africa, boxplot(PropWomenInParliament,main="",
                    ylab="Percentage of Women in Parliament"))
dev.off()

# Boxplots, three variables:

pdf("MultipleBoxplot15.pdf",8,5)
par(mar=c(2,4,2,2))
boxplot(Africa[,c("PropWomenInParliament","HIVPrevalence","HomicidesPer100K")],
        main="",ylab="Rate",names=c("Prop. Women in Parliament","HIV Rate",
                                    "Homicides (per 100K)"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bivariate & Multivariate Plots                   ####
#
# First, scatterplots...
#
# Basic scatterplot:

pdf("FertilityPWPScatterplot15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(AdFertilityRate,PropWomenInParliament))
dev.off()

# Nicer scatterplot:

pdf("AltFertilityPWPScatterplot15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(AdFertilityRate,PropWomenInParliament,
                  pch=19,xlab="Adolescent Fertility Rate",
                  ylab="Percentage of Women in Parliament",
                  xlim=c(-10,200),ylim=c(0,80)))
with(Africa, text(AdFertilityRate,PropWomenInParliament,
                  labels=ISO3,pos=3,cex=0.8))
abline(v=mean(Africa$AdFertilityRate,na.rm=TRUE),lty=2)
abline(h=mean(Africa$PropWomenInParliament,na.rm=TRUE),lty=2)
dev.off()

# Skewed data:

pdf("PopDenHIVScatterplot15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(PopulationDensity,HIVPrevalence,
                  pch=19,xlab="Population Density",
                  ylab="HIV Prevalence"))
dev.off()

# Skewed data, logged:

pdf("LoggedPopDenHIVScatterplot15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(PopulationDensity,HIVPrevalence,
                  pch=19,log="xy",
                  xlab="Population Density (log scale)",
                  ylab="HIV Prevalence (log scale)"))
dev.off()

# How Not To Draw A Scatterplot:

pdf("SubSaharanConflictScatterplot15.pdf",5,5)
par(mar=c(4,4,2,2))
with(Africa, plot(as.numeric(SubSaharan),ConflictOnset,
                  pch=19,xaxp=c(0,1,1),yaxp=c(0,1,1),
                  xlab="Sub-Saharan",ylab="Conflict Onset"))
dev.off()

# Frequency tables are better:

with(Africa, xtabs(~SubSaharan+ConflictOnset))

# Binary-Continuous:

pdf("DemocracyConflictScatterplot15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(VDEMDemocracy,ConflictOnset,pch=19,
                  yaxp=c(0,1,1),xlab="VDEM Democracy Score",
                  ylab="Conflict Onset"))
dev.off()

# Add lowess:

pdf("LowessDemocracyConflictScatterplot15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(VDEMDemocracy,ConflictOnset,pch=19,
                  xlab="VDEM Democracy Score",
                  ylab="Conflict Onset"))
ss<-with(Africa[is.na(Africa$VDEMDemocracy)==FALSE,],
         smooth.spline(VDEMDemocracy,ConflictOnset,spar=0.8))
lines(ss,lwd=2,col="red",lty=2)
dev.off()

# Bivariate / conditioned boxplots:

pdf("SubSaharanFertilityBoxplots15.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, boxplot(AdFertilityRate~Region,xlab="Region",
                     ylab="Adolescent Fertility Rate"))
dev.off()

# Multiple conditioned boxplots:

pdf("SubsaharanMultipleBoxplots15.pdf",9,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
with(Africa, boxplot(AdFertilityRate~Region,xlab="Region",
                     ylab="Ad. Fertility Rate",cex=0.6,
                     main="Ad. Fertility Rate"))
with(Africa, boxplot(VDEMDemocracy~Region,xlab="Region",
                     ylab="V-DEM Democracy",cex=0.6,
                     main="V-DEM Democracy"))
with(Africa, boxplot(PopulationDensity~Region,xlab="Region",
                     ylab="Population Density",cex=0.6,
                     main="Population Density"))
dev.off()

# QQ-plot comparisons

pdf("Fertility-QQ-Conflict15.pdf",6,5)
par(mar=c(4,4,4,2))
with(Africa, qq(ConflictOnset~AdFertilityRate,
                col="black",pch=20,xlab="No Conflict Onset",
                ylab="Conflict Onset",
                main="Adolescent Fertility Rate"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Multivariate plots...
#
# Scatterplot matrix:

pdf("ScatterplotMatrixAfrica15.pdf",6,5)
par(mar=c(4,4,2,2))
dd <- Africa[,c("PopulationDensity","VDEMDemocracy",
                "AdFertilityRate","PropWomenInParliament")]
scatterplotMatrix(dd,reg.line=FALSE,smoother=FALSE,pch=20,
                  var.labels=c("Pop. Density","Democracy",
                               "Fertility","Wmn. in Leg."),
                  col="black",cex.labels=0.9)
dev.off()

# Conditional scatterplots:

pdf("Fert-PWP-Region-15.pdf",7,5)
par(mfrow=c(1,2)) # <- Create a combined plot: 1 row, 2 columns
par(mar=c(4,4,4,2))
with(Africa[Africa$Region=="Not Sub-Saharan",],
     plot(AdFertilityRate,PropWomenInParliament,
          pch=19,main="Not Sub-Saharan",log="x",
          xlab="Ad. Fertility Rate",
          ylab="Prop. Women in Parliament"))
with(Africa[Africa$Region=="Sub-Saharan",],
     plot(AdFertilityRate,PropWomenInParliament,
          pch=19,main="Sub-Saharan",log="x",
          xlab="Ad. Fertility Rate",
          ylab="Prop. Women in Parliament"))
dev.off()

# Contour plot (requires akima package):

cpdata<-data.frame(Z=Africa$PropWomenInParliament,
                   X=Africa$AdFertilityRate,
                   Y=Africa$VDEMDemocracy)
cpdata<-cpdata[complete.cases(cpdata),]
cpdata<-with(cpdata,interp(X,Y,Z,duplicate="mean"))

pdf("PWP-Fertility-Dem-ContourR.pdf",7,5)
par(mar=c(4,4,2,2))
filled.contour(cpdata,color.palette=topo.colors,
               xlab="Adolescent Fertility Rate",
               ylab="V-DEM Democracy")
dev.off()

# "3-D" scatterplot:

pdf("PWP-Fertility-Dem-Scatter15.pdf",6,5)
par(mar=c(2,2,2,2))
cloud(PropWomenInParliament~AdFertilityRate*VDEMDemocracy,
      Africa,col="red",pch=20,zlab="PWP")
dev.off()

# Interactive "3D scatterplot" - for this to work, you'll
# need a viewer like (e.g.) https://www.xquartz.org/ (for 
# MacOS) or similar...

fig<-plot_ly(x=~AdFertilityRate,y=~VDEMDemocracy,
             z=~PropWomenInParliament,data=Africa,
             type='scatter3d',mode='markers',
             symbols=20)
fig

# (file in the slides was saved interactively -- this is not,
# technically speaking, 100% reproduceable, but seems OK in
# context...)
#
# Multivariate Data display:

Africa$Big<-factor(Africa$Population>median(Africa$Population),
                   labels=c("Big","Small")) # Splitting population at its median
Africa$Conflict<-factor(Africa$ConflictOnset,
                        labels=c("No Conflict","Conflict")) # creating a "factor" 
                                                            # variable for conflict

pdf("PWP-Fert-Size-Conflict-Scatter15.pdf",6,5)
with(Africa, xyplot(PropWomenInParliament~AdFertilityRate|Conflict*Big,
                    col="black",panel=function(x,y){panel.xyplot(x,y);
                      panel.loess(x,y,span=1)},
                    xlab="Ad. Fertility Rate",
                    ylab="Prop. Women in Parliament"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Tidyverse plotting...                            ####
#
# Example 1: (Horizontal) barchart:

pdf("TidyBarplot15.pdf",8,6)
p<-ggplot(data=Africa, aes(x=reorder(Country,Population),
                           y=Population/1000000)) +
  geom_bar(stat="identity") +
  labs(y="Population (in millions)",x="Country") +
  theme_classic() + coord_flip()
p
dev.off()

# Example 2: Scatterplot:

pdf("TidyScatter15.pdf",7,5)
p2<-ggplot(data=Africa, aes(x=AdFertilityRate,y=PropWomenInParliament)) +
  geom_point() + theme_classic() +
  labs(y="Proportion Women In Parliament",x="Adolescent Fertility Rate") +
  geom_text(label=Africa$ISO3,size=3,nudge_y=2)
p2
dev.off()


# Example 3: Contour plot:
#
# Data fix:

df<-as.data.frame(interp2xyz(cpdata))

pdf("TidyContour15.pdf",7,5)
p3<-ggplot(data=df,aes(x=x,y=y,fill=z)) +
  geom_raster(interpolate = TRUE) + 
  scale_fill_gradientn(colours = c("blue","green","gold"),
                     na.value = "#FFFFFF",name="PWP") +
  labs(y="V-DEM Democracy",x="Ad. Fertility Rate") +
  theme_classic()
p3
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# General R graphics things...                     ####
#
# Power of "plot":

with(Africa, xtabs(~Region+Big))

pdf("PowerOfPlot15.pdf",7,5)
plot(with(Africa,xtabs(~Region+Big)))
dev.off()

# Making PDFs, PNGs, etc.

pdf("MyPDF.pdf",7,5) # Turn on the PDF device; make the aspect ratio 7:5
plot(muslperc,adrate,data=Africa) # Make the plot
dev.off()  # Turn off the PDF-maker device

# fin