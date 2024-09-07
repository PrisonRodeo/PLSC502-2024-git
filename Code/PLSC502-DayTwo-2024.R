#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PLSC 502 -- Fall 2024
#
# INTRODUCTION                                         ####
#
# This is the R code for Day Two of PLSC 502. It contains
# everything you need to replicate the materials in the
# class slides (although it does not actually create those
# slides - they were created using LaTeX). 
#
# In general class code will start with some preliminaries
# that do things like load required R packages, set the
# working directory, and so forth. We will typically read
# data directly from the course Github repository (which
# means that the code will usually need to be run from a 
# computer attached to the internet).
#
# The lines beginning with "#" are commented out. If a
# commented-out line contains a command, you can consider
# it "optional," in the sense that you might not need to 
# run it to make the code work.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries                                        ####
#
# First, load a package or two that we'll use in a bit. If
# you haven't already installed the package called "readr,"
# uncomment (erase the "#" at the beginning of the line) 
# and run this line:
#
# install.packages("readr")
#
# Then run this:

library(readr)

# Repeat this for another package, "rjson":
#
# install.packages("rjson")
#
# Then run this:

library(rjson)

# There are more efficient ways to install and load
# packages, which we'll get to in a bit.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data examples                                        ####
#
# DH data (basic rectangular):

DH<-as.data.frame(read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/DH.csv"))

select<-c("respon","age","female","followbaseball","DH_appr")
head(DH[select],8)

# Clerks / time-series:

Clerks<-as.data.frame(read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/annualclerks.csv"))

select<-c("Term","female","white","top5law","lcclerk")
head(Clerks[select],15)

# Panel/TSCS country data:

Panel<-as.data.frame(read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/Assass.csv"))

select<-c("country","ccode","year","gdppc","polity","region","coldwar")
Panel<-Panel[order(Panel$ccode,Panel$year),] # sort
Panel[1:200,select]

# Relational data: Country "dyads" (1968):

Dyads<-as.data.frame(read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/dyads1968.csv"))

select<-c("ccode1","ccode2","dyadid","dem1","dem2","allies","distance")
Dyads[1:300,select]

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data Formats                                         ####
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# A tiny, fake rectangular data frame:

df<-data.frame(id=c(1,2,3),
               guitar=c("Stratocaster","Les Paul","Telecaster"),
               pickups=c(3,2,2))
df

# Convert to JSON:

df.JSON<-toJSON(df)
df.JSON

# (I didn't do anything for a SQL example... it's not a big deal,
# trust me.)
#
# fin
