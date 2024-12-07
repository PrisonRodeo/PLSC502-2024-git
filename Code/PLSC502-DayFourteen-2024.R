#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                 ####
#
# PLSC 502 -- Fall 2024
#
# Day Fourteen materials: Bayesian Statistics and
#                         Missing Data...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:

P<-c("readr","remotes","psych","plyr","car","ggplot2",
     "mvtnorm","DescTools","rstan","rstanarm","brms",
     "texreg")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Also run this:

devtools::install_github("crubba/htmltab")
library(htmltab)

# Set significant digits:

options(digits=4)

# Also-also: -setwd()- in here if you'd like...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Missing data: A simulation                   ####
#
# Let's draw 600 observations on X and Y, such that 
# their correlation is r = 0.707 (so that R^2=0.5):

set.seed(7222009)
data<-as.data.frame(rmvnorm(600,mean=c(0,0),
                    sigma=matrix(c(1,0.707,0.707,1),nrow=2)))
colnames(data)<-c("X","Y")
summary(lm(Y~X,data=data))

# Plot all N=600 observations:

pdf("MDComplete.pdf",6,5)
par(mar=c(4,4,2,2))
plot(data,pch=20,xlim=c(-3,3),ylim=c(-3,3))
abline(lm(Y~X,data=data),lwd=2)
dev.off()

# MCAR:
#
# Flag 300 observations randomly for deletion:

MCARs<-sample(nrow(data),300,replace=FALSE)
data$MCAR<-ifelse(rownames(data) %in% MCARs,1,0)

# Is the missingness related to X or Y?

t.test(Y~MCAR,data=data)$statistic
t.test(X~MCAR,data=data)$statistic

# Plot:

pdf("MD-MCAR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(data[data$MCAR==0,]$X,data[data$MCAR==0,]$Y,pch=20,
     xlab="X",ylab="Y",xlim=c(-3,3),ylim=c(-3,3))
points(data[data$MCAR==1,]$X,data[data$MCAR==1,]$Y,pch=1,col="red")
abline(lm(Y~X,data=data),lwd=2)
abline(lm(Y~X,data=data[data$MCAR==0,]),
       lty=2,lwd=2,col="red")
legend("topleft",bty="n",lwd=2,lty=c(1,2),col=c("black","red"),
       legend=c("No Missing Data","MCAR"))
dev.off()

# MAR:
#
# Flag 300 observations for deletion based on their 
# values on X:

MARs<-sample(nrow(data),300,replace=FALSE,prob=pnorm(data$X))
data$MAR<-ifelse(rownames(data) %in% MARs,1,0)

# Is the missingness related to X or Y?

t.test(Y~MAR,data=data)$statistic
t.test(X~MAR,data=data)$statistic

# Plot:

pdf("MD-MAR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(data[data$MAR==0,]$X,data[data$MAR==0,]$Y,pch=20,
     xlab="X",ylab="Y",xlim=c(-3,3),ylim=c(-3,3))
points(data[data$MAR==1,]$X,data[data$MAR==1,]$Y,pch=1,col="red")
abline(lm(Y~X,data=data),lwd=2)
abline(lm(Y~X,data=data[data$MAR==0,]),
       lty=2,lwd=2,col="red")
legend("topleft",bty="n",lwd=2,lty=c(1,2),col=c("black","red"),
       legend=c("No Missing Data","MAR"))
dev.off()

# NMAR, part 1:
#
# Flag 300 observations for deletion based on their 
# values on Y:

NMARs<-sample(nrow(data),300,replace=FALSE,prob=pnorm(data$Y))
data$NMAR<-ifelse(rownames(data) %in% NMARs,1,0)

# Is the missingness related to X or Y?

t.test(Y~NMAR,data=data)$statistic
t.test(X~NMAR,data=data)$statistic

# Plot:

pdf("MD-NMAR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(data[data$NMAR==0,]$X,data[data$NMAR==0,]$Y,pch=20,
     xlab="X",ylab="Y",xlim=c(-3,3),ylim=c(-3,3))
points(data[data$NMAR==1,]$X,data[data$NMAR==1,]$Y,pch=1,col="red")
abline(lm(Y~X,data=data),lwd=2)
abline(lm(Y~X,data=data[data$NMAR==0,]),
       lty=2,lwd=2,col="red")
legend("topleft",bty="n",lwd=2,lty=c(1,2),col=c("black","red"),
       legend=c("No Missing Data","NMAR"))
dev.off()

# NMAR, part 2 (confounding):
#
# Flag 300 observations for deletion based on their 
# values on an "unmeasured" variable U that is related 
# to X and Y:

data$U <- (2*data$Y)-(2*data$X)
NMAR2<-sample(nrow(data),300,replace=FALSE,prob=pnorm(data$U))
data$NMAR2<-ifelse(rownames(data) %in% NMAR2,1,0)

# Is the resulting missingness related to X or Y?

t.test(Y~NMAR2,data=data)$statistic
t.test(X~NMAR2,data=data)$statistic

# Plot:

pdf("MD-NMAR2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(data[data$NMAR2==0,]$X,data[data$NMAR2==0,]$Y,pch=20,
     xlab="X",ylab="Y",xlim=c(-3,3),ylim=c(-3,3))
points(data[data$NMAR2==1,]$X,data[data$NMAR2==1,]$Y,pch=1,col="red")
abline(lm(Y~X,data=data),lwd=2)
abline(lm(Y~X,data=data[data$NMAR2==0,]),
       lty=2,lwd=2,col="red")
legend("topleft",bty="n",lwd=2,lty=c(1,2),col=c("black","red"),
       legend=c("No Missing Data","NMAR"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bayesian things...                           ####
#
# Heuristic Bayes pics...
#
# (Using a beta distribution just for the pictures;
# it doesn't have any special importance here...)

set.seed(7222009)
D<-rbeta(101,2,5)

# Prior only:

pdf("HBayes1.pdf",3,4)
par(mar=c(4,4,2,2))
plot(seq(0,1,by=0.01),dbeta(seq(0,1,by=0.01),5,2),t="l",
     lwd=2,xlab="Parameter Value",ylab="Density")
# points(D,rep(0,101),pch=20,col="orange")
legend("topleft",bty="n",lwd=2,legend="Prior",cex=0.8)
dev.off()

# Prior + Data:

pdf("HBayes2.pdf",3,4)
par(mar=c(4,3,2,2))
plot(seq(0,1,by=0.01),dbeta(seq(0,1,by=0.01),5,2),t="l",
     lwd=2,xlab="Parameter Value",ylab="")
points(D,rep(0,101),pch=20,col="orange")
legend("topleft",bty="n",lwd=c(2,NA),col=c("black","orange"),
       pch=c(NA,20),cex=0.8,legend=c("Prior","Data"))
dev.off()

# Prior + Data = Posterior:

pdf("HBayes3.pdf",3,4)
par(mar=c(4,3,2,2))
plot(seq(0,1,by=0.01),dbeta(seq(0,1,by=0.01),5,2),t="l",
     lwd=2,xlab="Parameter Value",ylab="")
points(D,rep(0,101),pch=20,col="orange")
lines(seq(0,1,by=0.01),dbeta(seq(0,1,by=0.01),2.5,3.5),
      lwd=2,col="blue",lty=2)
legend("topleft",bty="n",lwd=c(2,NA),col=c("black","orange","blue"),
       pch=c(NA,20,NA),lty=c(1,NA,2),cex=0.8,
       legend=c("Prior","Data","Posterior"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Court data, once again...                    ####
#
# Once again: code to aggregate SCOTUS votes from the raw 
# vote data at http://scdb.wustl.edu/data.php:

temp <- tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2024_01/SCDB_2024_01_justiceCentered_Docket.csv.zip",temp)
SCDB <- read.csv(unz(temp, "SCDB_2024_01_justiceCentered_Docket.csv"))
unlink(temp)
rm(temp)

CivLib<-SCDB[SCDB$issueArea==2,]
CivLib$LibVote<-CivLib$direction-1
Justices <- ddply(CivLib,.(justiceName),summarize,
                  justice = mean(justice),
                  CivLibs = mean(LibVote,na.rm=TRUE)*100)

# Now get and merge the "Segal-Cover" scores:

url<-"https://en.wikipedia.org/wiki/Segal%E2%80%93Cover_score"
SC<-htmltab(doc = url,rm_nodata_cols=FALSE)
rm(url)
SC<-SC[SC$Nominee!="Harlan F. Stone",]
SC<-SC[SC$Nominee!="James F. Byrnes",]
SC<-SC[SC$Nominee!="Clement Haynsworth, Jr.",]
SC<-SC[SC$Nominee!="G. Harrold Carswell",]
SC<-SC[SC$Nominee!="Robert H. Bork",]
SC<-SC[SC$Nominee!="Douglas Ginsburg",]
SC<-SC[SC$Nominee!="Harriet E. Miers",]
SC<-SC[SC$Nominee!="Merrick Garland",]
SC<-SC[SC$SenateVote!="45 – 43 *",] # Fortas CJ
SC<-SC[SC$SenateVote!="65 – 33",]   # Rehnquist CJ
SC$justice<-as.numeric(SC$Nom.Order)+77
SC$justice<-ifelse(SC$justice>82,SC$justice-1,SC$justice)
SC$justice<-car::recode(SC$justice,"100=99;103=100;104=101;105=102;
                   106=103;107=104;109=105;112=106;113=107;
                   114=108;115=109;116=110;117=111;119=112;
                   120=113;121=114;123=115;124=116;125=117;
                   126=118")

# Merge (messily):

SCOTUS<-merge(Justices,SC,by=c("justice"))
SCOTUS$IdeologyScore<-as.numeric(SCOTUS$IdeologyScore)
SCOTUS$Year<-as.numeric(SCOTUS$Year)
SCOTUS<-SCOTUS[SCOTUS$justiceName!="KBJackson",]

# Clean up:

rm(SCDB,CivLib,Justices,SC)

describe(SCOTUS,skew=FALSE,trim=0)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# A Simple Little Bayesian Regression...             ####
#
# Change the plotting theme:

ggplot2::theme_set(theme_minimal())

# OLS regression using -brms-, with mostly default settings:

get_prior(CivLibs~IdeologyScore,data=SCOTUS) # default prior...

bfit<-brm(CivLibs~IdeologyScore,data=SCOTUS,
          chains=10,silent=2,seed=7222009)

summary(bfit) # a la summary(lm())

pdf("BayesOLS124.pdf",7,6)
plot(bfit)
dev.off()

# Compare to standard OLS:

fit<-lm(CivLibs~IdeologyScore,data=SCOTUS)

# Table:

texreg(l=list(fit,bfit),stars=0.05,
       custom.model.names=c("OLS","Bayesian"))

# Now, add one with "informative" priors. Specifically, we'll 
# set the prior on the slope of "IdeologyScore" to be Normal, 
# with a mean of 20 and a standard deviation (standard error) 
# of 15; that mean is a bit low, and the SD is a bit high:

Prior<-c(set_prior("normal(20,15)",class="b",coef="IdeologyScore"))

bfit2<-brm(CivLibs~IdeologyScore,data=SCOTUS,
          chains=10,silent=2,seed=7222009,
          prior=Prior)

summary(bfit2) # a la summary(lm())

pdf("BayesOLS224.pdf",7,6)
plot(bfit2)
dev.off()

# Plot prior and posterior:

pdf("PriorVsPosterior24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(seq(-30,80),dnorm(seq(-30,80),mean=20,sd=15),
     t="l",lwd=2,xlab="Estimated Beta",ylab="Probability",
     ylim=c(0,0.07),col="orange")
abline(v=20,lty=2,col="orange")
lines(seq(-30,80),dnorm(seq(-30,80),
                  mean=summary(bfit2)$fixed$Estimate[2],
                  sd=summary(bfit2)$fixed$Est.Error[2],),
      lwd=2,col="darkgreen")
abline(v=summary(bfit2)$fixed$Estimate[2],lty=2,col="darkgreen")
legend("topleft",bty="n",lwd=2,col=c("orange","darkgreen"),
       legend=c("Prior Distribution","Posterior Distribution"))
dev.off()

# Comparison table:

texreg(l=list(fit,bfit,bfit2),stars=0.05,
       custom.model.names=c("OLS","Bayesian","w/Priors"))

# \fin (for real this time)
