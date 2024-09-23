#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                           ####
#
# PLSC 502 -- Fall 2024
#
# Day Four materials: Central Tendency and
# variation...
#
# Set working directory, etc.:
#
# setwd("~/Dropbox (Personal)/PLSC 502/Notes & Slides")
#
# Packages, etc.:                               ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","psych","modeest","moments","remotes",
     "XML","stargazer","tinytable")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 7-8 times until you get all smileys.
# Then, run this:

remotes::install_github("htmltab/htmltab")

#... and type "1" at the prompt. Once that's done,
# run this:

library(htmltab)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Central Tendency                       ####
#
# Get current Premier League data:

url<-"https://www.skysports.com/premier-league-table"
PL<-htmltab(doc = url)
PL[,c(3:10)]<-apply(PL[,c(3:10)],2,
                 function(x) as.numeric(x)) # make data numeric
rm(url)                                     # clean up

# Rename some variables:

colnames(PL)<-c("Rank","Team","GamesPlayed","Won","Drew","Lost",
                "GoalsFor","GoalsAgainst","GoalDifference","Points")
# Print data:

print(PL[,c(2:10)],row.names=FALSE)

# Histogram:

pdf("PL-2024-Histogram.pdf",6,5)
par(mar=c(4,4,2,2))
with(PL, hist(Points,col="grey",main="",xlab="Points Total"))
dev.off()

# Geometric means example:

geometric.mean(c(12,5,-9,2,-10))
geometric.mean(c(1.12,1.05,0.91,1.02,0.90))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now, code for the illustrative figures...
#
# Modes: Discrete data

set.seed(7222009)
n<-1000
nomo <- rep(1:10,100)
u <- round(rnorm(n,5,1),0)
bi1 <- round(rnorm(n/2,3,1),0)
bi2 <- round(rnorm(n/2,7,1),0)
b <- append(bi1,bi2)

pdf("ManyModesD.pdf",7,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
hist(u,xlim=c(0,10),breaks=5,main="Unimodal",
     ylab="Frequency",xlab="X")
par(mar=c(4,2,2,2))
hist(b,main="Bimodal",xlab="X")
par(mar=c(4,2,2,2))
hist(nomo,breaks=0:10,ylim=c(0,200),main="No Mode",xlab="X")
dev.off()

# Modes: Continuous data

x<-seq(-1,1,by=0.01)
nomode <- dunif(x,-0.9,0.9)
uni <- dnorm(x,0,0.2)
b1 <- dnorm(x,-0.5,0.15)
b2 <- dnorm(x,0.5,0.15)
bi <- 1.2*b1+b2

pdf("ManyModesC.pdf",7,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(x,uni,t="l",lwd=3,xlab="X",yaxt="n",
     ylab="Density",main="Unimodal")
par(mar=c(4,2,2,2))
plot(x,bi,t="l",lwd=3,xlab="X",yaxt="n",
     ylab=" ",main="Bimodal")
plot(x,nomode,t="l",lwd=3,xlab="X",yaxt="n",
     ylab=" ",ylim=c(0,0.8),main="No Mode")
dev.off()

# Means/modes/medians and skewness:

p<-10
q<-1.5
m<-(p+q)/2
x<-seq(0,1,by=0.001)
left<-dbeta(x,p,q)
noskew<-dbeta(x,m,m)
right<-dbeta(x,q,p)

pdf("MeanMedianMode.pdf",6,5)
par(mfrow=c(3,1))
par(mar=c(4,4,2,2))
# right-skewed:
plot(x,right,t="l",lwd=2,main="Right-Skewed",
     xlab="X",ylab="Density")
abline(v=(q/(q+p)),lwd=2,col="red",lty=2) # mean
abline(v=((q-(1/3))/(p+q-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((q-1)/(q+p-2)),lwd=2,col="orange",lty=2) # mode
legend("topright",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
# symmetrical:
par(mar=c(4,4,2,2))
plot(x,noskew,t="l",lwd=2,main="Symmetrical",
     xlab="X",ylab="Density")
abline(v=(m/(m+m))+0.002,lwd=2,col="red",lty=2) # mean
abline(v=((m-(1/3))/(m+m-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((m-1)/(m+m-2))-0.002,lwd=2,col="orange",lty=2) # mode
legend("topright",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
# left-skewed:
plot(x,left,t="l",lwd=2,main="Left-Skewed",
     xlab="X",ylab="Density")
abline(v=(p/(p+q)),lwd=2,col="red",lty=2) # mean
abline(v=((p-(1/3))/(q+p-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((p-1)/(p+q-2)),lwd=2,col="orange",lty=2) # mode
legend("topleft",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
dev.off()

# Now, what if we have perfectly symmetrical 
# (here, normal) data?

set.seed(7222009)
XS<-rnorm(10001,mean=20,sd=3)
am<-mean(XS)
gm<-geometric.mean(XS)
hm<-harmonic.mean(XS)
me<-median(XS)
mo<-mlv(XS,"meanshift")

lw<-2.5 # set line width...

pdf("CTSymmetrical.pdf",7,5)
par(mar=c(4,4,1,1))
hist(XS,col="grey",breaks=30,main="",xlab="X")
abline(v=am, col="red",lty=2,lwd=lw)
abline(v=gm,col="black",lty=3,lwd=lw)
abline(v=hm,col="blue",lty=4,lwd=lw)
abline(v=me,col="darkgreen",lty=5,lwd=lw)
abline(v=mo,col="orange",lty=6,lwd=lw)
legend("topright",bty="n",legend=c("Arithmetic Mean",
       "Geometric Mean","Harmonic Mean","Median","Mode"),
       lwd=lw,lty=c(2,3,4,5,6),
       col=c("red","black","blue","darkgreen","orange"))
dev.off()

# Now, a variable that is very right-skewed:

set.seed(7222009)
XA<-rchisq(10001,2)
am<-mean(XA)
gm<-geometric.mean(XA)
hm<-harmonic.mean(XA)
me<-median(XA)
mo<-mlv(XA,"meanshift",bw=0.5)

pdf("CTASymmetrical.pdf",7,5)
par(mar=c(4,4,1,1))
hist(XA,col="grey",breaks=30,main="",xlab="X")
abline(v=am, col="red",lty=2,lwd=lw)
abline(v=gm,col="black",lty=3,lwd=lw)
abline(v=hm,col="blue",lty=4,lwd=lw)
abline(v=me,col="darkgreen",lty=5,lwd=lw)
abline(v=mo,col="orange",lty=6,lwd=lw)
legend("topright",bty="n",legend=c("Arithmetic Mean",
                                   "Geometric Mean","Harmonic Mean","Median","Mode"),
       lwd=lw,lty=c(2,3,4,5,6),
       col=c("red","black","blue","darkgreen","orange"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Back to Premier League data...
#
# Histogram with mean, median, and mode lines:

mean <- with(PL, mean(Points)) # arithmetic mean
gmean <- geometric.mean(PL$Points) # geometric mean
hmean <- harmonic.mean(PL$Points) # harmonic mean
median <- median(PL$Points) # median
mode <- mfv(PL$Points) # mode

lw<-2.5 # line width
pdf("PL-2024-Histogram2.pdf",8,6)
par(mar=c(4,4,2,2))
with(PL, hist(Points,col="grey",main="",xlab="Points Total",
              ylim=c(0,5)))
abline(v=mean, col="red",lty=2,lwd=lw)
abline(v=gmean,col="black",lty=3,lwd=lw)
abline(v=hmean,col="blue",lty=4,lwd=lw)
abline(v=median,col="darkgreen",lty=5,lwd=lw)
abline(v=mode,col="orange",lty=6,lwd=lw)
legend("topright",bty="n",legend=c("Arithmetic Mean (6.8)",
       "Geometric Mean (5.2)","Harmonic Mean (3.5)","Median (7)","Mode (1 & 3)"),
       lwd=lw,lty=c(2,3,4,5,6),
       col=c("red","black","blue","darkgreen","orange"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Variation                              ####
#
# Variance and SD:

summary(PL$Points)
var(PL$Points)
sd(PL$Points)

# Standardizing variables:

PLSmall<-PL[,4:10]
describe(PLSmall,trim=0,skew=FALSE)
PL.Z<-scale(PLSmall)
describe(PL.Z,trim=0,skew=FALSE)


# Plots illustrating skewness:

set.seed(7222009)
p<-10
q<-1.5
m<-(p+q)/2
ndraws<-500
left<-rbeta(ndraws,p,q)
noskew<-rbeta(ndraws,m,m)
right<-rbeta(ndraws,q,p)
lskew<-round(skewness(left),3) # skewness...
sskew<-round(skewness(noskew),3)
rskew<-round(skewness(right),3)

pdf("EmpiricalMeanMedianModeR.pdf",7,5)
par(mfrow=c(3,1))
par(mar=c(4,4,2,2))
# right-skewed:
hist(right,main="Right-Skewed",prob=TRUE,ylim=c(0,5),
     xlab="X",ylab="Density",col="grey",xlim=c(0,1))
lines(density(right),lwd=3)
legend("topright",legend=paste0("Skewness = ",rskew),bty="n")
# symmetrical:
par(mar=c(4,2,2,2))
hist(noskew,main="Symmetrical",prob=TRUE,ylim=c(0,3),
     xlab="X",col="grey",xlim=c(0,1))
lines(density(noskew),lwd=3)
legend("topright",legend=paste0("Skewness = ",sskew),bty="n")
# left-skewed:
par(mar=c(4,2,2,2))
hist(left,main="Left-Skewed",prob=TRUE,ylim=c(0,5.5),
     xlab="X",col="grey",xlim=c(0,1))
lines(density(left),lwd=3)
legend("topleft",legend=paste0("Skewness = ",lskew),bty="n")
dev.off()

# Plots illustrating kurtosis:

z<-seq(-5,5,by=0.01)
lw<-3
pdf("KurtosisR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(z,dcauchy(z,scale=0.6),t="l",lwd=lw,col="red",
     ylab="Probability",xlab="X")
lines(z,dnorm(z),lwd=lw,col="black",lty=2)
lines(z,dunif(z,min=-2,max=2),lwd=lw,col="darkgreen",lty=3)
legend("topleft",bty="n",lty=c(1,2,3),lwd=lw,
       legend=c("Leptokurtic","Mesokurtic","Platykurtic"),
       col=c("red","black","darkgreen"))
dev.off()

# Empirical / simulation-based examples of different
# degrees of kurtosis:

ndraws<-200
set.seed(7222009)
lepto<-rcauchy(ndraws,0,1.3)
meso<-rnorm(ndraws,0,1)
platy<-runif(ndraws,-1,1)
lkurt<-round(kurtosis(lepto),2)-3
mkurt<-round(kurtosis(meso),2)-3
pkurt<-round(kurtosis(platy),2)-3

bins<-30
pdf("EmpiricalKurtosis.pdf",8,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
hist(lepto,prob=TRUE,main="Leptokurtic",xlab="X",breaks=bins,
     ylim=c(0,0.32),xlim=c(-20,20))
lines(density(lepto),lwd=2,xlim=c(0,1))
legend("topleft",legend=paste0("Kurtosis = ",lkurt),bty="n")
hist(meso,prob=TRUE,main="Mesokurtic",xlab="X",breaks=bins,
     ylim=c(0,0.6))
lines(density(meso),lwd=2,xlim=c(0,1))
legend("topleft",legend=paste0("Kurtosis = ",mkurt),bty="n")
hist(platy,prob=TRUE,main="Platykurtic",xlab="X",breaks=bins)
lines(density(platy),lwd=2,xlim=c(0,1))
legend("topright",legend=paste0("Kurtosis = ",pkurt),bty="n")
dev.off()

# Skewness and kurtiosis of PL points variable:

skewness(PL$Points)
kurtosis(PL$Points)-3

# Binary variables...
#
# Illustrate skewness and kurtosis:

pi<-seq(0.01,0.99,by=0.01)
Bskew<-(1 - (2*pi))/(sqrt(pi * (1-pi)))
Bkurt<-(1 - (6*pi*(1-pi)))/(pi*(1-pi))

pdf("BinaryS&K.pdf",12,5)
par(mfrow=c(1,2))
par(mar=c(4,4,4,2))
plot(pi,Bskew,t="l",lwd=2,xlab=expression(pi),
     ylab="Skewness",main="Skewness")
abline(h=0,lty=2)
par(mar=c(4,4,4,2))
plot(pi,Bkurt,t="l",lwd=2,xlab=expression(pi),
     ylab="Kurtosis",main="Kurtosis")
abline(h=0,lty=2)
dev.off()

# Getting summary statistics:

summary(PLSmall)
describe(PLSmall)

# Nice-but-easy (LaTeX) table:

stargazer(PLSmall,title="Summary Statistics")

# /fin