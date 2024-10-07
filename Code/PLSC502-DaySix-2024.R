#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro materials...                                       ####
#
# PLSC 502 -- Fall 2024
#
# Day Six materials: Probability distributions!
#
# Most of this code just draws the pictures that are in the
# class slides, but there's a bit more than that at the end...
#
# This would be a good time to set your working directory,
# using setwd(). Alternatively, you can be one of those weirdos
# who uses projects and the -here- package and whatnot.
# Your choice. 
#
# Also, only ONE package is needed for today:

install.packages("Dowd")
library(Dowd)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Generic PDF / CDF / Q() example:                         ####
#
# We'll use a logistic, because it's pretty:

X<-seq(-5,5,by=0.01)
p<-dlogis(X)
c<-plogis(X)
q<-qlogis(c)

pdf("PDFCDFQ.pdf",11,4)
par(mfrow=c(1,3))
plot(X,p,t="l",xlab="x",ylab="Pr(X=x)",main="PDF",lwd=2)
plot(X,c,t="l",xlab="x",ylab=expression("Pr(X"<="x)"),main="CDF",lwd=2)
plot(c,q,t="l",xlab=expression("Pr(X"<="x)"),ylab="x",main="Q()",lwd=2)
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Discrete distributions                                   ####
#
# Bernoulli PDF (pi = 0.6):

Bern <- data.frame(X = c(0,1),
                   P = c(0.4,0.6))
pdf("BernoulliPDF-R.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bern, plot(X,P,pch=20,xlim=c(-0.5,1.5),ylim=c(0,1),
     cex=3,xlab="Values of X",ylab="Pr(X=x)"))
segments(0,0,0,0.4,lty=2,lwd=4)
segments(1,0,1,0.6,lty=2,lwd=4)
legend("topleft",bty="n",
       expression("Bernoulli with"~pi~"=0.6"))
dev.off()

# Bernoulli skewness:

pi<-seq(0.01,0.99,by=0.01)
bskew<-((1-pi)-pi) / (sqrt((1-pi)*pi))

pdf("BernoulliSkew.pdf",6,5)
par(mar=c(4,4,4,2))
plot(pi,bskew,t="l",lwd=2,xlab=expression(pi),ylab="Skewness",
     main="Skewness of the Bernoulli")
abline(h=0,lty=2)
dev.off()


# Binomial:

binomdata<-data.frame(count=seq(0,12),
                      B404 = dbinom(seq(0,12),4,0.4),
                      B1004 = dbinom(seq(0,12),10,0.4),
                      B408 = dbinom(seq(0,12),4,0.8),
                      B1008 = dbinom(seq(0,12),10,0.8))

pdf("BinomialsR.pdf",9,9)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
# n=4, p=0.4
with(binomdata, plot(count,B404,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 4, pi = 0.4"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B404[i],
                           lty=2,lwd=4))
}
# n=10, p=0.4
with(binomdata, plot(count,B1004,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 10, pi = 0.4"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B1004[i],
                           lty=2,lwd=4))
}
# n=4, p=0.8
with(binomdata, plot(count,B408,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 4, pi = 0.8"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B408[i],
                           lty=2,lwd=4))
}
# n=10, p=0.8
with(binomdata, plot(count,B1008,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 10, pi = 0.8"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B1008[i],
                           lty=2,lwd=4))
}
dev.off()

# Geometrics:

geomdata<-data.frame(count=seq(0,12),
                    G02 = dgeom(seq(0,12),0.2),
                    G05 = dgeom(seq(0,12),0.5),
                    G08 = dgeom(seq(0,12),0.8))

pdf("GeometricsR.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
# p=0.2
with(geomdata, plot(count,G02,pch=20,cex=3,xlab="Trials",
                     ylim=c(0,1),main="pi = 0.2"))
for (i in 1:13) {
  with(geomdata, segments(count[i],0,count[i],G02[i],
                           lty=2,lwd=3))
}
# p=0.5
with(geomdata, plot(count,G05,pch=20,cex=3,xlab="Trials",
                    ylim=c(0,1),main="pi = 0.5"))
for (i in 1:13) {
  with(geomdata, segments(count[i],0,count[i],G05[i],
                          lty=2,lwd=3))
}
# p=0.8
with(geomdata, plot(count,G08,pch=20,cex=3,xlab="Trials",
                    ylim=c(0,1),main="pi = 0.8"))
for (i in 1:13) {
  with(geomdata, segments(count[i],0,count[i],G08[i],
                          lty=2,lwd=3))
}
dev.off()

# Various Poisson densities:

x<-seq(0,20)
P05<-dpois(x,0.5) # lambda = 0.5
P1 <-dpois(x,2) # lambda = 1
P4 <-dpois(x,5) # lambda = 4
P8<-dpois(x,10) # lambda = 8

pdf("PoissonDensities.pdf",9,7)
par(mfrow=c(2,2))
plot(x,P05,pch=19,xlab="X",main="Lambda = 0.5",
     ylab="Probability",ylim=c(0,0.65))
segments(x,0,x,P05,lty=2)
plot(x,P1,pch=19,xlab="X",main="Lambda = 1",
     ylab="Probability",ylim=c(0,0.65))
segments(x,0,x,P1,lty=2)
plot(x,P4,pch=19,xlab="X",main="Lambda = 4",
     ylab="Probability",ylim=c(0,0.65))
segments(x,0,x,P4,lty=2)
plot(x,P8,pch=19,xlab="X",main="Lambda = 8",
     ylab="Probability",ylim=c(0,0.65))
segments(x,0,x,P8,lty=2)
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Continuous Distributions...                              ####
#
# Uniform distributions:

x <- seq(-3,3,by=0.01)
U01 <- dunif(x)
U115 <- dunif(x,-1,1.5)
U252 <- dunif(x,-2.5,2)

pdf("UniformsR.pdf",6,5)
par(mar=c(4,4,4,2))
plot(x,U01,t="l",lwd=2,col="black",xlim=c(-3,3),
     xlab="x value",ylab="Density",
     main="Probability Density Functions")
lines(x,U115,lwd=2,lty=2, col="red")
lines(x,U252,lwd=2,lty=3, col="darkgreen")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","red","darkgreen"),
       legend=c("U(0,1)","U(-1,1.5)","U(-2.5,2)"))
dev.off()

# Uniform CDFs:

PU01 <- punif(x)
PU115 <- punif(x,-1,1.5)
PU252 <- punif(x,-2.5,2)
pdf("UniformFsR.pdf",6,5)
par(mar=c(4,4,4,2))
plot(x,PU01,t="l",lwd=2,col="black",xlim=c(-3,3),
     xlab="x value",ylab="Cumulative Probability",
     main="Cumulative Distribution Functions")
lines(x,PU115,lwd=2,lty=2, col="red")
lines(x,PU252,lwd=2,lty=3, col="darkgreen")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","red","darkgreen"),
       legend=c("U(0,1)","U(-1,1.5)","U(-2.5,2)"))
dev.off()

# Normals:

y <- seq(-10,10,by=0.1)
N01 <- dnorm(y)
N21 <- dnorm(y,2,1)
NN32 <- dnorm(y,-3,2)
N54 <- dnorm(y,5,4)
pdf("NormalsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(y,N01,t="l",lwd=2,col="black",xlim=c(-10,10),
     xlab="x value",ylab="Density")
lines(y,N21,lwd=2,lty=2, col="red")
lines(y,NN32,lwd=2,lty=3, col="darkgreen")
lines(y,N54,lwd=2,lty=4, col="orange")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3,4),
       col=c("black","red","darkgreen","orange"),
       legend=c("N(0,1)","N(2,1)","N(-3,2)","N(5,4)"))
dev.off()

# CDFs:
PN01 <- pnorm(y)
PN21 <- pnorm(y,2,1)
PNN32 <- pnorm(y,-3,2)
PN54 <- pnorm(y,5,4)
pdf("NormalFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(y,PN01,t="l",lwd=2,col="black",xlim=c(-10,10),
     xlab="x value",ylab="Cumulative Probability")
lines(y,PN21,lwd=2,lty=2, col="red")
lines(y,PNN32,lwd=2,lty=3, col="darkgreen")
lines(y,PN54,lwd=2,lty=4, col="orange")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3,4),
       col=c("black","red","darkgreen","orange"),
       legend=c("N(0,1)","N(2,1)","N(-3,2)","N(5,4)"))
dev.off()

# Lognormals:

z<-seq(0,20,by=0.1)
LN01 <- dlnorm(z)
LN11 <- dlnorm(z,1,1)
LN105 <- dlnorm(z,1,0.5)
LN125 <- dlnorm(z,1,0.25)
LN225 <- dlnorm(z,2,0.25)
pdf("LogNormalsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(z,LN01,t="l",lty=1,lwd=2,col="black",xlim=c(0,20),
     xlab="x value",ylab="Density")
lines(z,LN11,lwd=2,lty=2, col="red")
lines(z,LN105,lwd=2,lty=3, col="darkgreen")
lines(z,LN125,lwd=2,lty=4, col="orange")
lines(z,LN225,lwd=2,lty=5, col="navy")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("lnN(0,1)","lnN(1,1)","lnN(1,0.5)",
                "lnN(1,0.25)","lnN(2,0.25)"))
dev.off()

# Chi-square:

w<-seq(0,4,by=0.01)
CS1 <- dchisq(w,1)
CS2 <- dchisq(w,2)
CS3 <- dchisq(w,3)
CS5 <- dchisq(w,5)
CS10 <- dchisq(w,10)
pdf("ChiSquaresR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(w,CS1,t="l",lty=1,lwd=2,col="black",xlim=c(0,4),
     xlab="x value",ylab="Density")
lines(w,CS2,lwd=2,lty=2, col="red")
lines(w,CS3,lwd=2,lty=3, col="darkgreen")
lines(w,CS5,lwd=2,lty=4, col="orange")
lines(w,CS10,lwd=2,lty=5, col="navy")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("Chi-square(1)","Chi-square(2)",
                "Chi-square(3)","Chi-square(5)",
                "Chi-square(10)"))
dev.off()

# Student's t:

q<-seq(-4,4,by=0.01)
T1 <- dt(q,1)
T3 <- dt(q,3)
T8 <- dt(q,8)
T30 <- dt(q,30)
TN <- dnorm(q)
pdf("T-R.pdf",6,5)
par(mar=c(4,4,2,2))
plot(q,TN,t="l",lty=2,lwd=2,col="black",xlim=c(-4,4),
     xlab="x value",ylab="Density")
lines(q,T1,lwd=2,lty=1, col="red")
lines(q,T3,lwd=2,lty=1, col="darkgreen")
lines(q,T8,lwd=2,lty=1, col="orange")
lines(q,T30,lwd=2,lty=1, col="navy")
legend("topright",bty="n",lwd=2,lty=c(2,1,1,1,1),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("Standard Normal","t(1)","t(3)","t(8)","t(30)"))
dev.off()

# F distributions:

F11 <- df(w,1,1)
F21 <- df(w,2,1)
F51 <- df(w,5,1)
F1010 <- df(w,10,10)
F100100 <- df(w,100,100)
pdf("FsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(w,F11,t="l",lty=1,lwd=2,col="black",xlim=c(0,4),
     ylim=c(0,2),xlab="x value",ylab="Density")
lines(w,F21,lwd=2,lty=2, col="red")
lines(w,F51,lwd=2,lty=3, col="darkgreen")
lines(w,F1010,lwd=2,lty=4, col="orange")
lines(w,F100100,lwd=2,lty=5, col="navy")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("F(1,1)","F(2,1)","F(5,1)","F(10,10)",
                "F(100,100)"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some practical / R Things...                           ####
#
# Plot some distributions...
#
# X~N(-4,12):

x<-seq(-20,12,by=0.1)
PlotNorm<-dnorm(x,-4,sqrt(12))

pdf("PlotNorm.pdf",6,5)
par(mar=c(4,4,2,2)) # plot margins...
plot(x,PlotNorm,t="l",lwd=2,xlab="x",ylab="Probability")
abline(v=-4,lty=2)
dev.off()

# Poisson with lambda = 4:

x<-seq(0,12)
PlotPois<-dpois(x,4)

pdf("PlotPoisson.pdf",6,5)
par(mar=c(4,4,2,2)) # plot margins...
plot(x,PlotPois,pch=19,lwd=2,xlab="x",ylab="Probability",
     xaxp=c(0,12,12))
segments(x,0,x,PlotPois,lwd=1)
dev.off()

# CDF of F(5,12):

x<-seq(0,5,by=0.01)
PlotF<-pf(x,5,12)

pdf("PlotF512.pdf",6,5)
par(mar=c(4,4,2,2)) # plot margins...
plot(x,PlotF,t="l",lwd=2,xlab="x",ylab="Cumulative Probability")
dev.off()

# CDF of binomial with pi = 0.3 and n=9:

x<-seq(0,9)
PlotBinom9<-pbinom(x,9,0.3)

pdf("PlotBinomialCDF.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,PlotBinom9,xlab="X",ylab="Cumulative Probability",
       ylim=c(0,1),xaxp=c(0,9,9),pch=19)
segments(x,0,x,PlotBinom9,lty=2)
dev.off()

# Quantiles of a chi-square with df=5:

P<-seq(0.01,0.99,by=0.01) # probabilities
ChiSq1<-qchisq(P,1) # df=1
ChiSq2<-qchisq(P,2) # df=2
ChiSq5<-qchisq(P,5) # df=5

pdf("PlotChiSqQ.pdf",6,5)
par(mar=c(4,4,2,2))
plot(P,ChiSq1,t="l",lwd=2,xlab="Probability",
       ylim=c(0,15),ylab="Quantile Value")
lines(P,ChiSq2,lwd=2,lty=2,col="orange")
lines(P,ChiSq5,lwd=2,lty=3,col="blue")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","orange","blue"),
       legend=c("DF=1","DF=2","DF=5"))
dev.off()


# First random draws example:

Xbinom5point2<-rbinom(10000,5,0.2)

# Plot of binomials:

pdf("BinomialDrawsPlotR.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(table(Xbinom5point2),xlab="Outcome",ylab="Frequency")
dev.off()

# 5000 draws from a t distribution with df = 3:

set.seed(7222009)
TDraws<-rt(5000,4)

# Plot those empirical draws and compare them to the 
# theoretical density:

x<-seq(-11,11,by=0.1)
pdf("TDrawsPlot.pdf",6,5)
hist(TDraws,breaks=101,xlim=c(-10,10),xlab="X",yaxt="n",ylab="",
     main="")
par(new=TRUE)
plot(x,dt(x,4),t="l",lwd=2,col="red",xlim=c(-10,10),xlab="",
     ylab="Probability")
dev.off()

# Can do the same using a Q-Q plot:

pdf("TQQPlot.pdf",7,5)
par(mar=c(4,6,4,6))
TQQPlot(TDraws,4)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Inverse transform sampling...             ####
#
# Let's use our friend the exponential. We will
#
# a) draw 20,000 draws from an exponential
# distribution with lambda = 0.7, using inverse
# transform sampling,
#
# b) draw another 20,000 draws from an exponential
# distribution using R's internal function rexp(),
#
# c) compare the two.

nreps<-20000
lambda<-0.7

set.seed(7222009)
U<-runif(nreps,0,1)
XITS<- (-((1)/(lambda))) * log(1-U) # Inv. trans. sampling
set.seed(7222009)
XR<-rexp(nreps,rate=0.7)            # R's internal function

# Plot the two densities:

pdf("TwoExponentials.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(XITS),xlim=c(0,10),lwd=2,xlab="X",
     main="",col="orange")
lines(density(XR),xlim=c(0,10),lwd=2,col="navy",lty=2)
legend("topright",bty="n",lwd=2,lty=c(1,2),
       col=c("orange","navy"),
       legend=c("Inv. Trans. Sampling","R's Internal Function"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Seeds, etc.                               ####

seed<-3229     # calling "seed" some thing
set.seed(seed) # setting the system seed
rt(3,1)        # three draws from a t distrib. w/1 d.f.

seed<-1077
set.seed(seed) # resetting the seed
rt(3,1)        # different values for the draws

seed<-3229     # original seed
set.seed(seed)
rt(3,1)        # identical values of the draws

# Seeds and simulations...
# 
# Good:

X<-matrix(NA,nrow=100,ncol=2)
set.seed(7222009)
for(i in 1:2){
  X[,i]<-rnorm(100)}

pdf("GoodSeed.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,pch=20,xlab="X1",ylab="X2")
dev.off()

# Not good:

X<-matrix(NA,nrow=100,ncol=2)
for(i in 1:2){
  set.seed(7222009)
  X[,i]<-rnorm(100)}

pdf("BadSeed.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,pch=20,xlab="X1",ylab="X2")
dev.off()

# /fin