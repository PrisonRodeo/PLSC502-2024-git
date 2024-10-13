#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things...                                          ####
#
# PLSC 502 -- Fall 2024
#
# Day Seven materials: Sampling and sampling distributions
#
# Let's have a reasonable number of digits displayed,
# shall we? 

options(scipen = 99) # bias against scientific notation
options(digits = 4)  # show fewer decimal places, for now

# Load a package or two, by running the following bit
# of code 3-4 times until all "smiley faces" :)
# come up:

P<-c("readr","Hmisc","psych","sampling")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Also, this is a good time to -setwd()-, if you care to.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some illustrative pictures...                      ####
#
# Make a nice figure for the margin of error for a binary 
# proportion, plotted against the sample size:

N<-seq(10,10000,by=10)
MOE90<- 0.82 / sqrt(N)
MOE95<- 0.98 / sqrt(N)
MOE99<- 1.29 / sqrt(N)

pdf("MOEbyNPlot.pdf",7,6)
par(mar=c(4,4,2,2))
plot(N,MOE99,t="l",log="x",lwd=2,xlab="Sample Size (log scale)",
     ylab="Relative Margin of Error",xaxp=c(10,10000,3))
lines(N,MOE95,lwd=2,lty=2,col="orange")
lines(N,MOE90,lwd=2,lty=3,col="green")
legend("topright",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","orange","green"),
       legend=c("99 Percent","95 Percent","90 Percent"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Finite population corrections...                         ####
#
# First, show the standard deviation of the mean for a "small"
# sample from a "large" population:

PopN<-100000         # Population is 100,000
N <- 50              # Sample size N=50
set.seed(7222009)
Pop<-rnorm(PopN,5,5) # Population with mu=5 and sigma=5

# Draw 1000 samples of size N=50 from the population, 
# calculate the mean of each, and show their characteristics:

Nsims<-1000
Means<-numeric(Nsims)
set.seed(7222009)
for(i in 1:Nsims){
  Means[i]<-mean(sample(Pop,N,replace=FALSE))
}

# The theoretical standard deviation of the sample means is
#
# s = 5 / sqrt(50)
#   = 5 / 7.07
#   = 0.707
#
# ...which is the same as the empirical standard deviation of
# the 1000 sample means:

psych::describe(Means)

# ...aaaand, plot:

pdf("SDofMeans.pdf",7,5)
par(mar=c(4,4,2,2))
hist(Means,col="grey",main="")
abline(v=mean(Means),lwd=3,lty=2,col="orange")
legend("topright",bty="n",lwd=3,lty=2,col="orange",
       legend="Mean of means")
dev.off()

# Plot of the difference between the uncorrected std. deviation 
# of the mean and the FPC-corrected s.d., as a function of
# n/N (that is, the proportion of the population that is in
# the without-replacement sample) for a sample with standard
# deviation s of 5, a population size of 1000, and varying sample
# sizes:

N <- 1000             # Population N=1000
n <- seq(1,999,by=1)  # Various sample sizes n
S <- 5 / (sqrt(n))    # No correction
S.FPC <- (5 / (sqrt(n))) * (sqrt(((N-n) / (N-1)))) # FPC
Ratio <- S / S.FPC

# Plot:

pdf("FPCPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot((n/N),S.FPC,t="l",lwd=2,xlab="Sample Proportion of Population",
      ylab="Standard Error of the Mean",ylim=c(0,5))
lines((n/N),S,lwd=2,lty=2,col="orange")
lines((n/N),Ratio,lwd=2,lty=3,col="darkgreen")
legend("topright",bty="n",lty=c(2,1,3),col=c("orange","black","darkgreen"),
       lwd=2,legend=c("Uncorrected Estimate",
                      "Finite-Population-Corrected Estimate",
                      "Ratio: Uncorrected / Corrected"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Sampling!                                                ####
#
# Read in the Warren & Burger Court data:

WB<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/WarrenBurger.csv")

psych::describe(WB)

# Histograms

pdf("FedPetHistogram.pdf",5,4)
par(mar=c(4,4,2,2))
with(WB, barplot(table(fedpet),
         names.arg=c("No Federal Petitioner","Federal Petitioner")))
dev.off()

pdf("SumAmHistogram.pdf",5,4)
par(mar=c(4,4,2,2))
with(WB, hist(sumam,col="grey",main="",
              xlab="Total Amici Filed"))
dev.off()

# Draw 1000 random samples of fedpet, each with N=10, 
# and calculate the mean for each:

set.seed(7222009)
MFP10<-numeric(1000)
for (i in 1:1000){
  MFP10[i]<- with(WB, mean(sample(fedpet,10,replace=F)))
}

# describe(MFP10)

pdf("FedPetMeansN-is-10R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(MFP10,breaks=7,xlab="Sample Means of FedPet",col="grey",
     main="",freq=FALSE)
abline(v = mean(WB$fedpet),lwd=3,lty=2)
legend("topright",bty="n",lwd=3,col="black",lty=2,
       legend="Population mean")
dev.off()

# Same with N = 20:

set.seed(7222009)
MFP20<-numeric(1000)
for (i in 1:1000){
  MFP20[i]<- with(WB, mean(sample(fedpet,20,replace=F)))
}

# describe(MFP20)

pdf("FedPetMeansN-is-20R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(MFP20,breaks=10,xlab="Sample Means of FedPet",col="grey",
     main="",freq=FALSE)
abline(v = mean(WB$fedpet),lwd=3,lty=2)
legend("topright",bty="n",lwd=3,col="black",lty=2,
       legend="Population mean")
dev.off()

# Same with N = 100:

set.seed(7222009)
MFP100<-numeric(1000)
for (i in 1:1000){
  MFP100[i]<- with(WB, mean(sample(fedpet,100,replace=F)))
}

# describe(MFP100)

pdf("FedPetMeansN-is-100R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(MFP100,breaks=20,xlab="Sample Means of FedPet",col="grey",
     main="",freq=FALSE)
abline(v = mean(WB$fedpet),lwd=3,lty=2)
lines(seq(0,0.5,by=0.001),dnorm(seq(0,0.5,by=0.001),
      mean=mean(WB$fedpet),sd=(sd(WB$fedpet)/sqrt(100))),
      lwd=2,col="red")
legend("topright",bty="n",lwd=c(3,2),col=c("black","red"),
       lty=c(2,1),
       legend=c("Population mean","Normal density"))
dev.off()

# Same with SUMAM (N = 100):

set.seed(7222009)
AM100<-numeric(1000)
for (i in 1:1000){
  AM100[i]<- with(WB, mean(sample(sumam,100,replace=F)))
}

# describe(AM100)

pdf("SumAmMeansN-is-100R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(AM100,breaks=20,xlab="Sample Means of SumAm",col="grey",
     main="",freq=FALSE,ylim=c(0,2))
abline(v = mean(WB$sumam),lwd=3,lty=2)
lines(seq(0,3,by=0.01),dnorm(seq(0,3,by=0.01),
      mean=mean(WB$sumam),sd=(sd(WB$sumam)/sqrt(100))),
      lwd=2,col="red")
legend("topright",bty="n",lwd=c(3,2),col=c("black","red"),
       lty=c(2,1),
       legend=c("Population mean","Normal density"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Next, variances...
#
# Remember that [(N-1)*s^2] / sigma^2 is chi-square with N-1 d.f.
#
# Draw 1000 random samples of fedpet, each with N=20, and 
# calculate the scaled variance for each:

s20<-numeric(1000)
for(i in 1:1000){ 
   s20[i]<- with(WB, 
          (19*var(sample(fedpet,20,replace=F)))/(var(fedpet)))
}

pdf("FedPetVarsN20R.pdf",5,4)
par(mar=c(4,4,4,2))
hist(s20,breaks=10,col="grey",freq=FALSE,ylim=c(0,0.08),main="N=20",
     xlab="Rescaled Sample Variances of FedPet")
lines(seq(0,40,length=401),dchisq(seq(0,40,length=401),19),
         lwd=2,col="red")
abline(v=19,lwd=3,lty=2,col="black")
legend("topleft",bty="n",lwd=2,col="red",
       legend="Chi-Square(19)")
dev.off()

# Same with larger samples (N = 500):

s500<-numeric(1000)
for(i in 1:1000){ 
  s500[i]<- with(WB, 
            (499*var(sample(fedpet,500,replace=F)))/(var(fedpet)))
}

pdf("FedPetVarsN500R.pdf",5,4)
par(mar=c(4,4,4,2))
hist(s500,breaks=20,col="grey",freq=FALSE,ylim=c(0,0.02),main="N=500",
     xlab="Rescaled Sample Variances of FedPet")
lines(seq(350,600,by=1),dchisq(seq(350,600,by=1),499),
      lwd=2,col="red")
abline(v=499,lwd=3,lty=2)
legend("topleft",bty="n",lwd=2,col="red",
       legend="Chi-Square(499)")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Stratified sampling...                                   ####

# Summary of the CONSTIT variable:

table(WB$constit)

# Draw a single stratified random sample, with 10 observations
# from constit=0 and 10 from constit=1

set.seed(7222009)
sample<-strata(WB,stratanames=c("constit"),
                        size=c(10,10),method="srswor")
sample.data<-getdata(WB,sample)
summary(sample.data)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Large samples, small populations (Meng 2018)             ####
#
# Let's compare our ability to estimate the mean value of SUMAM 
# with either a small simple random sample, or a much 
# larger sample that is slightly biased...
#
# More specifically, our larger sample will have N = 3580, which
# is 50 percent of the population. Our smaller sample will have 
# N = 179, which is 20 times smaller than our large one (and only 
# 2.5 percent of the population),
#
# Before we start, let's see things a bit more precisely:

options(digits = 8) # show more decimal places

# First, generate a sample that is very slightly biased. To do
# that, we will make the probability of being sampled vary 
# slightly positively with the number of amici in the case:

WB$BiasPr <- ((WB$sumam/max(WB$sumam))+1) / nrow(WB)

# Note that the ratio of the probabilities of the most probably-sampled
# observation to the least is 2 to 1:

max(WB$BiasPr) / min(WB$BiasPr)

# In other words, the probability of being sampled increases in the total
# number of amici briefs, such that the case with the highest number of 
# amicus briefs (sumam=39) has twice the chance of being sampled
# as one with zero such briefs.
#
# Now let's simulate 1000 samples of size n=3580, where the probability
# of being selected for a sample is that slightly biased one:

NSims<-1000                  # How many sims?
BigDataBias<-numeric(NSims)  # a place to put the estimated biases
set.seed(7222009)            # set your seed, my people

for(i in 1:1000){
  foo<-sample(WB$sumam,3580,replace=FALSE,prob=WB$BiasPr)
  BigDataBias[i]<-mean(foo) - mean(WB$sumam)
  rm(foo)
}

# Now do the same, but using a much smaller sample (N=179) that is 
# chosen via simple random sampling:

SmallSampleBias<-numeric(NSims) # a place to put the estimated means
set.seed(7222009)                # set your seed, again

for(i in 1:1000){
  foo<-sample(WB$sumam,179,replace=FALSE)
  SmallSampleBias[i]<-mean(foo) - mean(WB$sumam)
  rm(foo)
}

cx<-0.3  # Plotting: X-limits...

pdf("MengPlot1.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BigDataBias),xlim=c(-cx,cx),ylim=c(0,20),
     col="orange",main="",xlab="Bias")
lines(density(SmallSampleBias),col="green")
abline(v=mean(BigDataBias),col="orange",lty=2)
abline(v=mean(SmallSampleBias),col="green",lty=2)
legend("topleft",bty="n",lty=2,col=c("orange","green"),
       legend=c("Mean: Biased Samples (N=3580)",
                "Mean: Simple Random Samples (N=179)"),
       cex=0.7)
dev.off()

# Suppose we increase the sample size from 50 percent of the population to
# 90 percent (so that N = 6445); then what happens?

BDB2<-numeric(NSims)         # a place to put the estimated biases
set.seed(7222009)            # set your seed yet again

for(i in 1:1000){
  foo<-sample(WB$sumam,6445,replace=FALSE,prob=WB$BiasPr)
  BDB2[i]<-mean(foo) - mean(WB$sumam)
  rm(foo)
}

pdf("MengPlot2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BigDataBias),xlim=c(-cx,cx),ylim=c(0,20),
     col="orange",main="",xlab="Bias")
lines(density(SmallSampleBias),col="green")
lines(density(BDB2),col="black")
abline(v=mean(BigDataBias),col="orange",lty=2)
abline(v=mean(SmallSampleBias),col="green",lty=2)
abline(v=mean(BDB2),col="black",lty=2)
legend("topleft",bty="n",lty=2,col=c("black","orange","green"),
       legend=c("Mean: Biased Samples (N=6445)",
                "Mean: Biased Samples (N=3580)",
                "Mean: Simple Random Samples (N=179)"),
       cex=0.7)
dev.off()


# /fin