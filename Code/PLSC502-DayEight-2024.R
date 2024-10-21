#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro materials                                  ####
#
# PLSC 502 -- Fall 2024
#
# Day Eight materials: Estimation
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                               ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","psych","MASS","VGAM","fitdistrplus",
     "AICcmodavg")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 3-4 times until you get "Package count = 3"
#
# Then, run this:

remotes::install_github("htmltab/htmltab")

#... and type "1" at the prompt. Once that's done,
# run this:

library(htmltab)

# Also: setwd() here someplace...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Efficiency plot example                      ####

x <- seq(-4,4,by=0.01)
fat<-dnorm(x,0,2)
thin<-dnorm(x,0,1)

pdf("EfficiencyPicR.pdf",6,5)
par(mar=c(2,2,2,2))
plot(x,thin,t="l",lwd=2,lty=1,xaxt="n",yaxt="n",xlab="",
     ylab="")
lines(x,fat,lwd=2,lty=2)
arrows(x[450],thin[450],x[450]+2,thin[450],lwd=2,length=0.1,code=1)
text(x[450]+2.3,thin[450],labels=expression(paste(hat(theta)[1])))
arrows(x[600],fat[600],x[600]+1,fat[600]+0.1,lwd=2,length=0.1,code=1)
text(x[600]+1.3,fat[600]+0.1,labels=expression(paste(hat(theta)[2])))
axis(1,at=0,labels=expression(paste(theta)))
abline(v=0,lty=3,lwd=1)
legend(-5,0.42,bty="n",col="white",lty=c(1),cex=0.8,
       legend=c(expression(paste(hat(theta)[1])~" is more efficient than "~
       paste(hat(theta)[2]))))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MSE example plot...                          ####

MSE.X<-seq(-3,3,by=0.01)
MSE1<-dnorm(MSE.X,0.3,0.5)
MSE2<-dnorm(MSE.X,0,2)

pdf("MSE-Illustrated.pdf",6,5)
par(mar=c(5,4,2,2))
plot(MSE.X,MSE1,t="l",lwd=2,col="navy",xaxt="n",
     ylab="Density",xlab="Parameter Estimate")
lines(MSE.X,MSE2,lwd=2,col="orange",lty=2)
abline(v=0.3,lty=3,lwd=1.5,col="navy")
abline(v=0,lty=3,lwd=1.5,col="orange")
axis(1,at=0,labels="True Parameter\nValue",cex.axis=0.8)
legend("topleft",bty="n",lwd=2,lty=c(2,1),
       col=c("orange","navy"),
       legend=c("Bias = 0, Large MSE",
                "Bias > 0, Small MSE"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MSE / "6" plot...                            ####

x<-seq(4,8,by=0.01)
MSEsix <- 36-12*x+x^2
MSEMean20 <- 10/20
MSEMean100 <- 10/100
MSEMean1000 <- 10/1000

pdf("VariousMSEsR-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,MSEsix,t="l",lwd=2,
     xlab=expression(True~mu),
     ylab="MSE")
abline(h=MSEMean20,col="red",lty=2,lwd=2)
abline(h=MSEMean100,col="darkgreen",lty=3,lwd=2)
abline(h=MSEMean1000,col="blue",lty=4,lwd=2)
legend(6,4,bty="n",lty=c(1,2,3,4),lwd=2,
       col=c("black","red","darkgreen","blue"),
       legend=c(expression(paste(zeta,"=6")),"Mean (N=20)",
                "Mean (N=100)","Mean (N=1000)"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Consistency...                               ####

x <- seq(-4,4,by=0.01)
fat<-dnorm(x,-1.5,2)
med<-dnorm(x,-0.75,1.3)
thin<-dnorm(x,0,1)

pdf("ConsistencyPicR.pdf",6,5)
par(mar=c(2,2,2,2))
plot(x,thin,t="l",lwd=2,lty=1,xaxt="n",yaxt="n",xlab="",
     ylab="")
lines(x,med,lwd=2,lty=2)
lines(x,fat,lwd=2,lty=3)
arrows(x[450],thin[450],x[450]+2,thin[450],lwd=2,length=0.1,code=1)
text(x[450]+2.8,thin[450],labels="N=1000")
arrows(x[250],med[250],x[250]-0.5,med[250]+0.1,lwd=2,length=0.1,code=1)
text(x[250]-0.5,med[250]+0.12,labels="N=100")
arrows(x[150],fat[150],x[150]-0.5,fat[150]+0.1,lwd=2,length=0.1,code=1)
text(x[150]-0.5,fat[150]+0.12,labels="N=10")
axis(1,at=0,labels=expression(paste(theta)))
abline(v=0,lty=3,lwd=1)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Estimation example 1: simulation             ####
#
# The \lambdas:

L<-c(0.2,1,8)

# The Ns:

N<-c(5,50,500)

# We'll do 4000 simulations of each combination of 
# values for \lambda and N. So:

sims<-4000  # We'll simulate each combination of N and
            # \lambda 4000 times...

# Now build a data frame to keep our simulation results
# in. We'll need 3x3=9 columns and 4000 rows for that:

Out<-data.frame(matrix(nrow=sims,ncol=length(N)*length(L)))
colnames(Out) <- c("M0.2.5","M1.5","M8.5","M0.2.50","M1.50",
                   "M8.50","M0.2.500","M1.500","M8.500")

# Now, set a seed and write some loops:

c <- 0            # column indicator for "Out"
set.seed(7222009) # Seediness

for(i in 1:length(N)) {    # Looping over sample sizes...
  for(j in 1:length(L)) {  # Looping over lambdas
    c <- c+1               # increment column indicator
    for(k in 1:sims) {     # Looping over 4000 simulations each
      df<-rpois(N[i],L[j]) # Draw N values from Poisson(lambda)
      Out[k,c]<-mean(df)   # Store the mean of the N draws
      rm(df)
    } 
  }
}

# Now let's visualize those...
#
# Get empirical means:

EMs<-apply(Out,2,FUN=mean)

# Get the empirical variances:

EVs<-apply(Out,2,FUN=sd)^2

# Calculate the theoretical variances:

Opts<-expand.grid(L,N)
TVs<-Opts$Var1 / Opts$Var2

pdf("PoissonMeanSims.pdf",10,7)
par(mfrow=c(3,3))
c<-0
for(i in 1:length(N)){
  for(j in 1:length(L)) {
    c<-c+1
    Label<-paste0("Lambda=",L[j],", N=",N[i])
    plot(density(Out[,c]),lwd=2,xlim=c(0,15),
         main=Label,xlab="Means of X")
    abline(v=L[j],lwd=1,lty=2,col="red")
    legend("topright",bty="n",
           legend=c(paste0("Empirical Mean = ",round(EMs[c],4)),
                    paste0("Theoretical Variance = ",TVs[c]),
                    paste0("Empirical Variance = ",round(EVs[c],5))))
  }
}
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Estimation example 2: "real" data            ####

# Get current Peremier League data:

url<-"https://www.howzat.com/blog/fantasy-football/english-premier-league-table-fixtures-and-teams-2024-25"
# url<-"https://www.skysports.com/premier-league-table" # (old)
PL<-htmltab(doc = url,which=1)
PL[,c(3:10)]<-apply(PL[,c(3:10)],2,
                    function(x) as.numeric(x)) # make data numeric

# Rename some variables:

colnames(PL)<-c("Rank","Team","GamesPlayed","Won","Drew","Lost",
                "GoalsFor","GoalsAgainst","GoalDifference","Points")

# Zap URL:

rm(url)

# Print:

PL

# Describe the data:

psych::describe(PL)

# Now, let's fit a Poisson distribution to these data.
# Recall that the Poisson has one parameter, often
# called \lambda, that determines the shape and 
# location of the distribution.
#
# We can use -fitdistr- in the MASS package to fit parametric
# distributions to data. That looks like this:

PoisMean <- fitdistr(PL$Drew,"poisson")
PoisMean

# Components:

coef(PoisMean)
vcov(PoisMean)

# and note that the latter is the same as:

coef(PoisMean) / nrow(PL)

# and:

(PoisMean$sd)^2

# Finally, here's a histogram of the frequency of 
# draws, along with an overlay of a Poisson density 
# with \lambda equal to the estimated empirical mean 
# of the data:

pdf("PL-Draws-Barplot24.pdf",6,5)
par(mar=c(4,4,2,2))
p<-with(PL, barplot(table(factor(Drew,levels=0:5)),col="grey",main="",
                    xlim=c(0,7),ylim=c(0,10),xlab="Number of Draws",
                    ylab="Frequency"))
par(new=TRUE)
plot(p,dpois(seq(0,5),lambda=coef(PoisMean))*20,xlim=c(0,7),ylim=c(0,10),
     pch=19,col="red",xlab="",ylab="",xaxt="n",yaxt="n")
segments(p,0,p,dpois(seq(0,5),lambda=mean(PL$Drew))*20,
         lwd=2,lty=2,col="red")
legend("topright",bty="n",col=c("grey70","red"),
       pch=c(15,19),legend=c("Empirical Distribution",
                             "Theoretical Quantities"))
dev.off()

# The more correct distribution for this is the binomial,
# so let's try that one instead:

BinMean<-fitdist(PL$Drew,"binom",fix.arg=list(size=7))
BinMean

# Now once again compare the empirical number of 
# draws to the theoretical expectation:

pdf("PL-Draws-Barplot24-2.pdf",6,5)
par(mar=c(4,4,2,2))
p<-with(PL, barplot(table(factor(Drew,levels=0:5)),col="grey",main="",
                    xlim=c(0,7),ylim=c(0,10),xlab="Number of Draws",
                    ylab="Frequency"))
par(new=TRUE)
plot(p,dbinom(seq(0,5),size=7,prob=BinMean$estimate)*20,
     xlim=c(0,7),ylim=c(0,10),pch=19,col="red",xlab="",
     ylab="",xaxt="n",yaxt="n")
segments(p,0,p,dbinom(seq(0,5),size=7,prob=BinMean$estimate)*20,
         lwd=2,lty=2,col="red")
legend("topright",bty="n",col=c("grey70","red"),
       pch=c(15,19),legend=c("Empirical Distribution",
                             "Theoretical Quantities"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Model "fit": AIC / BIC                          ####
#
# AIC simulation...
#
# Generate N=200 observations of data that we 
# *know* follows a normal distribution with a 
# mean of zero and a variance of one:

set.seed(7222009)
X<-rnorm(200,0,1)

# Fit four different statistical distributions
# to these data:
#
# 1. A normal distribution
# 2. A t distribution
# 3. A LaPlace distribution
#
# Define the LaPlace density:

dlaplace <- function(x, mu=0, b=1){
  if(b<=0) return(NA)
  exp(-abs(x-mu)/b) / (2*b)
}

# Fit using -fitdist-:

NormFit<-fitdist(X,"norm")             # Normal
TFit<-fitdist(X,"t",start=list(df=3))  # t
LaPlaceFit<-fitdist(X,dlaplace,        # LaPlace
                    start=list(b=1,mu=0))

# Automagically make a table of the results:

Dists<-list("Normal"=NormFit,"t"=TFit,"LaPlace"=LaPlaceFit)
aictab(Dists)

# Back to the Premier League...
#
# Which real-data distribution is better? We can 
# compare using AIC / BIC...
#
# Re-fit the Poisson using -fitdist-:

BinMean<-fitdist(PL$Drew,"binom",fix.arg=list(size=7))
PoisMean<-fitdist(PL$Drew,"pois")
print(c(round(BinMean$bic,3),round(PoisMean$bic,3)))  # BICs

# Compare AIC + BIC:

Draws<-list("Poisson"=PoisMean,"Binomial"=BinMean)
aictab(Draws)

# \fin