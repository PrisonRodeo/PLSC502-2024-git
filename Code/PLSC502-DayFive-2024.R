#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries                                        ####
#
# PLSC 502 -- Fall 2024
#
# Day Five materials...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# There isn't much here in the "preliminaries" part... we
# only need a couple packages, and we won't even use them
# until next week. (Today is basically a lot of math.)
# 
# options(repos = c(
#   raphaels1 = 'https://raphaels1.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))
# install.packages('distr6')
# library(distr6)
#
# Also: you *should* set your working directory here:
#
# setwd("~/Dropbox/PLSC 502") # <-- or whatever...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Probability...                                       ####
#
# Odds plots:

Pr<-seq(0.01,0.99,by=0.01)
Odds<-Pr / (1-Pr)

# Some things for the plots:

spots<-c(20,50,80,90)
lc<-c("green","black","red","blue")

# Plot: Probability and Odds:

pdf("ProbabilityAndOddsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Odds,Pr,t="l",lwd=2.5,xlim=c(0,20),ylim=c(0,1),
     ylab="Probability")
for (i in 1:length(spots)) {
  segments(Odds[spots[i]],0,Odds[spots[i]],Pr[spots[i]],lty=2,lwd=2,
           col=lc[i])
  segments(Odds[spots[i]],Pr[spots[i]],-1,Pr[spots[i]],lty=2,lwd=2,
           col=lc[i])
}
legend("bottomright",bty="n",lwd=2,lty=2,col=lc,cex=0.75,
       legend=c("Pr = 0.2 (Odds = 1:4)",
                "Pr = 0.5 (Odds = 1:1)",
                "Pr = 0.8 (Odds = 4:1)",
                "Pr = 0.9 (Odds = 9:1)"))
lines(Odds,Pr,lwd=2,col="black")
dev.off()

# Log-Odds:

LogOdds<-log(Odds)

pdf("ProbAndLogOddsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(LogOdds,Pr,t="l",lwd=2.5,ylim=c(0,1),xlab="Log-Odds",
     ylab="Probability",xlim=c(-4.5,6))
for (i in 1:length(spots)) {
  segments(LogOdds[spots[i]],0,LogOdds[spots[i]],Pr[spots[i]],
           lty=2,lwd=2,col=lc[i])
  segments(LogOdds[spots[i]],Pr[spots[i]],-5,Pr[spots[i]],
           lty=2,lwd=2,col=lc[i])
}
legend("bottomright",bty="n",lwd=c(2,NA,2,NA,2,NA,2,NA),lty=2,
       col=rep(lc,each=2),cex=0.75,
       legend=c("Pr = 0.2",
                "(Log-Odds = -1.39)",
                "Pr = 0.5",
                "(Log-Odds = 0)",
                "Pr = 0.8",
                "(Log-Odds = 1.39)",
                "Pr = 0.9",
                "(Log-Odds = 2.20)"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Random variables...                                  ####
#
# Plot for the little hypothetical PDF:

x <- seq(-1,1,by=0.01)
fx <- 0.5*(x+1)
xcoord<-c(-1,1)

pdf("ASimplePDF-R.pdf",6,5)
plot(x,fx,t="l",lwd=3,xlab="X",ylab="f(X)",xlim=c(-1.2,1.2))
polygon(c(-1,1,1,-1),c(0,1,0,0),col="grey",border=NA)
segments(-1.4,0,-1,0,lwd=3)
segments(1,0,1,4,0,lwd=3)
segments(1,0,1,1,lwd=3,lty=2)
segments(1,0,1.4,0,lwd=3)
dev.off()

# Another one:

pdf("ASimplePDF-2-R.pdf",6,5)
plot(x,fx,t="l",lwd=3,xlab="X",ylab="f(X)",xlim=c(-1.2,1.2))
polygon(c(-1,1,1,-1),c(0,1,0,0),col="grey",border=NA)
segments(-1.4,0,-1,0,lwd=3)
segments(1,0,1,4,0,lwd=3)
segments(1,0,1,1,lwd=3,lty=2)
segments(1,0,1.4,0,lwd=3)
segments(1/3,0,1/3,1,lwd=3,lty=3,col="darkgreen")
legend("topleft",bty="n",lwd=3,lty=3,col="darkgreen",
       legend=c("E(X)")) 
dev.off()

# CDF (calculated):

FX <- 0.5* (0.5 * (x)^2 + (x)) + 0.25

# CDF (numerically):

integrand <- function(x) {(x+1)/2}
altFX <- numeric(201)
for(i in 1:201) {
  altFX[i] <- integrate(integrand,-1,x[i])$value
}

# Fix for plotting:

FX<-c(0,FX)
FX<-c(FX,1)
x<-c(-1.2,x)
x<-c(x,1.2)

# Plot the CDF:

pdf("ExampleCDF-R.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,FX,t="l",lwd=3,xlab="F(x) [integral of f(x)]",
     ylab="Cumulative Probability")
dev.off()

# fin

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
