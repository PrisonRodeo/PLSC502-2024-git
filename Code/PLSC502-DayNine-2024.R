#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things...                                    ####
#
# PLSC 502 -- Fall 2024
#
# Day Nine materials: Statistical inference
#
# Load packages (there aren't many today...)

P<-c("readr","sampling","DescTools","psych")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Set some options:

options(scipen = 6) # bias against scientific notation
options(digits = 5) # show fewer decimal places

# This is also a good place to setwd(), dowhutchalike.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Confidence Intervals                               ####
#
# Picture of "pivotal" method:

z <- seq(-3,3,by=0.01)
set.seed(7222009)
zn <- dnorm(z)

pdf("CIIllustratedR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(z,zn,t="l",lwd=2,lty=2,xaxt="n",yaxt="n",ylab="",
     xlab=expression(paste("Possible Values of ",bar(X))))
abline(h=0)
segments(0,0,0,zn[301],lwd=3,col="black")
segments(z[90],0,z[90],zn[90],lwd=2,col="red")
segments(z[510],0,z[510],zn[510],lwd=2,col="red")
axis(1,at=c(z[90],0,z[510]),
     labels=c(expression(hat(theta)[L]),
              expression(hat(theta)),
              expression(hat(theta)[H])))
arrows(0,zn[90],z[90],zn[90],code=3,lwd=2,length=0.15,col="red")
arrows(0,zn[511],z[511],zn[511],code=3,lwd=2,length=0.15,col="red")
text(z[200],zn[115],cex=0.75,
     labels=expression(hat(theta) - z[alpha/2]*frac(sigma,sqrt(N))))
text(z[400],zn[115],cex=0.75,
     labels=expression(hat(theta) + z[alpha/2]*frac(sigma,sqrt(N))))
dev.off()

# Conventional CIs around proportions:

Pi<-seq(0.001,0.999,by=0.001)    # Population value
PiHat<-seq(0.001,0.999,by=0.001) # Estimate

# CIs "by hand":

ub10 <- Pi + 1.96*(sqrt((Pi*(1-Pi))/(10)))
lb10 <- Pi - 1.96*(sqrt((Pi*(1-Pi))/(10)))
ub100 <- Pi + 1.96*(sqrt((Pi*(1-Pi))/(100)))
lb100 <- Pi - 1.96*(sqrt((Pi*(1-Pi))/(100)))
ub400 <- Pi + 1.96*(sqrt((Pi*(1-Pi))/(400)))
lb400 <- Pi - 1.96*(sqrt((Pi*(1-Pi))/(400)))

# Plot:

pdf("ProportionCIsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Pi,PiHat,t="l",lwd=3,xlab=expression(pi),
     ylab=expression(hat(pi)),xlim=c(-0.1,1.1),
     ylim=c(-0.05,1.05))
abline(h=c(0,1),lwd=1,lty=2,col="red")
lines(Pi,ub10,lwd=2,lty=3,col="black")
lines(Pi,lb10,lwd=2,lty=3,col="black")
lines(Pi,ub100,lwd=2,lty=4,col="orange")
lines(Pi,lb100,lwd=2,lty=4,col="orange")
lines(Pi,ub400,lwd=2,lty=5,col="darkgreen")
lines(Pi,lb400,lwd=2,lty=5,col="darkgreen")
legend(-0.1,1,bty="n",lty=c(3,4,5),lwd=2,
       col=c("black","orange","darkgreen"),
       legend=c("N=10","N=100","N=400"))
dev.off()

# Ugh, that's pretty bad. Let's do that again, this time
# using the R package DescTools (the -BinomCI- command):

CI10<-data.frame(BinomCI(c(0:10),10,method="wald"))    # N=10
CI100<-data.frame(BinomCI(c(0:100),100,method="wald")) # N=100
CI400<-data.frame(BinomCI(c(0:400),400,method="wald")) # N=400

# New plot:

pdf("ProportionCIsR2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(CI10$est,CI10$est,t="l",lwd=3,xlab=expression(pi),
     ylab=expression(hat(pi)),xlim=c(-0.1,1.1),
     ylim=c(-0.05,1.05))
abline(h=c(0,1),lwd=1,lty=2,col="red")
lines(CI10$est,CI10$lwr.ci,lwd=2,lty=3,col="black")
lines(CI10$est,CI10$upr.ci,lwd=2,lty=3,col="black")
lines(CI100$est,CI100$lwr.ci,lwd=2,lty=4,col="orange")
lines(CI100$est,CI100$upr.ci,lwd=2,lty=4,col="orange")
lines(CI400$est,CI400$lwr.ci,lwd=2,lty=5,col="darkgreen")
lines(CI400$est,CI400$upr.ci,lwd=2,lty=5,col="darkgreen")
legend(-0.1,1,bty="n",lty=c(3,4,5),lwd=2,
       col=c("black","orange","darkgreen"),
       legend=c("N=10","N=100","N=400"))
dev.off()

# That's also pretty bad...
#
# One can go down a rabbit hole here, but the best option
# is probably the "Wilson" version -- which happens to be
# the default for -BinomCI-:

WCI10<-data.frame(BinomCI(c(0:10),10))    # N=10
WCI100<-data.frame(BinomCI(c(0:100),100)) # N=100
WCI400<-data.frame(BinomCI(c(0:400),400)) # N=400

pdf("ProportionCIsR3.pdf",6,5)
par(mar=c(4,4,2,2))
plot(WCI10$est,WCI10$est,t="l",lwd=3,xlab=expression(pi),
     ylab=expression(hat(pi)),xlim=c(-0.1,1.1),
     ylim=c(-0.05,1.05))
abline(h=c(0,1),lwd=1,lty=2,col="red")
lines(WCI10$est,WCI10$lwr.ci,lwd=2,lty=3,col="black")
lines(WCI10$est,WCI10$upr.ci,lwd=2,lty=3,col="black")
lines(WCI100$est,WCI100$lwr.ci,lwd=2,lty=4,col="orange")
lines(WCI100$est,WCI100$upr.ci,lwd=2,lty=4,col="orange")
lines(WCI400$est,WCI400$lwr.ci,lwd=2,lty=5,col="darkgreen")
lines(WCI400$est,WCI400$upr.ci,lwd=2,lty=5,col="darkgreen")
legend(-0.1,1,bty="n",lty=c(3,4,5),lwd=2,
       col=c("black","orange","darkgreen"),
       legend=c("N=10","N=100","N=400"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Court example...
#
# Get the Supreme Court Judicial Database, straight from
# the source:

temp <- tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2024_01/SCDB_2024_01_caseCentered_Docket.csv.zip",temp)
df <- read.csv(unz(temp, "SCDB_2024_01_caseCentered_Docket.csv"))
unlink(temp)

# Make a variable indicating whether (=1) or not (=0) a 
# decision was a constitutional one:

df$Constitutional<-ifelse(df$authorityDecision1 %in% c(1,2),1,0)
df$Constitutional<-ifelse(df$authorityDecision2 %in% c(1,2),1,
                          df$Constitutional)

summary(df$Constitutional)

# Single sample of 20 observations, calculate pi-hat and 
# its C.I. for Constitutional:

set.seed(7222009)
DFsample <- with(df, sample(Constitutional,20,replace=F))
summary(DFsample)
BinomCI(sum(DFsample),length(DFsample))

# Same as:
#
# prop.test(sum(DFsample),length(DFsample),correct=FALSE)
#
# Now do that 1000 times:

N <- 20
reps <- 1000
PI20 <- numeric(reps)
UB20<-numeric(reps)
LB20<-numeric(reps)
set.seed(7222009)
for (i in 1:reps) {
  foo <- with(df, sample(Constitutional,N,replace=F))
  bar <- data.frame(BinomCI(sum(foo),length(foo)))
  PI20[i] <- bar$est
  LB20[i] <- bar$lwr.ci
  UB20[i] <- bar$upr.ci
}

# Data for plotting: 

hats<-data.frame(P=as.numeric(unlist(dimnames(table(PI20)))),
                 L=as.numeric(unlist(dimnames(table(LB20)))),
                 U=as.numeric(unlist(dimnames(table(UB20)))))

# Plot...

pdf("PropSimN20-95-R-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(hats, hist(PI20,xaxt="n",yaxt="n",main="",border="grey",
               xlab="",ylab=""))
par(new=TRUE)
with(hats, plot(PI20,PI20,t="p",pch=19,ylim=c(-0.1,1),
     xlab=expression(paste("Value of ",pi)),
     ylab=expression(hat(pi))))
with(hats, segments(P,L,P,U,lwd=4))
abline(h=mean(df$Constitutional),lwd=2,lty=2,col="red")
dev.off()

# Same thing, increase to N=100:

N <- 100
reps <- 1000
PI100 <- numeric(reps)
UB100<-numeric(reps)
LB100<-numeric(reps)
set.seed(7222009)
for (i in 1:reps) {
  foo <- with(df, sample(Constitutional,N,replace=F))
  bar <- data.frame(BinomCI(sum(foo),length(foo)))
  PI100[i] <- bar$est
  LB100[i] <- bar$lwr.ci
  UB100[i] <- bar$upr.ci
}

# Data for plotting: 

hats<-data.frame(P=as.numeric(unlist(dimnames(table(PI100)))),
                 L=as.numeric(unlist(dimnames(table(LB100)))),
                 U=as.numeric(unlist(dimnames(table(UB100)))))

# Plot...

pdf("PropSimN100-95-R-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(hats, hist(PI100,xaxt="n",yaxt="n",main="",border="grey",
                xlab="",ylab="",breaks=20))
par(new=TRUE)
with(hats, plot(PI100,PI100,t="p",pch=19,ylim=c(-0.1,1),
                xlab=expression(paste("Value of ",pi)),
                ylab=expression(hat(pi))))
with(hats, segments(P,L,P,U,lwd=4))
abline(h=mean(df$Constitutional),lwd=2,lty=2,col="red")
dev.off()


# Now N = 400...

N <- 400
reps <- 1000
PI400 <- numeric(reps)
UB400<-numeric(reps)
LB400<-numeric(reps)
set.seed(7222009)
for (i in 1:reps) {
  foo <- with(df, sample(Constitutional,N,replace=F))
  bar <- data.frame(BinomCI(sum(foo),length(foo)))
  PI400[i] <- bar$est
  LB400[i] <- bar$lwr.ci
  UB400[i] <- bar$upr.ci
}

# Data for plotting: 

hats<-data.frame(P=as.numeric(unlist(dimnames(table(PI400)))),
                 L=as.numeric(unlist(dimnames(table(LB400)))),
                 U=as.numeric(unlist(dimnames(table(UB400)))))

# Plot...

pdf("PropSimN400-95-R-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(hats, hist(PI400,xaxt="n",yaxt="n",main="",border="grey",
                xlab="",ylab="",breaks=30))
par(new=TRUE)
with(hats, plot(PI400,PI400,t="p",pch=19,ylim=c(0,1),
                xlab=expression(paste("Value of ",pi)),
                ylab=expression(hat(pi))))
with(hats, segments(P,L,P,U,lwd=4))
abline(h=mean(df$Constitutional),lwd=2,lty=2,col="red")
dev.off()

# How many of our confidence intervals contain the
# population mean of Constitutional?
#
# N = 20:

popmean<-mean(df$Constitutional)
prop.table(table(ifelse(UB20>popmean & LB20<popmean,1,0)))

# N = 100:

prop.table(table(ifelse(UB100>popmean & LB100<popmean,1,0)))


# N = 400:

prop.table(table(ifelse(UB400>popmean & LB400<popmean,1,0)))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Hypothesis testing...                              ####
#
# Plots of "tailedness":

OneTail <- qnorm(0.95)
sigma <- 1
mu <- 0
bounds <- c(mu-4*sigma, mu+4*sigma)
lower.x <- OneTail
upper.x <- max(bounds)
step <- (upper.x - lower.x) / 100
cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
cord.y <- c(0,dnorm(seq(lower.x,upper.x,step),mu,sigma),0)

pdf("OneTailedDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
curve(dnorm(x,mu,sigma),xlim=bounds,lwd=3,xlab="Z-score",
      ylab="Probability") 
polygon(cord.x,cord.y,col='red')
segments(0,0,0,z[500],lwd=2,lty=2)
arrows(3,0.2,2,0.06,length=0.12,lwd=2)
text(3,.22,label=c("Pr(Z > 1.65)"))
dev.off()

TwoTail <- qnorm(0.975)
LBU.x <- TwoTail
UBU.x <- max(bounds)
step <- (UBU.x - LBU.x) / 100
Top.x <- c(LBU.x,seq(LBU.x,UBU.x,step),UBU.x)
Top.y <- c(0,dnorm(seq(LBU.x,UBU.x,step),mu,sigma),0)
UBL.x <- -TwoTail
LBL.x <- min(bounds)
step <- (UBL.x - LBL.x) / 100
Bot.x <- c(LBL.x,seq(LBL.x,UBL.x,step),UBL.x)
Bot.y <- c(0,dnorm(seq(LBL.x,UBL.x,step),mu,sigma),0)

pdf("TwoTailedDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
curve(dnorm(x,mu,sigma),xlim=bounds,lwd=3,xlab="Z-score",
      ylab="Probability") 
polygon(Top.x,Top.y,col='red')
polygon(Bot.x,Bot.y,col='red')
# segments(0,0,0,z[500],lwd=2,lty=2)
arrows(0.5,0.1,1.95,0.03,length=0.12,lwd=2)
arrows(-0.5,0.1,-1.95,0.03,length=0.12,lwd=2)
text(0,.12,label=c("Pr(|Z| > 1.96)"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# The real-data example, from the Exercise Four data:

data<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Exercises/PLSC502-2024-ExerciseFour.csv")

data$DOB <- with(data, as.Date(DateOfBirth,
                               format = "%d-%b-%Y"))
data$Sign <- with(data, Zodiac(DOB))

popmean <- with(data, prop.table(table(Active)))[2]
popmean
SC<-data[data$Sign=="Scorpio",]
with(SC,prop.test(sum(Active,na.rm=TRUE),nrow(SC),p=popmean,
                  correct=FALSE))

# \fin
