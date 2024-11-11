#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro materials...                          ####
#
# PLSC 502 -- Fall 2024
#
# Measures of association...
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load some packages:

P<-c("readr","htmltab","gmodels","DescTools","psych",
     "epitools","epiDisplay","polycor","mvtnorm","car",
     "dplyr")

for (i in 1:length(P)) {
 ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
        print(":)"))
 library(P[i],character.only=TRUE)
  }

# Options:

options(scipen=99)
options(digits=4)

# Probably should setwd() in here someplace too...
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Nominal variables...                        ####
#
# Read the "Is calling someone a feminist an insult" 
# data; this is from a 1997 CBS / NYT national survey 
# of Americans (with N \approx 1000)...

Fem<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/femins.csv")

summary(Fem)

# Tables, etc.

table(Fem$feminsult)
tab1(Fem$feminsult) # from -epiDisplay-

# Crosstable with region:

tabpct(Fem$feminsult, Fem$cenreg)

# Accompanying plot:

pdf("FemMosaic.pdf",7,5)
par(mar=c(4,4,2,2))
# Note that the order of variables is different
# than it was above:
tabpct(Fem$cenreg,Fem$feminsult,xlab="Region",
       ylab="Feminist is a...",main="")
dev.off()

# Chi-square simulated examples:

I<-matrix(data=c(rep(10,times=9)),nrow=3,ncol=3)
I
chisq.test(I)
I<-matrix(data=c(rep(c(5,20,5),times=3)),nrow=3,ncol=3)
I
chisq.test(I)
I<-matrix(data=c(rep(c(20,5,5),each=3)),nrow=3,ncol=3)
I
chisq.test(I)
D<-matrix(data=c(20,5,5,5,20,5,5,5,20),nrow=3,ncol=3)
D
chisq.test(D)
D<-matrix(data=c(9,12,9,12,9,9,9,9,12),nrow=3,ncol=3)
D
chisq.test(D)


# One-way chi-square:

table(Fem$feminsult)

X1<-chisq.test(table(Fem$feminsult))
X1

# Two-way chi-square with region:

region<-with(Fem, table(feminsult,cenreg))
region
chisq.test(region)

# Alternative with "CrossTable":

region2<-with(Fem,
            CrossTable(feminsult,cenreg,prop.chisq=FALSE,chisq=TRUE))

# Three-Way crosstabs...

threeway<-with(Fem, table(feminsult,cenreg,intrace))
addmargins(threeway)
mantelhaen.test(threeway)

# Small cell frequencies:

with(Fem, table(feminsult,race))
with(Fem, chisq.test(table(feminsult,race)))

with(Fem, fisher.test(table(feminsult,race), workspace=20000000))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Binary variables...                         ####
#
# Fake data for the running example:

X <- c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1)
Y <- c(0,0,0,0,0,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1)

T <- table(Y,X)
T

chisq.test(T,correct=FALSE)

p1<-4/9
p2<-8/11
p <- 12/20
se <- sqrt(((p*(1-p)*(1/9+1/11))))
Z <- (p1-p2) / se
Z
Z^2

# Chi-square is not a measure of association...

chisq.test(T, correct=FALSE)
X <- rep(X,times=10)
Y <- rep(Y,times=10)
T10 <- table(Y,X)
T10
chisq.test(T10,correct=FALSE)

# Odds Ratios...

T
OR <- (T[1,1])*T[2,2] / (T[1,2]*T[2,1])
OR

# Odds ratios (via -DescTools-):

OddsRatio(T)

# Phi (via the -psych- package):

T
phi(T)
cor(X,Y)

Tpos<-as.table(rbind(c(10,0),c(0,10)))
Tpos
phi(Tpos)
Tneg<-as.table(rbind(c(0,10),c(10,0)))
Tneg
phi(Tneg)
T0<-as.table(rbind(c(5,5),c(5,5)))
T0
phi(T0)

# Tetrachoric degression: Bivariate normals...

set.seed(7222009)
N <- 10000
mu <- c(0,0)
var0 <- matrix(c(1,0,0,1),2,2)
var5<- matrix(c(1,0.5,0.5,1),2,2)
varn5 <- matrix(c(1,-0.5,-0.5,1),2,2)
var9<- matrix(c(1,0.9,0.9,1),2,2)
Z0 <- mvrnorm(N,mu,var0)
Z5 <- mvrnorm(N,mu,var5)
Zn5 <- mvrnorm(N,mu,varn5)
Z9 <- mvrnorm(N,mu,var9)

# Plot:
pdf("BivNormalsR.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
ellipses(Z0[,1],Z0[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topright",bty="n",legend="r=0")
ellipses(Z5[,1],Z5[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topleft",bty="n",legend="r=0.5")
ellipses(Zn5[,1],Zn5[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topright",bty="n",legend="r=-0.5")
ellipses(Z9[,1],Z9[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topleft",bty="n",legend="r=0.9")
dev.off()

# Error correlations:

set.seed(7222009)
N <- 1000
mu <- c(0,0)
var4 <- matrix(c(1,0.4,0.4,1),2,2)
E4 <- mvrnorm(N,mu,var4)
var75 <- matrix(c(1,0.75,0.75,1),2,2)
E75 <- mvrnorm(N,mu,var75)

# Plot:

pdf("LatentTetrachoricR.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
ellipses(E4[,1],E4[,2],pch=20,xaxt="n",yaxt="n",
         xlab=expression(e[1]),xlim=c(-4,4),
         ylab=expression(e[2]),ylim=c(-4,4),
         smooth=FALSE)
abline(v=0,lwd=2,lty=2,col="red")
abline(h=0,lwd=2,lty=2,col="red")
axis(1,at=0,labels=expression(tau[1]))
axis(2,at=0,labels=expression(tau[2]))
text(-3,-3,labels="{0,0}",col="red")
text(-3,3,labels="{0,1}",col="red")
text(3,-3,labels="{1,0}",col="red")
text(3,3,labels="{1,1}",col="red")
dev.off()

# Second plot:

pdf("LatentTetrachoricR2.pdf",6,5)
par(mar=c(4,4,2,2))
ellipses(E75[,1],E75[,2],pch=20,xaxt="n",yaxt="n",
         xlab=expression(e[1]),xlim=c(-4,4),
         ylab=expression(e[2]),ylim=c(-4,4),
         smooth=FALSE)
abline(v=-0.5,lwd=2,lty=2,col="red")
abline(h=1.5,lwd=2,lty=2,col="red")
axis(1,at=-0.5,labels=expression(tau[1]))
axis(2,at=1.5,labels=expression(tau[2]))
text(-3,-3,labels="{0,0}",col="red")
text(-3,3,labels="{0,1}",col="red")
text(3,-3,labels="{1,0}",col="red")
text(3,3,labels="{1,1}",col="red")
dev.off()

# Actual r_tet:

T
polychor(T)

# Compare:

phi(T)

# Approximate formula:

alpha <- (OR)^(pi/4)
rtet <- (alpha - 1) / (alpha + 1)
rtet

# Phi and Tetrachoric Correlations:
#
# Symmetrical marginals:

NTs <- 101
PHI <- numeric(NTs)
RTET <- numeric(NTs)

for (i in 1:NTs) {
  ST <- as.table(rbind(c(NTs-i,i-1),c(i-1,NTs-i)))
  PHI[i] <- phi(ST)
  RTET[i] <- polychor(ST)
}

pdf("PhiVsRtetSymR.pdf",7,5)
par(mar=c(4,4,2,2))
plot(PHI,RTET,t="l",lwd=2,col="red",xlab=expression(phi),
     ylab=expression(r[tet]))
abline(a=0,b=1,lwd=1,lty=2)
dev.off()

# Asymmetrical marginals:

NTs <- 100
PHI <- numeric(NTs)
RTET <- numeric(NTs)

for (i in 0:NTs) {
  AT <- as.table(rbind(c(NTs-i,(NTs/2)+i),c(i,(NTs/2)+i)))
  PHI[i] <- phi(AT)
  RTET[i] <- polychor(AT)
}

pdf("PhiVsRtetAsymR.pdf",7,5)
par(mar=c(4,4,2,2))
plot(PHI,RTET,t="l",lwd=2,col="red",xlab=expression(phi),
     ylab=expression(r[tet]),ylim=c(-1,1),xlim=c(-1,1))
abline(a=0,b=1,lwd=1,lty=2)
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ordinal variables...                         ####
#
# Simulate a bunch of 2x2 crosstabs, calculating gamma
# and the taus for each, then plot them against each
# other:

NTs <- 101
GAMMA <- numeric(NTs)
TauA<- numeric(NTs)
TauB<- numeric(NTs)
TauC<- numeric(NTs)

for (i in 1:NTs) { 
  ST <- as.table(rbind(c(NTs-i,i-1),c(i-1,NTs-i)))
  GAMMA[i] <- GoodmanKruskalGamma(ST)
  TauA[i] <- KendallTauA(ST)
  TauB[i] <- KendallTauB(ST)
  TauC[i] <- StuartTauC(ST)
}

dsts <- data.frame(Gamma=GAMMA,TauA=TauA,TauB=TauB,TauC=TauC)

pdf("GammasAndTaus.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(dsts,pch=20,smooth=FALSE,col="black",
                  regLine=list(lwd=1,col="red"),
                  var.labels=c("Gamma","Tau-A","Tau-B","Tau-C"))
dev.off()

# Now do the same for 3x3 crosstabs, where we sample over
# random permutations of values of X and Y:

reps<-1000   # number of sims
N<-500       # N in each table
T3<-data.frame(matrix(nrow=reps,ncol=4))
set.seed(7222009)
for(i in 1:reps){
  t1<-runif(1,0,0.67) # thresholds...
  t2<-runif(1,t1,1)
  Xl<-runif(N,0,1)  # underlying values in
  Yl<-runif(N,0,1)  # [0,1]
  X<-cut(Xl,breaks=c(0,t1,t2,1)) # ordinalize...
  Y<-cut(Yl,breaks=c(0,t1,t2,1))
  T<-xtabs(~X+Y)
  T3[i,1] <- GoodmanKruskalGamma(T)
  T3[i,2] <- KendallTauA(T)
  T3[i,3] <- KendallTauB(T)
  T3[i,4] <- StuartTauC(T)
}

pdf("GammaTau3x3.pdf",7,6)
scatterplotMatrix(T3,pch=".",smooth=FALSE,col="black",
                  regLine=list(lwd=1,col="red"),
                  var.labels=c("Gamma","Tau-A","Tau-B","Tau-C"))
dev.off()

# Get Palin data:

MamaGriz<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/Palin.csv")

MamaGriz$palin <- ordered(MamaGriz$palin,
                          levels=c("Very Unfavorable","Somewhat Unfavorable",
                                   "Somewhat Favorable","Very Favorable"))
MamaGriz$pid <- ordered(MamaGriz$pid,levels=c("Democrat",
                                              "Independent","GOP"))
MamaGriz$female <- ordered(MamaGriz$female,levels=c("Male",
                                                    "Female"))
summary(MamaGriz)

palinpid<-with(MamaGriz, xtabs(~palin+pid))
addmargins(palinpid)

# Plotting ordinal variables:
#
# Scatter:

pdf("PalinScatter.pdf",7,6)
par(mar=c(4,4,2,2))
with(MamaGriz,plot(as.numeric(palin),
                   as.numeric(pid),
                   main="",pch=19,xlab="Palin Favorability",
                   ylab="Party Identification"))
dev.off()

# Mosaic plot:

pdf("PalinMosaic.pdf",7,6)
par(mar=c(4,4,2,2))
with(MamaGriz, plot(table(palin,pid),las=2,color=TRUE,
                    main="",xlab="Palin Favorability",
                    ylab="Party Identification"))
dev.off()

# Gamma:

G<-GoodmanKruskalGamma(palinpid,conf.level=0.95)

#Tau-A:

TA<-KendallTauA(palinpid,conf.level=0.95)

# Tau-B:

TB<-KendallTauB(palinpid,conf.level=0.95)

# Tau-C:

TC<-StuartTauC(palinpid,conf.level=0.95)

# Plot:

foo<-data.frame(rbind(G,TA,TB,TC))
colnames(foo)<-c("s","l","u")
rownames(foo)<-c("Gamma","Tau-A","Tau-B","Tau-C")

pdf("PalinPIDs.pdf",7,6)
plot(foo$s,1:4,xlim=c(0.3,0.8),pch=19,
     xlab="Estimate",ylab="",yaxt="n")
axis(2,1:4,labels=rownames(foo),las=2)
segments(foo$l,1:4,foo$u,1:4)
dev.off()

# Palin Support: Male vs. Female:

palinfemale<-with(MamaGriz, xtabs(~palin+female))
addmargins(palinfemale)

GMF<-GoodmanKruskalGamma(palinfemale,conf.level=0.95)
TAMF<-KendallTauA(palinfemale,conf.level=0.95)
TBMF<-KendallTauB(palinfemale,conf.level=0.95)
TCMF<-StuartTauC(palinfemale,conf.level=0.95)

# Plot:

bar<-data.frame(rbind(GMF,TAMF,TBMF,TCMF))
colnames(bar)<-c("s","l","u")
rownames(bar)<-c("Gamma","Tau-A","Tau-B","Tau-C")

pdf("PalinMFs.pdf",7,6)
plot(bar$s,1:4,xlim=c(-0.2,0),pch=19,
     xlab="Estimate",ylab="",yaxt="n")
axis(2,1:4,labels=rownames(bar),las=2)
segments(bar$l,1:4,bar$u,1:4)
abline(v=0,lwd=1,lty=2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interval / ratio-level variables              ####
#
# Linear, Logarithmic and Exponential plots:

set.seed(7222009)
N <- 100
X <- runif(N,0,5)
Ylin <- X + runif(N)
Ylog <- log(X)+runif(N)
Yexp <- exp(X)+runif(N,0,20)

pdf("XYLinLogExp.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(X,Ylin,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Y = X + u")
plot(X,Ylog,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Y = ln(X) + u")
plot(X,Yexp,pch=19,xlim=c(0,5),ylab="Y")
legend("topleft",bty="n",legend="Y = exp(X) + u")
dev.off()

# Other relationships:

Ystep <- ifelse(X>2.5,4+runif(N),2+runif(N))
Ypoly <- 5 + 5*X - X^2 + 2*runif(N)
Ythresh <- ifelse(X>2.5,mean(X)-2.5+(X)+runif(N),mean(X)+runif(N))

pdf("XYOthers.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(X,Ystep,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Step Function")
plot(X,Ypoly,pch=19,xlim=c(0,5),ylab="Y")
legend("topright",bty="n",legend="Polynomial")
plot(X,Ythresh,pch=19,xlim=c(0,5),ylab="Y")
legend("topleft",bty="n",legend="Threshold /\nChange Point")
dev.off()

# Pearson's r plot:

set.seed(7222009)
N <- 100
X <- runif(N,0,5)
Yr <- X + 2*rnorm(N)

pdf("PearsonsRPlotR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Yr,pch=19,ylab="Y")
abline(h=mean(Yr),lty=2,lwd=2,col="red")
abline(v=mean(X),lty=2,lwd=2,col="red")
text(4,7,"I",col="red")
text(4,-1,"II",col="red")
text(1,-1,"III",col="red")
text(1,7,"IV",col="red")
text(mean(X)-0.08,-2.9,"mean of X",col="red",pos=4)
text(0.55,mean(Yr)-0.05,"mean of Y",col="red",pos=3)
dev.off()

# Four linear relationships:

set.seed(7222009)
XY9 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0.9,0.9,1),ncol=2))
XY5 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0.5,0.5,1),ncol=2))
XY0 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0,0,1),ncol=2))
XYN5 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,-0.5,-0.5,1),ncol=2))

pdf("FourLinearsR.pdf",7,7)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
plot(XY9,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend="r = 0.9")
plot(XY5,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend="r = 0.5")
plot(XY0,pch=19,xlab="X",ylab="Y")
legend("topright",bty="n",legend="r = 0")
plot(XYN5,pch=19,xlab="X",ylab="Y")
legend("topright",bty="n",legend="r = -0.5")
dev.off()

# Perfect Linearities:

YP1 <- X
YP2 <- 0.2*X - 3
YP3 <- 3*X - 5

pdf("PerfectRs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,YP3,pch=19,ylab="Y")
points(X,YP1,pch=4,col="darkgreen")
points(X,YP2,pch=17,col="red")
legend("topleft",bty="n",legend="All have r = 1.0")
dev.off()

# Quadratic & other bad non-linearities

set.seed(7222009)

# Quadratic
YQuad <- 5 + 5*X - X^2 + 2*runif(N)
YQr <- cor(X,YQuad)
# "Steps"
Ysteps <- ifelse(X>1.5,runif(N),3+runif(N))
Ysteps <- ifelse(X>3.5,3+runif(N),Ysteps)
YSr <- cor(X,Ysteps)
# Outlier
Yout <- runif(N)
X <- X[order(X)]
Yout <- Yout[order(X)]
Yout[N-1] <- 10
YOr <- cor(X,Yout)

pdf("BadPearsonsR.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(X,YQuad,pch=19,xlab="X",ylab="Y",main="Quadratic")
legend("topleft",bty="n",legend=paste0("r = ",round(YQr,2)))
plot(X,Ysteps,pch=19,xlab="X",ylab="Y",main="Step Function")
legend("bottomright",bty="n",legend=paste0("r = ",round(YSr,2)))
plot(X,Yout,pch=19,xlab="X",ylab="Y",main="Outlier")
legend("topleft",bty="n",legend=paste0("r = ",round(YOr,2)))
dev.off()

# Fisher's transformation plot:

r <- seq(-0.99,0.99,by=0.01)
z <- 0.5 * log((1+r)/(1-r))

pdf("RZPlotR.pdf",6,6)
par(mar=c(4,4,2,2))
plot(r,z,t="l",lwd=3)
dev.off()

# A little Pearson-Spearman comparison; first w/N=10:

Nreps <- 1000
N <- 10
Actual <- numeric(Nreps)
Rs <- numeric(Nreps)
Rhos <- numeric(Nreps)

set.seed(7222009)

for(i in 1:Nreps) {
  foo <- runif(1,-1,1)
  Actual[i] <- foo
  Xs<-rmvnorm(N,mean=c(0,0),sigma=matrix(c(1,foo,foo,1),ncol=2))
  Rs[i]<-cor(Xs[,1],Xs[,2])
  Rhos[i]<-SpearmanRho(Xs[,1],Xs[,2])
}

# Then with N=1000:

Nreps <- 1000
NK <- 1000
ActualK <- numeric(Nreps)
RsK <- numeric(Nreps)
RhosK <- numeric(Nreps)

set.seed(7222009)

for(i in 1:Nreps) {
  foo <- runif(1,-1,1)
  ActualK[i] <- foo
  Xs<-rmvnorm(NK,mean=c(0,0),sigma=matrix(c(1,foo,foo,1),ncol=2))
  RsK[i]<-cor(Xs[,1],Xs[,2])
  RhosK[i]<-SpearmanRho(Xs[,1],Xs[,2])
}

# Plots:

pdf("RvsRhoR.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(Rs,Rhos,pch=20,xlab="Pearson's r",
     ylab=expression(rho))
abline(a=0,b=1,col="grey")
legend("topleft",bty="n",legend="N = 10")
plot(RsK,RhosK,pch=20,xlab="Pearson's r",
     ylab=expression(rho))
abline(a=0,b=1,col="grey")
legend("topleft",bty="n",legend="N = 1000")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Examples: ANES 2016 feeling thermometers     ####
#
# Get the ANES data and extract the feeling 
# thermometers...

ANES <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/ANES2016.csv")

Tvars <- c("V162310","V162311","V162312","V162313",
           "V162314","V162078","V162079","V162080",
           "V162081","V162091","V162092","V162093",
           "V162094","V162095","V162096","V162097",
           "V162098","V162099","V162100","V162101",
           "V162102","V162103","V162104","V162105",
           "V162106","V162107","V162108","V162109",
           "V162110","V162111","V162112","V162113")

Therms <- ANES[Tvars]
Therms[Therms==-5] <- NA
Therms[Therms==-6] <- NA
Therms[Therms==-7] <- NA
Therms[Therms==-9] <- NA
Therms[Therms==998] <- NA
Therms[Therms==999] <- NA
Therms <- na.omit(Therms)
colnames(Therms) <- c("Asian-Americans","Hispanics","Blacks",
                      "Illegal Immigrants","Whites","Dem. Pres. Candidate",
                      "GOP Pres. Candidate","Libertarian Pres. Candidate",
                      "Green Pres. Candidate","Dem. VP", "GOP VP",
                      "John Roberts", "Pope Francis",
                      "Christian Fundamentalists","Feminists","Liberals",
                      "Labor Unions","Poor People","Big Business",
                      "Conservatives","SCOTUS","Gays & Lesbians",
                      "Congress","Rich People","Muslims","Christians",
                      "Jews","Tea Party","Police","Transgender People",
                      "Scientists","BLM")

describe(Therms,range=FALSE)

# Clinton and Trump:

pdf("ClinTrumpScatter.pdf",5,5)
par(mar=c(4,4,2,2))
with(Therms, plot(`Dem. Pres. Candidate`,`GOP Pres. Candidate`,
                  pch=20,main="",xlab="Clinton",ylab="Trump"))
abline(with(Therms, lm(`GOP Pres. Candidate`~`Dem. Pres. Candidate`)),
       lwd=2,col="darkorange")
dev.off()

rCT<-with(Therms, cor(`Dem. Pres. Candidate`,`GOP Pres. Candidate`))
rCT

rCT2<-with(Therms, cor.test(`Dem. Pres. Candidate`,`GOP Pres. Candidate`))
rCT2

# Identical:

(rCT*sqrt(nrow(Therms)-2)) / sqrt(1-(rCT^2))

# Liberals and conservatives:

# Clinton and Trump:

pdf("LibConScatter.pdf",5,5)
par(mar=c(4,4,2,2))
with(Therms, plot(Liberals,Conservatives,
                  pch=20,main=""))
abline(with(Therms, lm(Liberals~Conservatives)),
       lwd=2,col="darkorange")
dev.off()

rLC<-with(Therms, cor.test(Liberals,Conservatives))
rLC

rhoLC<-with(Therms, SpearmanRho(Liberals,Conservatives))
rhoLC


# Now do all (32*31)/2 = 496 of them, comparing r and rho:

labs<-colnames(Therms)
nvar<-length(labs)
pairs<-nvar^2 # no. of pairs of vars
Rs<-data.frame(matrix(nrow=pairs,ncol=3)) # a place to keep Rs
Therms<-as.data.frame(Therms) # get rid of the "tibble" nonsense

z <- 1 # a counter...
for(i in 1:nvar) {
  for(j in 1:(nvar)) {
    Rs[z,1]<-paste0(labs[j]," - ",labs[i])
    Rs[z,2]<-cor(Therms[,j],Therms[,i])
    Rs[z,3]<-SpearmanRho(Therms[,j],Therms[,i])
    z <- z+1
  }
}

Rs$Diff <- Rs$X2 - Rs$X3 # diff. b/w r and rho

# Plot difference vs. r:

pdf("R-Rho-Diffs.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Rs$X2,Rs$Diff,xlab="Pearson's r",ylab="r minus rho",
     pch=20)
abline(h=0,lwd=1,lty=2,col="red")
abline(v=0,lwd=1,lty=2)
dev.off()

# Pairs with the biggest differences:

pdf("WhiteHispScatter.pdf",5,5)
par(mar=c(4,4,4,2))
with(Therms, scatterplot(Hispanics,Whites,
                         pch=20,main="Difference = 0.06"))
dev.off()

pdf("GreenLibScatter.pdf",5,5)
par(mar=c(4,4,4,2))
with(Therms, scatterplot(`Green Pres. Candidate`,
                         `Libertarian Pres. Candidate`,
                         pch=20,main="Difference = -0.061"))
dev.off()

# /fin
