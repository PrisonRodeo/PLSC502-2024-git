#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                     ####
#
# PLSC 502 -- Fall 2023
#
# Day Ten materials: Two Group Comparisons
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load packages (there are only four...):

P<-c("readr","pwr","psych","lubridate")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Set significant digits for display:

options(digits=4)

# You could / should -setwd()- here too, or whatever
# you do to make that whole thing work.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Assumptions plot:

x<-seq(-6,16,by=0.1)

pdf("DOMExampleR.pdf",6,5)
par(mar=c(4,4,2,2))
curve(dnorm(x,4,3),-6,16,lwd=2,ylab="Probability",ylim=c(0,0.4),
      xlab="Distribution of Y")
lines(x,dnorm(x,12,1),lwd=2,lty=2,col="red")
legend("topleft",bty="n",lty=c(1,2),lwd=2,col=c("black","red"),
       legend=c("Y|X=0","Y|X=1"))
text(0,0.12,col="black",labels=expression(paste(mu,"=4, ",sigma^2,"=9")))
text(8.5,0.25,col="red",labels=expression(paste(mu,"=12, ",sigma^2,"=1")))
segments(4,0,4,dnorm(4,4,3),lty=3)
segments(12,0,12,dnorm(12,12,1),lty=3,col="red")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Two-group comparisons...                       ####
# 
# First, independent samples, equal variances:

Nsims <- 1000    # number of sims

N <- c(10,40,200)   # sample sizes 
D <- seq(0,1,by=0.1) # differences in means

P1<-as.data.frame(matrix(Nsims,length(N)*length(D)))
set.seed(7222009)

z=1                        # counter...
for(j in 1:length(N)){
  for(k in 1:length(D)){
    for(i in 1:Nsims){
      x<-rnorm(N[j],0,1)            # independent samples,
      y<-rnorm(N[j],0+D[k],1)       # same variance...
      t<-t.test(x,y,var.equal=TRUE) # t-test
      P1[i,z]<-t$p.value            # P-value
    }
    z<-z+1                 # increment counter
  }
}

D1<-data.frame(Diff=D,
               P=matrix(colMeans(P1),ncol=3))

pdf("DOMs-1-plot-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(D1$Diff,D1$P.1,ylim=c(0,0.6),t="l",lwd=2,
     xlab="Difference of Means",ylab="Mean P-Value")
lines(D1$Diff,D1$P.2,lty=2,lwd=2,col="orange")
lines(D1$Diff,D1$P.3,lty=3,lwd=2,col="navy")
abline(h=c(0.1,0.05,0.01),lwd=0.5)
text(c(0,0,0),c(0.11,0.06,0.02),cex=0.7,pos=4,
     labels=c("P=0.10","P=0.05","P=0.01"))
legend("topright",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","orange","navy"),
       legend=c(paste0("N=",2*N[1]),
                paste0("N=",2*N[2]),
                paste0("N=",2*N[3])))
dev.off()


# Now with different variances:

P2<-as.data.frame(matrix(Nsims,length(N)*length(D)))
set.seed(7222009)

z=1                        # counter...
for(j in 1:length(N)){
  for(k in 1:length(D)){
    for(i in 1:Nsims){
      x<-rnorm(N[j],0,1)            # independent samples,
      y<-rnorm(N[j],0+D[k],5)       # different variance...
      t<-t.test(x,y,var.equal=TRUE) # t-test
      P2[i,z]<-t$p.value            # P-value
    }
    z<-z+1                 # increment counter
  }
}

D2<-data.frame(Diff=D,
               P=matrix(colMeans(P2),ncol=3))

pdf("DOMs-2-plot-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(D2$Diff,D2$P.1,ylim=c(0,0.6),t="l",lwd=2,
     xlab="Difference of Means",ylab="Mean P-Value")
lines(D2$Diff,D2$P.2,lty=2,lwd=2,col="orange")
lines(D2$Diff,D2$P.3,lty=3,lwd=2,col="navy")
abline(h=c(0.1,0.05,0.01),lwd=0.5)
text(c(0,0,0),c(0.11,0.06,0.02),cex=0.7,pos=4,
     labels=c("P=0.10","P=0.05","P=0.01"))
legend("topright",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","orange","navy"),
       legend=c(paste0("N=",2*N[1]),
                paste0("N=",2*N[2]),
                paste0("N=",2*N[3])))
dev.off()

# Now do it with equal variances but dependent samples...

P3<-as.data.frame(matrix(Nsims,length(N)*length(D)))
set.seed(7222009)

z=1                        # counter...
for(j in 1:length(N)){
  for(k in 1:length(D)){
    for(i in 1:Nsims){
      x<-rnorm(N[j],0,1)                # dependent/paired samples,
      y<-(x + rnorm(N[j],0+2*D[k],1))/2 # same variances...
      t<-t.test(x,y,var.equal=TRUE)     # t-test
      P3[i,z]<-t$p.value                # P-value
    }
    z<-z+1                 # increment counter
  }
}

D3<-data.frame(Diff=D,
               P=matrix(colMeans(P3),ncol=3))

pdf("DOMs-3-plot-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(D3$Diff,D3$P.1,ylim=c(0,0.6),t="l",lwd=2,
     xlab="Difference of Means",ylab="Mean P-Value")
lines(D3$Diff,D3$P.2,lty=2,lwd=2,col="orange")
lines(D3$Diff,D3$P.3,lty=3,lwd=2,col="navy")
abline(h=c(0.1,0.05,0.01),lwd=0.5)
text(c(0,0,0),c(0.11,0.06,0.02),cex=0.7,pos=4,
     labels=c("P=0.10","P=0.05","P=0.01"))
legend("topright",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","orange","navy"),
       legend=c(paste0("N=",2*N[1]),
                paste0("N=",2*N[2]),
                paste0("N=",2*N[3])))
dev.off()


# Finally: Consider VERY skewed / non-normal variables: 

P4<-as.data.frame(matrix(Nsims,length(N)*length(D)))
set.seed(7222009)

z=1                        # counter...
for(j in 1:length(N)){
  for(k in 1:length(D)){
    for(i in 1:Nsims){
      x<-rexp(N[j],rate=4)              # independent samples,
      y<-rexp(N[j],rate=4+D[k])         # high skew
      t<-t.test(x,y,var.equal=TRUE)     # t-test
      P4[i,z]<-t$p.value                # P-value
    }
    z<-z+1                 # increment counter
  }
}

D4<-data.frame(Diff=D,
               P=matrix(colMeans(P4),ncol=3))

pdf("DOMs-4-plot-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(D4$Diff,D4$P.1,ylim=c(0,0.6),t="l",lwd=2,
     xlab="Difference of Means",ylab="Mean P-Value")
lines(D4$Diff,D4$P.2,lty=2,lwd=2,col="orange")
lines(D4$Diff,D4$P.3,lty=3,lwd=2,col="navy")
abline(h=c(0.1,0.05,0.01),lwd=0.5)
text(c(0,0,0),c(0.11,0.06,0.02),cex=0.7,pos=4,
     labels=c("P=0.10","P=0.05","P=0.01"))
legend("topright",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","orange","navy"),
       legend=c(paste0("N=",2*N[1]),
                paste0("N=",2*N[2]),
                paste0("N=",2*N[3])))
dev.off()


# #-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# #  Africa data example (NOT SHOWN)                 ####
# 
# Africa<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2024-git/master/Data/africa2001.csv")
# 
# # Summary:
# 
# stat.desc(Africa$adrate)
# 
# # HIV rates, by region:
# 
# with(Africa[Africa$subsaharan=="Not Sub-Saharan",], 
#      stat.desc(adrate))
# 
# with(Africa[Africa$subsaharan=="Sub-Saharan",], 
#      stat.desc(adrate))
# 
# # t-tests:
# 
# with(Africa, t.test(adrate~subsaharan))
# 
# with(Africa, t.test(literacy~subsaharan))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# District Court Judge Data Example...             ####
#
# Biographical Directory of Article III Federal Judges...
#
# Source: https://www.fjc.gov/history/judges/biographical-directory-article-iii-federal-judges-export

Js<-read_csv("https://www.fjc.gov/sites/default/files/history/judges.csv")

# Limit to first-time federal district court appointees:

Js<-Js[Js$`Court Type (1)`=="U.S. District Court",]

# Fix up dates, calculate (rough) age -- we don't use a more
# accurate age because many judges do not have their specific
# DOB recorded...

Js$YOB<-as.numeric(Js$`Birth Year`)
Js$AppYr<-as.numeric(substr(Js$`Commission Date (1)`,1,4))
Js$AppAge<-Js$AppYr-Js$YOB

# Summary statistics...
#
# All data:

describe(Js$AppAge)
table(Js$Gender)
tapply(Js$AppAge,Js$Gender,describe) # Appointment age by gender

# Density plot:

pdf("DCJAgeByGender-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Js[Js$Gender=="Male",]$AppAge,na.rm=TRUE),
     ylim=c(0,0.06),main="",xlab="Age",lwd=2)
lines(density(Js[Js$Gender=="Female",]$AppAge,na.rm=TRUE),
      lwd=2,lty=2,col="orange")
abline(v=mean(Js[Js$Gender=="Male",]$AppAge,na.rm=TRUE))
abline(v=mean(Js[Js$Gender=="Female",]$AppAge,na.rm=TRUE),
       col="orange",lty=2)
legend("topright",bty="n",lwd=2,lty=c(1,2),
       col=c("black","orange"),
       legend=c("Men","Women"))
dev.off()

# Calculate t-test by hand:

AgeDiff<-mean(Js[Js$Gender=="Male",]$AppAge,na.rm=TRUE) - 
  mean(Js[Js$Gender=="Female",]$AppAge,na.rm=TRUE)
vM<-(sd(Js[Js$Gender=="Male",]$AppAge,na.rm=TRUE))^2
vF<-(sd(Js[Js$Gender=="Female",]$AppAge,na.rm=TRUE))^2

T<-AgeDiff / sqrt((vM/2737)+(vF/484))
T

# Now using t.test:

T1<-t.test(AppAge~Gender,data=Js)
T1

# "Reverse" the coding:

Js$Female<-ifelse(Js$Gender=="Female",1,0)
Ta<-t.test(AppAge~Female,data=Js)
Ta

# Test male age > female age:

Tg<-t.test(AppAge~Female,data=Js,alternative="greater")
Tg

# Test male age < female age:

Tl<-t.test(AppAge~Female,data=Js,alternative="less")
Tl

# Test whether the difference is one year:

T1<-t.test(AppAge~Female,data=Js,mu=1)
T1

# Force variances to be equal:

Te<-t.test(AppAge~Female,data=Js,var.equal=TRUE)
Te


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Differences of proportions:                    ####
#
# Define "non-white" as anything other than "white":

Js$NonWhite<-ifelse(Js$`Race or Ethnicity`=="White",0,1)

prop.table(table(Js$NonWhite))
xtabs(~NonWhite+Female,data=Js)
prop.table(xtabs(~NonWhite+Female,data=Js),margin=1)
prop.table(xtabs(~NonWhite+Female,data=Js),margin=2)

# Tests...
#
# Proportion female across white/non-white ("by hand"):

PF<-prop.table(table(Js$Female))[2] # total prop. female
PFW<-prop.table(xtabs(~NonWhite+Female,data=Js),margin=1)[3] # P(female|white)
PFNW<-prop.table(xtabs(~NonWhite+Female,data=Js),margin=1)[4]# P(female|nonwhite)
NM<-table(Js$Female)[1] # N male
NF<-table(Js$Female)[2] # N female
s<-sqrt((PF*(1-PF))*((1/NM)+(1/NF))) # s

Z <- (PFW-PFNW) / s
Z
pnorm(Z)
Z^2   # z-squared is chi-square (1)
pchisq(Z^2,df=1,lower.tail=FALSE)

# Now using prop.test():

Nf<-xtabs(~Js$NonWhite+Js$Female)[c(3,4)]
Nt<-as.numeric(table(Js$NonWhite))
FT<-prop.test(Nf,Nt,correct=FALSE)
FT

# Proportion non-white across male/female:

Nnw<-xtabs(~Js$Female+Js$NonWhite)[c(3,4)]
Nt2<-as.numeric(table(Js$Female))
prop.test(Nnw,Nt2)


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Power...                                       ####
#
# Hypothetical example:
#
# Find sample size for d=0.67 with P=0.05 and power=0.80:

pwr.t.test(d=0.67,sig.level=0.05,power=0.80)

# Here's the default plot that creates:

pdf("HypoPowerPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(pwr.t.test(d=0.67,sig.level=0.05,power=0.80))
dev.off()

# Second example:

pwr.t.test(n=60,sig.level=0.05,power=0.80)

# Here's the plot:

pdf("HypoPowerPlot2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(pwr.t.test(n=60,sig.level=0.05,power=0.80))
dev.off()

# \fin
