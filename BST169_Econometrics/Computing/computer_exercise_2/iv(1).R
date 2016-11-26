#install.packages("AER")
library(gmm)
library(lmtest)
library("AER")
#setwd('C:/your/folder/here/')
data0=read.csv("wage2.csv")
n0=nrow(data0)
idx=which(is.na(data0$brthord))#birth order contains missing observations
data=data0[setdiff(1:n0,idx),]#remove the missing observations
n=nrow(data)
y=log(data$wage)
x=cbind(data$educ,data$exper,data$tenure,data$married,data$south,data$urban)
colnames(x)=c('educ','exper','tenure','married','south','urban')
z=cbind(data$exper,data$tenure,data$married,data$south,data$urban,data$sibs,data$brthord)
colnames(z)=c('exper','tenure','married','south','urban','sibs','brthord')
edres=lm(x[,1]~z)
#linearHypothesis(edres,hypothesis.matrix=matrix(c(rep(0,12),1,0,0,1),2))
#or linearHypothesis(edres,c('zsibs=0','zbrthord=0'),test="Chisq",white.adjust=FALSE)
#linearHypothesis is from package AER; another useful package for hypothesis test is lmtest
#the following command is to use standard errors corrected for heteroskedasticity (requires lmtest):
#coeftest(edres,vcov=hccm)
tsres=tsls(y~x,~z)
ivres=ivreg(y~x|z)
#gmmres=gmm(y~x,x=z,vcov='iid')
nx=ncol(x)
xht=matrix(0,n,nx)
for (i1 in 1:nx){
  xht[,i1]=fitted(lm(x[,i1]~z))
}
tsres2=lm(y~xht)
X=cbind(rep(1,n),x)
Z=cbind(rep(1,n),z)
wm1=solve(t(Z)%*%Z)
zx=t(Z)%*%X
btsvc=solve(t(zx)%*%wm1%*%zx)
btsls=btsvc%*%t(zx)%*%wm1%*%t(Z)%*%y
eht=y-X%*%btsls
sigsqht=c(t(eht)%*%eht/(n-nx-1))
btsvc=sigsqht*btsvc
#btsvc/vcov(tsres2)
wm2=solve(t(Z)%*%diag(c(eht^2))%*%Z)
btsvc2=solve(t(zx)%*%wm2%*%zx)
btsls2=btsvc2%*%t(zx)%*%wm2%*%t(Z)%*%y
bptest(tsres)