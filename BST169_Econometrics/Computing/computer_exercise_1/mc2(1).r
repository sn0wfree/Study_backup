library(tseries)

#uncomment the following two lines to let the codes run faster if your R>=2.14
library(compiler)
enableJIT(3)

n=5 #sample size
s=100 #number of simulations
x=runif(n,0,10) #just one explanatory variable
b=c(1,2) #the true value of the intercept and slope
sigs=100 #the true value of sigma squared
b1hat=rep(0,s);b2hat=rep(0,s);sighat=rep(0,s);sigsqhat=rep(0,s)
nts=100 #number of test values
btest=seq(0,4,len=nts) #test values
bp=rep(0,nts)
alp=0.05 #level of significance
t1err=0 #type one error
crv=qt(1-alp/2,n-2) #critical value
for (i1 in 1:s){
  y=b[1]+b[2]*x+rnorm(n,0,sigs^.5)
  lmyx=lm(y~x)
  sigsqhat[i1]=deviance(lmyx)/(n-2)
  sighat[i1]=sigsqhat[i1]^.5
  bcoef=coef(lmyx)
  b1hat[i1]=bcoef[1];b2hat[i1]=bcoef[2]
  bse=diag(vcov(lmyx))^.5 #estimated standard error
  bix=which(abs((b2hat[i1]-btest)/bse[2])>crv)
  bp[bix]=bp[bix]+1;
  t1err=t1err+(abs((b2hat[i1]-b[2])/bse[2])>crv)
}
bp=bp/s
t1err=t1err/s
plot(btest,bp,type='l')

