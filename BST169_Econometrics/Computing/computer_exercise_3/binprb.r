library(lmtest)
library(AER)
data0=read.csv("pntsprd.csv")
n=dim(data0)[1]
lpm=lm(favwin~spread,data=data0)
#attach(data0)#by this command, we can refer to data0$favwin by just favwin below
hclpm=coeftest(lpm,vcov=hccm)#standard errors corrected for heteroskedasticity
pvalb0=(1-pt((lpm$coefficients[1]-0.5)/(vcov(lpm)[1,1])^0.5,n-2))*2
pvalb0hc=(1-pt((hclpm[1,1]-0.5)/hclpm[1,2],n-2))*2
lkhpb=function(param){#the log likelihood function  
  return(sum(data0$favwin*log(pnorm(param[1]+param[2]*data0$spread))+(1-data0$favwin)*log(1-pnorm(param[1]+param[2]*data0$spread))))
}
pbrs1=optim(c(0,0.05),lkhpb,hessian=TRUE,control=list(fnscale=-1))#estimate the probit model by mle
pbrs2=glm(favwin~spread,family=binomial(link ="probit"),data=data0)#estimate probit model by R function
#summary(pbrs2)
mfrsq_2=1-pbrs2$deviance/pbrs2$null.deviance#McFadden) R-squared
pbrs3=glm(favwin~spread+favhome+fav25+und25,family=binomial(link ="probit"),data=data0)
mfrsq_3=1-pbrs3$deviance/pbrs3$null.deviance#McFadden) R-squared
lrst=pbrs2$deviance-pbrs3$deviance#likelihood ratio statistic
wst=t(pbrs3$coefficients[3:5])%*%solve(vcov(pbrs3)[3:5,3:5])%*%pbrs3$coefficients[3:5]#wald stat.
chsqcrv=qchisq(0.95,3)#critical value for lr and wald to be compared