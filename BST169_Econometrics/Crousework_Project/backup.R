
```{r Monte Carlo:heter-uncorrected}
require(lmtest)
require(MASS)
require(stats)

##boost up: translate programme language code into Byte-code.
require(compiler)
enableJIT(3)
##boost up-end for continues
#assumption part

loop=100#I have a multiplication factor:10, which means when you set loop=N, 
#it will generate N different (increased) sample size, and for each sample it will do N*10 times Monte Carlo simulations.
#be careful your settings, your computer may explode.

beta_1=0.4
beta_0=1

#x1_store=rchisq(80+20, 2)
#initial valueset
original_N=10
signlevel=0.05

#initial container for Wald, LM, LR
W_count=rep(0,loop)
LM_count=rep(0,loop)
LR_count=rep(0,loop)
P.value_homo_container=rep(0,loop)
# for loop start:Monte Carlo

for(j in 1:loop){#first for-loop for generating multi-sample
  W=0
  LM=0
  LR=0
  N=original_N+j
  
  #generation part:data
  x1=rchisq(N, 2)
  x2=runif(N,0,10)
  
  for (i in 1:loop*10){# second for-loop: the main Monte Carlo coding
    y=beta_0+beta_1*x1+(1-beta_1)*x2+sqrt(x1)*rt(N,6)
    #generation part:regression
    equ1<-lm(y~x1+x2)
    equ2<-lm((y-x2)~(x1-x2))
    #calculation
    #calc SSR and Wald,LM, and LR
    SSRu=sum(residuals(equ1)^2)
    SSRr=sum(residuals(equ2)^2)
    
    W[i]=N*((SSRr-SSRu)/(SSRu))
    LM[i]=N*((SSRr-SSRu)/(SSRr))
    LR[i]=N*(log(SSRr/SSRu))
    
    
    #if (bptest(equ1,studentize = 0)$p.value<signlevel){P.value_homo_container[j]=P.value_homo_container[j]+1}
    P.value_homo_container[j]=P.value_homo_container[j]+bptest(equ1,studentize = 0)$p.value
    if(ks.test(W,'pchisq',1)$p.value>signlevel){W_count[j]=W_count[j]+1}
    if(ks.test(LM,'pchisq',1)$p.value>signlevel){LM_count[j]=LM_count[j]+1}
    if(ks.test(LR,'pchisq',1)$p.value>signlevel){LR_count[j]=LR_count[j]+1}
    
  }
  
}
plot(P.value_homo_container/(loop*10), xlab = "Sample Size(+10)",ylab = "the P-value of Homoscedasticity",main="Homoscedasticity based on different Sample Size")
#plot(W_count/(loop*10),xlab = "Sample Size(+10)",ylab = "the Praboblity of Wald statistcs following the chisq distribution",main="the Praboblity of Wald~chisq based on different Sample Size")
#plot(LM_count/(loop*10),xlab = "Sample Size(+10)",ylab = "the Praboblity of LM statistcs following the chisq distribution",main="the Praboblity of LM~chisq based on different Sample Size")

```



```{r Monte Carlo for type1error}
require(lmtest)
require(MASS)
require(stats)
##boost up: translate programme language code into Byte-code.
require(compiler)
enableJIT(3)
##boost up-end for continues
#assumption part

loop=100
#Warning: the loop time cannot be larger any more;please forgive me, this all my Macbook fault. And the optimization of R is terrible.
#initial valueset
original_N=100
signlevel=0.05
theta_count=rep(0,loop)
crv=qt(1-0.05/2,N-2)
# for loop start:Monte Carlo
for(j in 1:2){#first for-loop for generating multi-sample
  theta=0
  N=original_N+j
  #generation part:data
  
  question4.x1=rchisq(N, 2)
  question4.x2=runif(N,0,10)
  question.pre.e=rt(N,6)
  question.pre.y=1.3+0.3*question4.x1+(1-0.3)*question4.x2+question.pre.e
  pre_equ<-lm((question.pre.y-question.pre.x2)~(question.pre.x1-question.pre.x2))
  theta_test=rep(seq(0.25,1.75,len=loop))
  for (i in 1:loop){
    question4.residuals=sample(residuals(pre_equ),N,replace=1)
    question4.y=pre_equ$coefficients[1]+pre_equ$coefficients[2]*question.pre.x1+(1-pre_equ$coefficients[2])*question.pre.x2+question4.residuals
    #generation part:regression
    question4.equ2<-lm(question4.y~question.pre.x1+question.pre.x2)
    #residuals.question4.equ1=resid(question4.equ1)
    #question4.equ2<-lm((question4.y-question4.x2)~(question4.x1-question4.x2))
    #calculation
    ##pre-cal:heteroscedasticity
    #if(bptest(residuals.question4.equ1^2~question4.x1*question4.x2+question4.x1^2+question4.x2^2)$p.value<signlevel){
    #  question4.equ1<-lm(question4.y~question4.x1+question4.x2,weights=(1/question4.x1^0.5))
    #question4.equ2<-lm((question4.y-question4.x2)~(question4.x1-question4.x2),weights=(1/question4.x1^0.5))}
    #calc 
    theta[i]=question4.equ2$coefficients[2]+question4.equ2$coefficients[3]
    #if(length(theta)>2){if(t.test(theta,mu=1)$p.value>signlevel){theta_count[j]=theta_count[j]+1}}
    theta_count[i]=theta_count[i]+(t.test(theta,mu=theta_test[i])$p.value<0.05)
  }
}


plot(theta_test,theta_count/(loop*10),xlab="Theta(value)",ylab="Praboblity of Type 1 error",main="The Praboblity of Type 1 error")
```
I choose t test for 


