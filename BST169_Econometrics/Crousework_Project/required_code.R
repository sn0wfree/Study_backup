#{r heteroscedasticity-package}for question2
pbp=read.csv("/Users/sn0wfree/Dropbox/PhD_1st_study/BST169_Econometrics/Crousework_Project/pbp.csv")
#head(pbp)
#str(pbp)
signlevel=0.05
require(lmtest)
equ1<-lm(y~x1 + x2,data=pbp)
equ2<-lm((y-x2)~(x1-x2), data=pbp)
if (bptest(resid(equ1)^2~pbp$x1*pbp$x2+pbp$x1^2+pbp$x2^2)$p.value<signlevel){
  equ1<-lm(y~x1+x2,weights=1/sqrt(x1),data=pbp)
  equ2<-lm(I(y-x2)~I(x1-x2),weights=1/sqrt(x1),data=pbp)
}
#Breusch-Pagan-Godfrey Test
bptest(equ1)
bptest(equ2)
#White test
bptest(residuals(equ1)^2~x1+x2+x1*x2+(x1)^2+(x2)^2,data=pbp)
bptest(residuals(equ2)^2~(x1-x2)+(x1-x2)^2,data=pbp)
#if heteroscedasticity
N=length(pbp$y)

beta=matrix(equ1$coefficients)
R=cbind(0,1,1)
r=cbind(1,0,0)
X=cbind(rep(1,length(pbp$y)),pbp$x1,pbp$x2)
residual=matrix(resid(equ1))
#U_N
U_tilde=matrix(0,3,3)
len=length(pbp$y)
for(i in 1:len){U_tilde=U_tilde+residual[i,]^2*X[i,]%*%t(X[i,])}
U_tilde=U_tilde/len
#U_q
U_q=matrix(0,3,3)
ee=rt(len,6)
est_y=equ2$coefficients[1]+equ2$coefficients[2]*(pbp$x1-pbp$x2)+ee+pbp$x2
temp_y=matrix(est_y-mean(est_y))
for(i in 1:len){U_q=U_q+temp_y[i,]^2*X[i,]%*%t(X[i,])}
U_q=U_q/len
#  
Wald=(1/N)*t(R%*%beta-1)%*%solve(R%*%solve(t(X)%*%X)%*%U_tilde%*%solve(t(X)%*%X)%*%t(R))%*%(R%*%beta-1)
LM=(1/N)*t(R%*%beta-1)%*%solve(R%*%solve(t(X)%*%X)%*%U_q%*%solve(t(X)%*%X)%*%t(R))%*%(R%*%beta-1)

#{r heteroscedasticity-package}for question2-end


###{r Monte Carlo:heter-uncorrected}for question3-1

require(lmtest)
require(MASS)
##boost up: translate programme language code into Byte-code.
require(compiler)
enableJIT(3)
##boost up-end for continues
#set seed
#set.seed(2112)
#assumption part
loop=100
#I have a multiplication factor:1, which means when you set loop=N, 
#It will generate N different (increased) sample size, and for each sample will do N*10 times Monte Carlo simulations.
#be careful your settings, your computer may explode.
#Warning: the loop time cannot be larger any more;please forgive me, this all my Macbook fault. And the optimization of R is terrible.

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
P.value_W_chisq_container=rep(0,loop)
P.value_LM_chisq_container=rep(0,loop)
# for loop start:Monte Carlo
for(j in 1:loop){#first for-loop for generating multi-sample
  W=0
  LM=0
  LR=0
  N=original_N+j
  #generation part:data
  x1=rchisq(N, 2)
  x2=runif(N,0,10)
  for (i in 1:loop){# second for-loop: the main Monte Carlo code
    e=rt(N,6)
    U_q=matrix(0,3,3)
    U_tilde=matrix(0,3,3)
    R=cbind(0,1,1)
    r=cbind(1,0,0)
    X=cbind(rep(1,N),x1,x2)
    y=beta_0+beta_1*x1+(1-beta_1)*x2+sqrt(x1)*e
    #generation part:regression
    equ1<-lm(y~x1+x2)
    equ2<-lm(I(y-x2)~I(x1-x2))
    #calculate beta and residual
    #beta=matrix(equ1$coefficients)
    #residual=matrix(resid(equ1))
    #calc SSR and Wald,LM, and LR
    #U_N
    #
    #for(i in 1:N){U_tilde=U_tilde+residual[i,]^2*X[i,]%*%t(X[i,])}
    #U_tilde=U_tilde/N
    #U_q
    #est_y=equ2$coefficients[1]+equ2$coefficients[2]*(x1-x2)+sqrt(x1)*e+x2
    #temp_y=matrix(est_y-mean(est_y))
    #for(i in 1:N){U_q=U_q+temp_y[i,]^2*X[i,]%*%t(X[i,])}
    #U_q=U_q/N
    #calculate Wald and LM
    SSRu=sum(residuals(equ1)^2)
    SSRr=sum(residuals(equ2)^2)
    #W[j]=(1/N)*t(R%*%beta-1)%*%solve(R%*%solve(t(X)%*%X)%*%U_tilde%*%solve(t(X)%*%X)%*%t(R))%*%(R%*%beta-1)
    #LM[j]=(1/N)*t(R%*%beta-1)%*%solve(R%*%solve(t(X)%*%X)%*%U_q%*%solve(t(X)%*%X)%*%t(R))%*%(R%*%beta-1)
    W[i]=N*((SSRr-SSRu)/(SSRu))
    LM[i]=N*((SSRr-SSRu)/(SSRr))
    LR[i]=N*(log(SSRr/SSRu))
    
    #if (bptest(equ1,studentize = 0)$p.value<signlevel){P.value_homo_container[j]=P.value_homo_container[j]+1}
    P.value_homo_container[j]=P.value_homo_container[j]+bptest(equ1,studentize = 0)$p.value
    P.value_W_chisq_container[j]=P.value_W_chisq_container[j]+ks.test(W,'pchisq',1)$p.value
    #P.value_LM_chisq_container[j]=P.value_LM_chisq_container[j]+ks.test(LM,'pchisq',1)$p.value
    if(ks.test(W,'pchisq',1)$p.value>signlevel){W_count[j]=W_count[j]+1}
    if(ks.test(LM,'pchisq',1)$p.value>signlevel){LM_count[j]=LM_count[j]+1}
    if(ks.test(LR,'pchisq',1)$p.value>signlevel){LR_count[j]=LR_count[j]+1}
    
  }
  
}
plot(P.value_homo_container/(loop*10), xlab = "Sample Size(+10)",ylab = "the Praboblity of Homoscedasticity",main="Homoscedasticity based on different Sample Size")
#plot(P.value_W_chisq_container/(loop*10), xlab = "Sample Size(+10)",ylab = "the P-value of Wald statistcs following the chisq distribution",main="Homoscedasticity based on different Sample Size")
#plot(P.value_LM_chisq_container/(loop*10),xlab = "Sample Size(+10)",ylab = "the P-value of LM statistcs following the chisq distribution(under 5%)",main="the P-value of LM~chisq based on different Sample Size")
par(mfrow=c(3,1))
plot(W_count/(loop),xlab = "Sample Size(+10)",ylab = "the Praboblity of Wald statistcs following the chisq distribution(under 5%)",main="the Praboblity of Wald~chisq based on different Sample Size")
points(lowess(W_count/(loop)),type="l",col="red")
plot(LM_count/(loop),xlab = "Sample Size(+10)",ylab = "the Praboblity of LM statistcs following the chisq distribution(under 5%)",main="the Praboblity of LM~chisq based on different Sample Size")
points(lowess(LM_count/(loop)),type="l",col="red")
plot(LR_count/(loop),xlab = "Sample Size(+10)",ylab = "the Praboblity of LR statistcs following the chisq distribution(under 5%)",main="the Praboblity of LR~chisq based on different Sample Size")
points(lowess(LR_count/(loop)),type="l",col="red")


###{r Monte Carlo:heter-uncorrected}for question3-1end

##{r Monte Carlo:heter-corrected}for question3-2

require(lmtest)
require(MASS)


##boost up: translate programme language code into Byte-code.
require(compiler)
enableJIT(3)
##boost up-end for continues
#set seed
set.seed(2112)
#assumption part

loop=100
m=1#I have a multiplication factor:1, which means when you set loop=N, 
#It will generate N different (increased) sample size, and for each sample will do N*10 times Monte Carlo simulations.
#be careful your settings, your computer may explode.
#Warning: the loop time cannot be larger any more;please forgive me, this all my Macbook fault. And the optimization of R is terrible.

beta_1=0.4
beta_0=1

#x1_store=rchisq(80+20, 2)
#initial valueset
original_N=30
signlevel=0.05

#initial container for Wald, LM, LR
W_count=rep(0,loop)
LM_count=rep(0,loop)
LR_count=rep(0,loop)
P.value_homo_container=rep(0,loop)
P.value_W_chisq_container=rep(0,loop)
P.value_LM_chisq_container=rep(0,loop)

# for loop start:Monte Carlo

for(j in 1:loop){#first for-loop for generating multi-sample
  W=0
  LM=0
  LR=0
  N=original_N+j
  
  #generation part:data
  x1=rchisq(N, 2)
  x2=runif(N,0,10)
  
  for (i in 1:loop*m){# second for-loop: the main Monte Carlo code
    e=rt(N,6)
    U_q=matrix(0,3,3)
    U_tilde=matrix(0,3,3)
    R=cbind(0,1,1)
    r=cbind(1,0,0)
    X=cbind(rep(1,N),x1,x2)
    y=beta_0+beta_1*x1+(1-beta_1)*x2+sqrt(x1)*e
    #generation part:regression
    equ1<-lm(y~x1+x2,weights=1/(x1^.5))
    equ2<-lm(I(y-x2)~I(x1-x2),weights=1/(x1^.5))
    #heter
    if (bptest(resid(equ1)^2~x1*x2+x1^2+x2^2)$p.value<signlevel){
      equ1<-lm(y~x1+x2,weights=1/sqrt(x1))
      equ2<-lm(I(y-x2)~I(x1-x2),weights=1/sqrt(x1))
    }
    #calculate beta and residual
    beta=matrix(equ1$coefficients)
    residual=matrix(resid(equ1))
    #calc SSR and Wald,LM, and LR
    #U_N
    #
    for(i in 1:N){U_tilde=U_tilde+residual[i,]^2*X[i,]%*%t(X[i,])}
    U_tilde=U_tilde/N
    #U_q
    est_y=equ2$coefficients[1]+equ2$coefficients[2]*(x1-x2)+sqrt(x1)*e+x2
    temp_y=matrix(est_y-mean(est_y))
    for(i in 1:N){U_q=U_q+temp_y[i,]^2*X[i,]%*%t(X[i,])}
    U_q=U_q/N
    #calculate Wald and LM
    W[j]=(1/N)*t(R%*%beta-1)%*%solve(R%*%solve(t(X)%*%X)%*%U_tilde%*%solve(t(X)%*%X)%*%t(R))%*%(R%*%beta-1)
    LM[j]=(1/N)*t(R%*%beta-1)%*%solve(R%*%solve(t(X)%*%X)%*%U_q%*%solve(t(X)%*%X)%*%t(R))%*%(R%*%beta-1)
    #SSRu=sum(residuals(equ1)^2)
    #SSRr=sum(residuals(equ2)^2)
    
    #W[i]=N*((SSRr-SSRu)/(SSRu))
    #LM[i]=N*((SSRr-SSRu)/(SSRr))
    #LR[i]=N*(log(SSRr/SSRu))
    
    
    #if (bptest(equ1,studentize = 0)$p.value<signlevel){P.value_homo_container[j]=P.value_homo_container[j]+1}
    P.value_homo_container[j]=P.value_homo_container[j]+bptest(equ1,studentize = 0)$p.value
    P.value_W_chisq_container[j]=P.value_W_chisq_container[j]+ks.test(W,'pchisq',1)$p.value
    P.value_LM_chisq_container[j]=P.value_LM_chisq_container[j]+ks.test(LM,'pchisq',1)$p.value
    if(ks.test(W,'pchisq',1)$p.value>signlevel){W_count[j]=W_count[j]+1}
    if(ks.test(LM,'pchisq',1)$p.value>signlevel){LM_count[j]=LM_count[j]+1}
    #if(ks.test(LR,'pchisq',1)$p.value>signlevel){LR_count[j]=LR_count[j]+1}
    gc()
  }
  
}

#plot(P.value_homo_container/(loop*m), xlab = "Sample Size(+10)",ylab = "the P-value of Homoscedasticity",main="Homoscedasticity based on different Sample Size")
#plot(P.value_W_chisq_container/(loop*m), xlab = "Sample Size(+10)",ylab = "the P-value of Wald statistcs following the chisq distribution",main="Homoscedasticity based on different Sample Size")
#plot(P.value_LM_chisq_container/(loop*m),xlab = "Sample Size(+10)",ylab = "the P-value of LM statistcs following the chisq distribution(under 5%)",main="the P-value of LM~chisq based on different Sample Size")
par(mfrow=c(2,1))
plot(W_count/(loop*m),xlab = "Sample Size(+30)",ylab = "the Praboblity of Wald statistcs following the chisq distribution(under 5%)",main="the Praboblity of Wald~chisq based on different Sample Size")
points(lowess(W_count/(loop*m)),type="l",col="red")
plot(LM_count/(loop*m),xlab = "Sample Size(+30)",ylab = "the Praboblity of LM statistcs following the chisq distribution(under 5%)",main="the Praboblity of LM~chisq based on different Sample Size")
points(lowess(LM_count/(loop*m)),type="l",col="red")
#plot(LR_count/(loop*m),xlab = "Sample Size(+30)",ylab = "the Praboblity of LR statistcs following the chisq distribution(under 5%)",main="the Praboblity of LR~chisq based on different Sample Size")

##{r Monte Carlo:heter-corrected}for question3-2end

##{r Monte Carlo:type1error}for question4-1

require(lmtest)
require(MASS)


##boost up: translate programme language code into Byte-code.
require(compiler)
enableJIT(3)
##boost up-end for continues
#set seed
set.seed(2112)
#assumption part

loop=100
m=1#I have a multiplication factor:1, which means when you set loop=N, 
#It will generate N different (increased) sample size, and for each sample will do N*10 times Monte Carlo simulations.
#be careful your settings, your computer may explode.
#Warning: the loop time cannot be larger any more;please forgive me, this all my Macbook fault. And the optimization of R is terrible.

beta_1=0.4
beta_0=1

#x1_store=rchisq(80+20, 2)
#initial valueset
original_N=10
signlevel=0.05

#initial container for Wald, LM, LR
W=rep(0,loop)
LM=rep(0,loop)
LR=rep(0,loop)
W_count=rep(0,loop)
LM_count=rep(0,loop)
LR_count=rep(0,loop)
P.value_homo_container=rep(0,loop)
P.value_W_chisq_container=rep(0,loop)
P.value_LM_chisq_container=rep(0,loop)
theta=rep(seq(0.25,1.75,len=loop))
crv=rep(qchisq(0.95,1),loop)





# for loop start:Monte Carlo
#for(j in 1:loop){#first for-loop for generating multi-sample



#generation part:data


#U_q=matrix(0,3,3)
#U_tilde=matrix(0,3,3)
#R=cbind(0,1,1)
#r=cbind(1,0,0)
#X=cbind(rep(1,N),x1,x2)

for(j in 1:loop){
  N=original_N+j
  x1=rchisq(N, 2)
  x2=runif(N,0,10)
  
  for (i in 1:loop*m){# second for-loop: the main Monte Carlo code
    e=rnorm(N,0,1)
    y=beta_0+beta_1*x1+(1-beta_1)*x2+sqrt(x1)*e
    #generation part:regression
    #true_equ2<-lm(I(y-x2)~I(x1-x2),weights=1/sqrt(x1))
    equ1<-lm(y~x1+x2,weights=1/sqrt(x1))
    equ2<-lm(I(y-theta[i]*x2)~I(x1-x2),weights=1/sqrt(x1))
    
    SSRu=sum(residuals(equ1)^2)
    SSRr=sum(residuals(equ2)^2)
    #true_SSRr=sum(residuals(true_equ2)^2)
    W[i]=W[i]+N*((SSRr-SSRu)/(SSRu))
    LM[i]=LM[i]+N*((SSRr-SSRu)/(SSRr))
    LR[i]=LR[i]+N*(log(SSRr/SSRu))
    
    #LR[i]=N*(log(SSRr/SSRu))
    W_count[j]=W_count[j]+(mean(W)>qchisq(0.95,1))
    LM_count[j]=LM_count[j]+(mean(LM)>qchisq(0.95,1))
    LR_count[j]=LR_count[j]+(mean(LR)>qchisq(0.95,1))
  }
  
}
#W_count=W_count+(ks.test(W,'pchisq',1)$p.value>signlevel)
#LM_count=LM_count+(ks.test(LM,'pchisq',1)$p.value>signlevel)
par(mfrow=c(3,1))
plot(W/(loop*m),xlab = "Sample Size(+10)",ylab = "the Wald statistcs(under 5%)",main="the Wald staitsices based on different Sample Size")
points(crv,type="l",col="red")

plot(LM/(loop*m),xlab = "Sample Size(+10)",ylab = "the LM statistcs(under 5%)",main="the Praboblity of LM~chisq based on different Sample Size")
#points(lowess(LM/(loop*m)),type="l",col="red")
points(crv,type="l",col="red")
plot(LR/(loop*m),xlab = "Sample Size(+10)",ylab = "the LR statistcs(under 5%)",main="the LR based on different Sample Size")
points(crv,type="l",col="red")

#plot(W_count/(loop*m),xlab = "Sample Size(+10)",ylab = "the Wald statistcs following the chisq distribution(under 5%)",main="the Wald staitsices based on different Sample Size")
#points(crv,type="l",col="red")

#plot(LM/(loop*m),xlab = "Sample Size(+10)",ylab = "the LM statistcs following the chisq distribution(under 5%)",main="the Praboblity of LM~chisq based on different Sample Size")
#points(lowess(LM/(loop*m)),type="l",col="red")
#points(crv,type="l",col="red")
#plot(LR/(loop*m),xlab = "Sample Size(+10)",ylab = "the LR statistcs following the chisq distribution(under 5%)",main="the Praboblity of LR~chisq based on different Sample Size")
#points(crv,type="l",col="red")

##{r Monte Carlo:type1error}for question4-1end


#{r Monte Carlo for type1error}for question4-2
require(lmtest)
require(MASS)

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
# for loop start:Monte Carlo
for(j in 1:loop){#first for-loop for generating multi-sample
  theta=0
  N=original_N+j
  #generation part:data
  s=sample(1:length(pbp$y),N,replace = 1)
  question.pre.y<-pbp$y[s]
  question.pre.x1<-pbp$x1[s]
  question.pre.x2<-pbp$x2[s]
  pre_equ<-lm(I(question.pre.y-question.pre.x2)~I(question.pre.x1-question.pre.x2))
  theta_test=rep(seq(0,4,len=loop))
  for (i in 1:loop*10){
    question5.residuals=sample(residuals(pre_equ),N,replace=1)
    question5.y=pre_equ$coefficients[1]+pre_equ$coefficients[2]*question.pre.x1+(1-pre_equ$coefficients[2])*question.pre.x2+question5.residuals
    #generation part:regression
    question5.equ1<-lm(question5.y~question.pre.x1+question.pre.x2)
    #residuals.question5.equ1=resid(question5.equ1)
    #question5.equ1<-lm((question5.y-question5.x2)~(question5.x1-question5.x2))
    #calculation
    ##pre-cal:heteroscedasticity
    #if(bptest(residuals.question5.equ1^2~question5.x1*question5.x2+question5.x1^2+question5.x2^2)$p.value<signlevel){
    #  question5.equ1<-lm(question5.y~question5.x1+question5.x2,weights=(1/question5.x1^0.5))
    #question5.equ1<-lm((question5.y-question5.x2)~(question5.x1-question5.x2),weights=(1/question5.x1^0.5))}
    #calc 
    theta[i]=question5.equ1$coefficients[2]+question5.equ1$coefficients[3]
    #if(length(theta)>2){if(t.test(theta,mu=1)$p.value>signlevel){theta_count[j]=theta_count[j]+1}}
    theta_count[j]=theta_count[j]+(t.test(theta,mu=theta_test[j])$p.value<0.05)
  }
}


plot(theta_test,theta_count/(loop*10),xlab="Theta(beta1+beta2)",ylab="Praboblity of Type 1 error",main="The Praboblity of Type 1 error")
#{r Monte Carlo for type1error}for question4-2end


##{r }for question5
pbp=read.csv("/Users/sn0wfree/Dropbox/PhD_1st_study/BST169_Econometrics/Crousework_Project/pbp.csv")
loop=50
m=1#I have a multiplication factor:1, which means when you set loop=N, 
#It will generate N different (increased) sample size, and for each sample will do N*10 times Monte Carlo simulations.
#be careful your settings, your computer may explode.
#Warning: the loop time cannot be larger any more;please forgive me, this all my Macbook fault. And the optimization of R is terrible.


#initial valueset
original_N=30
signlevel=0.05

#initial container for Wald, LM, LR
W_count=rep(0,loop)
LM_count=rep(0,loop)
LR_count=rep(0,loop)
P.value_homo_container=rep(0,loop)
P.value_W_chisq_container=rep(0,loop)
P.value_LM_chisq_container=rep(0,loop)




# for loop start:Monte Carlo

for(j in 1:loop){#first for-loop for generating multi-sample
  W=0
  LM=0
  LR=0
  N=original_N+j
  
  
  
  for (i in 1:loop*m){# second for-loop: the main Monte Carlo code
    s=sample(1:length(pbp$y),N,replace = 1)
    question5.y<-pbp$y[s]
    question5.x1<-pbp$x1[s]
    question5.x2<-pbp$x2[s]
    #generation part:data
    question5_equ2<-lm(I(question5.y-question5.x2)~I(question5.x1-question5.x2),weights=1/sqrt(question5.x1))
    #generation part:regression
    question5_equ1<-lm(question5.y~question5.x1+question5.x2,weights=1/(question5.x1^.5))
    
    #calculate beta and residual
    #beta=matrix(equ1$coefficients)
    #residual=matrix(resid(equ1))
    #calc SSR and Wald,LM, and LR
    
    SSRu=sum(residuals(question5_equ1)^2)
    SSRr=sum(residuals(question5_equ2)^2)
    
    W[i]=N*((SSRr-SSRu)/(SSRu))
    LM[i]=N*((SSRr-SSRu)/(SSRr))
    LR[i]=N*(log(SSRr/SSRu))
    
    
    #if (bptest(equ1,studentize = 0)$p.value<signlevel){P.value_homo_container[j]=P.value_homo_container[j]+1}
    P.value_homo_container[j]=P.value_homo_container[j]+bptest(equ1,studentize = 0)$p.value
    P.value_W_chisq_container[j]=P.value_W_chisq_container[j]+ks.test(W,'pchisq',1)$p.value
    P.value_LM_chisq_container[j]=P.value_LM_chisq_container[j]+ks.test(LM,'pchisq',1)$p.value
    if(ks.test(W,'pchisq',1)$p.value>signlevel){W_count[j]=W_count[j]+1}
    if(ks.test(LM,'pchisq',1)$p.value>signlevel){LM_count[j]=LM_count[j]+1}
    if(ks.test(LR,'pchisq',1)$p.value>signlevel){LR_count[j]=LR_count[j]+1}
    gc()
  }
  
}

#plot(P.value_homo_container/(loop*m), xlab = "Sample Size(+10)",ylab = "the P-value of Homoscedasticity",main="Homoscedasticity based on different Sample Size")
#plot(P.value_W_chisq_container/(loop*m), xlab = "Sample Size(+10)",ylab = "the P-value of Wald statistcs following the chisq distribution",main="Homoscedasticity based on different Sample Size")
#plot(P.value_LM_chisq_container/(loop*m),xlab = "Sample Size(+10)",ylab = "the P-value of LM statistcs following the chisq distribution(under 5%)",main="the P-value of LM~chisq based on different Sample Size")
par(mfrow=c(3,1))
plot(W_count/(loop*m),xlab = "Sample Size(+30)",ylab = "the Praboblity of Wald statistcs following the chisq distribution(under 5%)",main="the Praboblity of Wald~chisq based on different Sample Size")
points(lowess(W_count/(loop*m)),type="l",col="red")
plot(LM_count/(loop*m),xlab = "Sample Size(+30)",ylab = "the Praboblity of LM statistcs following the chisq distribution(under 5%)",main="the Praboblity of LM~chisq based on different Sample Size")
points(lowess(LM_count/(loop*m)),type="l",col="red")
plot(LR_count/(loop*m),xlab = "Sample Size(+30)",ylab = "the Praboblity of LR statistcs following the chisq distribution(under 5%)",main="the Praboblity of LR~chisq based on different Sample Size")
points(lowess(LR_count/(loop*m)),type="l",col="red")


##{r }for question5end