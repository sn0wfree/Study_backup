# I forgot to save the history of today's R. Here is my reconsructions of the important bits.


somA=read.table("clipboard",header=TRUE)
head(soma) # WRONG. Capital A.
head(somA)
attach(somA)

boxplot(IQ)
qqnorm(IQ)
boxplot(educ)
qqnorm(educ)

plot(IQ,educ)
plot(IQ,jitter(educ))
plot(lowess(IQ,educ))
plot(lowess(IQ,educ,f=1/3)) # for a more fine-grained smooth

cor.test(IQ,educ)

