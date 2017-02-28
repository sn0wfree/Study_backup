# history1.R 6 Oct 2016
# using Wages.xls

read.table("clipboard",header=TRUE)
bug=read.table("clipboard",header=TRUE)
bug
head(bug)
bug$Age
mean(bug$Age)
sd(bug$Age)
median(bug$Age)
Age
bug$Age
attach(bug)
Age
boxplot(Age)
boxplot(Wage)
plot(Age,Wage)
plot(lowess(Age,Wage))
plot(lowess(Age,Wage,f=1/3))
plot(lowess(Age,Wage,f=1/3),pch=19)
plot(lowess(Age,Wage,f=1/3),pch=19,cex=2)
plot(lowess(Age,Wage,f=1/3),pch=19,cex=2,col="red")
head(bug)
colnames(bug)
plot(lowess(Age[Sex="Female"],Wage[Sex="Female"],f=1/3),pch=19,cex=2,col="red")
plot(lowess(Age[Sex=="Female"],Wage[Sex=="Female"],f=1/3),pch=19,cex=2,col="red")
points(lowess(Age[Sex=="Male"],Wage[Sex=="Male"],f=1/3),pch=19,cex=2,col="blue")
plot(lowess(Age[Sex=="Male"],Wage[Sex=="Male"],f=1/3),pch=19,cex=2,col="blue")
plot(c(15,65),c(0,12))
plot(c(15,65),c(0,12),col="white")
ppoints(lowess(Age[Sex=="Male"],Wage[Sex=="Male"],f=1/3),pch=19,cex=2,col="blue")
points(lowess(Age[Sex=="Male"],Wage[Sex=="Male"],f=1/3),pch=19,cex=2,col="blue")
points(lowess(Age[Sex=="Female"],Wage[Sex=="Female"],f=1/3),pch=19,cex=2,col="red")
history()
history(100)
history(50)

