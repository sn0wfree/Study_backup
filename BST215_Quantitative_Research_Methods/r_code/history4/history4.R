#history 4 27 October 2016... Folowing on from hypothesis testing

xx=read.table("clipboard",header=TRUE)
head(xx)
attach(xx)
wilcox.test(Wage[Gender=="Male"],Wage[Gender=="Female"])
wilcox.test(Wage[Gender=="Male"],Wage[Gender=="Female"],alternative="greater")
wilcox.test(Wage[Gender=="Male"],Wage[Gender=="Female"],alternative="less")
cor.test(Age,Wage)
head(xx)
plot(Experience,Wage)
plot(lowess(Experience,Wage))
cor.test(Experience,Wage)
cor.test(Experience,Wage,alternate="greater")
cor.test(Experience,Wage)
cor.test(Experience,Wage,alternative="greater")
y=Experience[Gender=="Male"]
y
x=Experience[Gender=="Female"]
x
wilcox.test(x,y)
wilcox.test(x,y,alternative="greater")
wilcox.test(y,x,alternative="greater")
mean(x)
mean(y)
table(Experience)
hist(Experience)
table(Experience,Gender)
wilcox.test(y==10,x==10)
wilcox.test(y[Experience==10],x[Experience==10])
wilcox(Wage[Gender=="Female" & Experience==10],Wage[Gender=="Male" & Experience==10])
wilcox.test(Wage[Gender=="Female" & Experience==10],Wage[Gender=="Male" & Experience==10])
wilcox.test(Wage[Gender=="Female" & Experience==13],Wage[Gender=="Male" & Experience==13])
wilcox.test(Wage[Gender=="Female" & Experience==14],Wage[Gender=="Male" & Experience==14])
history(100)

