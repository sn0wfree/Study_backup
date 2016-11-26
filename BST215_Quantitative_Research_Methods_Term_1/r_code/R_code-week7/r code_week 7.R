#*coding==UTF8*
#dataset=attendSLIM.xls
#
attendSLIM=read.table(pipe("pbpaste"),header=TRUE)
attendSLIM=read.csv("/Users/sn0wfree/Desktop/attendSLIM.csv",header=TRUE)

head(attendSLIM)
par(mfrow=c(4,4))#输出成4*4 的图片组

boxplot(attendSLIM$attendPC,attendSLIM$hwPC)


#t-test and the related test：
"""
t-test 经常用于小样本<30）的两个平均值差异程度的检验方法。
它是用T分布理论来推断差异发生的概率，从而判定两个平均数的差异是否显著。)
"""
t.test(attendSLIM$attendPC,attendSLIM$hwPC) #normal t-test 
t.test(attendSLIM$attendPC,attendSLIM$hwPC, var.equal = 1) #t-test with var as equal # deprecated
t.test(attendSLIM$attendPC,attendSLIM$hwPC, paired = 1)#pair two sample to test whether the means are equal
#two tails test
t.test(attendSLIM$attendPC) #test mu =0:mu show the ture mean in the one sample t-test
t.test(attendSLIM$attendPC, mu =85) #test mu=85:mu show the ture mean in the one sample t-test
t.test(attendSLIM$attendPC-85) # equivalent of t.test(attendSLIM$attendPC, mu =85) 

t.test(attendSLIM$attendPC, mu =85, alternative = "less")# one tail test
t.test(attendSLIM$attendPC, mu =82.99312)
t.test(attendSLIM$attendPC, mu =81.70956)

#redo with another test: wilcox.test
"""
the data samples are independent if they come from distinct populations and the samples do not affect each other. 
Using the Mann-Whitney-Wilcoxon Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.
"""

wilcox.test(attendSLIM$attendPC,attendSLIM$hwPC, paired = 1)
wilcox.test(attendSLIM$attendPC, mu =85)
#plot
hist(table(attendSLIM$final))
hist((attendSLIM$final))#?
table(attendSLIM$final>=25)
table(attendSLIM$final>=25,attendSLIM$year)
chisq.test(table(attendSLIM$final>=25,attendSLIM$year))

#regression
