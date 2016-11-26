x=0
for (i in 10:2){
x[i] = log(i)
print(x[i])
}



x=0
y=-99999
for (j in 1:9){
x <- x + j
y[j] = x
print(c(j, y[j]))
}


runif(5)
rnorm(10, -2, 10)

x=0
for (j in 1:9){
x =rnorm(4,-2,3)
print(x)
}

sample(1:4, replace=TRUE)

x=rnorm(20,-.5,1)
t.test(x,mu=0,alternative="less")









leaveit[21] = 10
lmeans=0
for(i in 1:100000){
lmeans[i]=median(leaveit[sample(1:21,replace=TRUE)])
}

boxplot(lmeans)

pos=0
for(i in 1:100000){
  if(lmeans[i] > 0){
    pos = pos + 1
    }
}
pos




leaveit










