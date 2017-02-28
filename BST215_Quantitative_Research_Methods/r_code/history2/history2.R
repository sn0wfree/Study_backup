plot(c(14,66),c(0,12),main="Gender Dis", xlab="Age (years)",ylab="Wage (unknown units)")
points(lowess(Age[Sex=="Female"],Wage[Sex=="Female"]))
points(lowess(Age[Sex=="Male"],Wage[Sex=="Male"]),pch=19)
text(40,6,"Female")


plot(c(14,66),c(0,12),main="Gender Dis", xlab="Age (years)",ylab="Wage (unknown units)")
points(lowess(Age[Union=="Union"],Wage[Union=="Union"]))
points(lowess(Age[Union=="Non_Union"],Wage[Union=="Non_Union"]),pch=19)
text(40,6,"Female")

# Married="Married"


plot(c(14,66),c(0,12),main="Gender Dis", xlab="Age (years)",ylab="Wage (unknown units)")
points(lowess(Age[Sex=="Female" & Married=="Unmarried"],Wage[Sex=="Female" & Married=="Unmarried"]))
points(lowess(Age[Sex=="Male" & Married=="Unmarried"],Wage[Sex=="Male" & Married=="Unmarried"]),pch=19)
text(40,6,"Female")


table(Occupation)

#Occupation=="Management"
plot(c(14,66),c(0,20),main="Gender Dis", xlab="Age (years)",ylab="Wage (unknown units)")
points(lowess(Age[Sex=="Female" & Occupation=="Management"],Wage[Sex=="Female" & Occupation=="Management"]))
points(lowess(Age[Sex=="Male" & Occupation=="Management"],Wage[Sex=="Male" & Occupation=="Management"]),pch=19)
text(40,6,"Female")

qqnorm(Wage)
qqnorm(Age)


