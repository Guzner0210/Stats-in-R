setwd("C:/Users/xuanduc/Desktop/R")

p2_15 <- read.table("p2_15.txt", header=TRUE)

print(p2_15$educ)
print(p2_15$wage)

p2_15$educ -> educ
p2_15$wage -> wage

analyze <- function(arr) {
  print(paste(c("The mean is: ", mean(arr)), collapse = " "))
  print(paste(c("The median is: ", median(arr)), collapse = " "))
  print(paste(c("The variance is: ", var(arr)), collapse = " "))
  print(paste(c("The standard deviation is: ", sd(arr)), collapse = " "))
}

cor(educ, wage)

analyze(wage)
analyze(educ)

hist(educ, main="Histogram for years of education", xlab="Years of education",
     border="black", col="red", xlim=c(0,30), las=1, breaks=10)

hist(wage, main="Histogram for wage", xlab="Wage",
     border="black", col="red", xlim=c(0,100), las=1, breaks=20)

plot(educ, wage,
     main="Education versus Wage",
     xlab="Education", ylab="Wage", xlim=c(0,30),
     ylim=c(0,80), type="p", col="red", lty=1, lwd=1, pch=16)

abline(lm(wage~educ), col="red")
lm(wage~educ)

quantile(educ)
quantile(wage)
