library("readxl")
library("MASS")
library("glmnet")
setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB1")

data = read_excel("tecator.xlsx")

n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = data[id, ]
validation = data[-id, ]

plot(data$Moisture, data$Protein, xlab="Moisture", ylab = "Protein")
#Quite linear

MSE.train = numeric(length = 6)
MSE.valid = numeric(length = 6)

for (i in 1:6){
  M = lm(Moisture~poly(Protein, i), data = train)
  s = summary(M)

  MSE.train[i] = mean(s$residuals^2)
  pred = predict(M, validation)
  MSE.valid[i] = mean((pred-validation$Moisture) ^2)
}

xlab = "Polynomial factor"
main="Mean square errors"
ylab = "MSE"
ylim = range(22:45)

plot(1:6, MSE.train, type="b",
      main = main,xlab = xlab,ylab=ylab,ylim=ylim,col = "Red"
     )
par(new=TRUE)
plot(1:6, MSE.valid,type="b", main=main, xlab = xlab, ylab=ylab, ylim=ylim, col="Blue")
legend(2,1, c("Train", "Test"), col=c("Red","Blue"))

#4
fat = data$Fat
fatmodel = lm(fat~ . -(Sample + Protein + Moisture + Fat), data = data) 
steps = stepAIC(fatmodel)
length(steps$coefficients) #64

#5
fat = data.frame(data)[,"Fat"]
coefficients = data.frame(data)[,2:101]
lambdas = exp(seq(5,-2, -0.01))
fit = glmnet(as.matrix(coefficients), fat, lambda=lambdas, alpha = 0)
summary(fit)
plot(fit, xvar="lambda",label=TRUE)

#6
lambdas = exp(seq(10, -10, -0.1))
lasso = glmnet(as.matrix(coefficients), fat, lambda=lambdas, alpha=1)
summary(lasso)
plot(lasso, xvar="lambda")

#7
cross_validation = cv.glmnet(as.matrix(coefficients), fat, lambda=exp(seq(-10, 1,0.1)), alpha=1)
min(cross_validation$lambda)
plot(cross_validation)
summary(cross_validation)
