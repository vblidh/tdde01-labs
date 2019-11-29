library("readxl")
setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB1")
set.seed(12345)
data = (read_excel("machines.xlsx"))

n = dim(data)[1]
plot(data)

theta.vector = seq(0,8,0.001)

calc_lhood = function(theta, x) {
  lhood = theta^n*exp(-theta*sum(x))
  log(lhood)
}

calc_bayesian_lhood = function(theta, x){
  p0 = 10*exp(-10*theta)
  lhood = theta^n*exp(-theta*sum(x))*p0
  log(lhood)
}


log.likelyhoods = calc_lhood(theta.vector, data$Length)
ind = which(log.likelyhoods==max(log.likelyhoods))
max = ind*0.001
plot(theta.vector, log.likelyhoods, col="Red",xlab="Theta values", ylab="Log likelyhood", xlim=range(0,8), ylim=range(-300,400))
par(new=TRUE)
log.likelyhoods.6values = calc_lhood(theta.vector, data$Length[1:6])
ind = which(log.likelyhoods.6values==max(log.likelyhoods.6values))
max.6values = ind*0.001
plot(theta.vector, log.likelyhoods.6values, col="Blue", xlab="Theta values", ylab="Log likelyhood", xlim=range(0,8),ylim=range(-300,400))

par(new=FALSE)

bay.likelyhoods = calc_bayesian_lhood(theta=theta.vector, x=data$Length)
plot(theta.vector, bay.likelyhoods, col="Yellow", xlab="Theta values", ylab = "Log likelyhood", ylim=range(-300, -40))

par(new=TRUE)
plot(theta.vector, log.likelyhoods, col="Red",xlab="Theta values", ylab="Log likelyhood", ylim=range(-300, -40))

bay.max = max(bay.likelyhoods) #0.912
std_max = max(log.likelyhoods)
bay.max_theta = theta.vector[which.max(bay.likelyhoods)] #0.912
std.max_theta = theta.vector[which.max(log.likelyhoods)] #1.126

new_obs = rexp(50, std.max_theta)

hist(new_obs, main = "Histogram of observations generated with optimal theta", xlab="Length", breaks = seq(0,6,0.5))
hist(as.numeric(data$Length), main="Histogram of original data", xlab = "Length", breaks = seq(0,6,0.5))

