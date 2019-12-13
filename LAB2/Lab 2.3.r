library("ggplot2")
library("fastICA")
RNGversion('3.5.1')
set.seed(12345)
setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB2")
#setwd("/home/vikbl327/Courses/TDDE01/tdde01-labs/LAB2")

data = read.csv2("NIRSpectra.csv")

#1
PCA.fit = prcomp(x=data[1:126], scale. = FALSE)
plot(PCA.fit)
summary(PCA.fit)

plot(PCA.fit$x[,1], PCA.fit$x[,2], main = "PC1 versus PC2", col="maroon4",
     xlab = "PC1 ~= 95,3%", ylab = "PC2 ~= 4,2%")

#2
plot(PCA.fit$rotation[,1], main="Traceplot, PC1", ylab="PC1")
plot(PCA.fit$rotation[,2],main="Traceplot, PC2", ylab="PC2")


#3
ICA.fit = fastICA(X = data[1:126], n.comp = 2, alg.typ = "parallel", 
                  fun = "logcosh", alpha = 1)

W = ICA.fit$W
K = ICA.fit$K

W_prime = K %*% W

plot(W_prime[,1], main = "Traceplot, W'[1]", ylab = "W'[1]")
plot(W_prime[,2], main = "Traceplot, W'[2]", ylab = "W'[2]")

plot(ICA.fit$S[,1], ICA.fit$S[,2], main = "First two latent features", 
     xlab = "First feature", ylab = "Second feature", col = "maroon4")
