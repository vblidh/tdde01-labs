library("MASS")
library("ggplot2")
set.seed(12345)
RNGversion('3.5.1')
setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB2")

data = read.csv("australian-crabs.csv")

ggplot(data=data, mapping = aes(CL, RW, color=sex)) + geom_point()

lda.fit = lda(formula=sex~RW + CL, data=data)
lda.pred = predict(lda.fit, data)
ggplot(data=data, mapping=aes(CL, RW, color=lda.pred$class)) + geom_point()
