library("MASS")
library("ggplot2")
set.seed(12345)
#RNGversion('3.5.1')
#setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB2")
#2.1

data = read.csv("australian-crabs.csv")
p1 = ggplot(data=data, mapping = aes(CL, RW, color=sex)) + geom_point() +
  ggtitle("Original graph")

p1 + theme(plot.title = element_text(color="black", face="bold", hjust = 0.5))
#2.2

std.ldafit = lda(formula=sex~RW + CL, data=data)
std.ldapred = predict(std.ldafit, data)
p2 = ggplot(data=data, mapping=aes(CL, RW, color=std.ldapred$class)) + 
  geom_point() + ggtitle("Predicted graph without priors")
p2 + theme(plot.title = element_text(color="black", face="bold", hjust = 0.5))

std.missclass_e = mean(std.ldapred$class != data$sex)

#2.3
prior.ldafit = lda(formula=sex~RW + CL, data=data, prior=c(0.1, 0.9))
prior.ldapred = predict(prior.ldafit, data)
p3 = ggplot(data=data, mapping = aes(CL, RW, color=prior.ldapred$class)) + 
  geom_point()+ggtitle("Predicted graph with priors p(Male)=0.9, p(Female)=0.1")
p3 + theme(plot.title = element_text(color="black", face="bold", hjust = 0.5))
prior.missclass_e = mean(prior.ldapred$class != data$sex)

#2.4
glm.fit = glm(formula=sex~RW+CL, family = binomial, data = data)
glm.pred = ifelse(predict(glm.fit, data, type="response")>0.5, "Male", "Female")
p4 = ggplot(data=data, mapping=aes(CL, RW, color=glm.pred)) + 
  geom_point() + ggtitle("GLM predicted graph")
p4 + theme(plot.title = element_text(color="black", face="bold", hjust = 0.5))
glm.missclass_e = mean(glm.pred != data$sex)


