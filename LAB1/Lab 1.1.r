library("readxl")
library("kknn")
setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB1")
data <- read_excel("spambase.xlsx");

n <- dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train = data[id,]
test = data[-id,]

#2.
#Training data 
g = glm(Spam~.,data=train)
pred = ifelse(predict(g)>0.5,1,0)
conf_matrix = table(pred, unlist(train[,49]))
mrate = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))

#Test data
pred = ifelse(predict(g, newdata=test)>0.5,1,0)
conf_matrix = table(pred, unlist(test[,49]))
mrate = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))
conf_matrix

#3.
#Training data
g = glm(Spam~.,data=train)
pred = ifelse(predict(g)>0.8,1,0)

conf_matrix = table(pred, unlist(train[,49]))
mrate = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))

# Test data
pred = ifelse(predict(g, newdata = test)>0.8,1,0)
conf_matrix = table(pred, unlist(test[,49]))
mrate = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))

#4
kknn.k30.test = kknn(formula=as.factor(Spam)~., train=train, test=test, k=30)
kknn.k30.train = kknn(formula=as.factor(Spam)~., train=train, test=train, k=30)
conf_matrix=table(train$Spam, kknn.k30.train$fitted.values)
missrate.k30.train = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))
conf_matrix=table(test$Spam, kknn.k30.test$fitted.values)
missrate.k30.test = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))

#5
kknn.k1.train = kknn(as.factor(Spam)~., train=train, test=train, k=1)
kknn.k1.test = kknn(as.factor(Spam)~., train=train, test=test, k=1)
conf_matrix=table(train$Spam, kknn.k1.train$fitted.values)
missrate.k1.train = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))
conf_matrix=table(test$Spam, kknn.k1.test$fitted.values)
missrate.k1.test = 1 - (sum(diag(conf_matrix))/sum(conf_matrix))





