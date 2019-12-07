library("e1071")
library("MASS")
library("tree")
library("readxl")
library("ggplot2")
set.seed(12345)
RNGversion('3.5.1')
#setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB2")
setwd("/home/vikbl327/Courses/TDDE01/tdde01-labs/LAB2")
#data2 = read_excel("creditscoring.xls") Fuck xls files
data = read.csv("creditscoring.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,] 

tree_dev = tree(good_bad~., data=train, split = c("deviance"))
tree_dev.pred.train = predict(tree_dev, newdata=train, type="class")
tree_dev.pred.test = predict(tree_dev, newdata = test, type="class")
tree_dev.cm.train = table(train$good_bad, tree_dev.pred.train)
tree_dev.cm.test = table(test$good_bad, tree_dev.pred.test)
tree_dev.me.train = 1 - sum(diag(tree_dev.cm.train))/(sum(tree_dev.cm.train))
tree_dev.me.test = 1 - sum(diag(tree_dev.cm.test))/(sum(tree_dev.cm.test))


tree_gini = tree(good_bad~., data=train, split = c("gini"))
tree_gini.pred.train = predict(tree_gini, newdata=train, type="class")
tree_gini.pred.test = predict(tree_gini, newdata=test, type="class")
tree_gini.cm.train = table(train$good_bad, tree_gini.pred.train)
tree_gini.cm.test = table(test$good_bad, tree_gini.pred.test)
tree_gini.me.train = 1 - sum(diag(tree_gini.cm.train))/(sum(tree_gini.cm.train))
tree_gini.me.test = 1 - sum(diag(tree_gini.cm.test))/(sum(tree_gini.cm.test))

#3
fit = tree(good_bad~., data=train)
fit$frame
trainScore=rep(0,15)
testScore=rep(0,15)
for(i in 2:15) {
  prunedTree=prune.tree(fit,best=i)
  pred=predict(prunedTree, newdata=valid,
               type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:15, trainScore[2:15], type="b", col="red",
     ylim=c(250,600))
points(2:15, testScore[2:15], type="b", col="blue")

finalTree=prune.tree(fit, best=4)
finalTree.pred.class=predict(finalTree, newdata=test,
             type="class")
finalTree.cm = table(test$good_bad,finalTree.pred.class)
finalTree.me = 1 - sum(diag(finalTree.cm))/(sum(finalTree.cm))

#4
naiveBayes.fit = naiveBayes(good_bad~., data=train)
naiveBayes.pred.train = predict(naiveBayes.fit, newdata=train)
naiveBayes.pred.test = predict(naiveBayes.fit, newdata=test)
naiveBayes.cm.train = table(naiveBayes.pred.train, train$good_bad)
naiveBayes.cm.test = table(naiveBayes.pred.test, test$good_bad)
naiveBayes.me.train = 
  1 - sum(diag(naiveBayes.cm.train))/(sum(naiveBayes.cm.train))
naiveBayes.me.test = 
  1 - sum(diag(naiveBayes.cm.test))/(sum(naiveBayes.cm.test))

#5

Compute_TPR_FPR = function(prediction){
  pi_vector = seq(0.05, 0.95, 0.05)
  TPR_vector = rep(1, length(pi_vector))
  FPR_vector = rep(1, length(pi_vector))
  N.tot = length(which(test$good_bad == "bad"))
  print(N.tot)
  P.tot = length(which(test$good_bad == "good"))
  print(P.tot)
  for (i in 1:length(pi_vector)){
    classification = ifelse(prediction[,2] > pi_vector[i], "good", "bad")
    
    TP = length(which(classification=="good" & test$good_bad == "good"))
    FP = length(which(classification=="good" & test$good_bad == "bad"))
    
    TPR = TP/P.tot
    FPR = FP/N.tot

    TPR_vector[i] = TPR
    FPR_vector[i] = FPR
    
  }
  data.frame("Pi value" = pi_vector,"TPR" = TPR_vector, 
             "FPR" = FPR_vector)
}

naiveBayes.pred.test.numeric = predict(naiveBayes.fit, newdata = test, type="raw")
finalTree.pred.numeric = predict(finalTree, newdata = test)

tree_TPR_FPR = Compute_TPR_FPR(finalTree.pred.numeric)
bayes_TPR_FPR = Compute_TPR_FPR(naiveBayes.pred.test.numeric)

ggplot(data = NULL, aes(col="classifier")) + 
  geom_point(data = tree_TPR_FPR, aes(x=FPR, y=TPR, col="Tree")) + 
  geom_line(data = tree_TPR_FPR, aes(x=FPR, y=TPR, col="Tree")) + 
  geom_point(data = bayes_TPR_FPR, aes(x=FPR, y=TPR, col="Bayes")) + 
  geom_line(data = bayes_TPR_FPR, aes(x=FPR, y=TPR, col="Bayes"))


#6
L = matrix(c(0,10,1,0),nrow = 2, ncol = 2)

naiveBayes.pred.train.numeric = 
  predict(naiveBayes.fit, newdata = train, type="raw")
naiveBayes.pred.test.numeric = 
  predict(naiveBayes.fit, newdata = test, type="raw")

naiveBayes.pred.test.loss = 
  ifelse(L[1,2]*naiveBayes.pred.test.numeric[,2] > 
           L[2,1]*naiveBayes.pred.test.numeric[,1], "good","bad")
naiveBayes.test.loss.cm = table(test$good_bad, naiveBayes.pred.test.loss)

naiveBayes.pred.train.loss = 
  ifelse(L[1,2]*naiveBayes.pred.train.numeric[,2] > 
           L[2,1]*naiveBayes.pred.train.numeric[,1], "good","bad")
naiveBayes.train.loss.cm = table(train$good_bad, naiveBayes.pred.train.loss)
