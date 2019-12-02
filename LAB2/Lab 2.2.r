library("tree")
library("readxl")
set.seed(12345)
RNGversion('3.5.1')
setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB2")

data = data.frame(read_excel("creditscoring.xls"))
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

tree.deviance = tree(good_bad~., data=train, split = c("deviance"))

tree.gini = tree(good_bad~., data=train, split = c("gini"))

tree.deviance.pred = predict(tree.deviance, type="class")
