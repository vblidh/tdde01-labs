data <- read_excel("spambase.xlsx");
# setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB1")

n <- dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train = data[id,]
test = data[-id,]

Y = as.numeric(sapply(train, function(x) x > 0.5))
YY = lapply(train, function(x) x > 0.5)


X = c(length(Y))
g = glm(Y~X, data=train)
pred = predict.glm(g)

summary(g)