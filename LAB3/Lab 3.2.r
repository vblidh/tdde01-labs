library(neuralnet)
set.seed(1234567890)
RNGversion("3.6.1")
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1,1) # Your code here

mse <- function(pred, obs){
  return (mean((obs-pred)^2))
}
MSE = c(1:10)
for (i in 1:10){
  nn = neuralnet(Sin~Var, data = tr, threshold = i/1000, startweights = 
                   winit, hidden = c(10))
  pred = predict(nn, newdata = va)
  MSE[i] = mse(pred, va$Sin)
}

plot(1:10, MSE)
