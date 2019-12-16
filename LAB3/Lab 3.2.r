library(neuralnet)
set.seed(1234567890)

Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1,1) # Your code here

MSE = c(1:10)

for (i in 1:10){
  nn = neuralnet(Sin~Var, data = tr, threshold = i/1000, startweights = 
                   winit, hidden = c(10))
  pred = predict(nn, newdata = va)
  MSE[i] = mean((pred-va$Sin)^2)
}

plot(1:10, MSE, col="Blue", main = "MSE for different thresholds", 
     xlab = "Threshold * 1000")

plot(nn <- neuralnet(Sin~Var, data = tr, threshold = 4/1000, startweights = 
                        winit, hidden = c(10)))
## Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1)
points(trva, col="red")

