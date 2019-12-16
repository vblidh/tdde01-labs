library(neuralnet)
set.seed(1234567890)
RNGversion("3.5.1")
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1,1) # Your code here
for (i in 1:10){
  nn = neuralnet(Sin~Var, data = tr, threshold = i/1000, startweights = 
                   winit, hidden = c(10))
  pred = compute(nn, covariate = va$Var)
}
