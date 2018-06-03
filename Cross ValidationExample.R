# hyper parameters -> parameters of the algorithm generate the models
# examples
#   - order of polynomial regression
#   - number of hidden nodes in neural network

crossValidationExample = function() {
  require(stats)
  set.seed(1)
  n <- 10000
  # create a synthetic x vs y
  x <- runif(n, -2, 12)
  y <- (0.2 * x^4) - (3 * x^3) + (8 * x^2) - (15 * x) + 10 + rnorm(n, 0, 80)
  dataset <- data.frame(X=x, Y=y)
  
  # 2 way split
  rand_indices = sample(nrow(dataset))
  train = dataset[rand_indices[1:8000],]
  test = dataset[rand_indices[8001:10000],]
  
  # How cross-validation works:
  # Polynomial regression models of 3,4,5,6 using 10-fold cross-validation
  # 10 models of each order
  # 1st: train on 801:8000 test on 1:800
  # 2nd: train on 1:800, 1601:8000 test on 801:1600
  # last: train on 1:7200 test on 7201:8000
  # sum the square error of 10 models
    
  # polynomial regression 
  tses_ = sapply(3:6, function(order){
    firsts = (0:9*800) + 1
    lasts = 1:10*800
    sapply(1:10, function(i) {
      train_ = train[-(firsts[i]:lasts[i]),]
      test_ = train[firsts[i]:lasts[i],]
      model = lm(Y~poly(X, order), train_)
      p = predict(model, test_)
      sum((p-test_$Y)^2)
    })
  })
  tses = colSums(tses_)
    
  # which order lowest total SE
  best = (3:6)[which.min(tses)]  
  
  # create model with the lowest total SE
  model = lm(Y~poly(X,best), train)
  
  # MEAN square error
  mse = tses[which.min(tses)]/8000
  
  # use model to estimate
  p = predict(model, test)
  
  # calculate MSE and standard deviation
  mse = mean((p-test$Y)^2)
  sd = sd(p - test$Y)
  
  smoothScatter(dataset, main="Smooth Scatter")
  
  xseq = seq(-2, 12, .1)
  p = predict(model, data.frame(X=xseq))
  # condifence intervals based on the standard deviation
  upper = p+2*sd
  lower = p-2*sd
  points(xseq, p, type = "l", col="black")
  points(xseq, upper, type = "l", col="red")
  points(xseq, lower, type = "l", col="red")
}