# Importance of splitting the data in 3 categories:
# train, test and validation

# Performance on a second set of sample data is
# itself a random variable.

# Example of coin flipping with probability of 0.5

# Because of variance and the fact we pick
# the best model, validation performance is a biased estimator of the
# performance of the selected model.

# source explaining the differene on test and validation datasets:
# https://machinelearningmastery.com/difference-test-validation-datasets/


# n - number of models
coinFlippers=function(n=20) {
  # population is a coin
  cases=c("H","T")
  # replace = True, because sample larger than population
  train=sample(cases,10,T)
  valid=sample(cases,10,T)
  test=sample(cases,10,T)

  # create 20 different models
  models=lapply(1:n,function(i){
    list(valid=sample(cases,10,T),test=sample(cases,10,T))
  })

  # accuracy of the model in the valid set or development set
  valid_accuracy=sapply(models,function(model){
    length(which(model$valid==valid))/length(valid)
  })
  hist(valid_accuracy,main="Accuracy of models on validation data.",xlab="Validation Accuracy")

  ordered_valid_accuracy=order(valid_accuracy,decreasing=T)
  valid_accuracy[ordered_valid_accuracy]

  cat("The best model was model number",ordered_valid_accuracy[1],"with",valid_accuracy[ordered_valid_accuracy[1]],"accuracy.")

  # test the accuracy in the test set
  test_accuracy=length(which(models[[ordered_valid_accuracy[1]]]$test==test))/length(test)

  cat("The best model had accuracy ",test_accuracy,"on the test data (vs",valid_accuracy[ordered_valid_accuracy[1]],"on the validation data).")
}


# Explnation of bias in multiple runs.
coinFlippers2=function(m=1000,n=20) {
  res=sapply(1:m,function(run) {
    cases=c("H","T")
    train=sample(cases,10,T)
    valid=sample(cases,10,T)
    test=sample(cases,10,T)
    
    models=lapply(1:n,function(i){
      list(valid=sample(cases,10,T),test=sample(cases,10,T))
    })
    
    valid_accuracy=sapply(models,function(model){
      length(which(model$valid==valid))/length(valid)
    })
    
    ordered_valid_accuracy=order(valid_accuracy,decreasing=T)
    valid_accuracy[ordered_valid_accuracy]
    test_accuracy=length(which(models[[ordered_valid_accuracy[1]]]$test==test))/length(test)
    # create order collection
    c(valid_accuracy[ordered_valid_accuracy[1]],test_accuracy)
  })
  
  par(mfrow=c(1,2))
  hist(res[1,],breaks=seq(0,1,.1),main=paste("Selected Model\nValidation Error\nMean:",mean(res[1,])),xlab="Correct")
  hist(res[2,],breaks=seq(0,1,.1),main=paste("Selected Model\nTest Error\nMean:",mean(res[2,])),xlab="Correct")
  par(mfrow=c(1,1))
}

