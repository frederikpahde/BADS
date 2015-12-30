getAccuracy <- function(model, testSet){
  pred <- predict(model, newdata=testSet, type="response")
  class <- round(pred)
  confustionTable <- CrossTable(testSet$churn, class, prop.c=FALSE)$t
  return((confustionTable[1,1]+confustionTable[1,2])/length(y))
}

ModelPerformance <- function(y, yhat, cutoff=0.5){
  ##Brier Score
  #ynum <- as.numeric(y)-1
  #bs <- (1/length(y))*sum((ynum-yhat)^2)
  
  #Classification Error
  c <- factor(yhat >= cutoff, labels = c("good", "bad"))
  #c <- as.numeric(yhat >= cutoff)
  errorRate <- 1- sum(as.numeric(c == y))/length(y)
  
  return(errorRate)
}

ctrl <- trainControl(method="cv", number = 10, classProbs = TRUE)

trainNnet <- function(data.tr){
  nnGrid <- expand.grid(size=c(2,5,9), decay=c(.1,1,10))
  nn.tune <- train(churn~., data = data.tr, method="nnet", trControl = ctrl, tuneGrid=nnGrid)
  return(nn.tune)
}

trainRandomForest <- function(data.tr){
  rfGrid <- expand.grid(mtry=c(3,5,7,9))
  rf.tune <- train(churn~., data = data.tr, method="rf", trControl = ctrl, tuneGrid=rfGrid)
  return(rf.tune)
}

trainBayes <- function(data.tr){
  bayes.tune <- train(churn~., data = data.tr, method="bayesglm", trControl = ctrl)
  return(bayes.tune)
}

trainLogisticRegression <- function(data.tr){
  bayes.tune <- train(churn~., data = data.tr, method="glm", trControl = ctrl)
  return(bayes.tune)
}