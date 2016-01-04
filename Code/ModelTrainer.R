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

ModelPerformanceByClass <- function(y, yhat){
  errorRate <- 1- sum(as.numeric(yhat == y))/length(y)
  return(errorRate)
}

ctrl <- trainControl(method="cv", number = 10, classProbs = TRUE)

trainNnet <- function(data.tr){
  nnGrid <- expand.grid(size=c(2,5,9), decay=c(.1,1,10))
  nn.tune <- train(churn~., data = data.tr, method="nnet", trControl = ctrl, tuneGrid=nnGrid, trace=FALSE)
  return(nn.tune)
}

trainRandomForest <- function(data.tr){
  rfGrid <- expand.grid(mtry=c(7,9,11,15))
  rf.tune <- train(churn~., data = data.tr, method="rf", trControl = ctrl, tuneGrid=rfGrid)
  return(rf.tune)
}

trainNaiveBayes <- function(data.tr){
  nbGrid <- expand.grid(laplace=c(0,1), useKernel=c("TRUE", "FALSE"))
  bayes.tune <- train(churn~., data = data.tr, method="nb", trControl = ctrl, trace=TRUE)
  return(bayes.tune)
}

trainKNN <- function(data.tr){
  knnGrid <- expand.grid(k=c(3,5,7,9,11,13,15,17))
  knn.tune <- train(churn~., data = data.tr, method="knn", trControl = ctrl, tuneGrid=knnGrid)
  return(knn.tune)
}

trainSVM <- function(data.tr){
  svmGrid <- expand.grid(gamma=c(2), cost=c(0.01,0.1,0.2,0.3))
  svm.tune <- train(churn~., data = data.tr, method="svmLinear2", trControl = ctrl, tuneGrid=svmGrid)
  return(svm.tune)
}

trainJ48 <- function(data.tr){
  J48Grid <- expand.grid(C=c(0.1,0.2,0.3,0.4,0.5))
  J48.tune <- train(churn~., data = data.tr, method="J48", trControl = ctrl, tuneGrid=J48Grid)
  return(J48.tune)
}

trainAdaBag <- function(data.tr){
  J48Grid <- expand.grid(C=c(0.1,0.2,0.3,0.4,0.5))
  adaBag.tune <- train(churn~., data = data.tr, method="AdaBag", trControl = ctrl)
  return(adaBag.tune)
}

trainLogisticRegression <- function(data.tr){
  bayes.tune <- train(churn~., data = data.tr, method="glm", trControl = ctrl)
  return(bayes.tune)
}

#Ensemble Random Forest, Logistic Regression and SVMs
#trainEnsembledMethod <- function(data.tr){
#  rfGrid <- expand.grid(mtry=c(5,7,9,11))
#  svmGrid <- expand.grid(gamma=c(2,3,4), cost=c(0.01,0.1,0.2,0.3,0.4,0.5))
#  ensembledModel <- caretList(
#    churn~., data = data.tr, trControl = ctrl, methodList=c("rf", "svmLinear2", "glm")
#  )
#}