getAccuracy <- function(model, testSet){
  pred <- predict(model, newdata=testSet, type="response")
  class <- round(pred)
  confustionTable <- CrossTable(testSet$churn, class, prop.c=FALSE)$t
  return((confustionTable[1,1]+confustionTable[1,2])/length(y))
}