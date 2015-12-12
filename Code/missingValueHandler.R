excludeSparseData<-function(df, threshold){
  missingValueRates <- sapply(df, getMissingValueRate)
  return(df[missingValueRates<threshold])
}

getImputedData<-function(data){
  data <- excludeSparseData(data, 0.5)
  imputed <- c()
  for (i in 1:ncol(data)) {
    col <- Hmisc::impute(data[1:5000,i], fun=median)
    imputed <- c(imputed, col)
  }
  df <- data.frame(matrix(imputed, ncol = ncol(data)))
  colnames(df) <- colnames(data)
  return(df)
}