excludeSparseData<-function(df, threshold){
  missingValueRates <- sapply(df, getMissingValueRate)
  return(df[missingValueRates<threshold])
}

getImputedData<-function(data){
  data <- handleDefaultValues(data)
  data <- excludeSparseData(data, 0.5)
  list <- list(NULL)
  for (i in 1:ncol(data)) {
    if (is.numeric(data[,i])){
      col <- Hmisc::impute(data[,i], fun = median)
    }else{
      col <- Hmisc::impute(data[,i], fun = Mode)
    }
    list[[i]] <- col
  }
  df <- as.data.frame(list)
  colnames(df) <- colnames(data)
  return(df)
}

handleDefaultValues<-function(data){
  data$age1 = setNA(data$age1, 0)
  data$age2 = setNA(data$age2, 0)
  data$kid0_2 = setNA(data$kid0_2, 'U')
  data$kid3_5 = setNA(data$kid3_5, 'U')
  data$kid6_10 = setNA(data$kid6_10, 'U')
  data$kid11_15 = setNA(data$kid11_15, 'U')
  data$kid16_17 = setNA(data$kid16_17, 'U')
  data$car_buy = setNA(data$car_buy, 'UNKNOWN')
  data$dualband = setNA(data$dualband, 'U')
  data$hnd_webcap = setNA(data$hnd_webcap, 'UNKW')
  data$marital = setNA(data$marital, 'U')
  data$new_cell = setNA(data$new_cell, 'U')
  data$cartype = setNA(data$cartype, '')
  return(data)
}