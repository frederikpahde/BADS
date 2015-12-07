plotHists <- function(df, amountHistsPerPage){
  for (i in 0:(ceiling(length(df)/amountHistsPerPage)-1)) {
    i1 = i*amountHistsPerPage+1
    i2 = (i+1)*amountHistsPerPage
    
    if (i2 > length(df)){
      i2 = length(df)
    }
    hist.data.frame(df[,i1:i2])
    readkey()
  }
}

