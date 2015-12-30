handle.highly.correlated.for.Matrix<-function(data,cutoff, exclude){
  as<-as.vector(sapply(data,is.numeric))
  correlationMatrix <- cor(data[as])
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=cutoff, verbose = FALSE)
  highlyCorrelated<-setdiff(highlyCorrelated, exclude)
  data<-data[,-highlyCorrelated]
  return (data)
}