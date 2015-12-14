

set.Outliers.To.Mean <- function(variable, whiskerFactor ){
  
  quantiles<-quantile(variable,  probs = c(0.25, 0.75))
  IQA=quantiles["75%"]-quantiles["25%"]
  obere.antene<-quantiles["75%"]+IQA*whiskerFactor
  untere.antene<-quantiles["25%"]-IQA*whiskerFactor
  if(untere.antene<0){
    not.legal.Values.Indicies<-which(variable<quantiles["25%"], variable>obere.antene)
  }else{
    not.legal.Values.Indicies<-which(variable<untere.antene, variable>obere.antene) 
  }
  variable[not.legal.Values.Indicies]<-mean(variable)
  return (variable)
}

# question: don't we get too many outliers by using whiskerFactor=1.5? Maybe some of them aren't even outliers
