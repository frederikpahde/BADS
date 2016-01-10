#function z.scale() is generating a numeric vector 
#(with z-score = scaled) out of the vector v; same as scale function

#contin.var <- data.frame((read.csv("continous_variablenames.csv",header=TRUE, sep = ";")))

z.scale <- function(v){
  
  m<- mean(v)
  s<- sd(v)
  if(s != 0){
    for(i in 1:length(v)){
      v[i]<-((v[i]-m)/s)
    }
  }
  return(v)
}


#scales continous data out of the set

z.scale.data <- function(m,continous.var){
 
  e <- m
  for(i in 1:length(m)){
    for(k in 1:nrow(continous.var)){
    
      if(tolower((labels(m)[[2]][i]))==tolower(continous.var[k,])){
        m[,i]<-z.scale(m[,i]) 
      }
   }
  }
  return(m)
}
