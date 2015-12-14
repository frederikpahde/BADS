set.Outliers.To.Median <- function(variable, whiskerFactor ){
  
  quantiles<-quantile(variable,  probs = c(0.25, 0.75))
  IQA=quantiles["75%"]-quantiles["25%"]
  obere.antene<-quantiles["75%"]+IQA*whiskerFactor
  untere.antene<-quantiles["25%"]-IQA*whiskerFactor
  if(untere.antene<0){
    not.legal.Values.Indicies<-which(variable<quantiles["25%"], variable>obere.antene)
  }else{
    not.legal.Values.Indicies<-which(variable<untere.antene, variable>obere.antene) 
  }
  variable[not.legal.Values.Indicies]<-median(variable)
  return (variable)
}

# question: don't we get too many outliers by using whiskerFactor=1.5? Maybe some of them aren't even outliers

#high dimensional data
#install.packages("HighDimOut")
#library(HighDimOut)

#test<-cleanedDataCoplete[1:20,1:25]

#tim1<-Func.ABOD(test,basic=FALSE, .9) 
#braucht bei mir ewig f?r 0.01 prozent im 3D SPACE mit allen Obs (nach 1h noch kein Ergebnis)
#angle-based outlier detection (ABOD) algorithm, calculates the outlier due to there angel base
#small value = outlier
#high value = no outlier;explanation on the LMU slides

#plot(tim1)
#max(tim1)


#########################################################################
install.packages("HighDimOut")
library(HighDimOut)


test<-cleanedDataCoplete[1:100,1:37]
tim2<- Func.FBOD(test, iter=10, k.nn=37) # cannot allocate a vector of 3.5 Gb
#tim is calculated with LOF
# value <<1 the point is in the cluster; value >> the point is far away from cluster

plot(tim2)
#tim2 shows much better solutions than tim1!!!
#WHY? Berechnet mit LOF, zwar nicht mit Angel; dennoch st?rker f?r die Anwendung (erster Eindruck)


show.outlier<-tim2[tim2>=2] # TRUE/FALSE Vector which shows all the outliers (value 2 is manuel, lets discuss this)

test[tim2>=2,]

row.index<-which((tim2>=2)==TRUE)

for(i in 1:length(row.index)){
  paste("Potential outlier at row", row.index[i], sep = " ")
}

class(tim2) #numeric
typeof(tim2) #double
length(tim2) #50, value for all observations
tim[1:3]


###########################################################################################

# detect outliers by chi-squared-test
# very many outliers detected, output not easy to use

install.packages("extremevalues")
library("extremevalues")

K <- getOutliers(completeCases$blck_vce_Mean, method="I", rho=c(0.01,0.01))
outlierPlot(completeCases$blck_vce_Mean,K,mode="qq")
