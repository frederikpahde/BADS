############### For 1st iteration ################
#behandelt die ganze Matrix
handle.Outliers.for.Matrix<-function(data){
  as<-as.vector(sapply(data,is.numeric))
  #using the simple boxplot method
  #data[,as]<-apply(data[,as],2, function(x)  x<-set.Outliers.To.Antene(x,1.5))
  
  data[,as]<-apply(data[,as],2, function(x)  x<-z_score.transformatin(x))
  
  return (data)
}

#Boxplot outlier handling
set.Outliers.To.Antene <- function(variable, whiskerFactor ){
  
  quantiles<-quantile(variable,  probs = c(0.25, 0.75))
  IQA=quantiles["75%"]-quantiles["25%"]
  obere.antene<-quantiles["75%"]+IQA*whiskerFactor
  untere.antene<-quantiles["25%"]-IQA*whiskerFactor
  
    not.legal.Up.Indicies<-which(variable>obere.antene) 
    not.legal.Down.Indicies<-which(variable<untere.antene)
    variable[not.legal.Up.Indicies]<-obere.antene
    variable[not.legal.Down.Indicies]<-untere.antene

  return (variable)
}

z_score.transformatin<-function(variable){
  mE<-mean(variable)
  sD<-sd(variable)
  z_scores<-sapply(variable, function(x) zscore<-(x-mE)/sD) 
  variable[which(z_scores>3)]<-mE+(3*sD)
  variable[which(z_scores<(-3))]<-mE-(3*sD)
  
  return(variable)
}

############### For 2nd iteration ################
#multivariate outliers - combinations of several variables
# based on the mahalanobis distance

#multidimentional outlier handling
my.dataframe<-trainingset[1:100,2:3]
#function from Outliers.R
#detect.outliers.mahaladonis(my.dataframe, .975)
#use mvoutlier package
outlier.plot<-aq.plot(my.dataframe, delta=qchisq(0.975, df=ncol(my.dataframe)), quan=1/2, alpha=0.05)
outliers<-outlier.plot$outliers


#returns the outliers indizies of a data frame with the corresponding mahalanobis distance (data is a dataframe, 0<chi.quantil<1)
detect.outliers.mahaladonis<-function(data, chi.quantil){
#calculate mahalanobis distance for all samples  
m.dist<-mahalanobis(data, colMeans(data), cov(data))
quantile<-qchisq(chi.quantil, df=length(data[1,])) 
outliers.inizies<-m.dist[m.dist>quantile]
return(outliers.inizies)

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
#install.packages("HighDimOut")
#library(HighDimOut)


#test<-cleanedDataCoplete[1:100,1:37]
#tim2<- Func.FBOD(test, iter=10, k.nn=37) # cannot allocate a vector of 3.5 Gb
#tim is calculated with LOF
# value <<1 the point is in the cluster; value >> the point is far away from cluster

#plot(tim2)
#tim2 shows much better solutions than tim1!!!
#WHY? Berechnet mit LOF, zwar nicht mit Angel; dennoch st?rker f?r die Anwendung (erster Eindruck)


#show.outlier<-tim2[tim2>=2] # TRUE/FALSE Vector which shows all the outliers (value 2 is manuel, lets discuss this)

#test[tim2>=2,]

#row.index<-which((tim2>=2)==TRUE)

#for(i in 1:length(row.index)){
#  paste("Potential outlier at row", row.index[i], sep = " ")
#}

#class(tim2) #numeric
#typeof(tim2) #double
#length(tim2) #50, value for all observations
#tim[1:3]


###########################################################################################

# detect outliers by chi-squared-test
# very many outliers detected, output not easy to use

#install.packages("extremevalues")
#library("extremevalues")

#K <- getOutliers(completeCases$blck_vce_Mean, method="I", rho=c(0.01,0.01))
#outlierPlot(completeCases$blck_vce_Mean,K,mode="qq")
