createUsefulPlots <- function(trainingset, numericVariables, categoricVariables){
  
  #plots hists of all 138 numeric variables
  plotHists(numericVariables, 5)
  hist(trainingset$adults, main = "Number of Adults")
  hist(trainingset$age1, col="blue", main = "Age first and second household member")
  hist(trainingset_orig$age2, col="red", add=TRUE, main = "Age first and second household member")
  hist(numericVariables$adjrev, col="red",main = "Billing adjusted total revenue over the life of the customer")
  legend("topright", c("First Household Member","Second Household Member"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))
  boxplot(numericVariables$adjrev[numericVariables$adjrev],main = "Billing adjusted total revenue over the life of the customer")
  boxplot(numericVariables$adjrev,range = 3, main = "Billing adjusted total revenue over the life of the customer")
  plot( x =numericVariables$avgmou , y=numericVariables$months , main="", xlab="Average monthly minutes of use over the life of the customer ", ylab="Total number of month in service", pch=19)
  plot( x =numericVariables$months , y=numericVariables$drop_dat_Mean, main="", xlab="Total number of month in service", ylab="Mean number of dropped (failed) data calls", pch=19)
  plot( x =numericVariables$churn , y=numericVariables$drop_dat_Mean, main="", xlab="Total number of month in service", ylab="Mean number of dropped (failed) data calls", pch=19)
  
  
  library(rgl)
  #plot( x =numericVariables$totcalls, y=numericVariables$totrev,  main="", xlab="Total number of calls", ylab="Total revenue", pch=19)
  #plot( x =numericVariables$blck_vce_Mean, y=numericVariables$totrev,  main="", xlab="Mean number of blocked (failed) voice calls", ylab="Total revenue", pch=19)
  #plot( x =numericVariables$blck_vce_Mean+numericVariables$blck_dat_Mean, y=numericVariables$churn,  main="", xlab="Mean number of blocked (failed) voice calls", ylab="Churn", pch=19)
  #plot( x =numericVariables$custcare_Mean, y=numericVariables$churn,  main="", xlab="Mean number customer care calls", ylab="Churn", pch=19)
  #plot( x =numericVariables$totrev, y=numericVariables$churn,  main="", xlab="Mean number customer care calls", ylab="Churn", pch=19)
  #sapply(colnames(numericVariables), function(x) plot(x=numericVariables[,x], y=numericVariables$churn, xlab=x, ylab="Churn", pch=19))

  pairs(~numericVariables$rev_Mean+numericVariables$mou_Mean+numericVariables$totmrc_Mean+numericVariables$da_Mean,data=numericVariables, main="Simple Scatterplot Matrix")
  pairs(~rev_Mean+totrev+totmrc_Mean+da_Mean,data=numericVariables, main="Scatterplot Matrix")
  corrplot(cor(numericVariables))
  
  #pairs(~income+age1+adults,data=numericVariables,main="Simple Scatterplot Matrix")
  
  
  #churn =1
  subsetData_churn_true<-trainingset[trainingset$churn==1,]
  #churn = 0
  subsetData_churn_false<-trainingset[trainingset$churn==0,]
  
  hist(trainingset$adults, main = "Number of Adults")
  hist(trainingset$age1, col="blue", main = "Age first household member")
  hist(trainingset$age2, col="red", add=TRUE)
  legend("topright", c("First Household Member","Second Household Member"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))
  
  #plots hists of all 138 numeric variables
  plotHists(numericVariables, 5)
  
  corrplot(cor(trainingset))
  
}
