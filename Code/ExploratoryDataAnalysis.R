createUsefulPlots <- function(trainingset, numericVariables, categoricVariables){
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
