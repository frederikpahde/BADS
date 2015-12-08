dir <- Sys.getenv('BADS_Path')   
#C:/Users/D059348/dev/HU/BADS
#set working directory as ../Code

dir <-getwd()
source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 

#Load Data
source(paste0(dir, "/Code/DataLoader.R"))
trainingset = getTrainigset(dir)

#Exploratory Data Analysis
#churn =1
subsetData_churn_true<-trainingset[trainingset$churn==1,]
#churn = 0
subsetData_churn_false<-trainingset[trainingset$churn==0,]



hist(trainingset$adults, main = "Number of Adults")
hist(trainingset$age1, col="blue", main = "Age first household member")
hist(trainingset$age2, col="red", add=TRUE)
legend("topright", c("First Household Member","Second Household Member"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))


numericVariables = getNumericVariables(trainingset)
categoricVariables <- trainingset[setdiff(colnames(trainingset), colnames(numericVariables))]

#plots hists of all 138 numeric variables
#plotHists(numericVariables, 5)

completeInds <- complete.cases(t(numericVariables))
completeCases = numericVariables[completeInds]
corrplot(cor(completeCases))



#identify highly corelated coplete veriables (only numeric)



correlationMatrix <- cor(completeCases[,])
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
#verbose=TRUE
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
head(completeCases[,highlyCorrelated])
#delete highly corelated columns
cleanedDataCoplete<-completeCases[,-highlyCorrelated]

#remove highly corelated from the original data
colunmNames<-colnames(completeCases)[highlyCorrelated]
cleanedOriginalData<-trainingset[,!(names(trainingset) %in% colunmNames)]



