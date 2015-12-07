dir <- Sys.getenv("BADS_Path")   #C:/Users/D059348/dev/HU/BADS

source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 

#Load Data
source(paste0(dir, "/Code/DataLoader.R"))
trainingset = getTrainigset(dir)

#Exploratory Data Analysis
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

