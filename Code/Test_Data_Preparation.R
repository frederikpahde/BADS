dir <- Sys.getenv('BADS_Path')   

#setwd("~/Documents/HU_Berlin/WI_1516/BADS/Aufgabe/BADS")
#dir<-getwd()

source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 

#Load Data
source(paste0(dir, "/Code/DataLoader.R"))
test_set = getTestset(dir)
numericVariables = getNumericVariables(test_set)
categoricVariables <- test_set[setdiff(colnames(test_set), colnames(numericVariables))]
continousVariablesname <- getContinousset(dir)
print("Loaded Dataset")

##Missing Value Handling
source(paste0(dir,"/Code/missingValueHandler.R"))
#test_set <- getImputedData(test_set)
#numericVariables = getNumericVariables(test_set)
#categoricVariables <- test_set[setdiff(colnames(test_set), colnames(test_set))]
#write.csv(test_set, paste0(dir, "/Data/ImputedData_testSet.csv"), sep = ",")
test_set <- loadImputedTestSet(paste0(dir, "/Data/ImputedData.csv"))
#numericVariables = getNumericVariables(test_set)
#categoricVariables <- test_set[setdiff(colnames(test_set), colnames(numericVariables))]
print("Finished Missing Value Handling")
#test_set <- test_set[sample(1:50000,5000, replace = FALSE),]

#Outlier Handling
source(paste0(dir, "/Code/Outliers.R"))
#z-score one-dimentional outlier handling
test_set_withoutOutlier<- handle.Outliers.for.Matrix(test_set)
print("Finished Outlier Handling")

#Data scaling with z-score
source(paste0(dir, "/Code/scaling.R"))
test_set <- z.scale.data(m=test_set,continous.var=continousVariablesname)
test_set_withoutOutlier<- z.scale.data(m=test_set_withoutOutlier,continous.var=continousVariablesname)
print("Finished Scaling")

source(paste0(dir, "/Code/ModelTrainer.R"))
selectedFeatures <- getSelectedFeatureSet(dir)
selectedFeatures <- c(as.vector(selectedFeatures[,1]))
test_set <- test_set[,selectedFeatures]


source(paste0(dir, "/Code/PCA.R"))
#test_set<-rebuild_components_for_test_set(test_set)
print("Data is ready")

