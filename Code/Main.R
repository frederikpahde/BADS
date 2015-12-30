dir <- Sys.getenv('BADS_Path')   

source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 

#Load Data
source(paste0(dir, "/Code/DataLoader.R"))
#Imputed Data is loaded
  #trainingset_orig = getTrainigset(dir)
  #numericVariables = getNumericVariables(trainingset)
  #categoricVariables <- trainingset[setdiff(colnames(trainingset), colnames(numericVariables))]
continousVariablesname <- getContinousset(dir)

#Exploratory Data Analysis
source(paste0(dir, "/Code/ExploratoryDataAnalysis.R"))
#createUsefulPlots(trainingset, numericVariables, categoricVariables)
print("Loaded Dataset")

##Missing Value Handling
source(paste0(dir,"/Code/missingValueHandler.R"))
  #trainingset <- getImputedData(trainingset)
  #numericVariables = getNumericVariables(trainingset)
  #categoricVariables <- trainingset[setdiff(colnames(trainingset), colnames(trainingset))]
  #write.csv(trainingset, paste0(dir, "/Data/ImputedData.csv"), sep = ",")

print("Finished Missing Value Handling")
trainingset <- loadImputedTrainingset(paste0(dir, "/Data/ImputedData.csv"))
numericVariables = getNumericVariables(trainingset)
categoricVariables <- trainingset[setdiff(colnames(trainingset), colnames(numericVariables))]

#Outlier Handling
source(paste0(dir, "/Code/Outliers.R"))
#z-score one-dimentional outlier handling
trainingset_withoutOutlier<- handle.Outliers.for.Matrix(trainingset)
print("Finished Outlier Handling")

#Data scaling with z-score
source(paste0(dir, "/Code/scaling.R"))
#traingsset
trainingset <- z.scale.data(m=trainingset,continous.var=continousVariablesname)
#traingsset_withoutOutlier
trainingset_withoutOutlier<- z.scale.data(m=trainingset_withoutOutlier,continous.var=continousVariablesname)
print("Finished Scaling")

#Corelation
#identify highly corelated coplete veriables (only numeric)
#correlationMatrix <- cor(trainingset)
#correlationMatrix2 <- cor(trainingset_withoutOutlier)
##summary(correlationMatrix[upper.tri(correlationMatrix)])
# find attributes that are highly corrected (ideally >0.75)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.90, verbose = FALSE)
#highlyCorrelated2 <- findCorrelation(correlationMatrix2, cutoff=0.90, verbose = FALSE)
#delete highly corelated columns
#trainingset<-trainingset[,-highlyCorrelated]

#trainingset_withoutOutlier<-trainingset_withoutOutlier[,-highlyCorrelated2]

#Feature selection
# source(paste0(dir, "/Code/FeatureSelection.R"))
# new dataset only containing selected features
selectedFeatures <- getSelectedFeatureSet(dir)
selectedFeatures <- c(as.vector(selectedFeatures[,1]), "churn")

trainingset <- trainingset[,selectedFeatures]
trainingset_withoutOutlier <- trainingset_withoutOutlier[,selectedFeatures]
print("Finished Feature Selection")

#Split to test/trainigsset
idx.train <- createDataPartition(y = trainingset$churn, p=0.7, list=FALSE)
data.tr <- trainingset[idx.train,]
data.ts <- trainingset[-idx.train,]

data.tr_wo <- trainingset_withoutOutlier[idx.train,]
data.ts_wo <- trainingset_withoutOutlier[-idx.train,]

#Train Models
source(paste0(dir, "/Code/ModelTrainer.R"))
nnet <- trainNnet(data.tr_wo)
print("Finished NNET Training")
bayes <- trainBayes(data.tr)
print("Finished Bayes Training")
lr <- trainLogisticRegression(data.tr)
print("Finished Logistic Regression")
rf <- trainRandomForest(data.tr)
print("Finished Random Forest")

#Predict Test Set
yhat.nnet <- predict(nnet, newdata = data.ts_wo, type="prob")[,2]
yhat.bayes <- predict(bayes, newdata = data.ts, type="prob")[,2]
yhat.lr <- predict(lr, newdata = data.ts, type="prob")[,2]
yhat.rf <- predict(rf, newdata = data.ts, type = "prob")[,2]

#Assess Models
err.nnet <- ModelPerformance(data.ts_wo$churn, yhat.nnet)
err.bayes <- ModelPerformance(data.ts$churn, yhat.bayes)
err.lr <- ModelPerformance(data.ts$churn, yhat.lr)
err.rf <- ModelPerformance(data.ts$churn, yhat.rf)

print(paste0("ErrorRate (nnet): ", err.nnet))
print(paste0("ErrorRate (bayes): ", err.bayes))
print(paste0("ErrorRate (lr): ", err.lr))
print(paste0("ErrorRate (rf): ", err.rf))

##10 Fold CV for Logistic Regression
#n <- 10
#dataset = completeCases[1:50000,]
#setSize = round(dim(dataset)[1]/n)
#shuffledDataset <- dataset[sample(nrow(dataset)),]
#avg=0
#for (i in 1:n) {
#  print(i)
#  inds.test = ((i-1)*setSize+1):((i-1)*setSize+setSize)
#  inds.training = setdiff(1:nrow(dataset), inds.test)
#  test = shuffledDataset[inds.test,]
#  training = shuffledDataset[inds.training,]
#  lr<-glm(churn~.,data=training,family=binomial(link="logit"))
#  avg = avg + getAccuracy(model = lr, testSet = test)
#}

#rf <- randomForest(churn~.,data=completeCases[1:1000,],ntree=500, mtry=3)
#res = round(predict(rf, newdata = completeCases[1001:2000,]))
#y = completeCases[1001:2000,]
#CrossTable(y$churn, res, prop.c=FALSE)$t#



#Corelation
###############################################################################################
#identify highly corelated coplete veriables (only numeric)
#correlationMatrix <- cor(completeCases[,])
#print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
#verbose=TRUE
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
#print(highlyCorrelated)
#head(completeCases[,highlyCorrelated])
#delete highly corelated columns
#cleanedDataCoplete<-completeCases[,-highlyCorrelated]

#remove highly corelated from the original data
#colunmNames<-colnames(completeCases)[highlyCorrelated]
#cleanedOriginalData<-trainingset[,!(names(trainingset) %in% colunmNames)]


#HANDLING OUTLIERS
#completeCases has 91 variables
#find variables which must contain outliers
#difference.Median.Median<-abs(apply(completeCases,2, function(x) median(x)-mean(x)))
#st.d<-apply(completeCases,2,sd)
#indicies <- which(difference.Median.Median>apply(completeCases,2,median)/2)
#summary(completeCases[,indicies])


###########################################################################

#difference.Median.Median<-abs(apply(completeCases,2, function(x) median(x)-mean(x)))
#st.d<-apply(completeCases,2,sd)
#indicies <- which(difference.Median.Median>apply(completeCases,2,median)/2)
#summary(completeCases[,indicies])

#summary(trainingset[,50:78])


