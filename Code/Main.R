dir <- Sys.getenv('BADS_Path')   

source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 

#Load Data
source(paste0(dir, "/Code/DataLoader.R"))
  trainingset = getTrainigset(dir)
  numericVariables = getNumericVariables(trainingset)
  categoricVariables <- trainingset[setdiff(colnames(trainingset), colnames(numericVariables))]
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
trainingset <- loadImputedTrainingset(paste0(dir, "/Data/ImputedData.csv"))
numericVariables = getNumericVariables(trainingset)
categoricVariables <- trainingset[setdiff(colnames(trainingset), colnames(numericVariables))]
print("Finished Missing Value Handling")
trainingset <- trainingset[1:500,]

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
<<<<<<< HEAD
#identify highly corelated coplete veriables
source(paste0(dir, "/Code/Correlation.R"))
data<-trainingset_withoutOutlier
trainingset_withoutCorrelated<-handle.highly.correlated.for.Matrix(data, .75, 
        which(colnames(data)=="Customer_ID"|colnames(data)=="churn"))

=======
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
>>>>>>> 7ef7e07e425cd8e1ebd4017012efd92031e81143

#Feature selection
# source(paste0(dir, "/Code/FeatureSelection.R"))
# new dataset only containing selected features
selectedFeatures <- getSelectedFeatureSet(dir)
selectedFeatures <- c(as.vector(selectedFeatures[,1]), "churn")

<<<<<<< HEAD
#Split to test/trainigsset
=======
trainingset <- trainingset[,selectedFeatures]
trainingset_withoutOutlier <- trainingset_withoutOutlier[,selectedFeatures]
print("Finished Feature Selection")


#######################START TRAINING#########################################################
source(paste0(dir, "/Code/ModelTrainer.R"))
errorRates.nnet <- c()
errorRates.naiveBayes <- c()
errorRates.lr <- c()
errorRates.rf <- c()
errorRates.knn <- c()
errorRates.svm <- c()
errorRates.J48 <- c()
errorRates.ensemble <- c()
errorRates.ensemble_wo <- c()
#train multiple times 

###Temp
#idx.train <- createDataPartition(y = trainingset$churn, p=0.7, list=FALSE)
#data.tr <- trainingset[idx.train,]
#data.ts <- trainingset[-idx.train,]

#rf <- trainRandomForest(data.tr)
>>>>>>> 7ef7e07e425cd8e1ebd4017012efd92031e81143
idx.train <- createDataPartition(y = trainingset$churn, p=0.7, list=FALSE)
data.tr <- trainingset[idx.train,]
data.ts <- trainingset[-idx.train,]

#save(rf, file = "rfModel.RData")


#glm_ensemble <- caretStack(model_list_big, method='glm', trControl=trainControl(method='cv'))

for (i in c(1:5)) {
  print(paste("start Iteration ", i))

  #Split to test/trainigsset
  idx.train <- createDataPartition(y = trainingset$churn, p=0.7, list=FALSE)
  data.tr <- trainingset[idx.train,]
  data.ts <- trainingset[-idx.train,]
  
  data.tr_wo <- trainingset_withoutOutlier[idx.train,]
  data.ts_wo <- trainingset_withoutOutlier[-idx.train,]
  
  #Train Models
  #nnet <- trainNnet(data.tr_wo)
  #print("Finished NNET Training")
  #naiveBayes <- trainNaiveBayes(data.tr)
  #print("Finished Naive Bayes Training")
  #lr <- trainLogisticRegression(data.tr_wo)
  #print("Finished Logistic Regression")
  #rf <- trainRandomForest(data.tr)
  #print("Finished Random Forest")
  #knn <- trainKNN(data.tr)
  #print("Finished KNN Training")
  #svm <- trainSVM(data.tr_wo)
  #print("Finished SVM Training")
  #J48 <- trainJ48(data.tr)
  #print("Finished J48 Training")
  #adaBag <- trainAdaBag(data.tr)
  #print("Finished AdaBag Training")
  greedy_ensemble <- trainEnsembledMethod(data.tr)
  print("Finished Ensembled Training")
  greedy_ensemble_wo <- trainEnsembledMethod(data.tr_wo)
  print("Finished Ensembled Training")
  
  #library(e1071)
  #nb <- naiveBayes(churn~., data = data.tr)
  #yhat.nb <- predict(nb, newdata = data.ts, type="class")
  #errorRate <- 1- sum(as.numeric(yhat.nb == data.ts$churn))/length(data.ts$churn)
  #Predict Test Set
  #yhat.nnet <- predict(nnet, newdata = data.ts_wo, type="raw")
  #yhat.naiveBayes <- predict(naiveBayes, newdata = data.ts, type="raw")
  #yhat.lr <- predict(lr, newdata = data.ts_wo, type="prob")[,2]
  #yhat.rf <- predict(rf, newdata = data.ts, type = "prob")[,2]
  #yhat.knn <- predict(knn, newdata = data.ts, type = "raw")
  #yhat.svm <- predict(svm, newdata = data.ts_wo, type = "prob")[,2]
  #yhat.J48 <- predict(J48, newdata = data.ts, type = "raw")
  yhat_ens <- predict(greedy_ensemble, newdata = data.ts)
  yhat_ens_wo <- predict(greedy_ensemble_wo, newdata = data.ts_wo)
  
  #Assess Models
  #err.nnet <- ModelPerformanceByClass(data.ts_wo$churn, yhat.nnet)
  #err.naiveBayes <-  ModelPerformanceByClass(data.ts$churn, yhat.naiveBayes)
  #err.lr <- ModelPerformance(data.ts_wo$churn, yhat.lr)
  #err.rf <- ModelPerformance(data.ts$churn, yhat.rf)
  #err.knn <- ModelPerformanceByClass(data.ts$churn, yhat.knn)
  #err.svm <- ModelPerformance(data.ts$churn, yhat.svm)
  #err.J48 <- ModelPerformanceByClass(data.ts$churn, yhat.J48)
  err.ensemble_wo <- ModelPerformance(data.ts$churn, yhat_ens_wo)
  
  #print(paste0("ErrorRate (nnet): ", err.nnet))
  #print(paste0("ErrorRate (naiveBayes): ", err.naiveBayes))
  #print(paste0("ErrorRate (Logistic Regression): ", err.lr))
  #print(paste0("ErrorRate (Random Forest): ", err.rf))
  #print(paste0("ErrorRate (KNN): ", err.knn))
  #print(paste0("ErrorRate (svm): ", err.svm))
  #print(paste0("ErrorRate (J48): ", err.J48))
  print(paste0("ErrorRate (Ensemble): ", err.ensemble))
  print(paste0("ErrorRate (Ensemble WO): ", err.ensemble_wo))
  
  #errorRates.nnet <- c(errorRates.nnet, err.nnet)
  #errorRates.naiveBayes <- c(errorRates.naiveBayes, err.naiveBayes)
  #errorRates.lr <- c(errorRates.lr, err.lr)
  #errorRates.rf <- c(errorRates.rf, err.rf)
  #errorRates.knn <- c(errorRates.knn, err.knn)
  #errorRates.svm <- c(errorRates.svm, err.svm)
  #errorRates.J48 <- c(errorRates.J48, err.J48)
  errorRates.ensemble <- c(errorRates.ensemble, err.ensemble)
  errorRates.ensemble_wo <- c(errorRates.ensemble_wo, err.ensemble_wo)
  
  ##Send me an information:
  
  sendmail("frederik@pahde.com", subject="R Notification", message=paste("Finished Ensembling\n
                                                                         Error Rate: ", err.ensemble, 
                                                                         "\nError Rate (wo): ", err.ensemble_wo))
  
  #sendmail("frederik@pahde.com", subject="R Notification", message=paste("Finished Iteration ", i, ": 
  #                                                                      Logistic Regression: ", err.lr, "\n
  #                                                                      Random Forest: ", err.rf, "\n
  #                                                                      Support Vector Machine: ", err.svm), password="rmail")
}
#print("Error Rates NNET: ")

#print(errorRates.nnet)
#print("Error Rates Naive Bayes: ")
#print(errorRates.naiveBayes)
#print("Error Rates LR: ")
#print(errorRates.lr)
#print("Error Rates RF: ")
#print(errorRates.rf)
#print("Error Rates KNN: ")
#print(errorRates.knn)
#print("Error Rates SVM: ")
#print(errorRates.svm)
#print("Error Rates J48: ")
#print(errorRates.J48)
print("Error Rates Ensemble: ")
print(errorRates.ensemble)
print("Error Rates Ensemble WO: ")
print(errorRates.ensemble_wo)
##############################END TRAINING#################################################################
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
