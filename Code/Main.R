dir <- Sys.getenv('BADS_Path')   

#setwd("~/Documents/HU Berlin/WI 1516/BADS/Aufgabe/BADS")
#dir<-getwd()

### Not on windows #######
#library(doMC)           #
#registerDoMC(cores = 4) #
##########################


source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 
cl <- makeCluster((detectCores()))
registerDoParallel(cl)
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
#trainingset <- trainingset[sample(1:50000,20000, replace = FALSE),]

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

#identify highly corelated coplete veriables
#source(paste0(dir, "/Code/Correlation.R"))
#data<-trainingset_withoutOutlier
#trainingset_withoutCorrelated<-handle.highly.correlated.for.Matrix(data, .75, 
#        which(colnames(data)=="Customer_ID"|colnames(data)=="churn"))


#####when skeaping the correlation
#trainingset_withoutCorrelated<-trainingset_withoutOutlier

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
#selectedFeatures <- getSelectedFeatureSet(dir)
#selectedFeatures <- c(as.vector(selectedFeatures[,1]), "churn")
source(paste0(dir, "/Code/ModelTrainer.R"))

selectedFeatures <- getSelectedFeatureSet(dir)
selectedFeatures <- c(as.vector(selectedFeatures[,1]), "churn")
trainingset <- trainingset[,selectedFeatures]
trainingset_withoutOutlier <- trainingset_withoutOutlier[,selectedFeatures]


featureSelection <- function(){
  rf <- trainRandomForest(trainingset)
  importance <- varImp(rf, type= 1, scale=FALSE)
  importance_ranking <- importance$importance
  importance_ranking <- as.vector(importance_ranking)
  print("Importance:")
  print(importance)
  print("############################################")
  print("Importance Ranking:")
  print(importance_ranking)
}

#Split to test/trainigsset

#
#trainingset <- trainingset[,selectedFeatures]
#trainingset_withoutOutlier <- trainingset_withoutOutlier[,selectedFeatures]
#columns <-colnames(trainingset_withoutCorrelated)

#nicht alle selected features sind auch in trainingset_withoutCorrelated daher:   
#selectedFeatures_for_withoutCorrelated<-selectedFeatures[selectedFeatures %in% colnames(trainingset_withoutCorrelated)]
#trainingset_withoutCorrelated_selecterFeatures <- trainingset_withoutCorrelated[,selectedFeatures_for_withoutCorrelated]

###########da das Modetraining mit trainingset_withoutOutlier
#trainingset_withoutOutlier<-trainingset_withoutCorrelated_selecterFeatures
###########
print("Finished Feature Selection")


#PCA
source(paste0(dir, "/Code/PCA.R"))
#eingabe: frame mit numerischen und nicht nummerischen Variablen
#returns: frame mit nummerischen und nicht nummerischen Variablen, nummerische sind mit PCA behandelt
trainingset_withoutOutlier<-executePCA(trainingset_withoutOutlier)
trainingset<-executePCA(trainingset)

#######################START TRAINING#########################################################
source(paste0(dir, "/Code/ModelTrainer.R"))
errorRates.nnet <- c()
errorRates.naiveBayes <- c()
lms.lr <- c()
lms.rf <- c()
errorRates.knn <- c()
lms.svm <- c()
errorRates.J48 <- c()
lms.gbm <- c()
lms.ensemble <- c()
lms.ensembleG <- c()
errorRates.ensemble_wo <- c()

#save(rf, file = "rfModel.RData")

k <- 1
#res <- c()
#res_wo <- c()

for (i in c(1:k)) {
  print(paste("start Iteration ", i))

  #Split to test/trainigsset
  #idx.train <- createDataPartition(y = trainingset$churn, p=(1-1/k), list=FALSE)
  data.tr <- trainingset#[idx.train,]
  #data.ts <- trainingset[-idx.train,]
  
  data.tr_wo <- trainingset_withoutOutlier#[idx.train,]
  #data.ts_wo <- trainingset_withoutOutlier[-idx.train,]
  
  
  #Train Models
  #nnet <- trainNnet(data.tr_wo)
  #print("Finished NNET Training")
  #naiveBayes <- trainNaiveBayes(data.tr)
  #print("Finished Naive Bayes Training")
  #lr <- trainLogisticRegression(data.tr_wo)
  #print("Finished Logistic Regression")
  rf <- trainRandomForest(data.tr)
  save(rf, file = "randomForestModel.RData")
  print("Finished Random Forest")
  #knn <- trainKNN(data.tr)
  #print("Finished KNN Training")
  #svm <- trainSVM(data.tr_wo)
  #print("Finished SVM Training")
  #J48 <- trainJ48(data.tr)
  #print("Finished J48 Training")
  #adaBag <- trainAdaBag(data.tr)
  #print("Finished AdaBag Training")
  gbm <- trainGradientBoosting(data.tr)
  save(gbm, file = "gradientBoostingModel.RData")
  print("Finished Gradient Boosting Training")
  
  #greedy_ensemble <- trainEnsembledMethod2(data.tr)
  #print("Finished Ensembled Training")
  #greedy_ensemble_wo <- trainEnsembledMethod(data.tr_wo)
  #print("Finished Ensembled Training")
  
  models <- list(gbm, rf)
  
  #library(e1071)
  #nb <- naiveBayes(churn~., data = data.tr)
  #yhat.nb <- predict(nb, newdata = data.ts, type="class")
  #errorRate <- 1- sum(as.numeric(yhat.nb == data.ts$churn))/length(data.ts$churn)
  #Predict Test Set
  #yhat.nnet <- predict(nnet, newdata = data.ts_wo, type="prob")[,2]
  #yhat.naiveBayes <- predict(naiveBayes, newdata = data.ts, type="raw")
  #yhat.lr <- predict(lr, newdata = data.ts_wo, type="prob")[,2]
  #yhat.rf <- predict(rf, newdata = data.ts, type = "prob")[,2]
  #yhat.knn <- predict(knn, newdata = data.ts, type = "raw")
  #yhat.svm <- predict(svm, newdata = data.ts_wo, type = "prob")[,2]
  #yhat.gbm <- predict(gbm, newdata = data.ts, type = "prob")[,2]
  #yhat.J48 <- predict(J48, newdata = data.ts, type = "raw")
  #yhat_ensG <- predict(greedy_ensemble, newdata = data.ts)
  #yhat_ens_wo <- predict(greedy_ensemble_wo, newdata = data.ts_wo)
  #yhat_ens <- (yhat.rf + yhat.gbm)/2#predictionWrapper(models, data.ts, data.ts_wo)
  
  #Assess Models
  #err.nnet <- ModelPerformanceByClass(data.ts_wo$churn, yhat.nnet)
  #err.naiveBayes <-  ModelPerformanceByClass(data.ts$churn, yhat.naiveBayes)
  #lm.lr <- 0 #getLiftMeasure(data.ts_wo$churn, yhat.lr)
  #lm.rf <- getLiftMeasure(data.ts$churn, yhat.rf)
  #err.knn <- ModelPerformanceByClass(data.ts$churn, yhat.knn)
  #lm.svm <- 0 #getLiftMeasure(data.ts$churn, yhat.svm)
  #lm.gbm <- getLiftMeasure(data.ts$churn, yhat.gbm)
  #err.J48 <- ModelPerformanceByClass(data.ts$churn, yhat.J48)
  #err.ensemble_wo <- ModelPerformance(data.ts$churn, yhat_ens_wo)
  #lm.ensemble <- getLiftMeasure(data.ts$churn, yhat_ens)
  #lm.ensembleG <- 0 #getLiftMeasure(data.ts$churn, yhat_ensG)
  #err.rf <- getLiftMeasure(data.ts$churn, yhat.rf)
  
  #print(paste0("ErrorRate (nnet): ", err.nnet))
  #print(paste0("ErrorRate (naiveBayes): ", err.naiveBayes))
  #print(paste0("LiftMeasure (Logistic Regression): ", lm.lr))
  #print(paste0("LiftMeasure (Random Forest): ", lm.rf))
  #print(paste0("ErrorRate (KNN): ", err.knn))
  #print(paste0("LiftMeasure (svm): ", lm.svm))
  #print(paste0("LiftMeasure (GBM): ", lm.gbm))
  #print(paste0("ErrorRate (J48): ", err.J48))
  #print(paste0("LiftMeasure (Ensemble): ", lm.ensemble))
  #print(paste0("LiftMeasure (EnsembleCaret): ", lm.ensembleG))
  #print(paste0("LiftMeasure (Ensemble WO): ", lm.ensemble_wo))
  
  #errorRates.nnet <- c(errorRates.nnet, err.nnet)
  #errorRates.naiveBayes <- c(errorRates.naiveBayes, err.naiveBayes)
  #lms.lr <- c(lms.lr, lm.lr)
  #lms.rf <- c(lms.rf, lm.rf)
  #errorRates.knn <- c(errorRates.knn, err.knn)
  #lms.svm <- c(lms.svm, lm.svm)
  #lms.gbm <- c(lms.gbm, lm.gbm)
  #lms.ensemble <- c(lms.ensemble, lm.ensemble)
  #lms.ensembleG <- c(lms.ensemble, lm.ensembleG)
  #errorRates.J48 <- c(errorRates.J48, err.J48)
  #res <- c(res, lm.ensemble)
  #res_wo <- c(res_wo, lm.ensemble_wo)
  
  ##Send me an information:
  
  #sendmail("frederik@pahde.com", subject="R Notification", message=paste("Finished Ensembling\n
  #                                                                       Lift Measure: ", lm.ensemble, 
  #                                                                       "\nLift Measure (wo): ", lm.ensemble_wo))
  
  #sendmail("frederik@pahde.com", subject="R Notification", message=paste("Finished Iteration ", i, ": 
  #                                                                      Logistic Regression: ", lm.lr, "\n
  #                                                                      Random Forest: ", lm.rf, "\n
  #                                                                      Gradient Boosting: ", lm.gbm, "\n
  #                                                                      Ensemble (caret): ", lm.ensembleG, "\n
  #                                                                      Ensemble (Own): ", lm.ensemble), password="rmail")

  
}
#print("Error Rates NNET: ")

#print(errorRates.nnet)
#print("Error Rates Naive Bayes: ")
#print(errorRates.naiveBayes)
#print("Lift Measure LR: ")
#print(lms.lr)
#print("Lift Measure RF: ")
#print(lms.rf)
#print("Error Rates KNN: ")
#print(errorRates.knn)
#print("Lift Measure SVM: ")
#print(lms.svm)
#print("Lift Measure GBM: ")
#print(lms.gbm)
#print("Lift Measure Ensemble (Caret): ")
#print(lms.ensembleG)
#print("Lift Measure Ensemble (Own): ")
#print(lms.ensemble)
#print("Error Rates J48: ")
#print(errorRates.J48)
#print("Error Rates Ensemble: ")
#print(res)
#print(sum(res)/k)
#print("Error Rates Ensemble WO: ")
#print(res_wo)
#print(sum(res_wo)/k)

unused<-function(){
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

#summary(trainingset[,50:78]
}