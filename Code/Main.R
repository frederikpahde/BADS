dir <- Sys.getenv('BADS_Path')   

dir<-getwd()
source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 

#Load Data
source(paste0(dir, "/Code/DataLoader.R"))
trainingset = getTrainigset(dir)
numericVariables = getNumericVariables(trainingset)
categoricVariables <- trainingset[setdiff(colnames(trainingset), colnames(numericVariables))]

#Exploratory Data Analysis
source(paste0(dir, "/Code/ExploratoryDataAnalysis.R"))
createUsefulPlots(trainingset, numericVariables, categoricVariables)


##Missing Value Handling
source(paste0(dir,"/Code/missingValueHandler.R"))
numericCompleteCases <- getImputedData(numericVariables)
categoricCompleteCases <- getImputedData(categoricVariables)
completeCases <- getImputedData(trainingset)


#Train Models
source(paste0(dir, "/Code/ModelTrainer.R"))

##10 Fold CV for Logistic Regression
n <- 10
dataset = completeCases[1:50000,]
setSize = round(dim(dataset)[1]/n)
shuffledDataset <- dataset[sample(nrow(dataset)),]
avg=0
for (i in 1:n) {
  print(i)
  inds.test = ((i-1)*setSize+1):((i-1)*setSize+setSize)
  inds.training = setdiff(1:nrow(dataset), inds.test)
  test = shuffledDataset[inds.test,]
  training = shuffledDataset[inds.training,]
  lr<-glm(churn~.,data=training,family=binomial(link="logit"))
  avg = avg + getAccuracy(model = lr, testSet = test)
}

#rf <- randomForest(churn~.,data=completeCases[1:1000,],ntree=500, mtry=3)
res = round(predict(rf, newdata = completeCases[1001:2000,]))
y = completeCases[1001:2000,]
CrossTable(y$churn, res, prop.c=FALSE)$t


#Corelation
###############################################################################################
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


#HANDLING OUTLIERS
#completeCases has 91 variables
#find variables which must contain outliers
difference.Median.Median<-abs(apply(completeCases,2, function(x) median(x)-mean(x)))
#st.d<-apply(completeCases,2,sd)
indicies <- which(difference.Median.Median>apply(completeCases,2,median)/2)
summary(completeCases[,indicies])

source(paste0(dir, "/Code/Outliers.R"))
#replace outliers with means for variables
boxplot(completeCases[1:2])
variable<-handle.Outliers.for.Matrix(completeCases[,1:(length(completeCases-2))], 1.5)
colnames(variable)
data.without.outliers<-apply(dataset,2, function(x) x<-variable[, colnames(x) ])


###########################################################################
numericVariables

difference.Median.Median<-abs(apply(completeCases,2, function(x) median(x)-mean(x)))
#st.d<-apply(completeCases,2,sd)
indicies <- which(difference.Median.Median>apply(completeCases,2,median)/2)
summary(completeCases[,indicies])

summary(trainingset[,50:78])


