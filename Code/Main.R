dir <- Sys.getenv('BADS_Path')   
source(paste0(dir, "/Code/Utils.R"))
source(paste0(dir, "/Code/PlotHelper.R"))
print("tesfhsd")
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
plotHists(numericVariables, 5)

##Missing Value Handling
source(paste0(dir,"/Code/missingValueHandler.R"))
completeCases <- getImputedData(numericVariables)

#completeInds <- complete.cases(t(numericVariables))
#completeCases = numericVariables[completeInds]
corrplot(cor(completeCases))



getAccuracy <- function(model, testSet){
  pred <- predict(model, newdata=testSet, type="response")
  class <- round(pred)
  confustionTable <- CrossTable(testSet$churn, class, prop.c=FALSE)$t
  return((confustionTable[1,1]+confustionTable[1,2])/length(y))
}

n <- 10
dataset = completeCases[1:10000,]
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

##############################################################################################

#high dimensional data
#install.packages("HighDimOut")
#library(HighDimOut)

#test<-cleanedDataCoplete[1:20,1:25]

#tim1<-Func.ABOD(test,basic=FALSE, .9) 
#braucht bei mir ewig für 0.01 prozent im 3D SPACE mit allen Obs (nach 1h noch kein Ergebnis)
#angle-based outlier detection (ABOD) algorithm, calculates the outlier due to there angel base
#small value = outlier
#high value = no outlier;explanation on the LMU slides

#plot(tim1)
#max(tim1)


#########################################################################
install.packages("HighDimOut")
library(HighDimOut)


test<-cleanedDataCoplete[1:100,1:37]
tim2<- Func.FBOD(test, iter=10, k.nn=37) # cannot allocate a vector of 3.5 Gb
#tim is calculated with LOF
# value <<1 the point is in the cluster; value >> the point is far away from cluster

plot(tim2)
#tim2 shows much better solutions than tim1!!!
#WHY? Berechnet mit LOF, zwar nicht mit Angel; dennoch stärker für die Anwendung (erster Eindruck)


show.outlier<-tim2[tim2>=2] # TRUE/FALSE Vector which shows all the outliers (value 2 is manuel, lets discuss this)

test[tim2>=2,]

row.index<-which((tim2>=2)==TRUE)

for(i in 1:length(row.index)){
  paste("Potential outlier at row", row.index[i], sep = " ")
}

class(tim2) #numeric
typeof(tim2) #double
length(tim2) #50, value for all observations
tim[1:3]


###########################################################################################


#HANDLING OUTLIERS
#completeCases has 91 variables
#find variables which must contain outliers
difference.Median.Mean<-abs(apply(completeCases,2, function(x) median(x)-mean(x)))
#st.d<-apply(completeCases,2,sd)
indicies <- which(difference.Median.Mean>apply(completeCases,2,median)/2)
summary(completeCases[,indicies])

source(paste0(dir, "/Code/Outliers.R"))
#replace outliers with means for variables
variable<-set.Outliers.To.Mean(completeCases$mou_opkv_Range, 1.5)


