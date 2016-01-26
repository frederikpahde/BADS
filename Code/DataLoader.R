getTrainigset <- function(dir){
  data <- read.csv(paste0(dir, "/Data/trainingset.csv"), sep = ',', na.strings=c("","NA"))
  data$churn = factor(data$churn, labels = c("good", "bad"))
  return(data)
}

getTestset <- function(dir){
  data <- read.csv(paste0(dir, "/Data/testset.csv"), na.strings=c("","NA"))
  return(data)
}

getContinousset <- function(dir){
  data <- read.csv(paste0(dir, "/Data/continous_variablenames.csv"),header=TRUE, sep = ";")
  return(data)
}

loadImputedTrainingset <- function(dir){
  data <- read.csv(dir, sep = ",")
  data$churn = factor(data$churn, labels = c("good", "bad"))
  return(data)
}
loadImputedTestSet <- function(dir){
  data <- read.csv(dir, sep = ",")
  return(data)
}

getSelectedFeatureSet <- function(dir){
  data <- read.csv(paste0(dir, "/Data/feature_selection_variablenames3.csv"),header=TRUE, sep = ";")
  return(data)
}