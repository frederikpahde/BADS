getTrainigset <- function(dir){
  data <- read.csv(paste0(dir, "/data/trainingset.csv"), sep = ',')
  return(data)
}

getTestset <- function(dir){
  data <- read.csv(paste0(dir, "/data/testset.csv"))
  return(data)
}

getContinousset <- function(dir){
  data <- read.csv(paste0(dir, "/data/continous_variablenames.csv",header=TRUE, sep = ";"))
  return(data)
}