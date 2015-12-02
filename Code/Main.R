dir <- Sys.getenv("BADS_Path")   #C:/Users/D059348/dev/HU/BADS

#Script to install and load needed packages
source(paste0(dir, "/Code/Init.R")) 

#Load Data
source(paste0(dir, "/Code/DataLoader.R"))
trainingset = getTrainigset(dir)


