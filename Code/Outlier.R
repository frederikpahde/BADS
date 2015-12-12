#mihoboius bums bums
library("mvoutlier")

test<-cleanedDataCoplete[,1:3] #bei mehr als 3 variabeln spielt der Algorithmus nicht mehr mit

outlier <- pcout(test, makeplot = TRUE)

min(outlier$wfinal)

length(outlier$wfinal01)

outlier

outlieramount<-0
for(i in 1:length(outlier$wfinal)){
    if(outlier$wfinal[i]<=0.04){
        outlieramount<-(outlieramount+1)
    }
}

#outlier amount of 9000, this is around 20% of the observations :X

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
#outlier in high dimensions due to fastest algrothmus with subgroups (all automatically)
install.packages("rrcovHD")
library(rrcovHD)

test<-cleanedDataCoplete[,1:3]

suboutlier<- OutlierPCOut(test)
getFlag(suboutlier)

outlieramount<-0
for(i in 1:50000){
  if(getFlag(suboutlier)[i]==0){
    outlieramount<-(outlieramount+1)
  }
}
#outlieramount is 17658

length(getOutliers(suboutlier))

outlierweight<-getWeight(suboutlier)

suboutlier2<- OutlierPCOut(cleanedDataCoplete[1:5]) # hängt wieder im 3D fest?
getFlag(suboutlier2)

#Fuck ey 1 Dimensional approach
install.packages("outliers")
library(outliers)

outlier(test[,1])

boxplot(test[,1])