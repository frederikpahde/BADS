###PSA###
executePCA<-function(data){
data_numeric<-getNumericVariables(data)
data_numeric<-data_numeric[,-which(colnames(data_numeric)=="Customer_ID")]
new_data_numeric<-pca(data_numeric)
data_rest<-data[,setdiff(colnames(data),colnames(data_numeric))]
#plot(new_data_numeric)
complete_new_data<-data.frame(new_data_numeric,data_rest)
return(complete_new_data)
}
#################################################################################
pca<-function(trainingset_st_numeric){
  trainingset_st_numeric<-getNumericVariables(trainingset_withoutOutlier)
original_dimNumber<-length(trainingset_st_numeric)
pca<-princomp(x=trainingset_st_numeric, scores=TRUE, cor = TRUE)
#summary(pca)
#how many components we want to retan?
#-> where squared standard deviation (eigenvalue) is above 1 - they explain at least as much variation as the original variables
#Proportion of Variance gives how much variance is contained in the variables, so the smaller it is the less important is the variable

#pca$loadings # loadings are the the values in the eigenvektors
#summary(pca)
#plot(pca)
components<-pca$sdev[pca$sdev^2>1]
loadings<-pca$loadings[,1:length(components)]
write.csv(loadings, file = "Komponentenmatrix.csv")
print("wrote Komponentenmatrix to Komponentenmatrix.csv")
reduced_data<-pca$scores[,1:length(components)]
print(paste0("Reduced dimentions from original nummeric ",original_dimNumber, " to ", length(components)))

return(reduced_data)

}

###
pca_perHand_auskommentieren<-function(){
  trainingset_st<-trainingset_withoutOutlier
  
  #standardized<-trainingset
  plot(trainingset_st[5:6])
  #plot(standardized[4:5])
  my.cov<-cov(trainingset_st[5:6])
  my.eigen<-eigen(my.cov)
  #sum of eigenvalues is the total variance in the dataset
  sum(my.eigen$values)
  var(trainingset_st[,4])+var(trainingset_st[,5])
  #Eigenvektors of the var matrix are the principal components - the question is how much variation this principle components account for
  #Eigenvactors indicate the strength of the association of the variables with the principle component
  # Eigenvalue tells us hom much variance is in the data in the diraction of the corresponding eigenvector-> Eigenvector with the highest eigenvalue is the principal component   t
  # Principal Components are the diractions or Vecors in which the data is most sprad out -> where there is the most variance
  #Where there is most variation, there is most information (if everything was equal to 1, there would be no information)
  #https://georgemdallas.wordpress.com/2013/10/30/principal-component-analysis-4-dummies-eigenvectors-eigenvalues-and-dimension-reduction/
  #so, eigenvectors represnt new dimentions of the data, data is projected on these two dimentiions
  #sum of variance of the data is equal to the sum of eigenvalues because eigenvalues explain the variance, they tell us how much variance is explained by wich eigenvector
  pc1.slope=my.eigen$vectors[1,1]/my.eigen$vectors[2,1]
  pc2.slope=my.eigen$vectors[1,2]/my.eigen$vectors[2,2]
  abline(0,pc1.slope,col="red")
  abline(0,pc2.slope,col="green")
  # Eigenvalues devided by the sum of eigenvalues (which is the total variance in the dataset) * 100 = % of the variance variable (its principle component) accounts for
  #-> goal is to find components that account for the most variance in the dataset
  pc2.var = 100*round(my.eigen$values[2]/sum(my.eigen$values), digits=2)
  pc1.var = 100*round(my.eigen$values[1]/sum(my.eigen$values), digits=2)
  
  summary(trainingset_st)
}