############### For 1st iteration ################
############### Filter approach (univariate filters for feature selection)

# separation of input and output needed
features_input <- trainingset[,!names(test) %in% "churn"]
churn_var <- as.data.frame(trainingset$churn)


# using random forest
features_rf <- sbf(features_input, as.factor(churn_var[,1]), 
                    sbfControl=sbfControl(functions=rfSBF, 
                                          verbose = FALSE, 
                                          method = "repeatedcv",
                                          repeats=5))

# using naive bayes
filtered_nb <- sbf(features_input, as.factor(churn_var[,1]),
                     sbfControl = sbfControl(functions = nbSBF,
                                             verbose = FALSE, 
                                             method = "repeatedcv",
                                             repeats = 5))
  
# selected features
retained_features <- transpose(as.data.frame(features_rf$optVariables))
trainingset_SelectedFeatures <- trainingset[,names(trainingset) %in% retained_features]
trainingset_withoutOutlier_SelectedFeatures <- trainingset_withoutOutlier[,names(trainingset_withoutOutlier) %in% retained_features]

trainingset <- trainingset[,names(trainingset) %in% retained_features]

trainingset_withoutOutlier <- trainingset_withoutOutlier[,names(trainingset_withoutOutlier) %in% retained_features]


############### For 2nd iteration ################

############### LOGISTIC REGRESSION 
# train lr with elastic net penalty (feature selection is included)

# create matrix only with input data (except churn)
# features_input <- model.matrix(~.,data.tr[,!names(data.tr) %in% "churn"])

# use cv.glmnet for training the log regr incl cross validation 
# lr_enet <- cv.glmnet(features_input,data.tr$churn, alpha=0.5, family="binomial", type.measure="class", nfolds=5)

# plot(lr_enet) 
# plots lambda (for diff number of features) against misclassification error

# coeff_out = coef(lr_enet, s = "lambda.1s") 
# gives out coefficients, s=lambda.min with smallest misclass.error (but ca 112 features)
# s=lambda.1s gives number of features with smallest misclass.error + 1 std deviation (73 features) 


# data.ts_matrix <- model.matrix(~.,data.ts[,!names(data.ts) %in% "churn"])
# yhat.lrenet <- predict(lr_enet, newx=data.ts_matrix, type="response", s="lambda.1s")
# err.lrenet <- ModelPerformance(data.ts$churn, yhat.lrenet)



############### RANDOM FOREST
# train model like in ModelTrainer.R (but add importance=TRUE to the train function of random forest) and main (train rf)
# estimate variable importance
# importance <- varImp(rf, type= 1, scale=FALSE)
# summarize importance
# print(importance)
# plot importance
# plot(importance)


