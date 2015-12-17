############### For 2nd iteration ################
# train logistic regression with elastic net penalty (feature selection is included)

# create matrix only with input data (except churn)
#features_input <- model.matrix(~.,data.tr[,!names(data.tr) %in% "churn"])

# use cv.glmnet for training the log regr incl cross validation 
#lr_enet <- cv.glmnet(features_input,data.tr$churn, alpha=0.5, family="binomial", type.measure="class", nfolds=5)

#plot(lr_enet) # plots lambda (for diff number of features) against misclassification error

#coeff_out = coef(lr_enet, s = "lambda.1s") 
# gives out coefficients, s=lambda.min with smallest misclass.error (but ca 112 features)
# s=lambda.1s gives number of features with smallest misclass.error + 1 std deviation (73 features) 


#data.ts_matrix <- model.matrix(~.,data.ts[,!names(data.ts) %in% "churn"])
#yhat.lrenet <- predict(lr_enet, newx=data.ts_matrix, type="response", s="lambda.1s")

#err.lrenet <- ModelPerformance(data.ts$churn, yhat.lrenet)



# using smaller data set

test <- data.tr[1:1000,]
features_input_small <- test[,!names(test) %in% "churn"]
churn_test <- as.data.frame(test$churn)

#####################################
# Filter approach (univariate filters for feature selection)

features_rf <- sbf(churn~., test, 
                          sbfControl=sbfControl(functions=anovaScores, 
                          verbose = FALSE, 
                          method = "cv",
                          saveDetails = FALSE, 
                          p = 0.7))

features_rf2 <- rfe(churn~., test, 
                   rfeControl=rfeControl(functions=rfFuncs, 
                                         verbose = FALSE, 
                                         method = "cv",
                                         saveDetails = FALSE, 
                                         p = 0.7))

filtered_nb <- sbf(features_input_small, as.factor(churn_test[,1]),
                     sbfControl = sbfControl(functions = nbSBF,
                                             verbose = FALSE, 
                                             method = "repeatedcv",
                                             repeats = 5))

filtered_anova <- sbf(features_input_small, as.factor(churn_test[,1]),
                  sbfControl = sbfControl(functions = anovaScores,
                                          verbose = FALSE, 
                                          method = "repeatedcv",
                                          repeats = 5,
                                          multivariate = TRUE))


control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


