# train logistic regression with elastic net penalty (feature selection is included)

# create matrix only with input data (except churn)
features_input <- model.matrix(~.,data.tr[,!names(data.tr) %in% "churn"])

# use cv.glmnet for training the log regr incl cross validation 
lr_enet <- cv.glmnet(features_input,data.tr$churn, alpha=0.5, family="binomial", type.measure="class", nfolds=5)

plot(lr_enet) # plots lambda (for diff number of features) against misclassification error

coeff_out = coef(lr_enet, s = "lambda.1s") 
# gives out coefficients, s=lambda.min with smallest misclass.error (but ca 112 features)
# s=lambda.1s gives number of features with smallest misclass.error + 1 std deviation (73 features) 


data.ts_matrix <- model.matrix(~.,data.ts[,!names(data.ts) %in% "churn"])
yhat.lrenet <- predict(lr_enet, newx=data.ts_matrix, type="response", s="lambda.1s")

err.lrenet <- ModelPerformance(data.ts$churn, yhat.lrenet)



#trainlrenet <- function(features_input){
 # nnGrid <- expand.grid(size=c(2,5,9), decay=c(.1,1,10))
#  nn.tune <- train(churn~., data = data.tr, method="cv.glmnet", trControl = ctrl, tuneGrid=nnGrid)
 # return(nn.tune)
#}