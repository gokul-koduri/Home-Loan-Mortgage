#Model ####
#XGBoost
require(xgboost)

#Converting Features to Matrix
xTrainMat <- as.matrix(xTrain)
xTestMat <- as.matrix(xTest)

#Default Model
simpleXGB <- xgboost(
  data = xTrainMat,
  label = yTrain,
  objective = "binary:logistic", #For Binary Classification (0/1)
  eval_metric = c("logloss", "auc", "error"),
  nrounds = 100,
  verbose = 0
)

#Predictions
simplePredict <- predict(simpleXGB, xTestMat)
simplePredictLables <- ifelse(simplePredict > 0.5, 1, 0) #>50% is for 1, else 0

#Confusion Matix
simpleConMat <- confusionMatrix(factor(simplePredictLables), factor(yTest), 
                                positive = "1")
print(simpleConMat)
#Not Good Results;  Maybe Overfitting

#For XGBoost
dMatTrain <- xgb.DMatrix(data = xTrainMat, label = yTrain)
dMatTest <- xgb.DMatrix(data = xTestMat, label = yTest)

#parameters for Classification
param <- list(objective = "binary:logistic",
              eval_metric = "auc",
              max_depth = 6,
              eta = 0.3,
              subsample = 0.8,
              colsample_bytree = 0.8)
wList <- list(train = dMatTrain, eval = dMatTest)

#Model
xgModel <- xgb.train(data = dMatTrain,
                     params = param,
                     nrounds = 100,
                     watchlist = wList,
                     verbose = 0)
#Predict with Test
xgPredict <- predict(xgModel, xTestMat)
xgLabel <- ifelse(xgPredict > 0.5, 1, 0)

#Confusion Matrix
xgMatrix <- confusionMatrix(factor(xgLabel), factor(yTest), 
                            positive = "1")
print(xgMatrix)
#Still Not a good Matrix

#Confusion Matrix and Statistics
#
#Reference
#Prediction      0      1
#0 590628      0
#1      0 378174
#
#Accuracy : 1        
#95% CI : (1, 1)   
#No Information Rate : 0.6096   
#P-Value [Acc > NIR] : < 2.2e-16
#
#Kappa : 1        
#
#Mcnemar's Test P-Value : NA       
#                                   
#            Sensitivity : 1.0000   
#            Specificity : 1.0000   
#         Pos Pred Value : 1.0000   
#         Neg Pred Value : 1.0000   
#             Prevalence : 0.6096   
#         Detection Rate : 0.6096   
#   Detection Prevalence : 0.6096   
#      Balanced Accuracy : 1.0000   
#                                   
#       'Positive' Class : 0      

#Getting Feature Importance
xgbFeatures <- xgb.importance(model = xgModel)
print(xgbFeatures)
xgb.plot.importance(importance_matrix = xgbFeatures, top_n = 10)

#Affected by Class Imbalance: 0 - 1967891 vs 1 - 1261450
#Fixing Imbalance
#Approach 1
scalePosWeight <- (sum(yTrain == 0) / sum(yTrain == 1)) #1967891 / 1261450

scaled1Model <- xgboost(
  data = xTrainMat,
  label = yTrain,
  objective =  "binary:logistic",
  nrounds = 100,
  scale_pos_weight = scalePosWeight,
  verbose = 0
)

#Predict with Test
s1Predict <- predict(scaled1Model, xTestMat)
s1Label <- ifelse(s1Predict > 0.5, 1, 0)

#Confusion Matrix
s1Matrix <- confusionMatrix(factor(s1Label), factor(yTest))
print(s1Matrix)
#Same as Above

#Approach 2
#Adding Weights
Weights <- ifelse(yTrain == 1, 3, 1)
dMatTrainWgt <- xgb.DMatrix(data = xTrainMat, label = yTrain, weight = Weights)

weightModel <- xgb.train(params = param,
                         data = dMatTrainWgt,
                         nrounds = 100,
                         verbose = 0)
wPredict  <- predict(weightModel, xTestMat)
wLabel <- ifelse(wPredict > 0.5, 1, 0)

wMatrix <- confusionMatrix(factor(wLabel),factor(yTest))  
print(wMatrix)

#XGBoost is not working as expected
#Trying Random Forest
save.image("TillXGBModel.RData")
library(randomForest)

rfModel <- randomForest(LoanPurchased ~ .,
                        data = encodedData,
                        ntree = 100)
#Error: cannot allocate vector of size 3.8 Gb
#In addition: Warning message:
#  In randomForest.default(m, y, ...) :
#  The response has five or fewer unique values.  Are you sure you want to do regression?
#Trying caret
require(caret)

#tc <- trainControl(method = "cv", number = 5, sampling = "up")
#cModel <- train(LoanPurchased ~., data = trainData, method = "rf", trControl = tc)
#Above 2 Models will fail because of Nas

#Trying ranger
require(ranger)
rangerTrain <- trainData
rangerTest <- testData

rangerTrain$LoanPurchased <- as.factor(rangerTrain$LoanPurchased)
rangerTest$LoanPurchased <-  as.factor(rangerTest$LoanPurchased)
#Needs Categorical Data for Classification

rangerModel <- ranger(LoanPurchased ~ ., data = rangerTrain)
rPredict <- predict(rangerModel, data = testData)
#tLabels <- factor(testData$LoanPurchased)
rLabels <- rPredict$predictions
#pLables <- factor(rLabels, levels = levels(tLabels))
rMatrix <- confusionMatrix(
  factor(rLabels, levels = levels(factor(testData$LoanPurchased))),
  factor(testData$LoanPurchased)
  )
print(rMatrix)
#Confusion Matrix and Statistics
#
#          Reference
#Prediction      0      1
#         0 472657      0
#         1      0 100322
#                                   
#               Accuracy : 1        
#                 95% CI : (1, 1)   
#    No Information Rate : 0.8249   
#    P-Value [Acc > NIR] : < 2.2e-16
#                                   
#                  Kappa : 1        
#                                   
# Mcnemar's Test P-Value : NA       
#                                   
#            Sensitivity : 1.0000   
#            Specificity : 1.0000   
#         Pos Pred Value : 1.0000   
#         Neg Pred Value : 1.0000   
#             Prevalence : 0.8249   
#         Detection Rate : 0.8249   
#   Detection Prevalence : 0.8249   
#      Balanced Accuracy : 1.0000   
#                                   
#       'Positive' Class : 0

#Working with Non-Missing and Non-NotApplicable Data
#With Non-Missing Data
xTrainMatNM <- as.matrix(xTrainNM)
xTestMatNM <- as.matrix(xTestNM)

#Default Model with Non-Missing
simpleXGBNM <- xgboost(
  data = xTrainMatNM,
  label = yTrainNM,
  objective = "binary:logistic", #For Binary Classification (0/1)
  eval_metric = c("logloss", "auc", "error"),
  nrounds = 100,
  verbose = 0
)

#Predictions
simplePredictNM <- predict(simpleXGBNM, xTestMatNM)
simplePredictLablesNM <- ifelse(simplePredictNM > 0.5, 1, 0) #>50% is for 1, else 0

#Confusion Matix
simpleConMatNM <- confusionMatrix(factor(simplePredictLablesNM), factor(yTestNM), 
                                  positive = "1")
print(simpleConMatNM)

sniNonMissingFeatures <- xgb.importance(model = simpleXGBNM)
print(NonMissingFeatures)
xgb.plot.importance(importance_matrix = NonMissingFeatures, top_n = 10)


#For R2 ####

#Converting Features to Matrix
xTrainMatRM <- as.matrix(xTrainRM)
xTestMatRM <- as.matrix(xTestRM)

#Default Model
XGBforRM <- xgboost(
  data = xTrainMatRM,
  label = yTrainRM,
  objective = "reg:squarederror", #For Binary Classification (0/1)
  nrounds = 100,
  verbose = 0
)

#Predictions
PredictRM <- predict(XGBforRM, xTestMatRM)
PredictLablesRM <- ifelse(PredictRM > 0.5, 1, 0) #>50% is for 1, else 0

#Confusion Matix
RMMatrix <- confusionMatrix(factor(PredictLablesRM), factor(yTestRM), 
                                positive = "1")
print(RMMatrix)

featureImp <- xgb.importance(model = XGBforRM)
head(featureImp[,c("Feature","Importance")])

# Visualize top 20 features
xgb.plot.importance(importance_matrix = featureImp, top_n = 5)


save.image(file = "step4.RData")
