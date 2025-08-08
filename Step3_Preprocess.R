#Preprocess####
require(caret)
require(dplyr)

#Imbalance of Classes 
#(Observed in Histogram)
cData %>%
  filter(ActionTaken == 6) %>% dim() #1261450

cData %>% 
  filter(ActionTaken == 3) %>% dim() #1967891

#Resampling Data to fix Class Imbalance
#Purchased: 1261450
#Denied: 1967891
set.seed(37)
#minCount <- table(cData$ActionTaken) %>%
#  min()
  
balData <- cData %>% #Balanced Dataset (with Equal Num of Purchased and Denied)
  group_by(ActionTaken) %>%
  slice_sample(n = min(table(cData$ActionTaken))) %>%
  ungroup()

table(balData$ActionTaken)

#Performing One-Hot Encoding on cData
preEncode <- balData %>%
  select(!c(State, ActionTaken, ReverseMortgage)) #No One-Hot Encoding for State
#Don't want 53 Columns -_-, 
#ActionTaken is Target Column
#ReverseMortgage is ReverseMortgage (for RQ2)

dm <- dummyVars("~.", data = preEncode) #Creating Dummy Variables

##One-Hot Encoding####
encodedData <- data.frame(predict(dm,  newdata = preEncode))
#Attaching State and LoanPurchased (previously ActionTaken) // Target Variable
encodedData <- encodedData %>%
  mutate(State = balData$State,
         LoanPurchased = balData$ActionTaken) %>%
  select(State, everything())
#Modifying LoanPurchased to 0 and 1 (0 - Denied, 1 - Purchased)
levels(encodedData$LoanPurchased) <- c("0","1") #Replacing 3 with 0, 6 with 1
#Converting Target to numeric
encodedData$LoanPurchased <- as.numeric(as.character(encodedData$LoanPurchased))
#Labeling States
StateLevels <- levels(encodedData$State) #For Reference
encodedData$State <- as.numeric(encodedData$State)
#str(encodedData)
s3_1EncodedDataBkp <- encodedData

#Necessary Columns
NonMissingData <- encodedData %>% 
  select(!ends_with("NotApplicable") & !contains("Missing"))

#Train-Test Data 1####
set.seed(37) #Not-So Random Number (https://www.youtube.com/watch?v=d6iQrh2TK98)
partitionIndex <- createDataPartition(encodedData$LoanPurchased, 
                                      p = 0.7, #70-30 Train-Test
                                      list = FALSE)
##Getting Train and Test Data
trainData <- encodedData[partitionIndex,]
testData <- encodedData[-partitionIndex,]

##Separating Feature and Target
###Train Data
xTrain <- trainData %>%
                  select(-LoanPurchased)
yTrain <- trainData$LoanPurchased

###Test Data
xTest <- testData %>%
                  select(-LoanPurchased)
yTest <- testData$LoanPurchased

##With Non-Missing Data
#Train-Test Data 1####
set.seed(37) #Not-So Random Number (https://www.youtube.com/watch?v=d6iQrh2TK98)
partitionIndexNM <- createDataPartition(NonMissingData$LoanPurchased, 
                                      p = 0.7, #70-30 Train-Test
                                      list = FALSE)
##Getting Train and Test Data
trainDataNM <- NonMissingData[partitionIndexNM,]
testDataNM <- NonMissingData[-partitionIndexNM,]

##Separating Feature and Target
###Train Data
xTrainNM <- trainDataNM %>%
  select(-LoanPurchased)
yTrainNM <- trainDataNM$LoanPurchased

###Test Data
xTestNM <- testDataNM %>%
  select(-LoanPurchased)
yTestNM <- testDataNM$LoanPurchased



#Preprocess2 ####
#For Research Question 2
#Focus of Reverse Mortgage
#Interest Rate

cDataRM <- cData %>%
  filter(ReverseMortgage == "Yes" | ReverseMortgage == "No", 
         ActionTaken == 6, #for Purchased Loans
         Age == "65orMore" | Age == "NotProvided") %>% #
  droplevels()
preEncodeRM <- cDataRM %>%
    select(!c(State, ActionTaken, InterestRateMissing, DTRatio, InterestRate))

dmRM <- dummyVars("~.", data = preEncodeRM)
encodedRM <- data.frame(predict(dmRM, newdata = preEncodeRM))
encodedRM <- encodedRM %>%
  mutate(State = cDataRM$State,
         TargetInterestRate = cDataRM$InterestRate) %>%
  select(State, everything())
#Labeling States
encodedRM$State <- as.numeric(encodedRM$State)

NonMissingRM <- encodedRM %>%
  select(!ends_with("NotApplicable") & !contains("Missing"))

#Train-Test Data 2####
set.seed(37) #Not-So Random Number (https://www.youtube.com/watch?v=d6iQrh2TK98)
partitionIndexRM <- createDataPartition(encodedRM$TargetInterestRate, #IR 
                                      p = 0.7, #70-30 Train-Test
                                      list = FALSE)
##Getting Train and Test Data
trainDataRM <- encodedRM[partitionIndexRM,]
testDataRM <- encodedRM[-partitionIndexRM,]

##Separating Feature and Target
###Train Data
xTrainRM <- trainDataRM %>%
  select(-TargetInterestRate)
yTrainRM <- trainDataRM$TargetInterestRate

###Test Data
xTestRM <- testDataRM %>%
  select(-TargetInterestRate)
yTestRM <- testDataRM$TargetInterestRate

#With Non-Missing Values
partitionIndexNMRM <- createDataPartition(NonMissingRM$TargetInterestRate, #IR 
                                      p = 0.7, #70-30 Train-Test
                                      list = FALSE)
##Getting Train and Test Data
trainDataNMRM <- NonMissingRM[partitionIndexNMRM,]
testDataNMRM <- NonMissingRM[-partitionIndexNMRM,]

##Separating Feature and Target
###Train Data
xTrainNMRM <- trainDataNMRM %>%
  select(-TargetInterestRate)
yTrainNMRM <- trainDataNMRM$TargetInterestRate

###Test Data
xTestNMRM <- testDataNMRM %>%
  select(-TargetInterestRate)
yTestNMRM <- testDataNMRM$TargetInterestRate



save.image(file = "step3.RData")
