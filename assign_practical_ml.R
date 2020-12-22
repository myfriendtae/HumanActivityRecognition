# Load library and data
library(caret)
library(randomForest)

testing = read.csv('/Users/taepark/Dropbox/R/machine_learning_R/ml_assign/pml-testing.csv')
training = read.csv('/Users/taepark/Dropbox/R/machine_learning_R/ml_assign/pml-training.csv')

# Clearning dataset
nzvar = nearZeroVar(training)
training = training[, -nzvar]

cntlength = sapply(training, function(x) {
  sum(!(is.na(x)|x==""))
})

nullcol = names(cntlength[cntlength < 0.8 * length(training$classe)])

description = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'num_window')
excludecols = c(description, nullcol)
training = training[, !names(training) %in% excludecols]

training$classe = factor(training$classe)

# Slice the dataset for validation
inTrain = createDataPartition(training$classe, p=0.5)[[1]]
crossv_train = training[inTrain,]
crossv_test = training[-inTrain,]


# Train a model with the random forest
modFitRF = randomForest(classe~., importance=TRUE, data=crossv_train, ntree=10)

# Validation
pred_training = predict(modFitRF, newdata=crossv_train)
CM_training = confusionMatrix(pred_training, crossv_train$classe)
CM_training$overall['Accuracy']

pred_testing = predict(modFitRF, newdata=crossv_test)
CM_testing = confusionMatrix(pred_testing, crossv_test$classe)
CM_testing$overall['Accuracy']

## The training dataset achieves 99.95% accuracy, and the test dataset achieves 97.9%

# Prediction
prediction = predict(modFitRF, newdata=testing)

# Conclusion
## The model built with the Random Forest method has achieved the 97.79% accuracy. 


