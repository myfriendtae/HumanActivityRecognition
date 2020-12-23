# Background 
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

# Library and dataset
The training(https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and test(https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv) dataset are available in each link.

```{r, echo=TRUE, message=FALSE}
library(caret)
library(randomForest)
testing = read.csv('./pml-testing.csv')
training = read.csv('./pml-training.csv')
```

# Cleaning dataset
Removed the columns with low variance and more than 80% of data are null value. Additionally, sliced out the irrelvant columns based on the description.
```{r, echo=TRUE}
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
```

# Slice the dataset for validation
Decided to allocate 50% of the sample for the training set due to the low memory.
```{r, echo=TRUE}
inTrain = createDataPartition(training$classe, p=0.5)[[1]]
crossv_train = training[inTrain,]
crossv_test = training[-inTrain,]
```

# Train a model with the random forest
Choosen the training method because:
- applicable for the various types of type
- having lower capacity in computation, traning speed is important

```{r, echo=TRUE}
modFitRF = randomForest(classe~., importance=TRUE, data=crossv_train, ntree=10)
```

# Validation
```{r, echo=TRUE}
pred_training = predict(modFitRF, newdata=crossv_train)
CM_training = confusionMatrix(pred_training, crossv_train$classe)
CM_training$overall['Accuracy']

pred_testing = predict(modFitRF, newdata=crossv_test)
CM_testing = confusionMatrix(pred_testing, crossv_test$classe)
CM_testing$overall['Accuracy']
```

The training dataset achieves 99.95% accuracy, and the test dataset achieves 97.9%, showing high accuracy, so it does not require to combine other training methods to improve the model.

# Prediction
My prediction for the out of the sample dataset is the following.
```{r, echo=TRUE}
prediction = predict(modFitRF, newdata=testing)
prediction
```
