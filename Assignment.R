setwd("C:/Users/lerhshong/Desktop/For Elysium/Coursera/8. Practical Machine Learning")
library(data.table)
library(caret)

#######################
# READING IN THE DATA #
#######################
train <- fread("pml-training.csv")
test <- fread("pml-testing.csv")

#################
# PREPROCESSING #
#################
# Removing missing values
x <- sapply(train,function(x) sum(is.na(x))) # Number of missing values
missingvalues <- x[which(x!=0)] # All the missing values are in the same rows.
colstodelete <- names(missingvalues)
train <- train[,(colstodelete) := NULL]
test <- test[,(colstodelete) := NULL]


# Removing near zero variance columns
zerovariance <- nearZeroVar(train)
train <- train[,-zerovariance,with=FALSE]
test <- test[,-zerovariance,with=FALSE]

# Remove all non-numeric columns.
train <- train[,c("V1","user_name","cvtd_timestamp"):=NULL]
test <- test[,c("V1","user_name","cvtd_timestamp"):=NULL]

############
# TRAINING #
############

# Good spread of classes, will opt not to use any oversampling algorithms
# Proceed with xgboost first.

lstrain5 <- function(dataset,method){
        set.seed(100)
        inTrain <- createDataPartition(dataset$classe,p=0.8,list=FALSE)
        training <- dataset[inTrain,]
        testing <- dataset[-inTrain,]
        objControl <- trainControl(method="cv", number=3, returnResamp="final",classProbs=TRUE,savePredictions=TRUE)
        bmmodel <- train(classe~., data=training,trControl = objControl, method=method, preProcess = c("center","scale"))
        predictbm <- predict(bmmodel,testing)
        confbm <- confusionMatrix(predictbm, testing$classe)
        finally <- list(model=bmmodel,conf=confbm)
        return(finally)
}

xgbLinear <- lstrain5(train,"xgbLinear") # Extremely good accuracy, but we have scaled the data..
xgbTree <- lstrain5(train,"xgbTree")
resamples <- resamples(list(Tree=xgbTree$model,Linear=xgbLinear$model))
# Both extremely good, Linear is somewhat better.

