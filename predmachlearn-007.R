# http://groupware.les.inf.puc-rio.br/har
# load library
library(caret)

# read data
pml <- read.csv("pml-training.csv", stringsAsFactors = F)
data <- pml[,-(1:5)]
rm(pml)

# some variable is numeric but wrongly read as char
data$kurtosis_roll_belt <- as.numeric(data$kurtosis_roll_belt)
data$kurtosis_picth_belt <- as.numeric(data$kurtosis_picth_belt)
data$skewness_roll_belt <- as.numeric(data$skewness_roll_belt)
data$skewness_roll_belt.1 <- as.numeric(data$skewness_roll_belt.1)
data$max_yaw_belt <- as.numeric(data$max_yaw_belt)
data$min_yaw_belt <- as.numeric(data$min_yaw_belt)
data$kurtosis_roll_arm <- as.numeric(data$kurtosis_roll_arm)
data$kurtosis_picth_arm <- as.numeric(data$kurtosis_picth_arm)
data$kurtosis_yaw_arm <- as.numeric(data$kurtosis_yaw_arm)
data$skewness_roll_arm <- as.numeric(data$skewness_roll_arm)
data$skewness_pitch_arm <- as.numeric(data$skewness_pitch_arm)
data$skewness_yaw_arm <- as.numeric(data$skewness_yaw_arm)
data$kurtosis_roll_dumbbell <- as.numeric(data$kurtosis_roll_dumbbell)
data$kurtosis_picth_dumbbell <- as.numeric(data$kurtosis_picth_dumbbell)
data$skewness_roll_dumbbell <- as.numeric(data$skewness_roll_dumbbell)
data$skewness_pitch_dumbbell <- as.numeric(data$skewness_pitch_dumbbell)
data$max_yaw_dumbbell <- as.numeric(data$max_yaw_dumbbell)
data$min_yaw_dumbbell <- as.numeric(data$min_yaw_dumbbell)
data$kurtosis_roll_forearm <- as.numeric(data$kurtosis_roll_forearm)
data$kurtosis_picth_forearm <- as.numeric(data$kurtosis_picth_forearm)
data$skewness_roll_forearm <- as.numeric(data$skewness_roll_forearm)
data$skewness_pitch_forearm <- as.numeric(data$skewness_pitch_forearm)
data$max_yaw_forearm <- as.numeric(data$max_yaw_forearm)
data$min_yaw_forearm <- as.numeric(data$min_yaw_forearm)

# some variables with strange or meaningless values:
# data$kurtosis_yaw_belt
# data$skewness_yaw_belt
# data$amplitude_yaw_belt
# data$kurtosis_yaw_dumbbell
# data$skewness_yaw_dumbbell
# data$amplitude_yaw_dumbbell
# data$kurtosis_yaw_forearm
# data$skewness_yaw_forearm
# data$amplitude_yaw_forearm
# remove them
data <- subset(data, select = -c(kurtosis_yaw_belt, skewness_yaw_belt,
                                 amplitude_yaw_belt, kurtosis_yaw_dumbbell,
                                 skewness_yaw_dumbbell, amplitude_yaw_dumbbell,
                                 kurtosis_yaw_forearm, skewness_yaw_forearm,
                                 amplitude_yaw_forearm))

# set dummy variables
data$new_window <- as.factor(data$new_window)
data$classe <- as.factor(data$classe)
dummies <- dummyVars(classe ~ ., data = data)
df <- as.data.frame(predict(dummies, newdata = data))

# imput NA as 0
df[is.na(df)] <- 0

# Zero- and Near Zero-Variance Predictors
nzv <- nearZeroVar(df)
df <- df[, -nzv]

#split to training set and data set
set.seed(99889)
inTrain <- createDataPartition(data$classe, p = 0.75, list=F)
trainingX <- df[inTrain,]
trainingY <- data$classe[inTrain]
testingX <- df[-inTrain,]
testingY <- data$classe[-inTrain]

# use trainControl to control the resampling
# repeated(10 times) K(K=10) fold cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE)

rfModel <- train(x=trainingX, y=trainingY, method = "rf", trControl = ctrl)

# start testing
pml_test <- read.csv("pml-testing.csv", stringsAsFactors = F)
data_test <- pml_test[,-(1:5)]

data_test$kurtosis_roll_belt <- as.numeric(data_test$kurtosis_roll_belt)
data_test$kurtosis_picth_belt <- as.numeric(data_test$kurtosis_picth_belt)
data_test$skewness_roll_belt <- as.numeric(data_test$skewness_roll_belt)
data_test$skewness_roll_belt.1 <- as.numeric(data_test$skewness_roll_belt.1)
data_test$max_yaw_belt <- as.numeric(data_test$max_yaw_belt)
data_test$min_yaw_belt <- as.numeric(data_test$min_yaw_belt)
data_test$kurtosis_roll_arm <- as.numeric(data_test$kurtosis_roll_arm)
data_test$kurtosis_picth_arm <- as.numeric(data_test$kurtosis_picth_arm)
data_test$kurtosis_yaw_arm <- as.numeric(data_test$kurtosis_yaw_arm)
data_test$skewness_roll_arm <- as.numeric(data_test$skewness_roll_arm)
data_test$skewness_pitch_arm <- as.numeric(data_test$skewness_pitch_arm)
data_test$skewness_yaw_arm <- as.numeric(data_test$skewness_yaw_arm)
data_test$kurtosis_roll_dumbbell <- as.numeric(data_test$kurtosis_roll_dumbbell)
data_test$kurtosis_picth_dumbbell <- as.numeric(data_test$kurtosis_picth_dumbbell)
data_test$skewness_roll_dumbbell <- as.numeric(data_test$skewness_roll_dumbbell)
data_test$skewness_pitch_dumbbell <- as.numeric(data_test$skewness_pitch_dumbbell)
data_test$max_yaw_dumbbell <- as.numeric(data_test$max_yaw_dumbbell)
data_test$min_yaw_dumbbell <- as.numeric(data_test$min_yaw_dumbbell)
data_test$kurtosis_roll_forearm <- as.numeric(data_test$kurtosis_roll_forearm)
data_test$kurtosis_picth_forearm <- as.numeric(data_test$kurtosis_picth_forearm)
data_test$skewness_roll_forearm <- as.numeric(data_test$skewness_roll_forearm)
data_test$skewness_pitch_forearm <- as.numeric(data_test$skewness_pitch_forearm)
data_test$max_yaw_forearm <- as.numeric(data_test$max_yaw_forearm)
data_test$min_yaw_forearm <- as.numeric(data_test$min_yaw_forearm)

data_test <- subset(data_test, select = -c(kurtosis_yaw_belt, skewness_yaw_belt,
                                 amplitude_yaw_belt, kurtosis_yaw_dumbbell,
                                 skewness_yaw_dumbbell, amplitude_yaw_dumbbell,
                                 kurtosis_yaw_forearm, skewness_yaw_forearm,
                                 amplitude_yaw_forearm))

data_test$new_window <- as.factor(data_test$new_window)
df_test <- data_test[, colnames(df)]

# imput NA as 0
df_test[is.na(df_test)] <- 0

# predict testing data
answers <- predict(rfModel, df_test)

pml_write_files = function(x)
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)