# Load packages
source("/Users/ulas/Drive/Projects/HIBIT20_ML_Workshop/funs.R") # Change the path accordingly
library(caret)
library(rattle)

# Set random seed for reproducibility
set.seed(234)

## Read data
# You can directly get it from GitHub
mydata = read.csv("https://raw.githubusercontent.com/rsgturkey/Workshop2020/master/02-Intro_to_ML/data/processeddata.csv")

# Data overview
dim(mydata)
names(mydata)
View(mydata)

table(mydata$outcome)

## Downsampling for class imbalance
balanced_data = downSample(mydata, mydata$outcome)
table(balanced_data$outcome)

## Split features and response
outcome = balanced_data$outcome                                         # Response
features = balanced_data[, c("age", "sex", "latitude", "longitude")]    # Features

## Training/test split
train_idx = createDataPartition(outcome, p=0.8, list=FALSE)

train_x = features[train_idx, ]   # Training data features
test_x = features[-train_idx, ]   # Testing data features

train_y = outcome[train_idx]      # Training data labels
test_y = outcome[-train_idx]      # Testing data features

## Set 5-fold cross-validation
ctrl = trainControl(method = "cv", number = 5)

## Logistic regression
logreg = train(x = train_x,
               y = train_y,
               method = "glm",
               family = binomial(),
               trControl = ctrl)

cm_logreg = confusionMatrix(data = predict(logreg, newdata = test_x),
                            reference = test_y)

## Decision tree
dtree = train(x = train_x,
              y = train_y,
              method = "rpart",
              trControl = ctrl)
fancyRpartPlot(dtree$finalModel)

cm_dtree = confusionMatrix(data = predict(dtree, newdata = test_x),
                           reference = test_y)

## Random forest
rforest = train(x = train_x,
                y = train_y,
                method = "rf",
                trControl = ctrl)
cm_rforest = confusionMatrix(data = predict(rforest, newdata = test_x),
                           reference = test_y)
# Shows variable importance
varImp(rforest)

## Plot confusion matrices
plot_cm(list(LogRegression = cm_logreg$table,
             DecisionTree = cm_dtree$table,
             RandForest = cm_rforest$table))

## Compare statistics
stat_cm(list(LogRegression = cm_logreg,
             DecisionTree = cm_dtree,
             RandForest = cm_rforest))




