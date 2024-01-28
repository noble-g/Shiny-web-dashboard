## Load model libraries
library(caret)
library(randomForest)

### Modelling
set.seed(42) # for reproducibility
## Train - Test Split
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

## Logistics Reg
lr_model <- train(Species ~ ., data = trainData, method = "multinom", family = "binomial", 
                  trControl = trainControl(method = "cv", number = 5), 
                  control = glm.control(maxit = 1000),
                  preProcess = c("center", "scale"))
# Since LR is a binary classifier and the data has a response categorical var of three levels not 2
# LR cannot be directly applied to a multiclass classification problem like the iris dataset
# in the above LR model, the one vs rest (OvR) approach was implemented with the 
## ... `multinom` method where separate binary logistics reg models are trained for...
# ... for each class versus the other classes.
lr_pred <- predict(lr_model, newdata = testData)
lr_accuracy <- confusionMatrix(lr_pred, testData$Species)$overall['Accuracy']

## Random Forest
rf_model <- train(Species ~ ., data = trainData, method = "rf", trControl = trainControl(method = "cv", number = 5))
rf_pred <- predict(rf_model, newdata = testData)
rf_accuracy <- confusionMatrix(rf_pred, testData$Species)$overall['Accuracy']

# Print the acc. of both models
cat("Logistic Regression Accuracy:", lr_accuracy, "\n")
cat("Random Forest Accuracy:", rf_accuracy, "\n")

## Save the models
saveRDS(lr_model, "lr_model.rds")
saveRDS(rf_model, "rf_model.rds")

## Live prediction of one row

data[1, -5] ## live independent variables
data[1,] ## live data (dependent var inclusive)
# Live data frame creation
live_data <- data.frame(
  Sepal.Length = 5.1,
  Sepal.Width = 3.5,
  Petal.Length = 1.4,
  Petal.Width = 0.2
)

lr_pred <- predict(lr_model, newdata = live_data, type = "raw")
rf_pred <- predict(rf_model, newdata = live_data, type = "raw")
# Print the predictions
print(as.character(lr_pred[1]))
print(rf_pred)
