# Install and load necessary library

library(ElemStatLearn)

# Load the South Africa Heart Disease Data
data(SAheart)

# Create training and test sets
set.seed(8484)
train = sample(1:dim(SAheart)[1], size = dim(SAheart)[1] / 2, replace = FALSE)
trainSA = SAheart[train, ]
testSA = SAheart[-train, ]

# Fit a logistic regression model
set.seed(13234)
logistic_model <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                      data = trainSA, 
                      family = "binomial")

# Make predictions on the training and test sets
train_pred <- predict(logistic_model, newdata = trainSA, type = "response")
test_pred <- predict(logistic_model, newdata = testSA, type = "response")

# Define the misclassification function
missClass <- function(values, prediction) {
  sum(((prediction > 0.5) * 1) != values) / length(values)
}

# Calculate misclassification rate for the training set
train_misclass_rate <- missClass(trainSA$chd, train_pred)
print(paste("Misclassification Rate on Training Set:", train_misclass_rate))

# Calculate misclassification rate for the test set
test_misclass_rate <- missClass(testSA$chd, test_pred)
print(paste("Misclassification Rate on Test Set:", test_misclass_rate))
