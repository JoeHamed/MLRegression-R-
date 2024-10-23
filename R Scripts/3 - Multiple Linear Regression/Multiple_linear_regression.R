# Multiple Linear regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding Categorial features
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into training and test sets
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
split
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
# OR
regressor = lm(formula = Profit ~ .,
               data = training_set)
# summary(regressor)
# Predicting the test set results
y_pred = predict(regressor, newdata = test_set)

# ---------------------------------------------------------------------------------------

# Building the optimal model using backward elimination

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset) # dataset (in order to have complete information about which independent variable is static.seg.)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, 
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, 
               data = dataset)
summary(regressor)
# (P-VALUE) =~ 0.05 or 5% (decided to keep marketing spend)

# Predicting the test set results
y_pred = predict(regressor, newdata = test_set)
coef(summary(regressor))
length(dataset)

# ---------------------------------------------------------------------------------------
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)