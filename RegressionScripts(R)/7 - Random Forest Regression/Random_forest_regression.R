# Random Forest Regression

# Importing dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(123)
regressor = randomForest(x = dataset[1], # independent variables (dataframe) --> []
                         y = dataset$Salary, # dependent variable (vector) --> $
                         ntree = 120)

# Predecting a new result
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))

# Visualising the random forest regression results (Higher Resolution)
library(ggplot2)

X_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red') +
  geom_line(aes(x = X_grid, y = predict(regressor, newdata = data.frame(Level = X_grid))), color = 'blue') +
  ggtitle('Random Forest Regression') +
  xlab('Position Level') +
  ylab('Salary')
