# Decision Tree Regression

# Import the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

#install.packages('rpart')
library(rpart)
library(ggplot2)

# Fitting Decision Tree Regression to thge dataset
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1)) #Number of splits 

# Predcting a new result
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))
X_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)

# Visualising the Decision Tree Regression results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red') +
  geom_line(aes(x = X_grid, y = predict(regressor, newdata = data.frame(Level = X_grid))), color = 'blue') +
  ggtitle('Decision Tree Regression') +
  xlab('Position Level')+
  ylab('Salaries')

plot(regressor)
text(regressor)