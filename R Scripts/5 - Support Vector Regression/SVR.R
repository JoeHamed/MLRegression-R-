# SVR

#install.packages('e1071')
library(e1071)
library(ggplot2)

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# X = dataset[, 1]
# y = dataset[, 2]
# # Feature Scaling (No need for it when using the e1071 library)
# X = scale(X)
# y = scale(y)
# # Merge after scaling
# dataset = data.frame(Position.levels = c(X),
#                      Salary = c(y))

regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = "eps-regression")

# Predicting a Single result
y_pred = predict(regressor, data.frame(Level = 6.5))

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)), color = 'blue') +
  ggtitle("Support Vector Regression") +
  xlab('Position Levels') +
  ylab('Salaries')