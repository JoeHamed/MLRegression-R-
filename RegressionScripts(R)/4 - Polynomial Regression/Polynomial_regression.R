# Polynomial Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3] 
# I'm not going to split the dataset due to it being small..

# Fitting Linear regression to the dataset
lin_reg = lm(Salary ~ Level, dataset)
# OR 
lin_reg = lm(Salary ~ ., dataset)
summary(lin_reg)
# predicting the dependent variable results (salary)
y_pred = predict(lin_reg, newdata = dataset)

# Predicting a new result with Linear Regression
lin_pred = predict(lin_reg, newdata = data.frame(Level = 6.5))

# Predicting a new result with Polynomial Regression
poly_pred = predict(poly_reg, newdata = data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4))

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red') +
  geom_point(aes(x = 6.5, y = lin_pred), color = 'green') +
  geom_line(aes(x = dataset$Level, y = y_pred), color = 'blue') +
  ggtitle('Linear Regression') +
  xlab('Level') +
  ylab('Salary')

# dataset$Level2 = dataset$Level^ 2
# dataset$Level3 = dataset$Level^ 3
# dataset$Level4 = dataset$Level^ 4
n = 4
for (i in c(2:n)){
  dataset[[paste0('Level', i)]] = dataset$Level^i
}

# Fitting Polynomial regression to the dataset
poly_reg = lm(Salary ~ .
              , data = dataset)
y_pred2 = predict(poly_reg, newdata = dataset)
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red') +
  geom_point(aes(x = 6.5, y = poly_pred), color = 'green') +
  geom_line(aes(x = dataset$Level, y = y_pred2), color = 'blue') +
  ggtitle('Polynomial Regression') +
  xlab('Level') +
  ylab('Salary')
