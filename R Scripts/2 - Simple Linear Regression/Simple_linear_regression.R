# Simple linear regression 

# importing dataset
dataset = read.csv('Salary_Data.csv')

#splitting dataset into training and test sets
library(caTools)
set.seed(123) #random_state
split = sample.split(dataset$Salary, SplitRatio = 2/3)
split
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting simple linear regression to the training set
regressor = lm(formula = Salary ~ YearsExperience, training_set)
#summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Visualising the Training set results
# install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of Experience') + 
  ylab('Salary')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'green') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of Experience') +
  ylab('Salary')