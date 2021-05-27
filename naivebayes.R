#install.packages("naivebayes")
library(naivebayes)
library(caret)
library(ggplot2)
library(psych)
library(e1071)

salary_train<- `SalaryData_Train`
str(salary_train)
salary_train$Salary<-as.factor(salary_train$Salary)
class(salary_train)

salary_test<-`SalaryData_Test`
str(salary_test)
salary_test$Salary<-as.factor(salary_test$Salary)
class(salary_test)

#Visualization 
# Plot and ggplot 
ggplot(data=salary_train,aes(x=Salary, y = age, fill =Salary)) +geom_boxplot() +ggtitle("Box Plot")
ggplot(data=salary_train,aes(x=Salary, y = hoursperweek, fill = Salary)) +geom_boxplot() +ggtitle("Box Plot")

#Density Plot 
ggtitle("Age - Density Plot")
ggplot(data=salary_train,aes(x = age, fill = Salary)) + geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Workclass Density Plot")
ggplot(data=salary_train,aes(x = workclass, fill =Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggtitle("education Density Plot")
ggplot(data=salary_train,aes(x = education, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggtitle("educationno Density Plot")
ggplot(data=salary_train,aes(x =educationno, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggtitle("maritalstatus Density Plot")
ggplot(data=salary_train,aes(x = maritalstatus, fill = Salary)) + geom_density(alpha = 0.9, color = 'Violet')

ggtitle("occupation Density Plot")
ggplot(data=salary_train,aes(x = occupation, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggtitle("sex Density Plot")
ggplot(data=salary_train,aes(x = sex,fill = Salary))+geom_density(alpha = 0.9, color = 'Violet')

ggtitle("relationship Density Plot")
ggplot(data=salary_train,aes(x = relationship, fill = Salary))+geom_density(alpha = 0.9, color = 'Violet')

ggtitle("race Density Plot")
ggplot(data=salary_train,aes(x = race, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggtitle("capitalgain Density Plot")
ggplot(data=salary_train,aes(x = capitalgain, fill = Salary)) + geom_density(alpha = 0.9, color = 'Violet')

ggtitle("capitalloss Density Plot")
ggplot(data=salary_train,aes(x = capitalloss, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggtitle("hoursperweek Density Plot")
ggplot(data=salary_train,aes(x = hoursperweek, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggtitle("native Density Plot")
ggplot(data=salary_train,aes(x = native, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

# Naive Bayes Model 
Model <- naiveBayes(salary_train$Salary ~ ., data = salary_train)
Model

Model_pred <- predict(Model,salary_test)
mean(Model_pred==salary_test$Salary)

confusionMatrix(Model_pred,salary_test$Salary)
