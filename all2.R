install.packages('caret')
library('caret')
install.packages("readxl")

library("readxl")

my_data <- read_excel("/Users/HP/Desktop/Stressdata.xlsx")
View(my_data)

install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(my_data$Levels,SplitRatio = 0.8)  
training_set= subset(my_data, split == TRUE)
test_set=subset(my_data, split == FALSE)
training_set[,8:9] = scale(training_set[,8:9])
test_set[,8:9] = scale(test_set[,8:9])
str(my_data)
library(e1071)
ir=my_data
train=ir[1:7,8:9]
test=ir[8:10,8:9]
model=naiveBayes(levels~.,data=train, na.action = )
pred=predict(model,test)
table(pred)

