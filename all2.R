install.packages('caret')
library('caret')
install.packages("readxl")

library("readxl")

my_data <- read_excel("/Users/HP/Desktop/stresstest.xlsx")
str(my_data)
library(e1071)
ir=my_data
train=ir[1:7,]
test=ir[8:10,]
model=naiveBayes(score~.,data=train)
pred=predict(model,test)
table(pred)

