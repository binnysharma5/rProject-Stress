install.packages('caret')
library('caret')
install.packages('caTools')
install.packages("readxl")

library("readxl")

my_data <- read_excel("/Users/HP/Desktop/stresstest.xlsx")
str(my_data)

# splitting data into train and test 

set.seed(2)
library(caTools)

split <- sample.split(my_data, SplitRatio = 0.7)
split

train <- subset(my_data, split="TRUE")
train

test <- subset(my_data, split="FALSE")
test

# create model 

Model <- lm(score ~., data = train)
summary(Model)


# Prediction
pred <- predict(Model, test)
pred


plot(my_data)
plot(test$score, type = "l", lty = 1.8, col = "red")
lines(pred, type = "l", col = "blue")
plot(pred, type = "l", lty = 1.8, col = "blue")
