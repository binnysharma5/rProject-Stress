install.packages('caret')
library('caret')
install.packages('caTools')
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
training_set[1:268,8:9] = scale(training_set[1:268,8:9])
test_set[269:,8:9] = scale(test_set[,8:9])

training_set[["Levels"]] = factor(training_set[["Levels"]])

trctrl <- trainControl(method = "repeatedcv", number = 10 , repeats = 3)
svm_linear <- train(Levels ~. , data = training_set, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


svm_linear
summary(svm_linear)

test_pred <- predict(svm_linear, test_set$Levels)
test_pred

confusionMatrix(table(test_pred, test_set$Levels))