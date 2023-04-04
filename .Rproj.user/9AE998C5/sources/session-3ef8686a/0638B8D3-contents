install.packages('caret')
library('caret')
install.packages("readxl")

library("readxl")

my_data <- read_excel("/Users/HP/Desktop/Book2.xlsx")
str(my_data)

set.seed(3033)
intrain <- createDataPartition(y = my_data$target, p = 0.8, list = FALSE)
training <- my_data[intrain,]
testing <- my_data[-intrain,]

dim(training);
dim(testing);

anyNA(my_data)
summary(my_data)

training[["target"]] = factor(training[["target"]])

trctrl <- trainControl(method = "repeatedcv", number = 10 , repeats = 2)
svm_linear <- train(target ~., data = training, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_linear

test_pred <- predict(svm_linear, newData = testing)
test_pred

length(testing$target)
length(test_pred)

confusionMatrix(table(test_pred, testing$target))
