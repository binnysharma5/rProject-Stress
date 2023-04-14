# Jai Shree Ram

install.packages("rtools")
install.packages("FSelector")
install.packages("rpart")
install.packages("caret", dependencies = TRUE )
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("xlsx")
install.packages("data.tree")
install.packages("caTools")
install.packages("e1071")

library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)
library(e1071)


Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_202\\jre")
Sys.getenv('JAVA_HOME')
setwd("C:\\Users\\HP\\Desktop")
add <- "Stress2.xlsx"
df <- read.xlsx(add, sheetName = "Sheet1")


df <- dplyr::select(df, Score, Levels)


df <- mutate(df, Score= as.numeric(Score), Levels= factor(Levels))

set.seed(5)
sample = sample.split(df$Levels, SplitRatio = .70)
train = subset(df,sample==TRUE)
test = subset(df, sample == FALSE)


trctrl <- trainControl(method = "repeatedcv", number = 10 , repeats = 3)
svm_linear <- train(Levels ~., data = test, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm.model = svm(Levels ~., data = test, type='C-classification', kernel='linear', scale=FALSE)
summary(svm.model)
test_pred <- predict(svm.model, newData = test)
svm_linear


test_pred <- predict(svm_linear, newData = test)
test_pred
confusionMatrix(test_pred, test$Levels)




length(test$Levels)
length(test_pred)

confusionMatrix(table(test_pred, testing$Levels))
str(test_pred)
str(test$Levels)



