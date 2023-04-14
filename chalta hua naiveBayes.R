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
install.packages("class")

library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)
library(e1071)
library(class)


Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_202\\jre")
Sys.getenv('JAVA_HOME')
setwd("C:\\Users\\HP\\Desktop")
add <- "Stress2.xlsx"
df <- read.xlsx(add, sheetName = "Sheet1")


df <- dplyr::select(df, Score, Levels)


df <- mutate(df, Score= as.numeric(Score), Levels= factor(Levels))

set.seed(5)
sample = sample.split(df$Levels, SplitRatio = .70)
train_scale = subset(df,sample==TRUE)
test_scale = subset(df, sample == FALSE)

classifier_cl <- naiveBayes(Levels ~ ., data = train_scale)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_scale)

# Confusion Matrix
cm <- table(test_scale$Levels, y_pred)
cm

confusionMatrix(cm)




























