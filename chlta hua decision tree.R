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


library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)


Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_202\\jre")
Sys.getenv('JAVA_HOME')
setwd("C:\\Users\\HP\\Desktop")
add <- "Stressdata.xlsx"
df <- read.xlsx(add, sheetName = "Sheet1")


df <- dplyr::select(df, Score, Levels)


df <- mutate(df, Score= as.numeric(Score), Levels= factor(Levels))







df <- mutate(df, Score= as.numeric(Score), Levels= as.numeric(Levels))

set.seed(5)
sample = sample.split(df$Levels, SplitRatio = .70)
train = subset(df,sample==TRUE)
test = subset(df, sample == FALSE)

tree <- rpart(Levels ~., data = train)

tree_pred <- predict(tree, test, type='class')

confusionMatrix(tree_pred,test$Levels )

str(tree_pred)
str(test$Levels)

