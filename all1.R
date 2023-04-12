install.packages('caret')
library('caret')
install.packages("readxl")

library("readxl")

my_data <- read_excel("/Users/HP/Desktop/stresstest.xlsx")
head(my_data)
str(my_data)
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}

my_data.n <- as.data.frame(lapply(my_data[1:9],normalize))
head(my_data.n)

set.seed(123)
dat.d<- sample(1:nrow(my_data.n),size=nrow(my_data.n)*0.7,replace=FALSE)

train.my_data <- my_data[dat.d,]
test.my_data <- my_data[dat.d,]

train.my_data_labels <- my_data[dat.d,1]
test.my_data_labels <- my_data[-dat.d,1]

install.packages("class")
library(class)

NROW(train.my_data_labels)
my_data_train <- my_data[1:6,8]
my_data_test <- my_data[7:9,9]
my_data_train_target <- my_data[1:6,9]
my_data_test_target <- my_data[7:9,9]
dim(my_data_test_target)
dim(my_data_train_target)



m1 <- knn3(train=my_data_train, test=my_data_test, cl=my_data_train_target, k=3)
m2 <- knn3(train=my_data_train, test=my_data_test, cl=my_data_train_target, k=4)

ACC.3<- 100*sum(test.my_data==knn.3)/NROW(test.my_data)
ACC.4<- 100*sum(test.my_data==knn.4)/NROW(test.my_data)
