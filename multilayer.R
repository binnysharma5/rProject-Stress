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
install.packages("neuralnet")

library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)
library(e1071)
library(MASS)


Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_202\\jre")
Sys.getenv('JAVA_HOME')
setwd("C:\\Users\\HP\\Desktop")
add <- "Stress2.xlsx"
df <- read.xlsx(add, sheetName = "Sheet1")


df <- dplyr::select(df, Score, Levels)


df <- dplyr::mutate(df, Score= as.numeric(Score), Levels= factor(Levels))



vars_scale <- df %>% 
  select_if(is.numeric) %>% 
  colnames()


scale_min_max <- function(x) scale(x, center = min(x), scale = max(x) - min(x))


df %>% 
  mutate_at(vars_scale, scale_min_max)



set.seed(5)
sample = sample.split(df$Levels, SplitRatio = .70)
train = subset(df,sample==TRUE)
test = subset(df, sample == FALSE)

apply(df, 2, function(x) sum(is.na(x)))
lm.fit <- glm(Score ~., data = train)
summary(lm.fit)
pr.lm <- predict(lm.fit, test)
MSE.lm <- sum((pr.lm - test$Score)^2) / nrow(test)


maxs <- apply(df, 2, max)
mins <- apply(df, 2, min)
scaled <- as.data.frame(scale(df,scale_min_max))
train_ <- scale_min_max[index, ]
test_ <- scale_min_max[-index, ]

# Applying Neural network concepts
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~",
                      paste(n[!n %in% "medv"],
                            collapse = " + ")))
nn <- neuralnet(f, data = train_,
                hidden = c(4, 2),
                linear.output = T)


training_set = subset(df, subset = split)
test_set = subset(df, subset = !split)

apply(df, 2, function(x) sum(is.na(x)))
index <- 234
train <- df[index, ]
test <- df[-index, ]
lm.fit <- glm(formula = Levels ~ ., data = training_set)
summary(lm.fit)
pr.lm <- predict(lm.fit, test)
MSE.lm <- sum((pr.lm - test$medv)^2) / nrow(test)
maxs <- apply(df, 2, max)
mins <- apply(df, 2, min)
scaled <- as.data.frame(scale(df,
                              center = mins,
                              scale = maxs - mins))
train_ <- scaled[index, ]
test_ <- scaled[-index, ]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("Levels ~",
                      paste(n[!n %in% "Levels"],
                            collapse = " + ")))
nn <- neuralnet(f, data = train_,
                hidden = c(4, 2),
                linear.output = T)

plot(nn)
tree_pred <- predict(nn, test_)

confusionMatrix(tree_pred,test_$Levels )
dim(tree_pred)
dim(test_$Levels)


results <- data.frame(actual = testset$consumption, prediction = nn.results$net.result)
results

nn$result.matrix

