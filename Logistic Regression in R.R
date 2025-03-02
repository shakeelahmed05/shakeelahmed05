install.packages("caTools") # For Logistic regression
install.packages("ROCR")
library(caTools)##Package required for splitting data
library(ROCR) ##Call library for ROC curve
dat=read.csv(file.choose())
head(dat)
str(dat)
is.na(dat)
split <- sample.split(dat, SplitRatio = 0.8) 
train_reg <- subset(dat, split == "TRUE")
test_reg <- subset(dat, split == "FALSE")
mod1=glm(happy~ infoavail+housecost+schoolquality
+policetrust+streetquality+event, data = train_reg, family = "binomial")
###Running a logistic regression model

summary(mod1)## Checking model summary which yield
predict_reg <- predict(mod1, test_reg, type = "response")
predict_reg_p<- ifelse(predict_reg >0.5, 1, 0) 
table(test_reg$happy, predict_reg)
library(caret)


# create confusion matrix 
confusionMatrix(as.factor(test_reg$happy), as.factor(predict_reg_p),
                mode = "everything",
                positive="1")