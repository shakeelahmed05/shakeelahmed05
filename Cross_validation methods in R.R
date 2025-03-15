library(readr)
heart <- read_csv("C:/Users/Admin/OneDrive - National University of Sciences & Technology/Desktop/DATA ANALYSIS WITH R/heart.csv")
head(as.data.frame(heart))

#Validation Set Approach
#Leave one out cross-validation(LOOCV)
#K-fold cross-Validation
#Repeated K-fold cross-validation
# package to perform data manipulation and visualization
library(tidyverse)
# package to compute  # cross - validation methods
library(ggplot2)
library(caret)
##Validation Set Approach
##A random sampling of the dataset
##Model is trained on the training data set
##The resultant model is applied to the testing data set
##Calculate prediction error by using model performance metrics
# setting seed to generate a 
# reproducible random sampling
set.seed(123)
head(heart)
# creating training data as 80% of the dataset
random_sample <- createDataPartition(heart$oldpeak, 
                                     p = 0.8, list = FALSE)
# generating training dataset
# from the random_sample
training_dataset  <- heart[random_sample, ]
# generating testing dataset
# from rows which are not 
# included in random_sample
testing_dataset <- heart[-random_sample, ]
# Building the model

# training the model by assigning oldpeak column
# as target variable and rest other columns
# as independent variables
model <- lm(oldpeak ~., data = training_dataset[1:10])
summary(model)
# predicting the target variable
predictions <- predict(model, testing_dataset[1:10])

# computing model performance metrics
CV<-data.frame(R2 = R2(predictions, testing_dataset $oldpeak),
            RMSE = RMSE(predictions, testing_dataset $oldpeak),
            MAE = MAE(predictions, testing_dataset $ oldpeak))
##Easy to implementation but highly dependent on data spliting




###Leave One Out Cross Validation
#LOOCV carry out the cross-validation in the following way:
##1-Train the model on N-1 data points
##2-Testing the model against that one data points which was left in the previous step
##3-Calculate prediction error
##4-Repeat Steps 1-3 until the model is not trained and tested on all data points
##Generate overall prediction error by taking the average of prediction errors
##In R
# training the model by assigning Oldpeak column
# as target variable
prediction<-c(); Error<-c(); AError<-c()
for (i in 1:nrow(heart)){
  training_dataset  <- heart[-i, ]
  testing_dataset <- heart[i, ]
  # Building the model
  
  # training the model by assigning oldpeak column
    DF<-training_dataset[-i,1:10]
  model<- lm(oldpeak ~., data =DF )
  # predicting the target variable
  prediction[i]<- c(predict(model, testing_dataset[,1:10]))
  
  # computing model performance metrics
  Error[i]<-c(prediction[i]-testing_dataset[,1:10]$oldpeak)
  
  AError[i]<-c(abs(prediction[i]-testing_dataset[,1:10]$oldpeak))
}
Error_mean<-heart[,1:10]$oldpeak-mean(heart[,1:10]$oldpeak)
RMSE<-sqrt(mean(Error^2))
MAE<-mean(AError)
R_sq<-1-sum(Error^2)/sum(Error_mean^2)
# printing model performance metrics
# along with other details
LOOCV<-c(R_sq,RMSE, MAE)
CV;LOOCV
######################


##K-fold Cross-Validation
##Split the dataset into K subsets randomly
##Use K-1 subsets for training the model
##Test the model against that one subset that was left in the previous step
##Repeat the above steps for K times i.e., until the model is not trained and tested on all subsets
##Generate overall prediction error by taking the average of prediction errors 
# R program to implement
# K-fold cross-validation

# setting seed to generate a 
# reproducible random sampling
set.seed(125) 

# defining training control as cross-validation and 
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 20)
# training the model by assigning oldpeak column
# as target variable and rest other column
# as independent variable
model3 <- train(oldpeak ~., data = heart[,1:10], 
               method = "lm",
               trControl = train_control)

# printing model performance metrics with other details

KF_CV<-c(model3[[4]]$Rsquared,   model3[[4]]$RMSE, model3[[4]]$MAE)
CV;LOOCV;KF_CV


#########Repeated K-Fold CV

##K-fold Cross-Validation is repeated a number of time
# setting seed to generate a 
# reproducible random sampling
set.seed(125) 

# defining training control as cross-validation and 
# value of K equal to 10
train_control <- trainControl(method = "repeatedcv",
                              number = 20, repeats=3)
# training the model by assigning oldpeak column
# as target variable and rest other column
# as independent variable
model4 <- train(oldpeak ~., data = heart[,1:10], 
                method = "lm",
                trControl = train_control)

# printing model performance metrics with other details

Rep_KF_CV<-c(model4[[4]]$Rsquared,   model4[[4]]$RMSE, model4[[4]]$MAE)
CV;LOOCV;KF_CV;Rep_KF_CV
