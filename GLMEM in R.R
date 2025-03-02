library(lattice)
##A Plot 
library(readr)
DF<- read_csv("C:/Users/Admin/OneDrive - National University of Sciences & Technology/Desktop/DATA ANALYSIS WITH R/heart.csv")
head(DF)
library(lme4)
xyplot(target ~ age |thal, DF, type=c('g','p','p'),
       layout=c(2,2), index.cond = function(x,y)max(y))
(glmm1 <- glmer(target ~ age+sex+cp+trestbps+chol+fbs+restecg + (1 |thal),
              data = DF, family = binomial))
summary(glmm1)
#When there is no clustering we can still use the glmer taking the individual observations as group
#For now we include the individual observation as grouping variable and see the difference in result using ANOVA
DF$obs <- 1:nrow(DF)
(glmm2 <- glmer(target ~ age+sex+cp+trestbps+chol+fbs+restecg + (1 |thal)+  (1|obs),
              family = binomial, data = DF))
anova(glmm1,glmm2)
#The two models are almost giving the same result
(glmm3 <- glmer(target ~ age+sex+cp+trestbps+chol+fbs+restecg +  (1|obs),
                family = binomial, data = DF))
anova(glmm1, glmm3)

##Simple Logistic regression
(glm0 <- glm(target ~ age+sex+cp+trestbps+chol+fbs+restecg,
                family = binomial, data = DF))
anova(glmm1, glmm3, glm0)



#### GLMEM For Count Data 
##GLMEM using Poisson Link FUnction
##GLMEM using NB Link FUnction
DF<- read_excel("C:/Users/Admin/Downloads/final_cycle_data.xlsx")
dim(DF)
DF$chronic_count
library(lattice)
freq(DF$chronic_count)
histogram(~ chronic_count| sex,
          data=DF,
          layout=c(1,3)      #  columns and rows of individual plots
)
histogram(~ chronic_count| Ethincity,
          data=DF,
          layout=c(1,3)      #  columns and rows of individual plots
)
xyplot(chronic_count~ age |Ethincity, DF, type=c('g','p','p'),
       layout=c(2,2), index.cond = function(x,y)max(y))
(glmm1 <- glmer(chronic_count ~ age+ (1 |Ethincity),
                data = DF, family = poisson))
summary(glmm1)
##Negative Binomial
(glmm2 <- glmer.nb(chronic_count ~ age+ (1 |Ethincity),
                   data = DF, verbose=TRUE))
summary(glmm2)



