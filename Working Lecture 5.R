##Regression Modeling for Count Data in R
library(psych)
library(readxl)
library(summarytools)
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

DF$Ethincity
Poisson_1 = glm(chronic_count ~ sex+Ethincity+age,
              data=DF,
              family="poisson")
summary(Poisson_1)
library(car)
Anova(Poisson_1,
      type="II",
      test="LR")
##Check for influencial observations
influencePlot(Poisson_1)


### Interpretation:

## Check for Over dispersion
library(AER)
deviance(Poisson_1)/Poisson_1$df.residual
dispersiontest(Poisson_1)
### Negative binomial regression example

library(MASS)
NB_1 = glm.nb(chronic_count ~ sex+Ethincity,
              data=DF,
              control = glm.control(maxit=500))
summary(NB_1)

library(car)
Anova(NB_1,
      type="II",
      test="LR")

influencePlot(NB_1)
##Zero Inflated and Hurdle Models
#Zero-inflated models assume there are two sources of zeros ("structural zeros" and "sampling zeros") 
## Hurdle models consider all zeros as a single category,
##essentially a "hurdle" that needs to be crossed before observing positive counts; 
##both models uses a binary logistic component to model the probability of a zero  and 
## a count distribution like Poisson or negative binomial to model the positive counts


library(pscl)

ZIP_1= zeroinfl(chronic_count~factor(Ethincity)+sex+Income_c,
                    data = DF,
                    dist = "poisson")
summary(ZIP_1)
confint(ZIP_1)
Report<-data.frame(coef(ZIP_1),confint(ZIP_1) )
##Check Overall model performance
library(rcompanion)
nagelkerke(ZIP_1)



##Zero Inflated NB

ZINB_1 = zeroinfl(chronic_count ~ sex+Ethincity+Income_c,
                data = DF,
                dist = "negbin")
summary(ZINB_1)
confint(ZINB_1)
library(rcompanion)
nagelkerke(ZINB_1)

##Checking for inflated zeros
hist(DF$chronic_count)
vuong(Poisson_1,
      ZIP_1,
      digits = 4)
### Positive Vuong z-statistic suggests that model 1 is superior,
###   The p value suggests that there is significance difference  in the AIC and BIC of two models,
## Things to Remember
##Since zip has both a count model and a logit model, each of the two models should have good predictors.
##The two models do not necessarily need to use the same predictors.
##Problems of perfect prediction, separation or partial separation can occur in the logistic part of the zero-inflated model.
##Count data often use exposure variables to indicate the number of times the event could have happened.
##You can incorporate a logged version of the exposure variable into your model by using the offset() option.
##It is not recommended that zero-inflated Poisson models be applied to small samples. What constitutes a small sample does not seem to be clearly defined in the literature.
##Pseudo-R-squared values differ from OLS R-squareds, please see FAQ: What are pseudo R-squareds? for a discussion on this issue.
##The Vuong test is only applicable when comparing models that are not nested within each other, 
##meaning one model cannot be derived by adding or removing parameters from the other
# Hurdle Count Models
##Logit Poisson
Hurdle_1 <- hurdle(chronic_count ~ sex+Ethincity+Income_c, data = DF)
summary(Hurdle_1)
confint(Hurdle_1)
##Interpretation
##Coefficient in Zero partIndicates that an increase in the predictor variable increases the probability of observing a zero count.
###Indicates that an increase in the predictor variable leads to a higher expected non-zero count.
## logit-negbin
Hurdle_2 <- hurdle(chronic_count ~ sex+Ethincity+Income_c, data = DF, dist = "negbin")
summary(Hurdle_2)
confint(Hurdle_2)
vuong(Poisson_1,
      Hurdle_1,
      digits = 4)