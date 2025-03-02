library(ROCit)
library(ggplot2)##For Curve

library(readr)
DF<- read_csv("C:/Users/Admin/OneDrive - National University of Sciences & Technology/Desktop/DATA ANALYSIS WITH R/heart.csv")

# check the class variable
class(DF$thal)
head(DF)
logistic.model <- glm(as.factor(target)~trestbps+age+sex+chol,
                      data = DF,family = "binomial")
class <- logistic.model$y ##Class lebel
score <- logistic.model$fitted.values ##Probabilities
# -------------------------------------------------------------
measure <- measureit(score = score, class = class,
                     measure = c("ACC", "SENS",  "FSCR"))
names(measure)
##
plot(measure$ACC~measure$Cutoff, type = "l")##Cut off values for probabilities
##We can find following things using the measureit command
#ACC: Overall accuracy of classification.
#MIS: Misclassification rate.
#SENS: Sensitivity.
#SPEC: Specificity.
#PREC: Precision.
#REC: Recall. Same as sensitivity.
#PPV: Positive predictive value.
#NPV: Positive predictive value.
#TPR: True positive rate.
#FPR: False positive rate.
#TNR: True negative rate.
#FNR: False negative rate.
#pDLR: Positive diagnostic likelihood ratio.
#nDLR: Negative diagnostic likelihood ratio.
#FSCR: F-score,.
###ROC CURVE
roc_1 <- rocit(score = DF$chol, class = DF$target,
                       negref = "0") 
##Check the class
class(roc_1)
##See the Summary
summary(roc_1)
##Check Names
names(roc_1)
##Number of Positive Counts
message("Number of positive responses used: ", roc_1$pos_count)
#> Number of positive responses used: 60
message("Number of negative responses used: ", roc_1$neg_count)
#> Number of negative responses used: 329
#> 
##The Cutoffs are in descending order. TPR and FPR are in ascending order.
##The first cutoff is set to Inf
##and the last cutoff is equal to the lowest score in the data that are used for ROC curve estimation. 
##A score greater or equal to the cutoff is treated as positive. 
head(cbind(Cutoff=roc_1$Cutoff, 
           TPR=roc_1$TPR, 
           FPR=roc_1$FPR))

tail(cbind(Cutoff=roc_1$Cutoff, 
           TPR=roc_1$TPR, 
           FPR=roc_1$FPR))
##Ploting ROC
plot(roc_1, values = F)

# AUC value
ciAUC(roc_1)
##95% CI
ciAUC(roc_1, level = 0.95)
###ROC assuming a Binomial Model
roc_bin<- rocit(score = DF$chol, 
                      class = DF$target,
                      negref = "0", 
                      method = "bin") 

roc_nonpar <- rocit(score = DF$chol, 
                    class = DF$target,
                    negref = "0", 
                           method = "non") 
summary(roc_bin)
summary(roc_nonpar)

##AUC using Delong method
#The DeLong method compares the differences between paired AUC values and their standard errors
ciAUC(roc_bin, delong = TRUE)
# bootstrap method for obtaining AUC
set.seed(200)
ciAUC_boot <- ciAUC(roc_nonpar, 
                    level = 0.95, nboot = 200)
print(ciAUC_boot)
##Confidence interval of ROC curve:
ciROC_1<- ciROC(roc_1, level = 0.95)
set.seed(200)
ciROC_bin <- ciROC(roc_bin, 
                     level = 0.95, nboot = 200)
plot(ciROC_1, col = 1, legend = FALSE)
lines(ciROC_bin$TPR~ciROC_bin$FPR, 
      col = 2, lwd = 2)
lines(ciROC_bin$LowerTPR~ciROC_bin$FPR, 
      col = 2, lty = 2)
lines(ciROC_bin$UpperTPR~ciROC_bin$FPR, 
      col = 2, lty = 2)
legend("bottomright", c("Empirical ROC",
                        "Binormal ROC",
                        "95% CI (Empirical)", 
                        "95% CI (Binormal)"),
       lty = c(1,1,2,2), col = 
         c(1,2,1,2), lwd = c(2,2,1,1))
###