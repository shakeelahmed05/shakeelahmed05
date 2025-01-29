library(haven)
DF <- read_dta("C:/Users/Admin/Downloads/Dr Shakeel/NHANES_2005-18_with_mortality.dta")
setwd("C:\\Users\\Admin\\Downloads")
library(summarytools)
names(DF)[names(DF) == "riagendr"] <- "sex" ##Gender
names(DF)[names(DF) == "ridageyr"] <- "age"##Age of respondent
names(DF)[names(DF) == "indfmin2"] <- "Income"
names(DF)[names(DF) == "ridreth3"] <- "Ethincity" ##Ethinicity
names(DF)[names(DF) == "dmdmartl"] <- "Marital"##marital status1,2,3,4,5 (ever married) vs 5(never married)
names(DF)[names(DF) == "dmdeduc2"] <- "Edu" ##Education <=2 >2

##Replacing missing values by 99

DF[is.na(DF)] <- 99

freq(DF$Income)
summary(DF$age)
freq(DF$sex)
freq(DF$Ethincity)
freq(DF$Marital)
freq(DF$Edu)

##Recoding Variables

DF$Edu_c<-ifelse(DF$Edu<=3,0,ifelse(DF$Edu>3&DF$Edu<7,1,99))
DF$Income_c<-ifelse(DF$Income<=8,0,ifelse(DF$Income>8&DF$Income<77,1,99))

##Labeling (Values)

DF$Edu_c <- factor(DF$Edu_c,
                   levels = c(0,1,99),
                   labels = c("Low Education", "High Education", "Do not know"))
DF$Income_c <- factor(DF$Income_c,
                      levels = c(0,1,99),
                      labels = c("Low Income", "High Income", "Do not know"))

library("haven")
library("survey")
library("jtools")
library("remotes")
library("svrepmisc")
DF$wtmec2yr##Weight two year 
## The svydesign function tells R about the design elements in the survey. 
##Once this command has been issued, all you need to do for your analyses is use the object that contains this information in each command. 
##Because the 2011-2012 NHANES data were released with a sampling weight (wtmec2yr), 
##a PSU variable (sdmvpsu) and a strata variable (sdmvstra), we will use these our svydesign function.
##The svydesign function looks like this:


#SOme Health related variables for Analysis

freq(DF$dpq010)##Ordv1 Frequency of Feeling Depressed (DPQ010) Scale: Not at all, Several days, More than half the days, Nearly every day.
freq(DF$smq040)## ord 2 Smoking Intensity (SMQ040) Categories: Non-smoker, Occasionally, Regularly (specific definitions may vary by cycle).
freq(DF$dbq700)###Frequency of Sugary Drinks (DBQ700):Responses: Never, Rarely, Sometimes, Often, Very Often.
###Constructing Composite End-points
#We construct an ordinal outcome variable with the following six classes ($J=6$) (no chronic condition, 1, 2, 3, 4, or 5 of the following conditions present).
#Diabetes: The doctor told  you have diabetes ("diq010"),\\
freq(DF$diq010)
#High blood pressure:  Ever told you had high blood pressure (bpq020),\\
freq(DF$bpq020)
#Congestive heart failure: Ever told had congestive heart failure (mcq160b),\\
freq(DF$mcq160b)
#Coronary heart disease: Ever told you had coronary heart disease (mcq160c)\\
freq(DF$mcq160c)
#Heart attack: Ever told you had a heart attack (mcq160e);\\
freq(DF$mcq160e)

DF$chronic_count <- rowSums(DF[, c("diq010", "bpq020", "mcq160b", "mcq160c", "mcq160e")] == 1, na.rm = TRUE)
freq(DF$chronic_count)
DF$chronic_binary<-ifelse(DF$chronic_count==0,0,1)
head(DF[, c("diq010", "bpq020", "mcq160b", "mcq160c", "mcq160e")])
## Odds Ratio
##Association between Income and Chronic Condition
freq(DF$chronic_binary)
DF_Cycle_7<-DF[DF$sddsrvyr==7,]
write.csv(DF_Cycle_7,row.names = FALSE)
write.csv(DF_Cycle_7, "C:\\Users\\Admin\\Downloads", row.names = FALSE)
library(epitools)
##Method 1
fit= glm(chronic_binary~ Income_c, data=DF, family=binomial(link="logit"))
exp(cbind(coef(fit), confint(fit)))

##Second Method
table(DF$Income_c,DF$chronic_binary)
Income_name<-c("low income", "High income", "Unknown")
Chronic_name<-c("No Chronic", "Have Chronic")
Data1 <- matrix(c(25633, 8972, 17061, 4815, 10917,  2792), nrow=3, ncol=2, byrow=TRUE)
dimnames(Data1) <- list('Program'=Income_name, 'Outcome'=Chronic_name)
OR<-oddsratio(Data1)
cbind(OR$measure, OR$p.value)

####Adjusted Log ODDs
fit= glm(chronic_binary~ Income_c+age+sex, data=DF, family=binomial(link="logit"))
exp(cbind(coef(fit), confint(fit)))
##P-value
svychisq(~sex+Edu_c, SDes, statistic="adjWald")#
fit
###Survey Weighted Odds ratio
SDes <- svydesign(id=~sdmvpsu, weights=~wtmec2yr,strata=~sdmvstra, nest=TRUE, survey.lonely.psu = "adjust", data=DF)
SDes
attach(DF)
Fit1<-svyglm(chronic_binary~Income_c, family=quasibinomial, design=SDes, na.action = na.omit)
summary(Fit1)
exp(cbind(coef(Fit1), confint(Fit1)))
D1<-data.frame(exp(cbind(coef(Fit1), confint(Fit1))), coef(summary(Fit1))[,'Pr(>|t|)'])
names(D1)<-c("OR", "95%LCL", "95%UCL", "P value")
D1
###Survey Weighted  and Adjusted for Covariates Odds ratio
Fit2<-svyglm(chronic_binary~Income_c+age+sex, family=quasibinomial, design=SDes, na.action = na.omit)
summary(Fit2)
exp(cbind(coef(Fit2), confint(Fit2)))
D2<-data.frame(exp(cbind(coef(Fit2), confint(Fit2))), coef(summary(Fit2))[,'Pr(>|t|)'])
names(D2)<-c("OR", "95%LCL", "95%UCL", "P value")
D2
###Survey Weighted  and Adjusted for Covariates with interactions Odds ratio
Fit3<-svyglm(chronic_binary~Income_c+age+sex+age*Income+sex*Income, family=quasibinomial, design=SDes, na.action = na.omit)
summary(Fit3)
exp(cbind(coef(Fit3), confint(Fit3)))
D3<-data.frame(exp(cbind(coef(Fit3), confint(Fit3))), coef(summary(Fit3))[,'Pr(>|t|)'])
names(D3)<-c("OR", "95%LCL", "95%UCL", "P value")
D3

subset1 <- subset(SDes, age > 60)
Fit4<-svyglm(chronic_binary~Income_c, family=quasibinomial, design=subset1, na.action = na.omit)
summary(Fit4)
D4<-data.frame(exp(cbind(coef(Fit4), confint(Fit4))), coef(summary(Fit4))[,'Pr(>|t|)'])
names(D4)<-c("OR", "95%LCL", "95%UCL", "P value")
D4
###Model fit Criteria for Logistic regression
psrsq(Fit1, method = c("Cox-Snell"))
psrsq(Fit2, method = c("Cox-Snell"))
psrsq(Fit3, method = c("Cox-Snell"))
###ROC plot for survey weighted data
library(pROC)
library(ROCR)
# Calculate ROC curve
fit= glm(chronic_binary~ Income_c+age+sex, data=DF, family=binomial(link="logit"))
# Create prediction object
pred <- prediction(fit$fitted.values, chronic_binary)
# Calculate performance (True Positive Rate and False Positive Rate)
perf <- performance(pred, "tpr", "fpr")

# Plot the ROC curve
plot(perf, col = "darkgreen", lwd = 2, main = "ROC Curve with ROCR")

# Add a diagonal reference line
abline(a = 0, b = 1, col = "red", lty = 2)
###Method 2 with cutpoint
library(pROC)
AUC <- roc(chronic_binary ~ fit$fitted.values, data = DF, )

plot(AUC)
plot(AUC, print.thres=TRUE)














###Survey Weighted ROC
subset1 <- subset(SDes, age > 60)
Fit4<-svyglm(chronic_binary~Income_c, family=quasibinomial, design=subset1, na.action = na.omit)
summary(Fit4)
library(svyROC)
phat <- predict(Fit4, newdata = DF, type = "response")
myaucw <- wauc(response.var = DF$chronic_binary, phat.var = phat, weights.var = DF$wtmec2yr)
myaucw
write.csv(DF_Cycle_7, "final_cycle_data.csv", row.names = FALSE)
