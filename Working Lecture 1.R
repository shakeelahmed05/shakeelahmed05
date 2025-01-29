library(haven)
DF <- read_dta("C:/Users/Admin/Downloads/Dr Shakeel/NHANES_2005-18_with_mortality.dta")
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
freq(DF$Edu_c)
freq(DF$Income_c)
##Replacing Specific Value by other values( Example: merging two categories in Ethnicity)

DF$Ethincity[DF$Ethincity == "7"] <- "6"
freq(DF$Ethincity)
##Tabulation in R
###2-Way Frequency Table
# 2-Way Frequency Table
###########################################################################
mytable <- table(Edu_c,Income_c) # A will be rows, B will be columns
mytable # print table
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages
summary(mytable) # chi-square test of indepedence
chisq.test(DF$Income_c, DF$Edu_c)
##3-Way table
# 3-Way Frequency Table
attach(DF)
mytable3 <- table(Edu_c,Income_c, Ethincity)
ftable(mytable3)
summary(mytable3) # chi-square test of indepedence
###
# cell percentages
###Descriptive statistics for survey weighted data
###############Required Packages
#install.packages("survey")
#install.packages("jtools")
#install.packages("remotes")
#remotes::install_github("carlganz/svrepmisc")

##After the packages are downloaded, they need to be loaded. 
##This needs to be done at the beginning of each R session.

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
SDes <- svydesign(id=~sdmvpsu, weights=~wtmec2yr,strata=~sdmvstra, nest=TRUE, survey.lonely.psu = "adjust", data=DF)
SDes
##Summary of design information
summary(SDes)
##Descriptive statistics with continuous variables 
##calculating the mean of a continuous variable. 
svymean(~age, SDes)
##calculating the SD of a continuous variable. 
svysd(~age, SDes)
###For more than one variables 
svymean(~age+Income, SDes, na=TRUE)
svysd(~age+Income, SDes)
##Mean with 95% CI For more than one variables 
confint(svymean(~age+Income, SDes))
cbind(svymean(~age+Income, SDes, na=TRUE), confint(svymean(~age+Income, SDes)))
####The cv function is used to get the coefficient of variation. 
##The coefficient of variation is the ratio of the standard error to the mean, multiplied by 100%.
##It is an indication of the variability relative to the mean in the population 
##It is not affected by the unit of measurement of the variable.
cv(svymean(~age+Income,design = SDes, na = TRUE))
###The Deff is a ratio of two variances.
##In the numerator we have the variance estimate from the current sample (including all of its design elements), 
###and in the denominator we have the variance from a hypothetical sample of the same size drawn as an SRS. 
###the Deff tells you how efficient your sample is compared to an SRS of equal size.
svymean(~age, SDes, na = TRUE, deff = TRUE)
###Quantiles are a useful descriptive statistic for continuous variables,
##particularly variables that are not normally distributed.
svyquantile(~age, design = SDes, na = TRUE, c(.25,.5,.75),ci=TRUE)



###Estimation of Weighted Proportion for a binary variable 
#options("scipen"=100000, "digits"=3)##Avoid Scientific notations
rbind(svytable(~sex, design=SDes), round(prop.table(svytable(~sex, design=SDes)),2))
rbind(svytable(~Income_c, design=SDes), round(prop.table(svytable(~Income_c, design=SDes)),2))
rbind(svytable(~Edu_c, design=SDes), round(prop.table(svytable(~Edu_c, design=SDes)),2))
###Cross Tabulation for two variables
prop.table(svytable(~sex+Edu_c, SDes), 1)##Survey Table for row %
prop.table(svytable(~sex+Edu_c, SDes), 2)##Survey Table for Column %
##Performing Chi_sqaure test for a 2 cross 2 Table
svychisq(~sex+Edu_c, SDes, statistic="Chisq")## For other options see documentation of svychisq
svychisq(~sex+Edu_c, SDes, statistic="adjWald")#

#Often we want estimates in a set of subpopulations,
#and svyby will do this. The first argument gives the analysis variables.
#The second gives the variables that specify subpopulations.
#The third is the survey design object and the fourth is the analysis function.
#Any other arguments are passed to the analysis function (eg quantiles=0.5 in the second example below). 
## For details:   http://r-survey.r-forge.r-project.org/survey/
##Producing mean and standard deviation of Age and Income by Ethnicity
svyby(~age+Income, ~Ethincity, SDes, svymean)
##Median age by ethnicity
svyby(~age, ~Ethincity, SDes, svyquantile, quantiles=0.5)
####Two way stratification
svyby(~age+Income, ~sex+Ethincity, SDes, svymean, keep.var=TRUE)
##Constructing  attractive tables of summary statistics using the output from these functions. 
##We use the  survey design  object created SDes in last code. 
##Estimation of proportions in the cells of a contingency table
cell_prop <- svymean(~interaction(sex, Ethincity), design = SDes)
cell_prop
##The ftable function reshapes output like this into a flattened table. 
cross_tab<- ftable(cell_prop)
cross_tab<- ftable(cell_prop,rownames = list(sex = c("Male", "Female"
             ), Ethincity = c("Hisp", "Nonhisp", "Non white", "Others","")))
round(cross_tab,digits=3)
###For continous variables 
Table_mean<-svyby(~age + Income, ~sex+Ethincity, SDes, svymean, keep.var=TRUE)
##Graphing continuous variable
svyhist(~age, SDes)
svyhist(~age, SDes, probability = FALSE)##using counts instead of probabilities
###Survey Weighted Box Plot
svyboxplot(~age~1, SDes, all.outliers=TRUE)
###boxplot by a grouping variable. The grouping variable must be a factor.

svyboxplot(~age~factor(sex), SDes, all.outliers=TRUE, main="Box Plot for Age")
###Survey Weighted Bar Chart for subpopulation analysis
barplt<-svyby(~Income_c+Edu_c, ~sex, SDes, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE, main="Box Plot for Age")
### Scatterplot with the sampling weights corresponding to the bubble size.
svyplot(~age+Income, SDes, style="bubble")
###We can make a variety of density and smoothed plots.

smth<-svysmooth(~age, design=SDes)
plot(smth)
dens<-svysmooth(~age, design=SDes,bandwidth=30)
plot(dens)

dens1<-svysmooth(~age, design=SDes)
plot(dens1)
##Survey Weighted Inference
###Wurvey Weighted t test
##Survey Weighted F test
##Adjusted Association between variables
##Survey Weighted linear Regression
Multiple linear regression
We need to use the summary function to get the standard errors, test statistics and p-values. Let’s start with a model that has no interaction terms.
 The outcome variable will be pad630, and the predictors will be female and hsq571.

summary(svyglm(pad630~female+hsq571, design=nhc, na.action = na.omit))

Now let’s add an interaction between the two predictor variables.

summary(svyglm(pad630~female*hsq571, design=nhc, na.action = na.omit))
glm1 <- (svyglm(pad630~female*hsq571, design=nhc, na.action = na.omit))
glm1
his example is just like the previous one, only here factor notation is used. This is important when the categorical predictor has more than two levels.

summary(svyglm(pad630~factor(female)*factor(dmdmartl), design=nhc, na.action = na.omit))
Non-parametric tests

Non-parametric tests can also be done. Let’s start with a Wilcoxon signed rank test, which is the non-parametric analog of an independent-samples t-test.

wil <- svyranktest(hsq496~female, design = nhc, na = TRUE, test = c("wilcoxon"))
wil

This is an example of a median test.

mtest <- svyranktest(hsq496~female, design = nhc, na = TRUE, test=("median"))
mtest
This is an example of a Kruskal Wallis test, which is the non-parametric analog of a one-way ANOVA.

kwtest <- svyranktest(hsq496~female, design = nhc, na = TRUE, test=("KruskalWallis"))
kwtest

##Survey Weighted logistic regression Regression
Logistic regression

Let’s see a few examples of logistic regression.

logit1 <- (svyglm(paq665~factor(hsd010)+ridageyr, family=quasibinomial, design=nhc, na.action = na.omit))
summary(logit1)
subset1 <- subset(nhc, ridageyr > 20)
logit2 <- (svyglm(paq665~factor(hsd010)+ridageyr, family=quasibinomial, design=subset1, na.action = na.omit))
summary(logit2)
We can also get a Wald test for a variable in the model.

regTermTest(logit2, ~ridageyr)


Instead of getting an R-squared value as you do in linear regression, a pseudo-R-squared is given in logistic regression. There are many different versions of pseudo-R-squared, and two of them are available with the psrsq function.

psrsq(logit2, method = c("Cox-Snell"))
[1] 0.05148869
psrsq(logit2, method = c("Nagelkerke"))
[1] 0.06873682
###Survey Weighted Ordinal Logistic regression 
Ordered logistic regression
#We construct a binary variable with the following six classes ($J=6$) (no chronic condition, 1, 2, 3, 4, or 5 of the following conditions present).
##Constructing a new end point
Ord_1=ifelse(Ord_10==0,1,ifelse(Ord_10==1,2,ifelse(Ord_10==2,3,ifelse(Ord_10==3,4, 
                                                                      ifelse(Ord_10==4,5, 6)))))
Below is an example of an ordered logistic regression. Note that the outcome variable must be a factor.

ologit1 <- svyolr(factor(dmdeduc2)~factor(female)+factor(dmdborn4)+pad680, design = nhc, method = c("logistic"))
summary(ologit1)

##Survey Weighted Count Regressions
Poisson regression

Poisson regression can be run. This is a type of count model (meaning that the outcome variable should be a count).

summary(svyglm(pad675~female, design=nhc, family=poisson()))
##Survey Weighted ROC Analysis
##Survey Weighted PRC Analysis
## Survey Weighted correlation Analysis
#########################################################

##Performing a t-test with weighted data:
svyttest(~variable, by = ~group, design = my_survey)


##Generalized linear models, including the linear model, are estimated by svyglm. 
##This has almost the same arguments as glm, 
##the difference being that the data argument to glm is replaced by a design argument to svyglm. 
