library(summarytools)
library(readxl)
DF<- read_excel("C:/Users/Admin/Downloads/final_cycle_data.xlsx")
dim(DF)
freq(DF$smq040)## ord 2 Smoking Intensity (SMQ040) Categories: 
##Non-smoker, Occasionally, Regularly (specific definitions may vary by cycle).
DF1<-DF[DF$smq040<7,]
freq(DF1$smq040)
freq(DF1$wtmec2yr)
require(MASS)
require(Hmisc)
OLR <- polr(as.factor(smq040) ~ age + sex, data = DF1, Hess=TRUE)
beta<-coef(summary(OLR))[c(1:2),]
exp(beta)
##For survey Weighted data
library(survey)
SDes<- svydesign(id=~sdmvpsu, weights=~wtmec2yr,strata=~sdmvstra, nest=TRUE, survey.lonely.psu = "adjust", data=DF1)
freq(SDes$allprob)
ologit1 <- svyolr(factor(smq040)~sex+age, design = SDes[SDes$allprob<Inf,], method = c("logistic"))
summary(ologit1)
beta<-coef(summary(ologit1))
p <- pnorm(abs(beta[, "t value"]), lower.tail = FALSE) * 2
Result=data.frame(beta, "p value" = p)

Final_table <- data.frame(Result,"OR"=exp(Result$Value), 
   "95 %LCL"=exp(Result$Value-1.96*Result$Std..Error), "95 %UCL"=exp(Result$Value+1.96*Result$Std..Error) )



