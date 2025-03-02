library(readr)
library(summarytools)
DF<- read_csv("C:/Users/Admin/Downloads/archive/Self_esteemed.csv")
#Q1. I feel that I am a person of worth, at least on an equal plane with others.	
#Q2. I feel that I have a number of good qualities.	
#Q3. All in all, I am inclined to feel that I am a failure.	
#Q4. I am able to do things as well as most other people.	
#Q5. I feel I do not have much to be proud of.	
#Q6. I take a positive attitude toward myself.	
#Q7. On the whole, I am satisfied with myself.	
#Q8. I wish I could have more respect for myself.	
#Q9. I certainly feel useless at times.	
#Q10. At times I think I am no good at all.
#https://www.kaggle.com/datasets/lucasgreenwell/rosenberg-self-esteem-scale-responses?resource=
library(psych)
psych::alpha(as.data.frame((DF[,c(1:10)])))
psych::alpha(as.data.frame((DF[,c(1:10)])), check.keys = TRUE)
head(DF)
#α ≥ 0.9 → Excellent reliability
#0.8 ≤ α < 0.9 → Good reliability
#0.7 ≤ α < 0.8 → Acceptable reliability
#0.6 ≤ α < 0.7 → Questionable reliability
#α < 0.6 → Poor reliability

DF<-DF[DF$Equal_worth_as_others>0&DF$Have_good_qualities>0&DF$Inclined_to_failure>0,]




DF$Equal_worth_as_others.ord<-factor(DF$Equal_worth_as_others, order = TRUE, 
                                 levels = c("1", "2", "3", "4"))

DF$Have_good_qualities.ord<-factor(DF$Have_good_qualities, order = TRUE, 
                                   levels = c("1", "2", "3", "4"))

DF$Inclined_to_failure.ord<-factor(DF$Inclined_to_failure, order = TRUE, 
                                 levels = c("1", "2", "3", "4"))

str(DF)
dim(DF)
Xtab = xtabs(~ Equal_worth_as_others.ord,
           data=DF)
barplot(Xtab,  
        col="dark gray",
        xlab="Likert Scale",
        ylab="Frequency")
boxplot(DF$Have_good_qualities.ord,
        ylab="Likert scores",
        xlab="I have High Qualities")


Xtab_gender = xtabs(~gender+ Have_good_qualities.ord, data=DF)

Xtab_gender

barplot(Xtab_gender,
        beside=TRUE,
        legend=TRUE,
        xlab="Likert score",
        ylab="Frequency")
boxplot(Equal_worth_as_others.ord~ gender,
        data=DF,
        names=c("Unknown","Male","Female", "Others"),
        ylab="Value")
library(psych)
median(DF$Equal_worth_as_others)
library(FSA)

Summarize(Equal_worth_as_others ~ 1,
          data=DF,
          digits=3)
##CI for median
wilcox.test(DF$Equal_worth_as_others,
            alternative="two.sided",
            correct=TRUE,
            conf.int=TRUE,
            conf.level=0.95)
