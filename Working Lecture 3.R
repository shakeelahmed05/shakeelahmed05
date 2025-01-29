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

write.csv(DF_Cycle_7, "C:\\Users\\Admin\\Downloads", row.names = FALSE)