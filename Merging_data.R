library(haven)  # For handling haven_labelled data
library(dplyr)  # For data manipulation
library(summarytools)
DF_child<-read.csv(file.choose())
DF_Women<-read.csv(file.choose())
DF_birth<-read.csv(file.choose())
DF_HH<-read.csv(file.choose())
DF_HL<-read.csv(file.choose())
dim(DF_Women)
dim(DF_child)
dim(DF_birth)
dim(DF_HH)
dim(DF_HL)
head(DF_child)
head(DF_HH)
names(DF_HH)[names(DF_HH) == "hh.HH1"] <- "ch.HH1"
names(DF_HH)[names(DF_HH) == "hh.HH2"] <- "ch.HH2"
names(DF_HH)[names(DF_HH) == "hh.HH26B"] <- "ch.LN"
merged_CH_HH1b <- left_join(DF_child, DF_HH, 
                            by = c("ch.HH1", 
                                   "ch.HH2"),
                            relationship = "many-to-one")
head(merged_CH_HH1b)
freq(DF_child$ch.LN)
freq(merged_CH_HH1b$hh.windex5)

