##Dealing with Missing Values in R
x<-c(1:8,NA, NA)
y<-c(NA, NA, 8:1)
z<-c(1:3,NA, NA, 5:1)
DF<-data.frame(x,y,z)
is.na(DF)

# Number of missing values column wise
100*colSums(is.na(DF))/nrow(DF)
# Number of missing values rows wise
rowSums(is.na(DF[ , 1:3]))

##Remove the variable with all missing values and removing missing cases
DF[complete.cases(DF), ]
##Remove the cases with missing value
na.omit(DF)
# excludes every row containing even one NA
na.exclude (DF)
##Returning rows with complete cases
DF1<-DF[rowSums(is.na(DF[ , 1:3])) == 0, ]
mean(DF$x, na.rm=TRUE) # Will exclude missing cases from the analysis
##na.fail– halts and does not proceed if NA is encountered
mean(DF$x, na.rm=TRUE) 
na.fail(DF)
na.fail(DF1)
##Visualizing Missing Values in R
# Install and load the 'visdat' package
#install.packages("visdat")
library(visdat)
vis_miss(DF)
DF$y[is.na(DF$y)]<-mean(DF$y,na.rm=TRUE)

