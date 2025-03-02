
ID<-c(1:10)

X<-c("a", "b", "a", "b","a", "b","a", "b", "a", "b")

Y<-c(3:5,5:1,1,2)

Z<-c(1.4,1:5,3,1.5,5.1,2)

DF<-data.frame(ID,X,Y,Z)

length(X)
DF[with(DF, order(X,-Z)), ]
##For Descending Order
DF[with(DF, order(-Y, -Z)), ]
##If you want to do this by column index, 
DF[order(DF[,3], DF[,4]), ]









ID<-c(1:10)

X<-c("a", "b", "a", "b","a", "b","a", "b", "a", "b")

Y<-c(3:5,5:1,1,2)

Z<-c(1.4,1:5,3,1.5,5.1,2)

DF1<-data.frame(ID,X,Y,Z)






###Merging Data
ID2<-c(6:15)
X2<-c("a", "a", "b", "b","a", "b","a", "a", "b", "a")
Y2<-c(4:6,6:2,1,2)
Z2<-c(1.5,5:1,3,1.5,5.1,2)
DF2<-data.frame(ID2,X2,Y2,Z2)
DF1<-data.frame(ID,X,Y,Z)
###How to join (merge) data frames (inner, outer, left, right)?
##

##Inner join: Mergers two data sets and keep only the cases which are included 
##in both data sets (Intersection)
merge(DF1, DF2, by.x="ID", by.y="ID2", all=FALSE)

##Outer join:It includes all values of first dataset and all values of second dataset
merge(DF1, DF2, by.x="ID", by.y="ID2", all=TRUE)

##Left outer: It includes all cases from fist data set and only matched cases from second data 
merge(x=DF1, y=DF2, by.x="ID", by.y="ID2", all.x=TRUE)

##Right outer:It includes all cases from second data set and only matched cases from first data  
merge(x=DF1, y=DF2, by.x="ID", by.y="ID2", all.y=TRUE)




## Merging based on Multiple Columns
##Inner Join
merge(DF1, DF2, by.x = c("ID", "X"), by.y= c("ID2", "X2"), all=FALSE)
##Outer Join
merge(DF1, DF2, by.x = c("ID", "X"), by.y= c("ID2", "X2"), all=TRUE)
##Lefter Outer
merge(x=DF1, y=DF2, by.x = c("ID", "X"), by.y= c("ID2", "X2"), all.x=TRUE)
##Right Outer
merge(x=DF1, y=DF2, by.x = c("ID", "X"), by.y= c("ID2", "X2"), all.y=TRUE)

#Cross join:  Used when you want to repeate the data set 2 for every cases of data set 1
Cross_DF<-merge(x = DF1[,c(1,2)], y = DF2[,3], by = NULL)
nrow(Cross_DF)
