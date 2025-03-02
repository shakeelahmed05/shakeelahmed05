##Apply Function in R
# Two dimensional matrix
A <- matrix(seq(1,16), 4, 4)

# apply min to rows
apply(A, 1, mean)

# apply max to columns
apply(A, 2, var)
# 3 dimensional array
B <- array(seq(1,18), dim = c(3,3,2))
B[1,1,2]
# Apply sum across each B[*, , ] - i.e Sum across 2nd and 3rd dimension
apply(B, 1, sum)



# Apply sum across each M[*, *, ] - i.e Sum across 3rd dimension
apply(B, c(1,2), sum)

#########################################################
x <- list(a = 1, b = 1:3, c = 10:100) 
x$c[10:20]
lapply(x, FUN = length) 
lapply(x, FUN = max) 

#####################################
##sapply – When you want to apply a function to each element of a list in turn,
##but you want a vector back, rather than a list. 
x <- list(a = 1, b = 1:3, c = 10:100)
# Compare with above; a named vector, not a list 
sapply(x, FUN = length)  
sapply(x, FUN = mean)   
##Generate Normal data size 3 for 5 columns
D<-sapply(1:5,function(x) rnorm(4,x))
rnorm(1,1)

###Mapply
#Sums the 1st elements, the 2nd elements, etc. 
mapply(max, 1:10, 0:9, 1:10) 
mapply(var, 1:10, 1:10, 1:10)
#To do rep("a",4), rep("b",3), etc.
mapply(rep, c("a", "b", "c"), 3:1)
mapply(rep, c("a", "b", "c"), c(4,5,6)) 

##tapply – When we want to apply a function to subsets of a vector and
##the subsets are defined by some other vector, usually a factor.
x <- 1:15
a <- factor(rep(letters[1:5], each = 3)) ##A factor
tapply(x, a, mean)
