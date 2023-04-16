m<-matrix(c(1,2,4,1),ncol=2,byrow = FALSE) #byrow는 TRUE가 default
m
solve(m)#역행렬
m%*%solve(m)
m
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") #variable names 
mydata
mydata[1]

mydata<-read.table("Lec2/R_practice_script1/dummy.txt")
mydata
head(mydata)
mydata[1]
mydata[,1]
mydata[1,]
is.na(mydata)

tmp<-c(1,2,NA,3)
mean(tmp)
mean(tmp,na.rm = TRUE)
sd(tmp,na.rm = TRUE)
x=tmp

mytrans <- function(x) {
  if (!is.matrix(x)) {
    warning("argument is not a matrix: returning NA")
    return(NA_real_)
    }
  y <- matrix(1, nrow=ncol(x), ncol=nrow(x))
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      y[j,i] <- x[i,j]
    }
  }
  return(y)
}
mytrans(m)

y<-rnorm(100,100,1)
y
mean(y)
y<-rnorm(10000,0,1)
mean(y)
hist(y)


attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")

head(mtcars)
