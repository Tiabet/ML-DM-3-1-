x<-c(1,3,2.5)
y<-c(1,4,3)
x+y #덧셈셈
x*y #곱셈
x%*%y #내적
#외적
outer(x,y)
#함수사용
f<-outer(x,y,function(x,y) cos(y)/(1+x^2))
f

#수열열
z<-seq(1,10)
z
z<-seq(-pi,pi,length=50)
z
z<-seq(1,100,by=3)
z
z<-seq(1,100,by=3)

ls()
rm(x,y)
ls()

#행렬 생성
x<-matrix(data=c(1,2,3,4), nrow=2, ncol=2, byrow=FALSE)
x

sqrt(x)
x^2


#무작위 자료 생성
y<-rnorm(0)
y<-rnorm()
y<-rnorm(25)
y

y1<-rnorm(25,50,1)
y+y1

cov(y1,y)
cor(y1,y)

set.seed(100) #난수표의 시작점을 지정하는 함수수
rnorm(5)

#그래픽스
x<-rnorm(100)
y<-rnorm(100)
plot(x,y,xlab = "x",ylab = "y",title(main="TITLE"))
plot(x,y,col="green")

A<-matrix(1:16, 4,4)
A
A[-c(1,3),]

