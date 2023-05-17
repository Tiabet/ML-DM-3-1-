library(ISLR2)
set.seed(1)
n<- nrow(Auto)
train <- sort(sample(n,196))
train

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

summary(lm.fit)

attach(Auto)
mean((mpg[train] - lm.fit$fitted.values)^2)

mean((mpg - predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)

mean((mpg[train] - lm.fit2$fitted.values)^2)

mean((mpg - predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)

mean((mpg[train] - lm.fit3$fitted.values)^2)

mean((mpg - predict(lm.fit3,Auto))[-train]^2)


library(boot)

glm.fit <- glm(mpg~horsepower, data = Auto)

cv.err <- cv.glm(Auto,glm.fit)

cv.err$delta

cv.error <- rep(0,10)

for (i in (1:10)){
  glm.fit <- glm(mpg~ poly(horsepower,i), data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)
}



#bootstraping
boot.fn <- function(data, index){
  coef(lm(mpg~ horsepower + I(horsepower^2), data = data, subset = index))
}
boot(Auto, boot.fn, 1000)
