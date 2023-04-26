Wage <- ISLR2::Wage

lm <- lm(wage ~ age, data=Wage)
lm$fitted.values

lm$coefficients[1] #beta_0_hat
lm$coefficients[2] #beta_1_hat
xbar <- mean(Wage$age)
ybar <- mean(Wage$wage)

ybar - lm$coefficients[2] * xbar #beta_0_hat

#Advertising data
Advertising <- read.csv("Advertising.csv", header=T)
plot(Advertising$TV, Advertising$sales, xlab="TV", ylab="Sales",
     pch=16, col=2)
lm_Adv <- lm(sales ~ TV, data=Advertising)
abline(a=lm_Adv$coefficients[1], b=lm_Adv$coefficients[2], col=4, lwd=2)

lm_Adv2 <- lm(sales ~ radio, data=Advertising)

lm_Adv3 <- lm(sales ~ newspaper, data=Advertising)


xbar <- mean(Advertising$TV)
ybar <- mean(Advertising$sales)
xminusxbar <- Advertising$TV - xbar
yminusybar <- Advertising$sales - ybar
xminusxbarsq <- (Advertising$TV - xbar)^2
yminusybarsq <- (Advertising$sales - ybar)^2
ei <- lm_Adv$residuals

#sample correlation coefficients
samp_corr_coeff <- sum(xminusxbar*yminusybar)/sqrt(sum(xminusxbarsq)*sum(yminusybarsq))
(samp_corr_coeff)^2 #R^2

beta1hat <- sum((xminusxbar*yminusybar))/sum(xminusxbarsq)
beta1hat

sqrt(sum(ei^2)/(sum(xminusxbarsq)*(198)))

summary(lm_Adv)

lm_Adv4 <- lm(sales ~ TV + radio + newspaper, data=Advertising)
summary(lm_Adv4)

lm_Adv5 <-lm(sales ~ TV + radio*TV + radio, data = Advertising)
summary(lm_Adv5)

#variable selection
library(olsrr)
ols_step_all_possible(lm_Adv4)
ols_step_best_subset(lm_Adv4)
ols_step_forward_p(lm_Adv4)
ols_step_backward_p(lm_Adv4)
ols_step_both_p(lm_Adv4)

Credit <- ISLR2::Credit
Credit <- cbind(Credit, Owndummy =ifelse(Credit$Own == 'Yes', 1, 0))
lm_Credit <- lm(Balance ~ Owndummy, data=Credit)
summary(lm_Credit)

########################################
##Experiment 1
########################################
X <- sort(runif(100, -2, 2))
Y <- 2 + 3*X + rnorm(100)
plot(X,Y)
lines(X, 2 + 3*X)

lm_obj <- lm(Y ~ X)
lines(X, fitted.values(lm_obj), col=2)

for(i in 1:10){
  Xnew <- sort(runif(100, -2, 2))
  Ynew <- 2 + 3*X + rnorm(100)
  lm_obj_new <- lm(Ynew ~ Xnew)
  lines(Xnew, fitted.values(lm_obj_new), lty=2, col="skyblue")
}
########################################
##Experiment 2
########################################
X <- sort(runif(100, 1, 5))
Y <- 2 + 3*X + rnorm(100)
plot(X,Y)
lines(X, 2 + 3*X)

#(1) 오차항의 분산이 상수가 아닐 수도 있음
sigm <-1
X <- sort(runif(100, 1, 5))
Y <- 2 + 3*X + rnorm(100, 0, sigm*X)
plot(X,Y)
lines(X, 2 + 3*X)

#(2) 오차항의 분포가 꼭 정규분포가 아니어도 됨
#(ex) t-분포, 자유도 3
X <- sort(runif(100, 1, 5))
Y <- 2 + 3*X + rt(100, df=3)
plot(X,Y)
lines(X, 2 + 3*X)

lm_obj2 <- lm(Y ~ X)#lm에서는 y축에 들어갈 것이 ~앞에 나옴
lm_obj2
lines(X, fitted.values(lm_obj2), col=2)
