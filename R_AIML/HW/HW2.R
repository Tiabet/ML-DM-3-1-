# 2. Simulation Data

set.seed(1)
n <- 100
X <- runif(n,0,1)
beta0 = 5
beta1 = 3
epsilon <- rnorm(n,0,1)
Y<- beta0 + beta1*X + epsilon

#(i)
plot(X,Y)

#(ii)
lm <- lm(Y ~ X)

#(iii)
abline(lm, col = 'red')

#(a)
coef(lm)["X"]
#2.93095가 나온다

#(b)
beta_hat1_values <- numeric(1000)

for (i in 1:1000) {
  X_sim <- runif(n, 0, 1)  # Generate X ~ Uniform(0, 1)
  epsilon_sim <- rnorm(n, 0, 1)  # Generate epsilon ~ N(0, 1)
  Y_sim <- beta0 + beta1 * X_sim + epsilon_sim  # Generate Y = beta0 + beta1*X + epsilon
  fit_sim <- lm(Y_sim ~ X_sim)  # Fit the regression line
  beta_hat1_values[i] <- coef(fit_sim)["X_sim"]  # Store the least squares estimate of beta1
}

#(i)
mean_beta_hat1 <- mean(beta_hat1_values)
#3.018009가 나온다

#(ii)
#mean은 3에 가까운 수가 나올 것이라고 예상할 수 있었는데 역시 그랬다.

#(iii)
hist(beta_hat1_values)


# 3.Stackloss Data
data("stackloss")

#(a)
plot(stack.loss ~ Air.Flow, data = stackloss)

#(b)
fit <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
summary(fit)

#(c)
conf_intervals <- confint(fit, level = 0.99)
conf_intervals

#(d)
new_obs <- data.frame(Air.Flow = 58, Water.Temp = 20, Acid.Conc. = 86)
pred_interval <- predict(fit, newdata = new_obs, interval = "prediction", level = 0.99)
pred_interval

#(e)
test_result <- summary(fit)$coef["Acid.Conc.", ]
coef_estimate <- test_result["Estimate"]
std_error <- test_result["Std. Error"]
t_value <- coef_estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = fit$df.residual)

conclusion <- ifelse(p_value < 0.05, "Reject H₀", "Fail to reject H₀")

p_value
conclusion

# 4. Bayes Theorem
# 구하고자 하는 확률은 P(D|X)이고, P(D|X) = P(X|D)*P(D)/P(X)로 구할 수 있다.
# P(X|D)는 Dividend일 때 X=4일 확률이고 X는 정규분포를 따르므로 정규분포의 확률누적분포함수에서 구할 수 있다.
# 또한 P(X) = P(X|D)*P(D) + P(X|not D)*P(not D) 로 구할 수 있다.

mean_dividend <- 10
mean_no_dividend <- 0
variance <- 36
p_dividend <- 0.8

p_X_given_D <- dnorm(4, mean = mean_dividend, sd = sqrt(variance))
p_X_given_no_D <- dnorm(4, mean = mean_no_dividend, sd = sqrt(variance))
p_X <- (p_X_given_D * p_dividend) + (p_X_given_no_D * (1 - p_dividend))
p_D_given_X <- (p_X_given_D * p_dividend) / p_X

p_D_given_X

# 5. Boston Housing Data
library(MASS)
summary(Boston)
attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim>median(crim)] = 1
Boston = data.frame(Boston, crime01)
Boston <- Boston[,-14]


# Split the data into train and test datasets
set.seed(1) 
train_indices <- sample(1:nrow(Boston), nrow(Boston) * 0.8)
train_data <- Boston[train_indices, ]
test_data <- Boston[-train_indices, ]

# logistic regression model
logit_model <- glm(crime01 ~ ., data = train_data, family = "binomial")
logit_pred <- predict(logit_model, newdata = test_data, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)
logit_accuracy <- mean(logit_pred_class == test_data$crime01)

# LDA model
lda_model <- lda(crime01 ~ ., data = train_data)
lda_pred <- predict(lda_model, newdata = test_data)
lda_pred_class <- lda_pred$class
lda_accuracy <- mean(lda_pred_class == test_data$crime01)

# KNN model
library(class)
knn_model <- knn(train = train_data[, -c(1, ncol(train_data))], test = test_data[, -c(1, ncol(test_data))], cl = train_data$crime01, k = 5)
knn_accuracy <- mean(knn_model == test_data$crime01)

# Print the accuracies
cat("Logistic Regression Accuracy:", logit_accuracy, "\n")
cat("LDA Accuracy:", lda_accuracy, "\n")
cat("KNN Accuracy:", knn_accuracy, "\n")

# 6. O-rings Data (Orings.txt)

orings <- read.table("Orings-2.txt", header = TRUE)
orings <- orings[-18,]
orings$Damaged <-ifelse(orings$Damaged >=1, 1, 0)
orings

#(a)
model <- glm(Damaged ~ Temp, data = orings, family = binomial)
summary(model)

#(b)
temperature <- 31
new_data <- data.frame(Temp = temperature)
probability <- predict(model, newdata = new_data, type = "response")

