brain <- readRDS('data_brain.RDS')
library(leaps)

# 1.(a)
fit <- lm(Y ~ ., data = brain[,])
fit$coefficients
summary(fit)

# 1.(b)
fit.fwd <- regsubsets(Y~., data = brain, nvmax = 8, method = "forward")
summary(fit.fwd)


AIC(lm(Y~circum, data = brain))
AIC(lm(Y~circum + headht, data = brain))
AIC(lm(Y~circum + headht + age, data = brain))
AIC(lm(Y~circum + headht + age + len, data = brain))
AIC(lm(Y~circum + headht + age + len + cephalic, data = brain))
AIC(lm(Y~circum + headht + age + len + cephalic, data = brain))
AIC(lm(Y~circum + headht + age + len + cephalic + sex, data = brain))
AIC(lm(Y~circum + headht + age + len + cephalic + sex + breadth, data = brain))

coef(fit.fwd, 6)

plot(fit.fwd)

# 1.(c)
fit.bwd <- regsubsets(Y~., data = brain, nvmax = 8, method = "backward")
plot(fit.bwd)

coef(fit.bwd,5)


# 2
library(ISLR2)
data(College)

set.seed(1)
n <- nrow(College)
train<-sample(1:nrow(x), nrow(x)/2)
train_indices <- sample(1:n, round(5/7 * n))
train_data <- College[train_indices, ]
test_data <- College[-train_indices, ]

# 2.(a)
lm_model <- lm(Apps ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)
lm_test_error <- mean((test_data$Apps - lm_predictions)^2)
print(lm_test_error)

# 2.(b)
library(glmnet)
x <- model.matrix(Apps ~ ., data = train_data)[,-1]
y <- train_data$Apps
ridge_model <- cv.glmnet(x, y, alpha = 0, nfolds = 5)
best_lambda <- ridge_model$lambda.min
ridge_predictions <- predict(ridge_model, s = best_lambda, newx = model.matrix(Apps ~ ., data = test_data)[,-1])
ridge_test_error <- mean((test_data$Apps - ridge_predictions)^2)
print(ridge_test_error)

# 2.(c)
lasso_model <- cv.glmnet(x, y, alpha = 1, nfolds = 5)
best_lambda_lasso <- lasso_model$lambda.min
lasso_predictions <- predict(lasso_model, newx = model.matrix(Apps ~ ., data = test_data)[,-1])
lasso_test_error <- mean((test_data$Apps - lasso_predictions)^2)
num_nonzero_coeffs <- coef(lasso_model, s = best_lambda_lasso)
print(lasso_test_error)
print(num_nonzero_coeffs)

# 2.(d)
library(pls)
pcr.fit<-pcr(Apps~., data = data, scale = TRUE, validation = "CV")
summary(pcr.fit)
pcr_model <- pcr(Apps ~ ., data = train_data, scale = TRUE, validation = "CV")
pcr_model$validation$PRESS
validationplot(pcr_model, val.type = "MSEP")
best_m <- which.min(pcr_model$validation$PRESS)
pcr_predictions <- predict(pcr_model, newdata = test_data, ncomp = 5)
pcr_test_error <- mean((test_data$Apps - pcr_predictions)^2)
print(pcr_test_error)
print(best_m)

# 2.(e)
pls_model <- plsr(Apps ~ ., data = train_data, scale = TRUE, validation = "CV")
best_m <- which.min(pls_model$validation$PRESS)
pls_predictions <- predict(pls_model, newdata = test_data, ncomp = best_m)
pls_test_error <- mean((test_data$Apps - pls_predictions)^2)
print( pls_test_error)
print( best_m)

# 3.(a)
data(Default)
set.seed(1)

logit_model <- glm(default ~ income + balance, data = Default, family = binomial)
summary(logit_model)

# 3.(b)
boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data = data[index, ], family = binomial)
  return(coef(fit))
}

# 3.(c)
library(boot)
set.seed(1)
boot_results=boot(Default, boot.fn, R = 100)
boot_results
