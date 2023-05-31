library(randomForest)
library(ISLR2)
library(tree)

set.seed(1)
train <- sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston <- tree(medv~.,Boston, subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

cv.boston <-cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

prune.boston<-prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty = 0)

boston.test<-Boston[-train,"medv"]


bag.boston<-randomForest(medv~., data=Boston, subset=train, mtry=12, importance = TRUE)

yhat.bag<-predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
