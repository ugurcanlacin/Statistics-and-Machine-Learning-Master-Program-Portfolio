data <- read.csv("/home/ugur/git/ML_Lab1_Block2_Group/spambase.csv", sep = ";",dec = ",")
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.66))
train=data[id,]
test=data[-id,]

library(mboost)
library(randomForest)

cars.gb <- blackboost(as.factor(Spam) ~ ., data = train,control = boost_control(mstop= 10),family = AdaExp())
res <- blackboost(Spam ~ ., data = train,control = boost_control(mstop= 10))

numberOfTrees <- seq(1500,1600,100)
randomForestErrorRates <- c()
for(i in 1:2){
  numberOfTree <- numberOfTrees[i]
  resRandomForest <- randomForest(as.factor(Spam) ~ .,data = train,ntree=numberOfTree)
  pred <- predict(resRandomForest,newdata=test)
  tab <- table(pred,test$Spam)
  misclassificationRate <- 1-sum(diag(tab))/sum(tab)
  randomForestErrorRates <- cbind(randomForestErrorRates,misclassificationRate)
}

AdaboostErrorRates <- c()
for(i in 1:2){
  numberOfTree <- numberOfTrees[i]
  resAdaboost <- blackboost(as.factor(Spam) ~ ., data = train,control = boost_control(mstop= numberOfTree),family = AdaExp())
  pred <- predict(resAdaboost,newdata=test,type="class")
  tab <- table(pred,test$Spam)
  misclassificationRate <- 1-sum(diag(tab))/sum(tab)
  AdaboostErrorRates <- cbind(AdaboostErrorRates,misclassificationRate)
}



library(ggplot2)
mx <- as.matrix(randomForestErrorRates)
rfdata <- as.data.frame(mx[1,])


mx2 <- as.matrix(AdaboostErrorRates)
adadata <- as.data.frame(mx2[1,])

ggplot() + geom_point(mapping = aes( x=numberOfTrees ,y = mx[1, ], colour = "red"),data = rfdata) +
 geom_point(mapping = aes( x=numberOfTrees ,y = mx2[1, ], colour = "blue"),data = adadata) +
  labs(x="Number of Tree",y="Error Rates", ttile="Adaboost vs Random Forest",color = "Algorithm") +
  scale_color_manual(labels = c("Adaboost", "Random Forest"), values = c("blue", "red"))


# numVec <- c(12,17,29,18,20,38,14,16,30,20,18,38,16,19,35)
# X <- matrix(numVec,5,3,byrow = TRUE)
# n <- nrow(X)
# I <- diag(n)
# ones <- as.matrix(rep(1, n))
# 
# 
# res <- cov(X)
# det(res)
# options(scipen=999)
# 
# S <- (1/(n-1) * t(X) %*% (I - (1/n) * ones %*% t(ones))) %*% X
# det(S)
# 
# 
# 
# xMean <- as.matrix(c(2,3,1))
# 
# t(xMean)
# 
# X - ones %*% t(xMean)
# 
# t(X)
