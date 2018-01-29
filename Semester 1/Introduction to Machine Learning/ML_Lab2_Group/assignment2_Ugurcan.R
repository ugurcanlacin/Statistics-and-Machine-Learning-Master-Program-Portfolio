# # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # ASSIGNMENT 2 # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # #
# # # # # ( Part 1 ) # # # # #
# # # # # # # # # # # # # # #
library(gdata)
data <- read.xls("/home/ugur/git/ML_Lab2_Group/creditscoring.xls")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]

rest=data[-id,]
n=dim(rest)[1]
id=sample(1:n, floor(n*0.5))
test=rest[id,]
valid=rest[-id,]


# # # # # # # # # # # # # # #
# # # # # ( Part 2 ) # # # # #
# # # # # # # # # # # # # # #

library(tree)

fitByGini=tree(good_bad~., data=train,method = "gini")
giniTrainPred <- predict(fitByGini, newdata=train,type="class")
giniTestPred <- predict(fitByGini, newdata=test,type="class")
table(giniTrainPred,train$good_bad)
table(giniTestPred,test$good_bad)


fitByDeviance=tree(good_bad~., data=train,method = "deviance")
devianceTrainPred <- predict(fitByDeviance, newdata=train,type="class")
devianceTestPred <- predict(fitByDeviance, newdata=test,type="class")
table(devianceTrainPred,train$good_bad)
table(devianceTestPred,test$good_bad)

# # # # # # # # # # # # # # #
# # # # # ( Part 3 ) # # # # #
# # # # # # # # # # # # # # #
fit=tree(good_bad~., data=train)
trainScore=rep(0,15)
testScore=rep(0,15)
for(i in 2:15) {
  prunedTree=prune.tree(fit,best=i) # i is tree depth here.
  pred=predict(prunedTree, newdata=valid,
               type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}

plot(2:15, trainScore[2:15], type="b", col="red",
     ylim=c(0,600))
points(2:15, testScore[2:15], type="b", col="blue")

minDevianceTrain <- min(trainScore[2:15])
minIndexTrain <- which(minDevianceTrain == trainScore)
bestTree <- prune.tree(fit,best=minIndexTrain)

YpredTest <- predict(bestTree, newdata=test,type="class")
tab <- table(YpredTest,test$good_bad)

misclassificationRate <- 1-sum(diag(tab))/sum(tab)



# # # # # # # # # # # # # # #
# # # # # ( Part 4 ) # # # # #
# # # # # # # # # # # # # # #
library(MASS)
library(e1071)
fit=naiveBayes(good_bad~., data=train)
fit
# Yfit=predict(fit, newdata=train,type = "raw")
Yfit=predict(fit, newdata=train)
tab <- table(Yfit,train$good_bad)
misclassificationRate <- 1-sum(diag(tab))/sum(tab)

Yfit=predict(fit, newdata=test)
tab <- table(Yfit,test$good_bad)
misclassificationRate <- 1-sum(diag(tab))/sum(tab)
