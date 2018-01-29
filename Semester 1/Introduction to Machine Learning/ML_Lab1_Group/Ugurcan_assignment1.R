library(gdata)
data <- read.xls("/home/ugur/git/ML_Lab1/spambase.xlsx")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]


knearest=function(data,k,newdata) {
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  X=as.matrix(data[,-p])
  Xn=as.matrix(newdata[-p])
  
  X_hat=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Y_hat=Xn/matrix(sqrt(rowSums(Xn^2)), nrow=n1, ncol=p-1)

  C = X_hat %*% t(Y_hat)
  
  D = 1 - C
  
  
#MISSING: implement steps ii)-iv)
  
  for (i in 1:n2 ){
#MISSING: use the computed distance matrix to find 
    #which observations are the nearest neighbors to case #i
    neighbor <- order(D[,i])[1:k]
    spam <- as.vector(data$Spam[neighbor])
    Prob[i] <- sum(spam)/k
#MISSING: derive probability value 'Prob[i]' by using the
    #target values of the nearest neighbors
  }
  return(Prob)
}

confusionMatrix <- function(test = test,Prob,rate = 0.5){
  svar <- data.frame(Pred = 0, Prob)
  svar$Pred[Prob > rate] <- 1 
  conf_matr <- table(test$Spam, svar$Pred)
  print(conf_matr)
  print(paste("Misclassification rate is ",(conf_matr[2,1] + conf_matr[1,2])/(n/2)))
}


resultKnearest5 <- knearest(train,5,test)
confusionMatrix(test,result)

library(kknn)
kknnPackageResult5 <- kknn(Spam~., train, test, k = 5)
fitted(kknnPackageResult5)
confusionMatrix(test,fitted(kknnPackageResult5))

kknnPackageResult1 <- kknn(Spam~., train, test, k = 7)
confusionMatrix(test,fitted(kknnPackageResult1))

ROC=function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    TPR[i]=t[2,2]/(t[1,2]+t[2,2])
    FPR[i]=t[2,1]/(t[2,1]+t[1,1])
  }
  return (list(TPR=TPR,FPR=FPR))
}

rates <- seq(0.05, 0.95, by=0.05)
rocResultKnearest5 <- ROC(test$Spam,resultKnearest5,rates)

plot(x=rocResultKnearest5$FPR,y=rocResultKnearest5$TPR,type="l")

SpecificityKnearest <- 1 - rocResultKnearest5$FPR
SensitivityKnearest <- rocResultKnearest5$TPR

rocResultKknn <- ROC(test$Spam,fitted(kknnPackageResult),rates)
plot(x=rocResultKknn$FPR,y=rocResultKknn$TPR,type="l")
SpecificityKknn <- 1 - rocResultKknn$FPR
SensitivityKknn <- rocResultKknn$TPR


dataFrameKnearest <- data.frame(FPR = rocResultKnearest5$FPR,TPR = rocResultKnearest5$TPR)
dataFrameKknn <- data.frame(FPR = rocResultKknn$FPR,TPR = rocResultKknn$TPR)

library(ggplot2)
ggplot() +
  geom_point(data = dataFrameKnearest, aes(x = FPR, y = TPR), color = "red") +
  geom_line(data = dataFrameKnearest, aes(x = FPR, y = TPR), color = "red") +
  geom_point(data = dataFrameKknn, aes(x = FPR, y = TPR)) +
  geom_line(data = dataFrameKknn, aes(x = FPR, y = TPR)) +
  geom_abline(slope = -1, intercept = 1) +
  xlab("FPR") +
  ylab("TPR") +
  ggtitle("ROC-curve")