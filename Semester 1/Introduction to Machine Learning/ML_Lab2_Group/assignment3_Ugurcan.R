# # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # ASSIGNMENT 3 # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # #
# # # # # ( Part 1 ) # # # # #
# # # # # # # # # # # # # # #

df <- read.csv("/home/ugur/git/ML_Lab2_Group/State.csv", 
               sep = ";")
library(dplyr)
options(digits=2)

# Replace , as . for R
df$MET <- gsub("\\,",".",df$MET)
# Change type as numeric from factor
df$MET <- as.numeric(df$MET)
df$EX <- as.numeric(df$EX)
# Reorder it
df <- arrange(df, MET) 

data <- data.frame(EX = df$EX, MET = df$MET)

library(ggplot2)
ggplot(data, aes(x=EX, y=MET)) + geom_point(shape=1)
# 

# # # # # # # # # # # # # # #
# # # # # ( Part 2 ) # # # # #
# # # # # # # # # # # # # # #
library(tree)

fit=tree(EX~MET, data=data,control = tree.control(nobs = 48,minsize = 8))
set.seed(12345)
cv.res=cv.tree(fit)
plot(cv.res$size, cv.res$dev, type="b",
     col="red")
plot(log(cv.res$k), cv.res$dev,
     type="b", col="red")

minDev <- min(cv.res$dev)
size <- which(minDev == cv.res$dev)
fit <- tree(EX~MET, data=data)
print(size)
prunedTree=prune.tree(fit,best=size)

pred=predict(prunedTree, newdata=data)
newData <- data.frame(EX = pred, MET = data$MET)

ggplot() + geom_point(data = data, aes(x=EX, y=MET,color = EX)) + geom_point(data = newData, colour = "red",aes(x=EX, y=MET,color = EX))

residual <- df$EX - pred
hist(residual)


# # # # # # # # # # # # # # #
# # # # # ( Part 3 ) # # # # #
# # # # # # # # # # # # # # #

library(boot)

# computing bootstrap samples
f=function(data, ind){
  data1 <- data[ind,]# extract bootstrap sample
  fit=tree(EX~MET, data=data1,control = tree.control(nobs = 48,minsize = 8))
  pred=predict(fit, newdata=data1)
  return(pred)
}
nonParametricBootstrap=boot(data, f, R=1000) #make bootstrap

e <- envelope(nonParametricBootstrap)


plot(df$EX, df$MET, pch=21, bg="orange")
points(pred,df$MET,type="l") #plot fitted line
#plot cofidence bands
points(e$point[2,],df$MET, type="l", col="blue")
points(e$point[1,],df$MET, type="l", col="blue")

# # # # # # # # # # # # # # #
# # # # # ( Part 4 ) # # # # #
# # # # # # # # # # # # # # #

mle=tree(EX~MET, data=data,control = tree.control(nobs = 48,minsize = 8))
rng=function(data, mle) {
  data1=data.frame(EX=data$EX,
                   MET=data$MET)
  n=length(data$EX)
  pred <- predict(mle,
                  newdata=data1)
  residual <- df$EX - pred
  data1$EX=rnorm(n,predict(mle,
                              newdata=data1),sd(residual))
  return(data1)
}
f1=function(data1){
  fit=tree(EX~MET, data=data1,control = tree.control(nobs = 48,minsize = 8))
  pred=predict(fit, newdata=data1)
  return(pred)
}
parametricBootstrap <- boot(data, statistic=f1, R=1000,
         mle=mle,ran.gen=rng, sim="parametric")


e2 <- envelope(parametricBootstrap)


plot(df$EX, df$MET, pch=21, bg="orange")
points(pred,df$MET,type="l") #plot fitted line
#plot cofidence bands
points(e2$point[2,],df$MET, type="l", col="blue")
points(e2$point[1,],df$MET, type="l", col="blue")

# # # # # # # # # # # # # # #
# # # # # ( Part 5 ) # # # # #
# # # # # # # # # # # # # # #

# just comment according to question

