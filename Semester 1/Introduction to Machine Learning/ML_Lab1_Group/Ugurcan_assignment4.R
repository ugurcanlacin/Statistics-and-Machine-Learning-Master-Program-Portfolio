library(gdata)
data <- read.xls("/home/ugur/git/ML_Lab1/tecator.xlsx")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]


# 1
library(ggplot2)
ggplot(data,aes(x=Protein,y=Moisture)) + geom_point()



#2
model <- function(i, data){
  m <- lm(Moisture ~ poly(Protein, i, raw = TRUE), data)
  return(m)
}

summary(model(6, train))


# 3


analyzeMSE <- function(train, test){
  output <- vector()
  
  for(i in 1:6){
    m <- model(i, train)
    pred <- predict(m, test)
    MSE_train <- mean(residuals(m)^2)
    MSE_test <- mean((pred - test$Moisture)^2)
    
    output <- rbind(output,c(MSE_train, MSE_test))
  }
  output <- cbind(output, c(1:6))
  colnames(output) <- c("MSE_train", "MSE_test", "model")
  return(output)
}

MSE <- analyzeMSE(train, test)
dfMSE <- data.frame(MSE)

library(ggplot2)
ggplot(data = dfMSE) + 
  geom_point(mapping = aes(x = dfMSE$model, y = dfMSE$MSE_train,colour ='red')) +
  geom_point(mapping = aes(x = dfMSE$model, y = dfMSE$MSE_test,colour ='blue')) +
  ylab("MSE") +
  xlab("Model")


library(MASS)
df <- data
df4 <- df[, 2:102]
mod <- lm(Fat ~., df4)
summary(mod)
aic <- stepAIC(mod, direction = "both")
summary(aic)

length(aic$coefficients)



library(glmnet)
ridge <- glmnet(x = as.matrix(scale(df4[,1:100])), y = as.matrix(scale(df4[, 101])), alpha = 0, family = "gaussian")
plot(ridge, xvar = "lambda", label = TRUE)


lasso <- glmnet(x = as.matrix(scale(df4[,1:100])), y = as.matrix(scale(df4[, 101])), alpha = 1, family = "gaussian")
plot(lasso, xvar = "lambda", label = TRUE)


lamSeq <- seq(0,100,by = 0.05)
CVlasso <- cv.glmnet(x = as.matrix(scale(df4[,1:100])), y = as.matrix(scale(df4[, 101])), alpha = 1, family = "gaussian",lambda = lamSeq)
CVlasso$lambda.min
CVlasso$lambda.1se
plot(CVlasso)
re <- coef(CVlasso, s = "lambda.min")
dfRE <- as.data.frame(as.matrix(re))



