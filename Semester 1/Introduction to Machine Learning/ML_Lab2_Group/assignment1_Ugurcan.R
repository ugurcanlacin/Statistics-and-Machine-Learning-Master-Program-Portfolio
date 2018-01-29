# # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # ASSIGNMENT 1 # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # #
# # # # # ( Part 1 ) # # # # #
# # # # # # # # # # # # # # #
df <- read.csv("/home/ugur/git/ML_Lab2_Group/australian-crabs.csv", 
                 sep = ",")

library(ggplot2)
# Set color by cond
ggplot(df, aes(x=CL, y=RW, color=sex)) + geom_point(shape=1)

# # # # # # # # # # # # # # #
# # # # # ( Part 2 ) # # # # #
# # # # # # # # # # # # # # #
Y <- df$sex
X <- data.frame(RW = df$RW,CL = df$CL)
X <- as.matrix(X)


#ASSIGNMENT 1.2
disc_fun=function(label, S){
  X1=X[Y==label,]
  #MISSING: compute LDA parameters w1 (vector with 2 values) and w0 (denoted here as b1)
  means <- as.matrix(colMeans(X1))
  b1 <- -0.5 * t(means) %*% solve(S) %*% means + log(0.5)
  w1 <- solve(S) %*% means
  return(c(w1[1], w1[2], b1[1,1]))
}

X1=X[Y=="Male",]
X2=X[Y=="Female",]

S=cov(X1)*dim(X1)[1]+cov(X2)*dim(X2)[1]
S=S/dim(X)[1]

#discriminant function coefficients
res1=disc_fun("Male",S)
res2=disc_fun("Female",S)
# MISSING: use these to derive  decision boundary coefficients 'res'
res <- res1 - res2

# classification
d=res[1]*X[,1]+res[2]*X[,2]+res[3]
Yfit=(d>0)

# # # # # # # # # # # # # # #
# # # # # ( Part 3 ) # # # # #
# # # # # # # # # # # # # # #
plot(X[,2], X[,1], col=Yfit+1, xlab="CL", ylab="RW")
intercept <- -res[3]/res[1]
slope <- -res[2]/res[1]
abline(intercept,slope)
table(Yfit,df$sex)


# # # # # # # # # # # # # # #
# # # # # ( Part 4 ) # # # # #
# # # # # # # # # # # # # # #
glmResult <- glm(data = df,formula = as.factor(sex) ~ CL + RW , family = binomial())
coef <- as.matrix(glmResult$coefficients)
rwWeight <- coef[3]
clWeight <- coef[2]
inter <- coef[1] - 0.5
d=coef[3]*X[,1]+coef[2]*X[,2]+coef[1]
YfitGLM=(d>0.5)
table(YfitGLM,df$sex)

plot(X[,2], X[,1], col=YfitGLM+1, xlab="CL", ylab="RW")
interceptGLM <- -inter/rwWeight
slopeGLM <- -(clWeight)/rwWeight
abline(interceptGLM,slopeGLM,col = "red")
abline(intercept,slope)

# comparison
table(YfitGLM,df$sex)
table(Yfit,df$sex)


