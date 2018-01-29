



#linear regression
mylin = function(X, Y, Xpred) {
  Xpred1 = cbind(1, Xpred)
  #MISSING: check formulas for linear regression and compute beta
  X <- cbind(1, X)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  # beta <- as.matrix(rbind(1, beta))
  Res = Xpred1 %*% beta
  return(Res)
}

# mylin(X = as.matrix(swiss[11:47,2:5]), Y = swiss[11:47,1], Xpred = as.matrix(swiss[1:10,2:5]))

myCV = function(X, Y, Nfolds) {
  n = length(Y)
  p = ncol(X)
  set.seed(12345)
  ind = sample(n, n)
  X1 = X[ind, ]
  Y1 = Y[ind]
  sF = floor(n / Nfolds)
  MSE = numeric(2 ^ p - 1)
  Nfeat = numeric(2 ^ p - 1)
  Features = list()
  curr = 0
  
  #we assume 5 features.
  
  for (f1 in 0:1)
    for (f2 in 0:1)
      for (f3 in 0:1)
        for (f4 in 0:1)
          for (f5 in 0:1) {
            model = c(f1, f2, f3, f4, f5)
            if (sum(model) == 0)
              next()
            SSE = 0
            
            for (k in 1:Nfolds) {
              startIndice <- (k * sF) - sF + 1
              endIndice <- 0
              if (k == Nfolds) {
                mod <- nrow(X1) %% Nfolds
                endIndice <- (k * sF) + mod
              } else{
                endIndice <- k * sF
              }
              
              selectedFeatures <- which(model == 1)
              Xvalidate <- as.matrix(X1[,selectedFeatures])
              Xvalidate <- as.matrix(Xvalidate[startIndice:endIndice,])
              Yvalidate <- as.matrix(Y1[startIndice:endIndice])
              
              Xtrain <- X1[-c(startIndice:endIndice), selectedFeatures]
              Ytrain <- Y1[-c(startIndice:endIndice)]
              Ypred <- mylin(X = Xtrain, Y = Ytrain, Xpred = Xvalidate)
              
              SSE = SSE + sum((Ypred - Yvalidate) ^ 2)
            }
            curr = curr + 1
            MSE[curr] = SSE / n
            Nfeat[curr] = sum(model)
            Features[[curr]] = model
            
          }
  numberOfFeatures <-lapply(Features,FUN = function(x) {return(length(which(x == 1)))})
  plot(x = numberOfFeatures, y = MSE)
  i = which.min(MSE)
  Features=Features[[i]]
  Features2 <- which(Features == 1)
  return(list(CV=MSE[i], 
              Features=colnames(X1)[Features2],
              Features = Features))
}



myCV(as.matrix(swiss[, 2:6]), swiss[[1]], 5)