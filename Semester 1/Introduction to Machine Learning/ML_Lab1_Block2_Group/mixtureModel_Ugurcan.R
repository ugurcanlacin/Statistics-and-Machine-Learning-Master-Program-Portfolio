set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}
K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}
pi
mu
for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  # points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  # Your code here
  xrow <- c()
  for(k in 1:K){
    for(n in 1:N){
      xrow <- dbinom(x = x[n, ], size = 1, prob = mu[k,])
      z[n,k] <- prod(xrow) * pi[k]
    }
  }
  z <- apply(z, 2, function(x){x/rowSums(z)})
  
  #Log likelihood computation.
  # Your code here
  # Equation 9.54 Bishop
  loglik <- 0
  for(n in 1:N){
    for(k in 1:K){
      tmp=0
      for(d in 1:D){
        tmp = tmp + x[n,d] * log(mu[k,d]) + (1 - x[n,d]) * log(1 - mu[k,d])
      }
      loglik <- loglik + z[n,k] * (log(pi[k]) + tmp)
    }
  }
  llik[it] <- loglik
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  if(it > 1){
    if((llik[it] - llik[it-1] < min_change)){break}
  }
  #M-step: ML parameter estimation from the data and fractional component assignments
  # Your code here
  pi <- colSums(z)/N
  # mu <- (t(x) %*% z)/colSums(z)
  denom <- (t(x) %*% z)
  mu_new <- matrix(NA, ncol = K, nrow = 10)
  for(k in 1:K){
    output <- c()
    mu_new[,k] <- as.matrix(denom[,k]/sum(z[,k]))
  }
  mu <- t(mu_new)
}
pi
mu
plot(llik[1:it], type="o")



