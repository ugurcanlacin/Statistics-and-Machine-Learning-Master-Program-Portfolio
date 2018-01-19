name <- "Ugurcan Lacin Fahad Hameed"
liuid <- "ugula687 fahha780"

#' @title Euclidean Algorithm Implementation
#' @name  euclidean
#' @param m number
#' @param n number
#' @return Greatest Common Divisor of two Numbers by Euclidean Algorithm
#' @description Euclidean Algorithm Implementation to find the Greatest Common Divisor(GCD) of two numbers one and two param 
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}

## Euclidean Algorithm
euclidean <- function(m,n){
  if(!is.numeric(m) || !is.numeric(n)){
    stop()
  }
  if(n == 0) return(m)
  return(euclidean(n, m %% n))
}

# aa <- euclidean(123612, 13892347912)
# aa <-  euclidean(100, 1000)