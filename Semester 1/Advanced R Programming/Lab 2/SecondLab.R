name <- "Ugurcan Lacin"
liuid <- "ugula687"

library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)

#1.1.1
sheldon_game <- function (player1,player2){
  player1WinMsg <- "Player 1 wins!"
  player2WinMsg <- "Player 2 wins!"
  elements <- c("scissors","rock","paper","lizard","spock")
  if(!(player1 %in% elements) || !(player2 %in% elements)){
    stop()
  }
  if(player1 == player2){
    return("Draw!")
  }
  if(player1 == "scissors" && (player2 == "paper" || player2 == "lizard")){
    return(player1WinMsg)
  }else if(player1 == "paper" && (player2 == "rock" || player2 == "spock")){
    return(player1WinMsg)
  }else if(player1 == "rock" && (player2 == "scissors" || player2 == "lizard")){
    return(player1WinMsg)
  }else if(player1 == "lizard" && (player2 == "spock" || player2 == "paper")){
    return(player1WinMsg)
  }else if(player1 == "spock" && (player2 == "scissors" || player2 == "rock")){
    return(player1WinMsg)
  }else{
    return(player2WinMsg)
  }
}

#1.2.1
my_moving_median <- function(x,n,...){
  if(!is.numeric(x) || !is.numeric(n)){
    stop()
  }

  lengthOfX <- length(x)
  result <- c()
  for(i in 1:lengthOfX){
    vectorForMedian <- c()
    if((i+n) > lengthOfX){
      break
    }
    for(j in i:(i+n)){
      vectorForMedian <- append(vectorForMedian, x[j])
    }
    if(missing(...)) {
      med <- median(vectorForMedian)
    } else {
      med <- median(vectorForMedian,na.rm=...)
    }
    result <- append(result,med )
  }
  return(result)
}

#1.2.2
for_mult_table <- function(from,to){
  if(!is.numeric(from) || !is.numeric(to)){
    stop()
  }
  result <- c()
  for(i in from:to){
    row <- c()
    for(j in from:to){
      row <- append(row,i*j )
    }
    result <- append(result,row)
  }
  size <- to - from +1
  result <- matrix(result,nrow = size,ncol = size)
  rownames(result) <- c(from:to)
  colnames(result) <- c(from:to)
  return(result)
}

#1.2.3 OPTIONAL

is.scalar <- function(x) is.atomic(x) && length(x) == 1L

# 1.3.1
find_cumsum <- function(x,find_sum){
  if(!is.numeric(x) || is.scalar(x)){
    stop()
  }
  result <- cumsum(x)
  for(i in result){
    if(i > find_sum){
      return(i)
    }
  }
}

# 1.3.2
while_mult_table <- function(from,to){
  if(!is.numeric(from) || !is.numeric(to)){
    stop()
  }
  result <- c()
  i <- from
  while(i <= to){
    row <- c()
    j <- from
    while(j <= to){
      row <- append(row,i*j )
      j<- j +1
    }
    result <- append(result,row)
    i <- i +1
  }
  size <- to - from +1
  result <- matrix(result,nrow = size,ncol = size)
  rownames(result) <- c(from:to)
  colnames(result) <- c(from:to)
  return(result)
}

#1.3.3 OPTIONAL

# 1.4.1 
repeat_find_cumsum <- function(x,find_sum){
  if(!is.numeric(x) || is.scalar(x)){
    stop()
  }
  result <- cumsum(x)
  lastVal <- 0
  i <- 1
  repeat{
    if(result[i] >find_sum || length(x) == i){
      lastVal <- as.numeric(result[i])
      break
    }
    else{
      i <- i +1
    }
  }
  return(lastVal)
}

# mark_my_assignment(tasks="repeat_find_cumsum")

#1.4.2
repeat_my_moving_median <- function(x,n,...){
  if(!is.numeric(x) || !is.numeric(n)){
    stop()
  }
  
  lengthOfX <- length(x)
  result <- c()
  i<- 1
  repeat{
    vectorForMedian <- c()
    if((i+n) > lengthOfX){
      break
    }
    j <- i
    repeat{
      if(j > (i+n))
        break
      else{
        vectorForMedian <- append(vectorForMedian, x[j])
        j <- j +1
      }
    }
    if(missing(...)) {
      med <- median(vectorForMedian)
    } else {
      med <- median(vectorForMedian,na.rm=...)
    }
    result <- append(result,med )
    if(i > lengthOfX){
      break
    }else{
      i <- i+1
    }
  }
  return(result)
}

# mark_my_assignment(tasks="repeat_my_moving_median")

# 1.5.1
in_environment <- function(env){
  envs <- ls(env)
}

# mark_my_assignment(tasks="in_environment")

# 1.6.1
cov <- function(X){
  if(!is.data.frame(X)){
    stop()
  }
  vec <- unlist(lapply(X, function(x){ sd(x)/mean(x)}))
}
# mark_my_assignment(tasks="cov")

# 1.7.1
moment <- function(i){
  if(!is.numeric(i)){
    stop()
  }
  m <- function(x){
    if(!is.numeric(x) || is.scalar(x)){
      stop()
    }
    return(mean((x-mean(x))^i))
  }
}

# mark_my_assignment(tasks="moment")
mark_my_assignment()