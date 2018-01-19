name <- "Ugurcan Lacin"
liuid <- "ugula687"

library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)

#1.1.1
my_num_vector <- function (){
  e <- exp(1)
  return (c(log10(11),cos(pi/5),e^(pi/3),1173%%7 /19)) 
}

#1.1.2
filter_my_vector <- function(x, leq){
  x[x >= leq] <- NA
  return(x)
}

#1.1.3
dot_prod <- function(a, b) {
  s <- 0
  for(i in 1:length(a)) s <- s + a[i]*b[i]
  return(s)
}

#1.1.4
approx_e<- function(N) {
  e <- 0
  for(i in 0:N) e <- e + 1/ factorial(i)
  return(e)
}
#a <- approx_e(N = 1)
#b <- exp(1)
# Come Back It

#1.2.1
my_magic_matrix <- function(){
  my_matrix <- matrix(c(4,9,2,3,5,7,8,1,6),ncol=3,nrow = 3,byrow = TRUE)
  return(my_matrix)
}

#1.2.2
calculate_elements <- function(A){
  row_size <- length(A[,1])
  col_size <- length(A[1,])
  return(col_size * row_size)
}

#1.2.3
row_to_zero <- function(A,i){
  A[i,] = 0
  return(A)
}

#1.2.4
add_elements_to_matrix <- function(A, x, i, j){
  A[i,j] <-  A[i,j] + x
  return(A)
}

#1.3.1
my_magic_list <- function(){
  return(list(info ="my own list",my_num_vector(),my_magic_matrix()))
}

#1.3.2
change_info <- function(x,text){
  x$info <- text
  return(x)
}

#1.3.3
add_note <- function(x,note){
  x$note <- note
  return(x)
}

#1.3.4
sum_numeric_parts <- function(x){
  x <- unlist(x)
  x <- as.numeric(x)
  x <- sum(x,na.rm=TRUE)
  return(x)
}

#1.4.1
my_data.frame <- function(){
  id <- c(1,2,3)
  name <- c('John','Lisa','Azra')
  income <- c(7.30,0.00,15.21)
  rich <- c(FALSE,FALSE,TRUE)
  return(data.frame(id, name, income,rich))
}

#1.4.2
sort_head <- function(df, var.name, n){
  df <- df[order(df[[var.name]],decreasing = TRUE),]
  df <- head(df, n = n)
  return(df)
}

#1.4.3
add_median_variable <- function(df, j){
  # try to debug by printing each variable, and see differences between them.
  med <- median(df[[j]], na.rm = TRUE)
  df$compared_to_median[med < df[[j]]] <- "Greater"
  df$compared_to_median[med > df[[j]]] <- "Smaller"
  df$compared_to_median[med == df[[j]]] <- "Median"
  return(df)
}


## 1.4.4 analyze_columns
data(iris)
data(faithful)
analyze_columns <- function(df, j){
  # mea <- apply(df[j], 2, mean, na.rm = TRUE)
  # med <- apply(df[j], 2, median, na.rm = TRUE)
  # std <- apply(df[j], 2, sd, na.rm = TRUE)
  
  v <- c(mean(df[[j[1]]]),median(df[[j[1]]]),sd(df[[j[1]]]))
  names(v) <- c("mean", "median","sd")
  
  v2 <- c(mean(df[[j[2]]]),median(df[[j[2]]]),sd(df[[j[2]]]))
  names(v2) <- c("mean", "median","sd")
  
  v3 <- cor(df[, j[1:2]])

  x <- list(v,v2,v3)
  columnNames <- colnames(df)
  names(x) <- c(columnNames[j[1]], columnNames[j[2]], "correlation_matrix")
  return(x)
}

# analyze_columns(faithful, 1:2)
# analyze_columns(iris, c(1,3))
aaaa <-analyze_columns(iris, c(4,1))

mark_my_assignment()
