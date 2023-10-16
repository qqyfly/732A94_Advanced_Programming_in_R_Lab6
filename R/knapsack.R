## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB 06
##
## Date Created: 2023-10-03
##
## Copyright (c) MIT
## ---------------------------

#' using brute force algorithm to solve knapsack problem
#'
#' @param x data.frame contains value and weight of every item
#' @param W total weight (max) can hold
#' @param fast using RCpp to implement this function or not,default set to FALSE 
#' @useDynLib knapsack
#' @importFrom Rcpp evalCppbrowser
#' @export 
#' 
brute_force_knapsack <- function(x, W, fast = FALSE){

  # record start time
  start_time <- Sys.time()

  # check if parameters [x] and [W] are data.frame and numeric respectively
  if (!(is.data.frame(x) && is.numeric(W) && W > 0)){
    stop("Input is not correct")
  }
  
  ret <- NULL
  
  if(fast){
    #RCpp implementation
    ret  <- brute_force_knapsack_rcpp(x,W)
  }else{
    # Normal R implementation
    n <- length(x$w)
    
    best_value <- 0
    best_subset <- NULL

    for(i in 0:(2^n-1)){
      subset <- integer(n)
      for(j in 1:n){
        subset[j] <- (i %/% 2^(j-1)) %% 2
      }
      
      total_value <- sum(subset * x$v)
      total_weight <- sum(subset * x$w)
      
      if (total_weight <= W && total_value > best_value){
        best_value <- total_value
        best_subset <- subset
      }
    }  
    
    ret <- list(value=round(best_value),
                elements=best_subset) 
  }
  
  condition <- ret$elements == 1
  
  # Get the indices where the condition is TRUE
  indices <- which(condition)
  
  ret <- list(value=ret$value,
              elements=indices) 
  #record end time
  end_time <- Sys.time()
  
  # print the time used
  #print(end_time-start_time)
  
  return(ret)
}

#' using DP algorithm to solve knapsack problem
#'
#' @param x data.frame contains value and weight of every item
#' @param W total weight (max) can hold
#' @export 
#'
knapsack_dynamic <- function(x, W){
  # check if parameters [x] and [W] are data.frame and numeric respectively
  if (!(is.data.frame(x) && is.numeric(W) && W > 0)){
    stop("Input is not correct")
  }
 
  # R implementation here
  n <- nrow(x)
  w <- x$w
  v <- x$v
  
  S <- matrix(NA, nrow = nrow(x) + 1, ncol = W + 1)
  S[1, ] <- 0
  S[, 1] <- 0
  
  for (i in 2:(n + 1)) {
    for(j in 2:(W+2)){
      if(j-2 < x$w[i-1]){
        S[i,j-1] = S[i-1,j-1]
      }else{
        S[i,j-1] = max(S[i-1,j-1], S[i-1,j - 1 - w[i-1]] + v[i-1])
      }
    }
  }
  
  row_n <- nrow(S)
  col_n <- ncol(S)
  best_subset <- c()
  while (col_n > 1 & row_n > 1) {
    if (S[row_n, col_n] != S[row_n - 1,col_n]) {
      best_subset <- c(best_subset, row_n - 1)
      col_n <- col_n - x$w[row_n - 1]
    }
    row_n <- row_n - 1
  }
  
  ret <- list(value=round(S[n+1,W+1]),
              elements=best_subset) 
  
  return(ret)
}

#' using greedy algorithm to solve knapsack problem
#'
#' @param x data.frame contains value and weight of every item
#' @param W total weight (max) can hold
#' @export 
#'
greedy_knapsack <- function(x, W){
  start_time <- Sys.time()
  # check if parameters [x] and [W] are data.frame and numeric respectively
  if (!(is.data.frame(x) && is.numeric(W) && W > 0)){
    stop("Input is not correct")
  }

  #value per weight
  value_per_weight <- x$v / x$w 
  x$value_per_weight <- value_per_weight
  
  #order by value_per_weight  
  x <- x[order(x$value_per_weight, decreasing = TRUE),]
  
  # set weight to W
  weight <- W
  
  # total value init to 0
  total_value <- 0
  
  # create elements vector 
  selected_items <- c()
  
  n <- nrow(x)
  
  #sum value until reach the weight
  for (i in 1:n)
  {
    if (x$w[i] < weight)
    {
      if (weight > 0)
      {
        weight <- weight - x$w[i]
        total_value <- total_value + x$v[i]
        selected_items <- c(selected_items, as.numeric(row.names(x)[i]))
      }
    } else  break()
  }
  
  ret <- list(value = round(total_value, 0), elements = selected_items)
  
  #record end time
  end_time <- Sys.time()
  
  # print the time used
  #print(end_time-start_time)
  
  return(ret)
}