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

library(Rcpp)

#' using brute force algorithm to solve knapsack problem
#'
#' @param x data.frame contains value and weight of every item
#' @param WD total weight (max) can hold
#' @param fast using RCpp to implement this function or not,default set to FALSE 
#' @param parallel using parallel package to implement this function or not,default set to FALSE 
#' @export 
#' 
brute_force_knapsack <- function(x, WD, fast = FALSE, parallel = FALSE){

  # record start time
  start_time <- Sys.time()

  # check if parameters [x] and [W] are data.frame and numeric respectively
  if (!(is.data.frame(x) && is.numeric(WD) && WD > 0)){
    stop("Input is not correct")
  }
  
  ret <- NULL
  
  if(fast){
    #RCpp implementation
    ret  <- brute_force_knapsack_rcpp(x, WD)
  }else{
    # R parallel implementation
    if(parallel){
      stop("Not implementated, it's optional")
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
        
        if (total_weight <= WD && total_value > best_value){
          best_value <- total_value
          best_subset <- subset
        }
      }  
      
      ret <- list(value=round(best_value),
                  elements=best_subset) 
      
    }
  }
  
  condition <- ret$elements == 1
  
  # Get the indices where the condition is TRUE
  indices <- which(condition)
  
  ret <- list(value=ret$value,
              elements=indices) 
  #record end time
  end_time <- Sys.time()
  
  # print the time used
  print(end_time-start_time)
  
  return(ret)
}

#' using DP algorithm to solve knapsack problem
#'
#' @param x data.frame contains value and weight of every item
#' @param WD total weight (max) can hold
#' @param fast using RCpp to implement this function or not,default set to FALSE 
#' @export 
#'
knapsack_dynamic <- function(x, WD, fast = FALSE){
  # check if parameters [x] and [W] are data.frame and numeric respectively
  if (!(is.data.frame(x) && is.numeric(WD) && WD > 0)){
    stop("Input is not correct")
  }
  
  if(fast){
    #RCpp implementation here  
    #ret  <- knapsack_dynamic_rcpp(x, WD)
  }else{
    # R implementation here
    n <- length(x$w)
    
    best_value <- 0
    best_subset <- NULL
    
    S <- array(0,dim=c(n,WD))
    
    for(i in 1:n+1){
      for (j in 1:WD){
        wi = x$w[i-1]; 
        vi = x$v[i-1]
      }
      if(j < wi){
        S[i,j] = S[i-1,j]
      }else{
        S[i,j] = max(S[i-1,j], S[i-1,j-wi] + vi)
      }
    }
    ret <- S[n,WD]       
  }
  return(ret)
}

#' using DP algorithm to solve knapsack problem
#'
#' @param x data.frame contains value and weight of every item
#' @param WD total weight (max) can hold
#' @param fast using RCpp to implement this function or not,default set to FALSE 
#' @export 
#'
greedy_knapsack <- function(x, WD, fast = FALSE){
  start_time <- Sys.time()
  # check if parameters [x] and [W] are data.frame and numeric respectively
  if (!(is.data.frame(x) && is.numeric(WD) && WD > 0)){
    stop("Input is not correct")
  }

  ret <- NULL
  
  if(fast){
    #Call RCpp 
    ret  <- greedy_knapsack_rcpp(x, WD)
  }else{
    # R implementation here
    n <- length(x$w)
    print
    value_per_weight <- x$v / x$w 
    
    item_indices <- 1:n
    
    sorted_items <- item_indices[order(-value_per_weight)]
    total_value <- 0
    total_weight <- 0
    selected_items <- numeric(n)
    
    for (i in sorted_items) {
      if (total_weight + x$w[i] <= WD) {
        selected_items[i] <- 1
        total_value <- total_value + x$v[i]
        total_weight <- total_weight + x$w[i]
      }
    }
    ret <- list(value=round(total_value),
                elements=selected_items) 
  }
  
  condition <- ret$elements == 1
  
  # Get the indices where the condition is TRUE
  indices <- which(condition)
  
  ret <- list(value=ret$value,
              elements=indices) 
  #record end time
  end_time <- Sys.time()
  
  # print the time used
  print(end_time-start_time)
  
  return(ret)
}

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
value1 <- brute_force_knapsack(x = knapsack_objects[1:15,], WD = 3500)
value2 <- brute_force_knapsack(x = knapsack_objects[1:15,], WD = 3500,fast = TRUE)

