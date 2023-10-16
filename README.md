# 732A94_Advanced_Programming_in_R_Lab6

<!-- badges: start -->
  [![R-CMD-check](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab6/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab6/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
# Overview

The `knapsack` package provides three exported functions to solve solve knapsack problem. An extra Rcpp version of brute_force_knapsack also provided.

# Installation

To install the `knapsack` package, use the following commands:

```
devtools::install_github("qqyfly/732A94_Advanced_Programming_in_R_Lab6", build_vignettes = TRUE)
```

## Load required libraries

In this step, we need to load the installed library `knapsack`. The code is showed below.

```
library(knapsack)
```

# Load the test data

Some test data is needed to execute the function.

```
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
```

## brute force knapsack R version
```
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
```


##  dynamic knapsack
```
knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
```

## greedy knapsack
```
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
```

## brute force knapsack Rcpp version
```
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, fast = TRUE)
```