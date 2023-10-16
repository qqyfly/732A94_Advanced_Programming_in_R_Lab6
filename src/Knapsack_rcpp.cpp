// ---------------------------
//
// Course: 732A94-Advanced R Programming
//
// Rcpp Code For: LAB 06
//
// Date Created: 2023-10-03
//
// Copyright (c) MIT
// ---------------------------

#include <Rcpp.h>
#include <cmath>
#include <iostream>

using namespace Rcpp;

//' using brute force algorithm (C++) to solve knapsack problem 
 //' 
 //' @param x data.frame contains value and weight of every item
 //' @param WD total weight (max) can hold
 //' @return List
 //' @export
// [[Rcpp::export]]
List brute_force_knapsack_rcpp(const DataFrame & x, const int WD) {
  // Convert Data frame to C++ vector
  std::vector<double> v = as<std::vector<double> >(x["v"]);
  std::vector<double> w = as<std::vector<double> >(x["w"]);
  
  // Get number of items
  int n = w.size();
  
  double best_value = 0;
  
  // Init a new vector to store the items selected
  std::vector<int> best_subset(n);
  
  for (int i = 0;i < pow(2,n);i++) {
    std::vector<int> subset(n);
    for(int j = 1; j <= n; j++){
      subset[j] = (i / int(pow(2,j-1))) % 2;
    }
  
    double total_v = 0;
    double total_w = 0;
    
    for(int k = 0;k < n; k++){
      total_v += subset[k] * v[k];
      total_w += subset[k] * w[k];
    }
    
    if (total_w <= WD && total_v > best_value) {
      best_value = total_v;
      best_subset = subset;
    }
  }
  
  return List::create(Named("value") = round(best_value),
                      Named("elements") = best_subset);
}
