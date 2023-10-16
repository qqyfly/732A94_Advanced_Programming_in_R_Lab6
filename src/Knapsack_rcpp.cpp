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
using namespace std;

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

// [[Rcpp::export]]
List knapsack_dynamic_rcpp(const DataFrame & x, const int WD) {

  // TODO: Implement the DP algorithm using C++

  // Convert Data from to C++ vectors
  std::vector<double> v = as<std::vector<double> >(x["v"]);
  std::vector<double> w = as<std::vector<double> >(x["w"]);

  // Get number of items
  int n = v.size();
  
  double total_value = 0;
  //double total_weight = 0;

  // Init a new vector to store the items selected
  std::vector<int> selected(n);

  return List::create(Named("value") = total_value,
                      Named("elements") = selected);
}

// [[Rcpp::export]]
List greedy_knapsack_rcpp(const DataFrame & x, const int WD) {
  
  // Convert Data from to C++ vectors
  std::vector<double> v = as<std::vector<double> >(x["v"]);
  std::vector<double> w = as<std::vector<double> >(x["w"]);
  
  // Get number of items
  int n = v.size();
  
  // Init a new vector to store value/weight ratios
  std::vector<double> vwr(n);
  for (int i = 0;i < n;i++) {
    vwr[i] = v[i]/w[i];
  }
  
  // reverse sort the value/weight ratios
  std::sort(vwr.begin(), vwr.end(), std::greater<double>());
  
  double total_value = 0;
  double total_weight = 0;
  
  // Init a new vector to store the items selected
  std::vector<int> selected(n);
  
  // Loop through the items
  // If the item fits in the knapsack, add it to the knapsack, and update the total value and weight
  for(int i = 0;i < n;i++) {
    if (total_weight + w[i] <= WD) {
      selected[i] = 1;
      total_value += v[i];
      total_weight += w[i];
    }
  }
  
  // return a list of the total value, and the selected elements
  return List::create(Named("value") = round(total_value),
                      Named("elements") = selected);
}
