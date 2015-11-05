#include <RcppArmadillo.h>

// [[Rcpp::depends("RcppArmadillo")]]

// Function to calculate weighted mean
// [[Rcpp::export(.wmean)]]
double wmean(arma::rowvec x, arma::colvec w) {
  return arma::as_scalar((x * w)) / sum(w);
}

// [[Rcpp::export(get_tree_pred)]]
arma::mat get_tree_pred(int n, int ntree, arma::mat pd_membership, arma::rowvec yvar, 
                 arma::mat inbag) {
  arma::mat out(n, ntree);
  for(int i = 0; i < n; ++i) { 
    for(int j = 0; j < ntree; ++j) {
      arma::colvec ccol = pd_membership.col(j);
      double cel = pd_membership(i, j);
      arma::uvec idx = arma::find(ccol == cel);
      arma::rowvec x = yvar(idx).t();
      arma::colvec y = inbag.col(j);
      arma::colvec w = y(idx);
      out(i, j) = wmean(x, w);
    }    
  }  
  return out;
}
