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

// [[Rcpp::export(inf_jackknife)]]
arma::mat inf_jackknife(int n, int b, arma::mat P, arma::mat N) {
  arma::colvec P_avg(n);
  arma::colvec P_sum(n);
  arma::mat P_sq(n, b);
  arma::colvec c_sum(n);
  arma::colvec N_avg(n);
  arma::mat N_sq(n, b);
  arma::colvec N_avg_sq(n);
  arma::colvec N_sq_avg(n);
  arma::mat C(n, b);
  arma::mat C_sq(n, b);
  arma::colvec raw_IJ(n);
  arma::mat N_var(1, 1);
  arma::colvec boot_var(n);
  arma::colvec mc_bias(n);

  N_sq = pow(N, 2);
  
  for (int i = 0; i < n; i++) {
    P_avg(i) = arma::mean(P.row(i));
    N_avg(i) = arma::mean(N.row(i));
    N_sq_avg(i) = arma::mean(N_sq.row(i));
  }
  P.each_col() -= P_avg;
  P_sq = pow(P, 2);
  N_avg_sq = pow(N_avg, 2);
  N_var = arma::as_scalar(arma::mean(N_sq_avg - N_avg_sq));

  for (int i = 0; i < n; i++) {
    boot_var(i) = arma::mean(P_sq.row(i));
    P_sum(i) = arma::sum(P.row(i));
  }

  C = (N * P.t()); - (N_avg.t() * P_sum);
  C_sq = pow(C, 2);

  for (int i = 0; i < n; i++) {
    c_sum(i) = arma::sum(C_sq.col(i));
  }
  
  raw_IJ = c_sum / arma::as_scalar(pow(b, 2));
  
  return raw_IJ - (arma::as_scalar(n) * arma::as_scalar(N_var) * boot_var / arma::as_scalar(b));
}
