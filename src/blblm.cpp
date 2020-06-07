#include <RcppArmadillo.h>

using namespace Rcpp;

//[[Rcpp::depends(RcppArmadillo)]]

//[[Rcpp::export]]
List wt_lm(const arma::vec &y, const arma::mat &X,const arma::vec & w) {

  int n = X.n_rows, k = X.n_cols;
  arma::mat wt = diagmat(w);
  arma::mat tx = arma::trans(X);
  arma::mat coef = solve((tx*wt)*X,(tx*wt)*y); //coef
  arma::colvec residual = y - X*coef; //residuals
  arma::vec fitval = X*coef;//fitted values
  double sig2 = arma::as_scalar(arma::trans(residual)*residual/(n-k));
  arma::colvec stderrest =
  arma::sqrt(sig2 * arma::diagvec( arma::inv(arma::trans(X)*wt*X)) );
  return List::create(Named("coefficients") = arma::trans(coef),
                      Named("stderr") = stderrest,
                      Named("fittedvals") = fitval,
                      Named("residuals") = residual,
                      Named("rank") = arma::rank(X),
                      Named("response") = y,
                      Named("weights") = w);
}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sample_intC(DataFrame df,int m){
  int n=df.nrow();
  IntegerVector subs = seq(1,m);
  IntegerVector subsam = sample(subs,n,true);
  return subsam;
}