#include "pagoda2.h"
// [[Rcpp::depends(RcppArmadillo)]]

#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP winsorizeMatrix(SEXP Mat, SEXP Trim){
  arma::mat m=Rcpp::as<arma::mat>(Mat);
  int n=m.n_cols; int k=m.n_rows;
  int ntr=round(n * Rcpp::as<double>(Trim)); // number of positions to trim (from each side)
  if(ntr==0) { return wrap(m); } // nothing needs to be done
  for(int i=0;i<k;i++) { // for every row of the matrix
    arma::rowvec z= m.row(i);
    // determine outliers
    // arma::urowvec o=sort_index(abs(z-median(z)),1);
    arma::ucolvec o=sort_index(z); // ascending order
    // determine range
    double minv=z(o(ntr));
    double maxv=z(o(n-ntr-1));
    for(int j=0;j<ntr;j++) {
      z(o(j))=minv;
      //R_CheckUserInterrupt();
    }
    for(int j=n-ntr;j<n;j++) {
      z(o(j))=maxv;
      //R_CheckUserInterrupt();
    }
    m.row(i)=z;
    R_CheckUserInterrupt();
  }
  return wrap(m);
}
