#include "pagoda2.h"

#include <stdlib.h>

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <cmath>
#include <queue>
#include <algorithm>
#include <iostream>
#include <chrono>
#include <vector>
#include <queue>
#include <list>


#ifdef _OPENMP
  #include <omp.h>
#endif

using namespace Rcpp;


// [[Rcpp::export]]
SEXP matWCorr(SEXP Mat, SEXP Matw){
  arma::mat m=Rcpp::as<arma::mat>(Mat);
  arma::mat w=Rcpp::as<arma::mat>(Matw);
  int n=m.n_cols; // int k=m.n_rows;
  arma::mat c(n,n,arma::fill::eye);
  for(int i=0;i<(n-1);i++) {
    for(int j=i+1;j<n;j++) {
      arma::colvec ic=m.col(i);
      arma::colvec jc=m.col(j);
      // need to avoid 'explicitly assigning value of variable of type 'arma::colvec' (aka 'Col<double>') to itself [-Wself-assign-overloaded]'
      arma::colvec *copyic = &ic;
      arma::colvec *copyjc = &jc;
      // weight for this i,j
      arma::colvec jw=w.col(i) % w.col(j); jw=sqrt(jw);
      jw/=sum(jw);
      // shift by weighted means
      ic-=dot(ic,jw); 
      jc-=dot(jc,jw);
      double nm=dot(ic % jc,jw);
      // we need to avoid warning which checks whether assignment operation survives self-assignment.
      // ic%=ic; 
      // jc%=jc;
      *copyic%=ic;
      *copyjc%=jc;
      ic = *copyic;
      jc =  *copyjc;   
      double dn=dot(ic,jw);
      dn*=dot(jc,jw);
      c(j,i)=nm/sqrt(dn);
      //R_CheckUserInterrupt();
    }
    Rcpp::checkUserInterrupt();
  }
  return wrap(c);
}


// [[Rcpp::export]]
SEXP winsorizeMatrix(SEXP Mat, SEXP Trim){
  arma::mat m=Rcpp::as<arma::mat>(Mat);
  int n=m.n_cols; 
  int k=m.n_rows;
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
    Rcpp::checkUserInterrupt();
  }
  return wrap(m);
}


// [[Rcpp::export]]
SEXP plSemicompleteCor2(SEXP Pl) {
  Rcpp::List pl(Pl);
  int np=pl.size();
  // calculate individual var

  arma::mat cm(np,np);
  cm.eye();
  arma::imat cn(np,np,arma::fill::zeros); // number of genes in the union
  // calculate covariance and correlation
  for(int i=0;i<np;i++) {
    Rcpp::NumericVector v1=Rcpp::as<Rcpp::NumericVector>(Rcpp::as<Rcpp::List>(pl[i])[1]);
    Rcpp::NumericVector i1=Rcpp::as<Rcpp::NumericVector>(Rcpp::as<Rcpp::List>(pl[i])[0]);
    int v1s=v1.size();
    for(int j=i+1;j<np;j++) {
      Rcpp::NumericVector v2=Rcpp::as<Rcpp::NumericVector>(Rcpp::as<Rcpp::List>(pl[j])[1]);
      Rcpp::NumericVector i2=Rcpp::as<Rcpp::NumericVector>(Rcpp::as<Rcpp::List>(pl[j])[0]);
      int v2s=v2.size();
      int sgc=0; // "same gene" counter
      double l12=0; // covar
      double l11=0; // var1
      double l22=0; // var2
      int k2=0;
      for(int k1=0;k1<v1s;k1++) {
        int id=i2[k2]-i1[k1]; // gene index difference
        if(id==0) { // hit the same gene
          sgc++;
          l12+=v2[k2]*v1[k1];
          l11+=v2[k2]*v2[k2];
          l22+=v1[k1]*v1[k1];
        } else if(id<0) { // need to advance k2
          do { k2++; } while(i2[k2]<i1[k1] && k2<v2s);
          if(k2==v2s) { 
            // v2 ended, done with this pair
            break; 
          } else if (i2[k2]==i1[k1]) { 
            // hit the same gene
            sgc++;
            l12+=v2[k2]*v1[k1];
            l11+=v2[k2]*v2[k2];
            l22+=v1[k1]*v1[k1];
          }
        }
      }
      Rcpp::checkUserInterrupt();
      double cv=l11*l22;
      if(cv>0) { cv=l12/sqrt(cv); }
      cm(i,j)=cv; cm(j,i)=cv;
      sgc=v1s+v2s-sgc;
      cn(i,j)=sgc; cn(j,i)=sgc;
    }
    Rcpp::checkUserInterrupt();
  };
  return List::create(Named("r") = wrap(cm),
                      Named("n") = wrap(cn));
}

