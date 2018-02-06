
#include <RcppArmadillo.h>
#include <queue>
#include <algorithm>
#include <iostream>
#include <chrono>
#include <vector>
#include <boost/progress.hpp>

#include "n2/hnsw.h"

using std::chrono::duration;
using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;

using namespace std;
using namespace Rcpp;

#define INDEX_TYPE_COSINE 2
#define INDEX_TYPE_LP 3


// [[Rcpp::export]]
arma::sp_mat n2Knn(const NumericMatrix& m, int k, int nThreads=10, bool verbose=true, string indexType="angular", int M=12, int MaxM0=24) {
  
  if(m.nrow()<=k) {
    if(verbose) cout<<"k>=m.nrow(): returning dense matrix\n";
    arma::sp_mat r(m.nrow(),m.nrow());
    r=spones(r); 
    return(r);
  }
  if(verbose) cout<<"creating space of type "<<indexType<<" done\n";
  n2::Hnsw index(m.ncol(), indexType);
  if(verbose) cout<<"adding data ... "<<flush;
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    index.AddData(v);
  }
  if(verbose) cout<<"done"<<endl;
  
  if(verbose) cout<<"building index ... "<<flush;
  index.Build(M, MaxM0, -1, nThreads);
  if(verbose) cout<<"done"<<endl;

  int ef_search = k*50;
  
  int nanswers=k*m.nrow();
  arma::umat ansloc(2,nanswers,arma::fill::zeros);
  arma::vec ansdist(nanswers);
  
  if(verbose) cout<<"querying ... "<<flush; 

  //#pragma omp parallel for num_threads(nThreads) shared(index,ansloc,ansdist,m,ef_search,nanswers) 
  for(int i=0;i<m.nrow();i++) {
    std::vector<std::pair<int, float> > result;
    index.SearchById(i, k, ef_search, result);
    int nr=result.size(); 
    if(nr>k) nr=k;
    for(int j=0;j<nr;j++) {
      int l=i*k+j;
      //cout<<"i="<<i<<"; j="<<j<<"; n="<<result[j].first<<"; d="<<result[j].second<<endl;
      ansloc(0,l)=result[j].first;
      ansloc(1,l)=i;
      ansdist(l)=result[j].second;
    }
  }
  if(verbose) cout<<"done"<<endl;


  arma::sp_mat r(ansloc,ansdist,m.nrow(),m.nrow(),true);
  return(r);

};


// find NN of A in B
// [[Rcpp::export]]
arma::sp_mat n2CrossKnn(const NumericMatrix& mA, const NumericMatrix& mB, int k, int nThreads=10, bool verbose=true, string indexType="angular", int M=12, int MaxM0=24) {
  
  if(mB.nrow()<=k) {
    if(verbose) cout<<"k>=mB.nrow(): returning dense matrix\n";
    arma::sp_mat r(mB.nrow(),mA.nrow());
    r=spones(r); 
    return(r);
  }
  
  if(verbose) cout<<"creating space of type "<<indexType<<" done\n";
  n2::Hnsw index(mB.ncol(), indexType);
  if(verbose) cout<<"adding data ... "<<flush;
  for(int i=0;i<mB.nrow();i++) {
    NumericVector nv = mB.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    index.AddData(v);
  }
  if(verbose) cout<<"done"<<endl;
  
  if(verbose) cout<<"building index ... "<<flush;
  index.Build(M, MaxM0, -1, nThreads);
  if(verbose) cout<<"done"<<endl;

  int ef_search = k*50;
  
  int nanswers=k*mA.nrow();
  arma::umat ansloc(2,nanswers,arma::fill::zeros);
  arma::vec ansdist(nanswers);
  
  if(verbose) cout<<"querying ... "<<flush; 

  for(int i=0;i<mA.nrow();i++) {
    NumericVector nv = mA.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    std::vector<std::pair<int, float> > result;
    index.SearchByVector(v, k, ef_search, result);
    int nr=result.size(); 
    if(nr>k) nr=k;
    for(int j=0;j<nr;j++) {
      int l=i*k+j;
      ansloc(0,l)=result[j].first;
      ansloc(1,l)=i;
      ansdist(l)=result[j].second;
    }
  }
  if(verbose) cout<<"done"<<endl;


  arma::sp_mat r(ansloc,ansdist,mB.nrow(),mA.nrow(),true);
  return(r);

};
  

