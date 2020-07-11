// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <queue>
#include <algorithm>
#include <iostream>
#include <chrono>
#include <vector>

#include "n2/hnsw.h"

using std::chrono::duration;
using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;

typedef Eigen::Triplet<double> T;

using namespace std;
using namespace Rcpp;

#define INDEX_TYPE_COSINE 2
#define INDEX_TYPE_LP 3


// [[Rcpp::export]]
Eigen::SparseMatrix<double> n2Knn(const NumericMatrix& m, int k, int nThreads=10, bool verbose=true, string indexType="angular", int M=12, int MaxM0=24) {
  
  Eigen::SparseMatrix<double> mat(m.nrow(),m.nrow());

  if(m.nrow()<=k) {
    if(verbose) Rcpp::Rcout <<"k>=m.nrow(): returning dense matrix\n";
    Eigen::MatrixXd md(m.nrow(),m.nrow());
    md.fill(1);
    mat=md.sparseView();
    return(mat);
  }

  if(verbose) Rcpp::Rcout <<"creating space of type "<<indexType<<" done\n";
  n2::Hnsw index(m.ncol(), indexType);
  if(verbose) Rcpp::Rcout <<"adding data ... "<<flush;
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    index.AddData(v);
  }
  if(verbose) Rcpp::Rcout <<"done"<<endl;
  
  if(verbose) Rcpp::Rcout <<"building index ... "<<flush;
  index.Build(M, MaxM0, -1, nThreads);
  if(verbose) Rcpp::Rcout <<"done"<<endl;

  int ef_search = k*50;
  
  int nanswers=k*m.nrow();
  
  if(verbose) Rcpp::Rcout <<"querying ... "<<flush; 

  //#pragma omp parallel for num_threads(nThreads) shared(index,ansloc,ansdist,m,ef_search,nanswers) 
  std::vector<T> tripletList;
  tripletList.reserve(nanswers);

  for(int i=0;i<m.nrow();i++) {
    std::vector<std::pair<int, float> > result;
    index.SearchById(i, k, ef_search, result);
    int nr=result.size(); 
    if(nr>k) nr=k;
    for(int j=0;j<nr;j++) {
      int l=i*k+j;
      //cout<<"i="<<i<<"; j="<<j<<"; n="<<result[j].first<<"; d="<<result[j].second<<endl;
      tripletList.push_back(T(result[j].first,i,result[j].second));
    }
  }
  if(verbose) Rcpp::Rcout <<"done"<<endl;

  mat.setFromTriplets(tripletList.begin(),tripletList.end());
  return(mat);
};


// find NN of A in B
// [[Rcpp::export]]
Eigen::SparseMatrix<double> n2CrossKnn(const NumericMatrix& mA, const NumericMatrix& mB, int k, int nThreads=10, bool verbose=true, string indexType="angular", int M=12, int MaxM0=24) {

  Eigen::SparseMatrix<double> mat(mB.nrow(),mA.nrow());

  if(mB.nrow()<=k) {
    if(verbose) Rcpp::Rcout <<"k>=mB.nrow(): returning dense matrix\n";
    Eigen::MatrixXd md(mB.nrow(),mA.nrow());
    md.fill(1);
    mat=md.sparseView();
    return(mat);
  }
  
  if(mB.nrow()<=k) {
    if(verbose) Rcpp::Rcout <<"k>=mB.nrow(): returning dense matrix\n";
    Eigen::MatrixXd md(mB.nrow(),mA.nrow());
    md.fill(1);
    mat=md.sparseView();
    return(mat);
  }
  
  if(verbose) Rcpp::Rcout <<"creating space of type "<<indexType<<" done\n";
  n2::Hnsw index(mB.ncol(), indexType);
  if(verbose) Rcpp::Rcout <<"adding data ... "<<flush;
  for(int i=0;i<mB.nrow();i++) {
    NumericVector nv = mB.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    index.AddData(v);
  }
  if(verbose) Rcpp::Rcout <<"done"<<endl;
  
  if(verbose) Rcpp::Rcout <<"building index ... "<<flush;
  index.Build(M, MaxM0, -1, nThreads);
  if(verbose) Rcpp::Rcout <<"done"<<endl;

  int ef_search = k*50;
  
  int nanswers=k*mA.nrow();
  std::vector<T> tripletList;
  tripletList.reserve(nanswers);

  if(verbose) Rcpp::Rcout <<"querying ... "<<flush; 

  for(int i=0;i<mA.nrow();i++) {
    NumericVector nv = mA.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    std::vector<std::pair<int, float> > result;
    index.SearchByVector(v, k, ef_search, result);
    int nr=result.size(); 
    if(nr>k) nr=k;
    for(int j=0;j<nr;j++) {
      int l=i*k+j;
      tripletList.push_back(T(result[j].first,i,result[j].second));
    }
  }
  if(verbose) Rcpp::Rcout <<"done"<<endl;

  mat.setFromTriplets(tripletList.begin(),tripletList.end());
  return(mat);
};
  

