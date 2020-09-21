// details here on avoiding compilation errors 
// https://github.com/kharchenkolab/N2R/pull/2

// -- explicitly used std:: and Rcpp:: 
// -- must place the '#include <RcppEigen.h>'' header before the '#include <RcppSpdlog>'' header

#define R_NO_MAP    // R redefines things like Erorr; afterwards it becomes Rf_Error
#define STRICT_R_HEADERS  // hides Calloc() as R_Calloc() etc.

#include <queue>
#include <algorithm>
#include <iostream>
#include <chrono>
#include <vector>


using std::chrono::duration;
using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;

#define INDEX_TYPE_COSINE 2
#define INDEX_TYPE_LP 3

#include <RcppEigen.h>

#include "n2/hnsw.h"   // <RcppSpdlog> included here


typedef Eigen::Triplet<double> T;

// [[Rcpp::export]]
Eigen::SparseMatrix<double> n2Knn(const Rcpp::NumericMatrix& m, int64_t k, int64_t nThreads=10, bool verbose=true, std::string indexType="angular", int64_t M=12, int64_t MaxM0=24, float ef_search_multiplier=50, bool quiet=false) {
  Eigen::SparseMatrix<double> mat(m.nrow(), m.nrow());

  if(m.nrow() <= k) {
    if (!quiet) {
      Rcpp::warning("k >= m.nrow(), reducing it to m.nrow() - 1");
    }

    k = m.nrow() - 1;
  }

  if(verbose) Rcpp::Rcout<<"creating space of type "<<indexType<<" done\n";
  n2::Hnsw index(m.ncol(), indexType);
  if(verbose) Rcpp::Rcout<<"adding data ... "<<std::flush;
  for(int64_t i=0;i<m.nrow();i++) {
    Rcpp::NumericVector nv = m.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    index.AddData(v);
  }
  if(verbose) Rcpp::Rcout<<"done"<<std::endl;

  if(verbose) Rcpp::Rcout<<"building index ... "<<std::flush;
  index.Build(M, MaxM0, -1, nThreads);
  if(verbose) Rcpp::Rcout<<"done"<<std::endl;

  int64_t ef_search = k*ef_search_multiplier;

  int64_t nanswers=k*m.nrow();

  if(verbose) Rcpp::Rcout<<"querying ... "<<std::flush;

  //#pragma omp parallel for num_threads(nThreads) shared(index,ansloc,ansdist,m,ef_search,nanswers)
  std::vector<T> tripletList;
  tripletList.reserve(nanswers);

  for(int64_t i=0;i<m.nrow();i++) {
    std::vector<std::pair<int, float> > result;
    index.SearchById(i, k, ef_search, result);
    int64_t nr=result.size();
    if(nr>k) nr=k;
    for(int64_t j=0;j<nr;j++) {
      int64_t l=i*k+j;
      //cout<<"i="<<i<<"; j="<<j<<"; n="<<result[j].first<<"; d="<<result[j].second<<endl;
      tripletList.push_back(T(result[j].first,i,result[j].second));
    }
  }
  if(verbose) Rcpp::Rcout<<"done"<<std::endl;

  mat.setFromTriplets(tripletList.begin(),tripletList.end());
  return(mat);
};


// [[Rcpp::export]]
Eigen::SparseMatrix<double> n2CrossKnn(const Rcpp::NumericMatrix& mA, const Rcpp::NumericMatrix& mB, int64_t k, int64_t nThreads=10, bool verbose=true, std::string indexType="angular", int64_t M=12, int64_t MaxM0=24, float ef_search_multiplier=50, bool quiet=false) {

  Eigen::SparseMatrix<double> mat(mB.nrow(),mA.nrow());

  if(mB.nrow() <= k) {
    if (!quiet) {
      Rcpp::warning("k >= mB.nrow(), reducing it to mB.nrow() - 1");
    }

    k = mB.nrow() - 1;
  }

  if(verbose) Rcpp::Rcout<<"creating space of type "<<indexType<<" done\n";
  n2::Hnsw index(mB.ncol(), indexType);
  if(verbose) Rcpp::Rcout<<"adding data ... "<<std::flush;
  for(int64_t i=0;i<mB.nrow();i++) {
    Rcpp::NumericVector nv = mB.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    index.AddData(v);
  }
  if(verbose) Rcpp::Rcout<<"done"<<std::endl;

  if(verbose) Rcpp::Rcout<<"building index ... "<<std::flush;
  index.Build(M, MaxM0, -1, nThreads);
  if(verbose) Rcpp::Rcout<<"done"<<std::endl;

  int64_t ef_search = k*ef_search_multiplier;

  int64_t nanswers=k*mA.nrow();
  std::vector<T> tripletList;
  tripletList.reserve(nanswers);

  if(verbose) Rcpp::Rcout<<"querying ... "<<std::flush;

  for(int64_t i=0;i<mA.nrow();i++) {
    Rcpp::NumericVector nv = mA.row(i);
    std::vector<float> v(nv.begin(),nv.end());
    std::vector<std::pair<int, float> > result;
    index.SearchByVector(v, k, ef_search, result);
    int64_t nr=result.size();
    if(nr>k) nr=k;
    for(int64_t j=0;j<nr;j++) {
      int64_t l=i*k+j;
      tripletList.push_back(T(result[j].first,i,result[j].second));
    }
  }
  if(verbose) Rcpp::Rcout<<"done"<<std::endl;

  mat.setFromTriplets(tripletList.begin(),tripletList.end());
  return(mat);
};



