#include <queue>
#include <algorithm>
#include <iostream>
#include <vector>
#include <vector>
#include <RcppArmadillo.h>

#include "LargeVis.h"

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix getLargeVisEmbedding(SEXP sY,int out_dim=2, int n_threads=8, int n_samples=-1, int n_propagation=-1, float alpha=-1, int n_trees=-1, int n_negative=-1, int n_neighbors=-1, float n_gamma=-1, float perplexity=-1) {
  
  // construct edge vectors from the sparse matrix
  S4 mat(sY);  
  const arma::uvec i(( unsigned int *)INTEGER(mat.slot("i")),LENGTH(mat.slot("i")),false,true); 
  const arma::ivec dims(INTEGER(mat.slot("Dim")),LENGTH(mat.slot("Dim")),false,true); 
  const arma::ivec p(INTEGER(mat.slot("p")),LENGTH(mat.slot("p")),false,true); 
  arma::vec Y(REAL(mat.slot("x")),LENGTH(mat.slot("x")),false,true); 
  
  int n_vertices=dims(0);
  
  // could save some memory copy here ... ohh well
  cout<<"converting data:"<<endl;
  std::vector<int> et=arma::conv_to< std::vector<int> >::from(i);
  std::vector<int> ef(i.size());
  int lp=0;
  for(int i=0;i<p.size();i++) {
    for(int j=0;j<(p[i+1]-p[i]);j++) {
      ef[lp]=i;
      lp++;
    }
  }
  cout<<"3 lp="<<lp<<endl;
  std::vector<float> ew=arma::conv_to< std::vector<float> >::from(Y);
  cout<<"initializing model:"<<endl;
  LargeVis model;
  cout<<"loading data:"<<endl;
  model.load_from_graph_data(ef,et,ew,n_vertices);
  cout<<"running largeVis:"<<endl;
  model.run(out_dim, n_threads, n_samples, n_propagation, alpha, n_trees, n_negative, n_neighbors, n_gamma, perplexity);

  cout<<"exporting result:"<<endl;
  float* vis=model.get_vis();

  // convert to NumericMatrix
  NumericMatrix em(n_vertices,out_dim);
  for(int i=0;i<n_vertices;i++) {
    for(int j=0;j<out_dim;j++) {
      em(i,j)=vis[i*out_dim +j];
    }
  }
  cout<<"cleaning model "<<flush;
  model.clean_model();
  cout<<"done"<<endl;
  // assign dimnames
  return(em);
  
}
