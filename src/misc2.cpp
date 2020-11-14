// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]

#include "pagoda2.h"

#ifdef _OPENMP
  #include <omp.h>
#endif

// [[Rcpp::export]]
int non0LogColLmS(SEXP sY, const arma::mat& X, const arma::vec& ldepth, const int maxCells=0, int ncores=1) {
// need to do this as SEXP, modify the slots on the fly
  S4 mat(sY);  
  const arma::uvec i(( unsigned int *)INTEGER(mat.slot("i")),LENGTH(mat.slot("i")),false,true); 
  //const arma::uvec i=Rcpp::as<arma::uvec>(mat.slot("i"));
  const arma::ivec p(INTEGER(mat.slot("p")),LENGTH(mat.slot("p")),false,true); 
  arma::vec Y(REAL(mat.slot("x")),LENGTH(mat.slot("x")),false,true); 
  
  // for each gene
#ifdef _OPENMP
#pragma omp parallel for num_threads(ncores) shared(Y) 
#endif
  for(int g=1;g<p.size();g++) {
    int p1=p[g]; int p0=p[g-1]; int ncells=p1-p0;
    if(ncells<2) { continue; }
    arma::colvec ly=Y.subvec(p0,p1-1);
    ly=log(ly);
    arma::uvec ri=i.subvec(p0,p1-1);
    ly-=ldepth.elem(ri); // the depth normalization could've been done much faster before transposition
    arma::mat Xs=X.rows(ri);
    arma::colvec coef=arma::solve(Xs,ly);
    arma::colvec lb = Xs * coef;
    lb = lb-mean(lb);
    ly = ly-lb;
    Y.subvec(p0,p1-1)=exp(ly);
  }

  return(0);
}


// calculate column mean and variance, optionally taking a subset of rows to operate on
// [[Rcpp::export]]
Rcpp::DataFrame colMeanVarS(SEXP sY,  SEXP rowSel, int ncores=1) {
// need to do this as SEXP, modify the slots on the fly
  S4 mat(sY);  
  const arma::uvec i(( unsigned int *)INTEGER(mat.slot("i")),LENGTH(mat.slot("i")),false,true); 
  const arma::ivec dims(INTEGER(mat.slot("Dim")),LENGTH(mat.slot("Dim")),false,true); 
  const arma::ivec p(INTEGER(mat.slot("p")),LENGTH(mat.slot("p")),false,true); 
  arma::vec Y(REAL(mat.slot("x")),LENGTH(mat.slot("x")),false,true); 
  
  bool rowSelSpecified=!Rf_isNull(rowSel);
  const arma::ivec rs=(rowSelSpecified) ? arma::ivec(INTEGER(rowSel),LENGTH(rowSel),false,true) : arma::ivec(); 

  int ncols=p.size()-1;
  int nrows=dims[0]; 
  if(rowSelSpecified) { 
    nrows=0; 
    for(int j=0;j<rs.size();j++) { if(rs[j]) { nrows++; } }
  }
  arma::vec meanV(ncols,arma::fill::zeros); arma::vec varV(ncols,arma::fill::zeros); arma::vec nobsV(ncols,arma::fill::zeros);
  // for each gene
#ifdef _OPENMP
#pragma omp parallel for num_threads(ncores) shared(meanV,varV,nobsV) 
#endif
  for(int g=0;g<ncols;g++) {
    int p0=p[g]; int p1=p[g+1]; 
    if(p1-p0 <1) { continue; }
    arma::colvec ly;
    arma::colvec *copyly = &ly; // need to avoid 'explicitly assigning value of variable of type 'arma::colvec' (aka 'Col<double>') to itself [-Wself-assign-overloaded]'
    if(rowSelSpecified) {
      // select valid rows
      int nvalid=0;
      ly=arma::vec(p1-p0);
      for(int j=p0;j<p1;j++) {
	if(rs[i[j]]) { 
	  ly[nvalid]=Y[j]; nvalid++;
	}
      }
      nobsV[g]=nvalid;
      ly=ly.head(nvalid);
    } else {
      nobsV[g]=p1-p0;
      ly=Y.subvec(p0,p1-1);
    }
    
    double m=sum(ly)/nrows;
    meanV[g]=m;
    ly-=m; 
    // ly%=ly; 
    *copyly%=ly; // we need to avoid warning which checks whether assignment operation survives self-assignment.
    ly = *copyly;
    varV[g]=(sum(ly)+(m*m*(nrows-ly.size())))/nrows;
  }
  return Rcpp::DataFrame::create(Named("m")=meanV,Named("v")=varV,Named("nobs",nobsV));
}


// calculates factor-stratified sums for each column
// rowSel is an integer factor; 
// note that the 0-th column will return sums for any NA values; 0 or negative values will be omitted
// note2: trailing levels that are not mentioned in the vector will be omitted, resulting in a smaller matrix
// [[Rcpp::export]]
arma::mat colSumByFac(SEXP sY,  SEXP rowSel) {
// need to do this as SEXP, modify the slots on the fly
  S4 mat(sY);  
  const arma::uvec i(( unsigned int *)INTEGER(mat.slot("i")),LENGTH(mat.slot("i")),false,true); 
  const arma::ivec dims(INTEGER(mat.slot("Dim")),LENGTH(mat.slot("Dim")),false,true); 
  const arma::ivec p(INTEGER(mat.slot("p")),LENGTH(mat.slot("p")),false,true); 
  arma::vec Y(REAL(mat.slot("x")),LENGTH(mat.slot("x")),false,true); 
  
  const arma::ivec rs=arma::ivec(INTEGER(rowSel),LENGTH(rowSel),false,true);

  int ncols=p.size()-1;
  int nlevels=0;
  for(int j=0;j<rs.size();j++) { 
    if(rs[j]!=NA_INTEGER) {
      if(rs[j]>nlevels) { nlevels=rs[j]; }
    }
  }
  if(nlevels==0) { stop("colSumByFac(): supplied factor doesn't have any levels!"); }
  arma::mat sumM(nlevels+1,ncols,arma::fill::zeros);

  // for each gene
//#pragma omp parallel for shared(sumM) 
  for(int g=0;g<ncols;g++) {
    int p0=p[g]; int p1=p[g+1]; 
    if(p1-p0 <1) { continue; }
    for(int j=p0;j<p1;j++) {
      int row=i[j];
      int f=rs[row];
      if(f==NA_INTEGER) {
	      sumM(0,g)+=Y[j];
      } else if(f>0) {
      	sumM(f,g)+=Y[j];
      }
    }
  }
  return sumM;
}



// multiply each column by a mult vector, optionally using a subset of rows (rowSel)
// [[Rcpp::export]]
int inplaceColMult(SEXP sY,const arma::vec& mult,SEXP rowSel, int ncores=1) {
// need to do this as SEXP, modify the slots on the fly
  S4 mat(sY);  
  const arma::uvec i(( unsigned int *)INTEGER(mat.slot("i")),LENGTH(mat.slot("i")),false,true); 
  const arma::ivec dims(INTEGER(mat.slot("Dim")),LENGTH(mat.slot("Dim")),false,true); 
  const arma::ivec p(INTEGER(mat.slot("p")),LENGTH(mat.slot("p")),false,true); 
  arma::vec Y(REAL(mat.slot("x")),LENGTH(mat.slot("x")),false,true); 

  bool rowSelSpecified=!Rf_isNull(rowSel);
  const arma::ivec rs=(rowSelSpecified) ? arma::ivec(INTEGER(rowSel),LENGTH(rowSel),false,true) : arma::ivec(); 

  int ncols=p.size()-1;
  
  // for each gene
#ifdef _OPENMP
#pragma omp parallel for num_threads(ncores) shared(Y) 
#endif
  for(int g=0;g<ncols;g++) {
    int p1=p[g+1]; int p0=p[g]; int ncells=p1-p0;
    if(ncells<1) { continue; }

    if(rowSelSpecified) {
      // select valid rows
      const double cmult=mult[g];
      for(int j=p0;j<p1;j++) {
	if(rs[i[j]]) { 
	  Y[j]*=cmult;
	}
      }
    } else {
      // multiply the whole column
      Y.subvec(p0,p1-1) *=mult[g];
    }
  }
  return(1);
}

// Winsorize top N values in each column of a sparse matrix
// [[Rcpp::export]]
int inplaceWinsorizeSparseCols(SEXP sY,const int n, int ncores=1) {
  // need to do this as SEXP, modify the slots on the fly
  S4 mat(sY);  
  const arma::uvec i(( unsigned int *)INTEGER(mat.slot("i")),LENGTH(mat.slot("i")),false,true); 
  const arma::ivec dims(INTEGER(mat.slot("Dim")),LENGTH(mat.slot("Dim")),false,true); 
  const arma::ivec p(INTEGER(mat.slot("p")),LENGTH(mat.slot("p")),false,true); 
  arma::vec Y(REAL(mat.slot("x")),LENGTH(mat.slot("x")),false,true); 
  int ncols=p.size()-1;
  // for each column
  
  arma::vec tv(ncols);
#ifdef _OPENMP
#pragma omp parallel for num_threads(ncores) shared(Y) 
#endif
  for(int g=0;g<ncols;g++) {
    priority_queue<std::pair<double,int> ,std::vector<std::pair<double,int> >, std::greater<std::pair<double,int> > > q; 
    int p1=p[g+1]; int p0=p[g]; int ncells=p1-p0;
    if(ncells<=(n+1)) { continue; } // not enough observations.. could 0 them all out, alternatively, but presumably this was done beforehand
    // find indices of top n+1 values
    // insert first n+1 values
    for(int j=p0;j<p0+n+1;j++) {
      q.push(std::pair<double,int>(Y[j],j));
    }
    for(int j=p0+n+1;j<p1;j++) {
      double v=Y[j];
      if(v>q.top().first) {
	q.pop(); 
	q.push(std::pair<double,int>(v,j));
      }
    }
    // set all values to the smallest (top) one
    double v=q.top().first;
    q.pop();
    while(!q.empty()) {
      Y[q.top().second]=v;
      q.pop();
    }
  }
  return(1);
}


// JS distance metric (sqrt(JS div)) between the columns of a dense matrix m
// returns vectorized version of the lower triangle (as R dist oject)
// [[Rcpp::export]]
arma::mat jsDist(const arma::mat& m, int ncores=1) {
  //arma::vec d(m.n_cols*(m.n_cols-1)/2);
  arma::mat d(m.n_cols,m.n_cols,arma::fill::zeros);
//#pragma omp parallel for num_threads(ncores) shared(d)
  for(int i=0;i<(m.n_cols-1);i++) {
    arma::vec li=log(m.col(i));
    for(int j=i+1;j<m.n_cols;j++) {
      arma::vec lj=log(m.col(j));
      arma::vec ji=m.col(j)+m.col(i);
      ji=m.col(j)%lj + m.col(i)%li - ji%(log(ji)-log(2.0)); 
      double v=arma::accu(ji.elem(arma::find_finite(ji))); 
      //d[m.n_cols*i - i*(i-1)/2 + j-i]=sqrt(v/2.0);
      d(j,i)=d(i,j)=v;
    }
  }

  return(d);
}



// helper function to get 10x sparse matrix rows into an increasing order
// [[Rcpp::export]]
arma::ivec orderColumnRows(const arma::ivec& p,arma::ivec& i) {
  //  #pragma omp parallel
  for(int j=0;j<(p.n_elem-1);j++) {
    i.subvec(p[j],p[j+1]-1)=sort(i.subvec(p[j],p[j+1]-1));
  }
  return(i);
}


// calculate Pearson linear correlation between a given vector (v) and the columns of the sparse matrix (Y)
// [[Rcpp::export]]
arma::vec smatColVecCorr(SEXP sY,  SEXP sv, bool parallel=true) {
  // parse out sparse matrix data
  S4 mat(sY);  
  const arma::uvec i(( unsigned int *)INTEGER(mat.slot("i")),LENGTH(mat.slot("i")),false,true); 
  const arma::ivec dims(INTEGER(mat.slot("Dim")),LENGTH(mat.slot("Dim")),false,true); 
  const arma::ivec p(INTEGER(mat.slot("p")),LENGTH(mat.slot("p")),false,true); 
  const arma::vec Y(REAL(mat.slot("x")),LENGTH(mat.slot("x")),false,true); 
  
  arma::vec v=arma::vec(REAL(sv),LENGTH(sv),true,true);

  int ncols=p.size()-1;
  if(v.size()!=dims[0]) { stop("smatColVecCorr(): vector must be the same length as the number of rows in the matrix!"); }
  double nrows=(double) dims[0];
  // output vector
  arma::vec r(ncols,arma::fill::zeros);
  
  // center v
  v-=mean(v);
  double vsd=sqrt(sum(v%v));
  double vsum=sum(v);

  // for each column
#ifdef _OPENMP
#pragma omp parallel for shared(r) if(parallel)
#endif
  for(int g=0;g<ncols;g++) {
    int p0=p[g]; int p1=p[g+1]; 
    if(p1-p0 <1) { continue; }
    // first pass to calculate the mean
    arma::vec colV=Y.subvec(p0,p1-1);
    double colMean=sum(colV)/nrows;
    colV-=colMean;
    double colSD=sqrt(sum(colV % colV) + (nrows-colV.size())*abs(colMean)*abs(colMean));
    // contribution of non-zero entries
    arma::vec nzv=v.elem(i.subvec(p0,p1-1));
    colV%=nzv;
    // numerator
    double colR=sum(colV) - colMean*(vsum-sum(nzv));
    // denominator
    colR/=vsd*colSD;
    r[g]=colR;
  }
  return r;
}

// fast(er) matrix correlation calculatiosn using arma
// [[Rcpp::export]]
arma::mat arma_mat_cor(const arma::mat& m) {
  return(cor(m));
}
