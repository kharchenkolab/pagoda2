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

static void validateViewKernelArgs(const arma::ivec& dims,
                                   const arma::vec& depth,
                                   const double depthScale,
                                   const bool normalize,
                                   const arma::ivec& batch,
                                   const arma::mat& batchFactors,
                                   const arma::vec& winsorCaps,
                                   const arma::vec& preWinsorDepth,
                                   const arma::vec& postWinsorDepth) {
  const int nrows = dims[0];
  const int ncols = dims[1];

  if (normalize) {
    if (depth.n_elem != (arma::uword)nrows) {
      stop("View depth vector must have one value per matrix row");
    }
    if (!R_finite(depthScale) || depthScale == 0) {
      stop("View depthScale must be finite and non-zero");
    }
  }

  if (batchFactors.n_elem > 0) {
    if (!normalize) {
      stop("Batch factors require a normalized matrix view");
    }
    if (batch.n_elem != (arma::uword)nrows) {
      stop("View batch vector must have one value per matrix row");
    }
    if (batchFactors.n_rows != (arma::uword)ncols) {
      stop("View batch factor matrix must have one row per matrix column");
    }
    for (int r = 0; r < nrows; r++) {
      if (batch[r] == NA_INTEGER || batch[r] < 1 || batch[r] > (int)batchFactors.n_cols) {
        stop("View batch vector contains invalid factor codes");
      }
    }
  }

  if (winsorCaps.n_elem > 0) {
    if (!normalize) {
      stop("Winsor caps require a normalized matrix view");
    }
    if (winsorCaps.n_elem != (arma::uword)ncols) {
      stop("View winsorCaps vector must have one value per matrix column");
    }
    if (preWinsorDepth.n_elem != (arma::uword)nrows || postWinsorDepth.n_elem != (arma::uword)nrows) {
      stop("View winsor depth vectors must have one value per matrix row");
    }
  }
}

static inline double viewKernelValue(double value,
                                     const int row,
                                     const int col,
                                     const arma::vec& depth,
                                     const double depthScale,
                                     const bool normalize,
                                     const bool logScale,
                                     const arma::ivec& batch,
                                     const arma::mat& batchFactors,
                                     const arma::vec& winsorCaps,
                                     const arma::vec& preWinsorDepth,
                                     const arma::vec& postWinsorDepth) {
  if (normalize) {
    double d = depth[row];

    if (batchFactors.n_elem > 0) {
      value /= batchFactors(col, batch[row] - 1);
    }

    if (winsorCaps.n_elem > 0) {
      const double preDepth = preWinsorDepth[row];
      value /= preDepth;
      if (value > winsorCaps[col]) {
        value = winsorCaps[col];
      }
      value *= preDepth;
      d = postWinsorDepth[row];
    }

    value /= (d / depthScale);
  }

  if (logScale) {
    value = std::log(value + 1.0);
  }

  return value;
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

// calculate column mean and variance of a sparse view without materializing it
// [[Rcpp::export]]
Rcpp::DataFrame colMeanVarView(SEXP sY,
                               SEXP rowSel,
                               const arma::vec& depth,
                               double depthScale,
                               bool normalize,
                               bool logScale,
                               const arma::ivec& batch,
                               const arma::mat& batchFactors,
                               const arma::vec& winsorCaps,
                               const arma::vec& preWinsorDepth,
                               const arma::vec& postWinsorDepth,
                               int ncores=1) {
  S4 mat(sY);
  const arma::uvec i((unsigned int *)INTEGER(mat.slot("i")), LENGTH(mat.slot("i")), false, true);
  const arma::ivec dims(INTEGER(mat.slot("Dim")), LENGTH(mat.slot("Dim")), false, true);
  const arma::ivec p(INTEGER(mat.slot("p")), LENGTH(mat.slot("p")), false, true);
  const arma::vec Y(REAL(mat.slot("x")), LENGTH(mat.slot("x")), false, true);

  validateViewKernelArgs(dims, depth, depthScale, normalize, batch, batchFactors, winsorCaps, preWinsorDepth, postWinsorDepth);

  bool rowSelSpecified=!Rf_isNull(rowSel);
  const arma::ivec rs=(rowSelSpecified) ? arma::ivec(INTEGER(rowSel),LENGTH(rowSel),false,true) : arma::ivec();
  if(rowSelSpecified && rs.n_elem != (arma::uword)dims[0]) {
    stop("rowSel must have one value per matrix row");
  }

  int ncols=p.size()-1;
  int nrows=dims[0];
  if(rowSelSpecified) {
    nrows=0;
    for(int j=0;j<rs.size();j++) { if(rs[j] == TRUE) { nrows++; } }
  }
  if(nrows == 0) {
    stop("rowSel does not select any rows");
  }

  arma::vec meanV(ncols,arma::fill::zeros);
  arma::vec varV(ncols,arma::fill::zeros);
  arma::vec nobsV(ncols,arma::fill::zeros);

#ifdef _OPENMP
#pragma omp parallel for num_threads(ncores) shared(meanV,varV,nobsV)
#endif
  for(int g=0;g<ncols;g++) {
    int p0=p[g]; int p1=p[g+1];
    if(p1-p0 <1) { continue; }

    double sumV = 0.0;
    double sumSqV = 0.0;
    int nvalid = 0;

    for(int j=p0;j<p1;j++) {
      const int row = i[j];
      if(!rowSelSpecified || rs[row] == TRUE) {
        const double v = viewKernelValue(Y[j], row, g, depth, depthScale, normalize, logScale,
                                         batch, batchFactors, winsorCaps, preWinsorDepth, postWinsorDepth);
        sumV += v;
        sumSqV += v * v;
        nvalid++;
      }
    }

    nobsV[g] = nvalid;
    const double m = sumV / nrows;
    meanV[g] = m;
    varV[g] = (sumSqV / nrows) - (m * m);
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

// calculates factor-stratified sums for each column of a sparse view without materializing it
// [[Rcpp::export]]
arma::mat colSumByFacView(SEXP sY,
                          SEXP rowSel,
                          const arma::vec& depth,
                          double depthScale,
                          bool normalize,
                          bool logScale,
                          const arma::ivec& batch,
                          const arma::mat& batchFactors,
                          const arma::vec& winsorCaps,
                          const arma::vec& preWinsorDepth,
                          const arma::vec& postWinsorDepth) {
  S4 mat(sY);
  const arma::uvec i((unsigned int *)INTEGER(mat.slot("i")), LENGTH(mat.slot("i")), false, true);
  const arma::ivec dims(INTEGER(mat.slot("Dim")), LENGTH(mat.slot("Dim")), false, true);
  const arma::ivec p(INTEGER(mat.slot("p")), LENGTH(mat.slot("p")), false, true);
  const arma::vec Y(REAL(mat.slot("x")), LENGTH(mat.slot("x")), false, true);

  validateViewKernelArgs(dims, depth, depthScale, normalize, batch, batchFactors, winsorCaps, preWinsorDepth, postWinsorDepth);

  const arma::ivec rs=arma::ivec(INTEGER(rowSel),LENGTH(rowSel),false,true);
  if(rs.n_elem != (arma::uword)dims[0]) {
    stop("rowSel must have one value per matrix row");
  }

  int ncols=p.size()-1;
  int nlevels=0;
  for(int j=0;j<rs.size();j++) {
    if(rs[j]!=NA_INTEGER) {
      if(rs[j]>nlevels) { nlevels=rs[j]; }
    }
  }
  if(nlevels==0) { stop("colSumByFacView(): supplied factor doesn't have any levels!"); }
  arma::mat sumM(nlevels+1,ncols,arma::fill::zeros);

  for(int g=0;g<ncols;g++) {
    int p0=p[g]; int p1=p[g+1];
    if(p1-p0 <1) { continue; }
    for(int j=p0;j<p1;j++) {
      int row=i[j];
      int f=rs[row];
      if(f==NA_INTEGER) {
        sumM(0,g)+=viewKernelValue(Y[j], row, g, depth, depthScale, normalize, logScale,
                                   batch, batchFactors, winsorCaps, preWinsorDepth, postWinsorDepth);
      } else if(f>0) {
        sumM(f,g)+=viewKernelValue(Y[j], row, g, depth, depthScale, normalize, logScale,
                                   batch, batchFactors, winsorCaps, preWinsorDepth, postWinsorDepth);
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
arma::vec smatColVecCorr(SEXP sY,  SEXP sv, int ncores=1) {
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
#pragma omp parallel for shared(r) num_threads(ncores) if(ncores > 1)
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
