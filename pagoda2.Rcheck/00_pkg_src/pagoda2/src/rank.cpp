#include "pagoda2.h"

#ifdef _OPENMP
  #include <omp.h>
#endif

// thanks to nrussel of stackoverflow!
class Comparator {
private:
    const Rcpp::NumericVector& ref;

    bool is_na(double x) const 
    {
        return Rcpp::traits::is_na<REALSXP>(x);    
    }

public:
    Comparator(const Rcpp::NumericVector& ref_)
        : ref(ref_)
    {}

    bool operator()(const int ilhs, const int irhs) const
    {
        double lhs = ref[ilhs], rhs = ref[irhs]; 
        if (is_na(lhs)) return false;
        if (is_na(rhs)) return true;
        return lhs < rhs;
    }
};



// [[Rcpp::export]]
Rcpp::NumericVector avg_rank(Rcpp::NumericVector x)
{
    R_xlen_t sz = x.size();
    Rcpp::IntegerVector w = Rcpp::seq(0, sz - 1);
    std::sort(w.begin(), w.end(), Comparator(x));

    Rcpp::NumericVector r = Rcpp::no_init_vector(sz);
    for (R_xlen_t n, i = 0; i < sz; i += n) {
        n = 1;
        while (i + n < sz && x[w[i]] == x[w[i + n]]) ++n;
        for (R_xlen_t k = 0; k < n; k++) {
            r[w[i + k]] = i + (n + 1) / 2.;
        }
    }

    return r;
};


class Comparator2 {
private:
  const Rcpp::NumericVector::const_iterator it;
  
  bool is_na(double x) const {
    return Rcpp::traits::is_na<REALSXP>(x);    
  }

public:
  Comparator2(const Rcpp::NumericVector::const_iterator it_) : it(it_) {}
  
  bool operator()(const int ilhs, const int irhs) const {
    double lhs = *(it+ilhs), rhs = *(it+irhs);
    if (is_na(lhs)) return false;
    if (is_na(rhs)) return true;
    return lhs < rhs;
  }
};

// same thing but ranks are done within blocks specified by size vector p
// x - @x of the sparse matrix
// p - @p specifying start/end of each column
// dims - number of rows/collumns in the matrix (used to shift the ranks to account for zeros)
// [[Rcpp::export]]
S4 sparse_matrix_column_ranks(const SEXP sY)
{

  const S4 mat(sY);  
  const NumericVector x=mat.slot("x");
  const IntegerVector p=mat.slot("p");
  const IntegerVector dims=mat.slot("Dim");
  
  int ncols=p.size()-1;
  int nrows=dims[0]; 

  NumericVector xbr(x.size());
  
  // iterate over columns
  for(int g=0;g<ncols;g++) {
    int p0=p[g]; int p1=p[g+1]; 
    int sz=p1-p0;
    if(sz <1) { continue; }
    Rcpp::IntegerVector w = Rcpp::seq(0, sz - 1);
    std::sort(w.begin(), w.end(), Comparator2(x.begin()+p0));
    
    // determine the number of zeroes
    int nzeros=nrows-sz;
    for (R_xlen_t n, i = 0; i < sz; i += n) {
      n = 1;
      while (i + n < sz && x[p0+w[i]] == x[p0+w[i + n]]) ++n;
      for (R_xlen_t k = 0; k < n; k++) {
	xbr[p0+w[i + k]] = nzeros + i + (n + 1) / 2.;
      }
    }
  }

  S4 r("dgCMatrix");
  r.slot("i")=mat.slot("i");
  r.slot("p")=mat.slot("p");
  r.slot("Dim")=mat.slot("Dim");
  r.slot("Dimnames")=mat.slot("Dimnames");
  r.slot("x")=xbr;
  r.slot("factors")=mat.slot("factors");
  
  return(r);
  
  
  // return r;
}


// returns a greedy factor grouping posiitions p within a windowSize distance
// [[Rcpp::export]]
IntegerVector nearbyPointsGreedyCluster(NumericVector p,double windowSize) {
  IntegerVector f(p.size());
  int k=1; int kstart=0;
  for(int i=1;i<p.size();i++) {
    if(p[i]-p[kstart]>windowSize) {
      for(int j=kstart;j<i;j++) { f[j]=k; }
      k++; kstart=i;
    }
  }
  for(int j=kstart;j<p.size();j++) { f[j]=k; }
  return(f);
}

// return a distance from a point to a segment - 0 if inside a segment
double psegDist(double p, double s, double e) {
  if(p<s) { return(p-s); }
  if(p>e) { return(p-e); }
  return(0); // point is inside the fragment
}

typedef std::pair<int, double> paired;

bool cmp_second(const paired & left, const paired & right) {
  return fabs(left.second) < fabs(right.second);
}

Rcpp::IntegerVector order(const Rcpp::NumericVector & x) {
    const size_t n = x.size();
    std::vector<paired> pairs;
    pairs.reserve(n);

    for(size_t i = 0; i < n; i++)
        pairs.push_back(std::make_pair(i, x(i)));

    std::sort(pairs.begin(), pairs.end(), cmp_second);

    Rcpp::IntegerVector result = Rcpp::no_init(n);
    for(size_t i = 0; i < n; i++)
        result(i) = pairs[i].first;
    return result;
}


// returns closest N points to each gene
// s, e - start/end positions of the genes
// tss - integer indicating whether the s position is the TSS (i.e. +-strand gene)
// p - point positions
// return elements are matrices (length(s) * N):
//  - i : indices of the points
//  - d : distance to the segment (0 if within) (signed)
//  - s : distance to the TSS (signed)
// [[Rcpp::export]]
List closestNPointsToSegments(NumericVector s,NumericVector e,NumericVector p,IntegerVector tss,int N) {
  IntegerMatrix im(s.size(),N);
  std::fill( im.begin(), im.end(), IntegerVector::get_na() ) ;
  NumericMatrix dm(s.size(),N);
  std::fill( dm.begin(), dm.end(), NumericVector::get_na() ) ;
  NumericMatrix sm(s.size(),N);
  std::fill( sm.begin(), sm.end(), NumericVector::get_na() ) ;
  
  
  
  int sp=0; int ep= p.size()>N ? N-1 : p.size()-1; // start/end point indices
  
  for(int i=0;i<s.size();i++) {
    // if the p[ep+1] is closer to the fragment then p[ep], then advance the N window;
    while(ep<(p.size()-1) && fabs(psegDist(p[ep+1],s[i],e[i])) < fabs(psegDist(p[sp],s[i],e[i]))) {
      //cout<<"advancing for i="<<i<<", sp="<<sp<<", ep="<<ep<<endl;
      sp++; ep++;
    }

    // (rare rollback) if the p[sp-1] is closer to the fragment then p[ep], then roll back the N window
    while(sp>0 && fabs(psegDist(p[sp-1],s[i],e[i])) < fabs(psegDist(p[ep],s[i],e[i]))) {
      //cout<<"rolling back for i="<<i<<", sp="<<sp<<", ep="<<ep<<endl;
      sp--; ep--;
    }

    // record
    int trueN=min(N,ep-sp+1);
    NumericVector di(trueN);
    IntegerVector ii(trueN);
    NumericVector si(trueN);
    for(int j=0;j<trueN;j++) {
      int k=sp+j;
      ii[j]=k;
      di[j]=psegDist(p[k],s[i],e[i]);
      if(tss[i]){ 
	si[j]=p[k]-s[i];
      } else {
	si[j]=p[k]-e[i];
      }
    }
    IntegerVector m=order(di);
    for(int j=0;j<m.size();j++) {
      im(i,j)=ii[m[j]];
      dm(i,j)=di[m[j]];
      sm(i,j)=si[m[j]];
    }
    
  }
  return List::create(Named("i")=im,Named("d")=dm,Named("s")=sm);
}


// returns closest N genes to each point
// s, e - start/end positions of the genes
// p - point positions
// return elements are matrices (length(s) * N):
//  - i : indices of the genes
//  - d : distance to the segment (0 if within) (signed)
// [[Rcpp::export]]
List closestNSegmentsToPoints(NumericVector s,NumericVector e,NumericVector p,IntegerVector tss,int N) {
  IntegerMatrix im(p.size(),N);
  std::fill( im.begin(), im.end(), IntegerVector::get_na() ) ;
  NumericMatrix dm(p.size(),N);
  std::fill( dm.begin(), dm.end(), NumericVector::get_na() ) ;
  NumericMatrix sm(p.size(),N);
  std::fill( sm.begin(), sm.end(), NumericVector::get_na() ) ;

  int ss=0; int es= s.size()>N ? N-1 : s.size()-1; // start/end segment indices
  for(int i=0;i<p.size();i++) {
    //cout<<"."<<flush;
    // if the segment(es+1) is closer to the point then segment(es), then advance the N window
    while(es<(s.size()-1) && fabs(psegDist(p[i],s[es+1],e[es+1])) < fabs(psegDist(p[i],s[ss],e[ss]))) {
      //cout<<"advancing for i="<<i<<", ss="<<ss<<", es="<<es<<endl;
      ss++; es=min(ss+N,(int)s.size()-1);
      while(es<(s.size()-1) && e[ss]>=s[es]) {
	//cout<<"extending for i="<<i<<", ss="<<ss<<", es="<<es<<endl;
	es++;
      }
    }

    // (rare rollback) if the p[ss-1] is closer to the fragment then p[es], then roll back the N window
    while(ss>0 && fabs(psegDist(p[i],s[ss-1],e[ss-1])) < fabs(psegDist(p[i],s[es],e[es]))) {
      //cout<<"rolling back for i="<<i<<", ss="<<ss<<", es="<<es<<endl;
      ss--; es=ss+N;
    }
    
    // record
    int trueN = min(N,es-ss+1);
    NumericVector di(trueN);
    IntegerVector ii(trueN);
    NumericVector si(trueN);
    for(int j=0;j<trueN;j++) {
      int k=ss+j;
      ii[j]=k;
      di[j]=psegDist(p[i],s[k],e[k]);
      if(tss[k]){ 
	si[j]=p[i]-s[k];
      } else {
	si[j]=p[i]-e[k];
      }
    }

    // sort the records according to dm
    IntegerVector m=order(di);
    for(int j=0;j<m.size();j++) {
      im(i,j)=ii[m[j]];
      dm(i,j)=di[m[j]];
      sm(i,j)=si[m[j]];

    }
  }
  return List::create(Named("i")=im,Named("d")=dm,Named("s")=sm);
}
