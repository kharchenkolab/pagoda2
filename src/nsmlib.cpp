// [[Rcpp::plugins(cpp10)]]

#include "pagoda2.h"

#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]

#define INDEX_TYPE_JS 1
#define INDEX_TYPE_COSINE 2
#define INDEX_TYPE_LP 3

// [[Rcpp::export]]
arma::sp_mat hnswKnn(NumericMatrix m, int k=5, int nThreads=30, float p=2.0, int efConstruction=20,
    int indexThreadQty=4, int searchMethod=4, int seed=-1, bool verbose=true, int indexType=1) {

  initLibrary(LIB_LOGNONE, NULL);

  // Use base class pointer for space to allow different space types
  VectorSpace<float> *space = NULL;
  VectorSpace<float> *space2 = NULL;

  if (indexType == INDEX_TYPE_JS) {
    space = new SpaceJSMetric<float> (SpaceJSBase<float>::kJSFastPrecompApprox);
    space2 = new SpaceJSMetric<float> (SpaceJSBase<float>::kJSFastPrecompApprox); // used for real distance calculations
  } else if (indexType == INDEX_TYPE_COSINE) {
    space = new SpaceCosineSimilarity<float>();
    space2 = new SpaceCosineSimilarity<float>();
  } else if (indexType == INDEX_TYPE_LP) {
    space = new SpaceLp<float>(p);
    space2 = new SpaceLp<float>(p);
  } else {
     cout << "Invalid index type specified" << endl << flush;
  }

  // each row of a matrix will be an entry
  if(seed!=-1) srand(seed);
  if(verbose) cout << "reading points ... " << flush;
  vector<vector<float> > data;
  ObjectVector    dataSet;
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    std::vector<float> v=Rcpp::as<std::vector<float> >(nv);
    Object *o = space->CreateObjFromVect(i, -1, v);
    dataSet.push_back(o);
  }
  if(verbose) cout << "done ("<<dataSet.size()<< " points)" << endl;

  AnyParams IndexParams({
    "M=20",
    "efConstruction=100",
    "indexThreadQty="+std::to_string(nThreads), /* indexing threads */
    "searchMethod=4",
  });
  AnyParams QueryTimeParams({
    "efSearch=100",
  });

  if(verbose) cout << "building the index ... " << flush;
  auto t1 = high_resolution_clock::now();

  Index<float> *index;
  if (indexType == INDEX_TYPE_JS) {
    index = MethodFactoryRegistry<float>::Instance().CreateMethod(true,"hnsw", "jsmetrfastapprox",*space, dataSet);
  } else if (indexType == INDEX_TYPE_COSINE) {
    index = MethodFactoryRegistry<float>::Instance().CreateMethod(verbose,"hnsw", "consinesimil",*space, dataSet);
  } else if (indexType == INDEX_TYPE_LP) {
    index = MethodFactoryRegistry<float>::Instance().CreateMethod(verbose,"hnsw", "L1",*space, dataSet);
  }

  index->CreateIndex(IndexParams);
  index->SetQueryTimeParams(QueryTimeParams);

  auto t2 = high_resolution_clock::now();
  auto elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  if(verbose) cout << endl<<"done (" << elapsed_time <<"s)"<< endl;

  int nqueries=m.nrow();

  if(verbose) cout << "running queries with k="<<k<<" ... " << flush;
  t1 = high_resolution_clock::now();
  int nanswers=nqueries*k;
  arma::vec ansdist(nanswers);
  arma::umat ansloc(2,nanswers,arma::fill::zeros);

  #ifdef _OPENMP
  omp_set_num_threads(nThreads);
  #endif
  
  unique_ptr<boost::progress_display> query_bar(verbose ? new boost::progress_display(nqueries) : NULL);

  #pragma omp parallel for
  for(int i=0;i <nqueries;i++) {
    const Object* queryObj=dataSet[i];
    KNNQuery<float> knnQ(*space, queryObj, k);
    index->Search(&knnQ);

    KNNQueue<float>* res = knnQ.Result()->Clone();
    vector<int32_t> candidates;
    vector<float> candidateDists;
    int j=0;
    while (j<k && !res->Empty()) {
      int l=i*k+j;
      ansloc(0,l)=res->TopObject()->id(); //row
      ansloc(1,l)=i; // column 
      //ansdist(l)= res->TopDistance();
      ansdist(l)= space2->IndexTimeDistance(dataSet[ansloc(0,l)],dataSet[i]);
      res->Pop();
      j++;
    }
    
    if(query_bar) { ++(*query_bar); }
  }
  t2 = high_resolution_clock::now();
  elapsed_time = duration_cast<duration<double>>(t2 - t1).count();

  if(verbose) cout << endl << "done (" << elapsed_time << "s)"<< endl;

  arma::sp_mat r(ansloc,ansdist,m.nrow(),m.nrow(),true);
  return(r);
};



// [[Rcpp::export]]
arma::sp_mat hnswKnn2(NumericMatrix m, int k=5, int nThreads=20,
    int efConstruction=20, int indexThreadQty=4,int searchMethod=4, int seed=-1,bool verbose=true) {
  return hnswKnn(m, k, nThreads, 1.0, efConstruction, indexThreadQty, searchMethod, seed, verbose, INDEX_TYPE_COSINE);
};

// [[Rcpp::export]]
arma::sp_mat hnswKnnJS(NumericMatrix m, int k=5, int nThreads=20,int efConstruction=20,
    int indexThreadQty=4, int searchMethod=4, int seed=-1,bool verbose=true) {
  return hnswKnn(m, k, nThreads, 1.0, efConstruction, indexThreadQty, searchMethod, seed, verbose, INDEX_TYPE_JS);
};

// [[Rcpp::export]]
arma::sp_mat hnswKnnLp(NumericMatrix m, int k = 5, int nThreads = 30, float p = 2.0,
    int efConstruction = 20,int indexThreadQty = 4, int searchMethod = 4, int seed = -1,bool verbose = true) {
  return hnswKnn(m, k, nThreads, p, efConstruction, indexThreadQty, searchMethod, seed, verbose, INDEX_TYPE_LP);
};

