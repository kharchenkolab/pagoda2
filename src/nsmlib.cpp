// [[Rcpp::plugins(cpp10)]]

#include "pagoda2.h"


#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]

#define INDEX_TYPE_JS 1
#define INDEX_TYPE_COSINE 2
#define INDEX_TYPE_LP 3

// Function Prototype
DataFrame hnswKnn(NumericMatrix m,int k=5,int nThreads=30,float p=2.0,
    int efConstruction=20,int indexThreadQty=4,  int searchMethod=4,int seed=-1,bool verbose=true,int indexType = 1);

// [[Rcpp::export]]
DataFrame hnswKnn2(NumericMatrix m, int k=5, int nThreads=20,
    int efConstruction=20, int indexThreadQty=4,int searchMethod=4, int seed=-1,bool verbose=true) {
  return hnswKnn(m, k, nThreads, 1.0, efConstruction, indexThreadQty, searchMethod, seed, verbose, INDEX_TYPE_COSINE);
};

// [[Rcpp::export]]
DataFrame hnswKnnJS(NumericMatrix m, int k=5, int nThreads=20,int efConstruction=20,
    int indexThreadQty=4, int searchMethod=4, int seed=-1,bool verbose=true) {
  return hnswKnn(m, k, nThreads, 1.0, efConstruction, indexThreadQty, searchMethod, seed, verbose, INDEX_TYPE_JS);
};

// [[Rcpp::export]]
DataFrame hnswKnnLp(NumericMatrix m, int k = 5, int nThreads = 30, float p = 2.0,
    int efConstruction = 20,int indexThreadQty = 4, int searchMethod = 4, int seed = -1,bool verbose = true) {
  return hnswKnn(m, k, nThreads, p, efConstruction, indexThreadQty, searchMethod, seed, verbose, INDEX_TYPE_LP);
};

DataFrame hnswKnn(NumericMatrix m, int k, int nThreads, float p, int efConstruction,
    int indexThreadQty, int searchMethod, int seed, bool verbose, int indexType) {

  initLibrary(LIB_LOGNONE, NULL);

  // Use base class pointer for space to allow different space types
  VectorSpace<float> *space = NULL;

  if (indexType == INDEX_TYPE_JS) {
    space = new SpaceJSMetric<float> (SpaceJSBase<float>::kJSFastPrecompApprox);
  } else if (indexType == INDEX_TYPE_COSINE) {
    space = new SpaceCosineSimilarity<float>();
  } else if (indexType == INDEX_TYPE_LP) {
    space = new SpaceLp<float>(p);
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
  vector<vector<int32_t> > answers(nqueries);

  if(verbose) cout << "running queries with k="<<k<<" ... " << flush;
  t1 = high_resolution_clock::now();
  int nanswers=nqueries*k;
  vector<vector<int32_t> > ids(nqueries);
  vector<vector<float> > dists(nqueries);

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
    while (!res->Empty()) {
      candidates.push_back(res->TopObject()->id());
      candidateDists.push_back(res->TopDistance());
      res->Pop();
    }

    ids[i]=candidates;
    dists[i]=candidateDists;
    if(query_bar) { ++(*query_bar); }
  }
  t2 = high_resolution_clock::now();
  elapsed_time = duration_cast<duration<double>>(t2 - t1).count();

  if(verbose) cout << endl << "done (" << elapsed_time << "s)"<< endl;

  // recode into an edge data frame ($s $e $d)
  if(verbose) cout << "creating report data frame ... " << flush;

  vector<int> startV,endV;
  vector<float> distV;
  for(auto i=0;i<nqueries;i++) {
    auto k = dists[i].begin();
    for(auto j = ids[i].begin(); j != ids[i].end(); ++j, ++k) {
      startV.push_back(i);
      endV.push_back(*j);
      distV.push_back(*k);
    }
  }

  if(verbose) cout<<"done"<<endl;

  return DataFrame::create(_["s"] = startV,
                           _["e"] = endV,
                           _["d"] = distV);

};



