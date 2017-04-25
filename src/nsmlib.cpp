// [[Rcpp::plugins(cpp10)]]

#include "pagoda2.h"

/*
#include <iostream>
#include <chrono>
#include <boost/progress.hpp>
#include <vector>
#include "space.h"
#include "space/space_scalar.h"
#include "space/space_js.h"
#include "init.h"
#include "index.h"
#include "params.h"
#include "rangequery.h"
#include "knnquery.h"
#include "knnqueue.h"
#include "methodfactory.h"
#include "spacefactory.h"
#include "ztimer.h"
*/




#include <omp.h>
// [[Rcpp::plugins(openmp)]]



// [[Rcpp::export]]
vector<vector<int32_t> > hnswKnn(NumericMatrix m, int efConstruction=20, int indexThreadQty=4, int searchMethod=4) {
  //const Map<MatrixXd> M(as<Map<MatrixXd> >(MM));
  //const char spaceName="cosinesimil";
  initLibrary(LIB_LOGNONE, NULL);
  //initLibrary(LIB_LOGSTDERR, NULL);
  AnyParams empty;
//Space<float>    *space = SpaceFactoryRegistry<float>::Instance().CreateSpace("cosinesimil", empty);
  SpaceCosineSimilarity<float> space;
  // each row of a matrix will be an entry
  cout << "reading points ... " << flush;
  vector<vector<float> > data;
  ObjectVector    dataSet; 
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    std::vector<float> v=Rcpp::as<std::vector<float> >(nv);
    //data.push_back(v);
    //Object *o = space.CreateObjFromVect(i, NULL, v);
    Object *o = space.CreateObjFromVect(i, NULL, v);
    dataSet.push_back(o);
  }
  cout << "done ("<<dataSet.size()<< " points)" << endl;  
  cout << "converting ... " << flush;

//  vector<LabelType> labels(data.size()); 
//space->CreateDataset(dataSet,data,labels);
  cout << " done" << endl;  

  AnyParams IndexParams({
            "M=20",
            "efConstruction=100",
            "indexThreadQty=30", /* number of indexing threads */
	     "searchMethod=4",
	    });  
  AnyParams QueryTimeParams({
            "efSearch=100",
            });


  cout << "building the index ... " << flush;
  auto t1 = high_resolution_clock::now();
  //auto table = construct_table<Point>(dataset, params);
  Index<float> *index=MethodFactoryRegistry<float>::Instance().CreateMethod(true,"hnsw", "consinesimil",space, dataSet);
  index->CreateIndex(IndexParams);
  index->SetQueryTimeParams(QueryTimeParams);

  auto t2 = high_resolution_clock::now();
  auto elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  cout << endl<<"done (" << elapsed_time <<"s)"<< endl;
  
  int nqueries=m.nrow();
  vector<vector<int32_t> > answers(nqueries);
  int k=20;
  cout << "running queries with k="<<k<<" ... " << flush;
  t1 = high_resolution_clock::now();

  omp_set_num_threads(20);
  #pragma omp parallel for shared(index,answers) schedule(auto)
  for(int i=0;i <nqueries;i++) {
    const Object* queryObj=dataSet[i];
    KNNQuery<float>   knnQ(space, queryObj, k);
    index->Search(&knnQ);
    
    KNNQueue<float>* res = knnQ.Result()->Clone();
    vector<int32_t> candidates;
    while (!res->Empty()) {
      candidates.push_back(res->TopObject()->id());
      //cout << res->TopObject()->id() << " : " << res->TopDistance() << endl;
      res->Pop();
    }

    //table.get_candidates_with_duplicates(dataset[i],&candidates);
    //Point p=dataset[i];
    //table->get_candidates_with_duplicates(p, &candidates);
    //table->find_k_nearest_neighbors(p,20,&candidates);
    answers[i]=candidates;
  }
  t2 = high_resolution_clock::now();
  elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  cout << "done (" << elapsed_time << "s)"<< endl;
  
  return(answers);
}



// [[Rcpp::export]]
  DataFrame hnswKnn2(NumericMatrix m, int k=5, int nThreads=30, int efConstruction=20, int indexThreadQty=4, int searchMethod=4, int seed=-1,bool verbose=true) {
  //const Map<MatrixXd> M(as<Map<MatrixXd> >(MM));
  //const char spaceName="cosinesimil";
  initLibrary(LIB_LOGNONE, NULL);
  //initLibrary(LIB_LOGSTDERR, NULL);
  AnyParams empty;
  //Space<float>    *space = SpaceFactoryRegistry<float>::Instance().CreateSpace("cosinesimil", empty);
  SpaceCosineSimilarity<float> space;
  //SpaceLp<float> space(2);
  //SpaceJSMetric<float> space(SpaceJSBase<float>::kJSFastPrecompApprox);
  //SpaceJSDiv<float> space(SpaceJSBase<float>::kJSFastPrecomp);
  //SpaceAngularDistance<float> space;
  // each row of a matrix will be an entry
  if(seed!=-1) srand(seed);
  if(verbose) cout << "reading points ... " << flush;
  vector<vector<float> > data;
  ObjectVector    dataSet; 
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    std::vector<float> v=Rcpp::as<std::vector<float> >(nv);
    //data.push_back(v);
    Object *o = space.CreateObjFromVect(i, NULL, v);
    dataSet.push_back(o);
  }
  if(verbose) cout << "done ("<<dataSet.size()<< " points)" << endl;  
  if(verbose) cout << "converting ... " << flush;

//  vector<LabelType> labels(data.size()); 
//space->CreateDataset(dataSet,data,labels);
  if(verbose)   cout << " done" << endl;  

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
  //auto table = construct_table<Point>(dataset, params);
  Index<float> *index=MethodFactoryRegistry<float>::Instance().CreateMethod(verbose,"hnsw", "consinesimil",space, dataSet);
  index->CreateIndex(IndexParams);
  index->SetQueryTimeParams(QueryTimeParams);

  auto t2 = high_resolution_clock::now();
  auto elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  if(verbose) if(verbose) cout << endl<<"done (" << elapsed_time <<"s)"<< endl;
  
  int nqueries=m.nrow();
  vector<vector<int32_t> > answers(nqueries);

  if(verbose) cout << "running queries with k="<<k<<" ... " << flush;
  t1 = high_resolution_clock::now();
  int nanswers=nqueries*k;
  vector<vector<int32_t> > ids(nqueries);
  vector<vector<float> > dists(nqueries);

  omp_set_num_threads(nThreads);
  unique_ptr<boost::progress_display> query_bar(verbose ? new boost::progress_display(nqueries) : NULL);
  //boost::progress_display query_bar(nqueries);
  #pragma omp parallel for shared(index,ids,dists) schedule(auto)
  for(int i=0;i <nqueries;i++) {
    const Object* queryObj=dataSet[i];
    KNNQuery<float>   knnQ(space, queryObj, k);
    index->Search(&knnQ);
    
    KNNQueue<float>* res = knnQ.Result()->Clone();
    vector<int32_t> candidates;
    vector<float> candidateDists;
    while (!res->Empty()) {
      candidates.push_back(res->TopObject()->id());
      candidateDists.push_back(res->TopDistance());
      //candidateDists.push_back(space->IndexTimeDistance(dataSet[i],dataSet[res->TopObject()->id()]))
      //cout << res->TopObject()->id() << " : " << res->TopDistance() << endl;

      res->Pop();
    }
    
    //table.get_candidates_with_duplicates(dataset[i],&candidates);
    //Point p=dataset[i];
    //table->get_candidates_with_duplicates(p, &candidates);
    //table->find_k_nearest_neighbors(p,20,&candidates);
    ids[i]=candidates;
    dists[i]=candidateDists;
    if(query_bar) { ++(*query_bar); }
  }
  t2 = high_resolution_clock::now();
  elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  if(verbose) cout << endl << "done (" << elapsed_time << "s)"<< endl;

  // recode into an edge data frame ($s $e $d)
  if(verbose) cout << "creating report data frame ... " << flush;
  vector<int> startV,endV; vector<float> distV;
  for(auto i=0;i<nqueries;i++) {
    auto k=dists[i].begin();
    for(auto j=ids[i].begin();j!=ids[i].end();++j, ++k) { 
      startV.push_back(i); 
      endV.push_back(*j); 
      distV.push_back(*k);
    }
  }
  if(verbose) cout<<"done"<<endl;
  return DataFrame::create(_["s"] = startV,
                           _["e"] = endV,
                           _["d"] = distV);
}

// [[Rcpp::export]]
DataFrame hnswKnnJS(NumericMatrix m, int k=5, int nThreads=20, int efConstruction=20, int indexThreadQty=4, int searchMethod=4, int seed=-1) {
  //const Map<MatrixXd> M(as<Map<MatrixXd> >(MM));
  //const char spaceName="cosinesimil";
  initLibrary(LIB_LOGNONE, NULL);
  //initLibrary(LIB_LOGSTDERR, NULL);
  AnyParams empty;
  //Space<float>    *space = SpaceFactoryRegistry<float>::Instance().CreateSpace("cosinesimil", empty);
  //SpaceCosineSimilarity<float> space;
  //SpaceLp<float> space(2);
  SpaceJSMetric<float> space(SpaceJSBase<float>::kJSFastPrecompApprox);
  //SpaceJSDiv<float> space(SpaceJSBase<float>::kJSFastPrecomp);
  //SpaceAngularDistance<float> space;
  
  // each row of a matrix will be an entry
  if(seed!=-1) srand(seed);
  cout << "reading points ... " << flush;
  vector<vector<float> > data;
  ObjectVector    dataSet; 
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    std::vector<float> v=Rcpp::as<std::vector<float> >(nv);
    //data.push_back(v);
    Object *o = space.CreateObjFromVect(i, NULL, v);
    dataSet.push_back(o);
  }
  cout << "done ("<<dataSet.size()<< " points)" << endl;  
  cout << "converting ... " << flush;

//  vector<LabelType> labels(data.size()); 
//space->CreateDataset(dataSet,data,labels);
  cout << " done" << endl;  

  AnyParams IndexParams({
            "M=20",
            "efConstruction=100",
	      "indexThreadQty="+std::to_string(nThreads), /* indexing threads */
            "searchMethod=4",
	    });  
  AnyParams QueryTimeParams({
            "efSearch=100",
            });


  cout << "building the index ... " << flush;
  auto t1 = high_resolution_clock::now();
  //auto table = construct_table<Point>(dataset, params);
  Index<float> *index=MethodFactoryRegistry<float>::Instance().CreateMethod(true,"hnsw", "jsmetrfastapprox",space, dataSet);
  //Index<float> *index=MethodFactoryRegistry<float>::Instance().CreateMethod(true,"hnsw", "jsdivfast",space, dataSet);
  index->CreateIndex(IndexParams);
  index->SetQueryTimeParams(QueryTimeParams);

  auto t2 = high_resolution_clock::now();
  auto elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  cout << endl<<"done (" << elapsed_time <<"s)"<< endl;
  
  int nqueries=m.nrow();
  vector<vector<int32_t> > answers(nqueries);

  cout << "running queries with k="<<k<<" ... " << flush;
  t1 = high_resolution_clock::now();
  int nanswers=nqueries*k;
  vector<vector<int32_t> > ids(nqueries);
  vector<vector<float> > dists(nqueries);

  omp_set_num_threads(nThreads);
  boost::progress_display query_bar(nqueries);
  #pragma omp parallel for shared(index,ids,dists) schedule(auto)
  for(int i=0;i <nqueries;i++) {
    const Object* queryObj=dataSet[i];
    KNNQuery<float>   knnQ(space, queryObj, k);
    index->Search(&knnQ);
    
    KNNQueue<float>* res = knnQ.Result()->Clone();
    vector<int32_t> candidates;
    vector<float> candidateDists;
    while (!res->Empty()) {
      candidates.push_back(res->TopObject()->id());
      candidateDists.push_back(res->TopDistance());
      //candidateDists.push_back(space->IndexTimeDistance(dataSet[i],dataSet[res->TopObject()->id()]))
      //cout << res->TopObject()->id() << " : " << res->TopDistance() << endl;

      res->Pop();
    }
    
    //table.get_candidates_with_duplicates(dataset[i],&candidates);
    //Point p=dataset[i];
    //table->get_candidates_with_duplicates(p, &candidates);
    //table->find_k_nearest_neighbors(p,20,&candidates);
    ids[i]=candidates;
    dists[i]=candidateDists;
    ++query_bar;
  }
  t2 = high_resolution_clock::now();
  elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  cout << endl << "done (" << elapsed_time << "s)"<< endl;

  // recode into an edge data frame ($s $e $d)
  cout << "creating report data frame ... " << flush;
  vector<int> startV,endV; vector<float> distV;
  for(auto i=0;i<nqueries;i++) {
    auto k=dists[i].begin();
    for(auto j=ids[i].begin();j!=ids[i].end();++j, ++k) { 
      startV.push_back(i); 
      endV.push_back(*j); 
      distV.push_back(*k);
    }
  }
  cout<<"done"<<endl;
  return DataFrame::create(_["s"] = startV,
                           _["e"] = endV,
                           _["d"] = distV);
}

// [[Rcpp::export]]
 DataFrame hnswKnnLp(NumericMatrix m, int k=5, int nThreads=30, float p=2.0, int efConstruction=20, int indexThreadQty=4, int searchMethod=4, int seed=-1,bool verbose=true) {
  //const Map<MatrixXd> M(as<Map<MatrixXd> >(MM));
  //const char spaceName="cosinesimil";
  initLibrary(LIB_LOGNONE, NULL);
  //initLibrary(LIB_LOGSTDERR, NULL);
  AnyParams empty;
  //Space<float>    *space = SpaceFactoryRegistry<float>::Instance().CreateSpace("cosinesimil", empty);
  SpaceLp<float> space(p);
  //SpaceJSMetric<float> space(SpaceJSBase<float>::kJSFastPrecompApprox);
  //SpaceJSDiv<float> space(SpaceJSBase<float>::kJSFastPrecomp);
  //SpaceAngularDistance<float> space;
  // each row of a matrix will be an entry
  if(seed!=-1) srand(seed);
  if(verbose) cout << "reading points ... " << flush;
  vector<vector<float> > data;
  ObjectVector    dataSet; 
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    std::vector<float> v=Rcpp::as<std::vector<float> >(nv);
    //data.push_back(v);
    Object *o = space.CreateObjFromVect(i, NULL, v);
    dataSet.push_back(o);
  }
  if(verbose) cout << "done ("<<dataSet.size()<< " points)" << endl;  
  if(verbose) cout << "converting ... " << flush;

//  vector<LabelType> labels(data.size()); 
//space->CreateDataset(dataSet,data,labels);
  if(verbose)   cout << " done" << endl;  

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
  //auto table = construct_table<Point>(dataset, params);
  Index<float> *index=MethodFactoryRegistry<float>::Instance().CreateMethod(verbose,"hnsw", "L1",space, dataSet);
  index->CreateIndex(IndexParams);
  index->SetQueryTimeParams(QueryTimeParams);

  auto t2 = high_resolution_clock::now();
  auto elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  if(verbose) if(verbose) cout << endl<<"done (" << elapsed_time <<"s)"<< endl;
  
  int nqueries=m.nrow();
  vector<vector<int32_t> > answers(nqueries);

  if(verbose) cout << "running queries with k="<<k<<" ... " << flush;
  t1 = high_resolution_clock::now();
  int nanswers=nqueries*k;
  vector<vector<int32_t> > ids(nqueries);
  vector<vector<float> > dists(nqueries);

  omp_set_num_threads(nThreads);
  unique_ptr<boost::progress_display> query_bar(verbose ? new boost::progress_display(nqueries) : NULL);
  //boost::progress_display query_bar(nqueries);
  #pragma omp parallel for shared(index,ids,dists) schedule(auto)
  for(int i=0;i <nqueries;i++) {
    const Object* queryObj=dataSet[i];
    KNNQuery<float>   knnQ(space, queryObj, k);
    index->Search(&knnQ);
    
    KNNQueue<float>* res = knnQ.Result()->Clone();
    vector<int32_t> candidates;
    vector<float> candidateDists;
    while (!res->Empty()) {
      candidates.push_back(res->TopObject()->id());
      candidateDists.push_back(res->TopDistance());
      //candidateDists.push_back(space->IndexTimeDistance(dataSet[i],dataSet[res->TopObject()->id()]))
      //cout << res->TopObject()->id() << " : " << res->TopDistance() << endl;

      res->Pop();
    }
    
    //table.get_candidates_with_duplicates(dataset[i],&candidates);
    //Point p=dataset[i];
    //table->get_candidates_with_duplicates(p, &candidates);
    //table->find_k_nearest_neighbors(p,20,&candidates);
    ids[i]=candidates;
    dists[i]=candidateDists;
    if(query_bar) { ++(*query_bar); }
  }
  t2 = high_resolution_clock::now();
  elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  if(verbose) cout << endl << "done (" << elapsed_time << "s)"<< endl;

  // recode into an edge data frame ($s $e $d)
  if(verbose) cout << "creating report data frame ... " << flush;
  vector<int> startV,endV; vector<float> distV;
  for(auto i=0;i<nqueries;i++) {
    auto k=dists[i].begin();
    for(auto j=ids[i].begin();j!=ids[i].end();++j, ++k) { 
      startV.push_back(i); 
      endV.push_back(*j); 
      distV.push_back(*k);
    }
  }
  if(verbose) cout<<"done"<<endl;
  return DataFrame::create(_["s"] = startV,
                           _["e"] = endV,
                           _["d"] = distV);
}


// [[Rcpp::export]]
DataFrame hnswKnn3test(NumericMatrix m, int k=5, int multiplex=1, int nqueries=1000, int nThreads=30, int efConstruction=20, int indexThreadQty=4, int searchMethod=4, int seed=-1,bool verbose=true) {
  //const Map<MatrixXd> M(as<Map<MatrixXd> >(MM));
  //const char spaceName="cosinesimil";
  initLibrary(LIB_LOGNONE, NULL);
  //initLibrary(LIB_LOGSTDERR, NULL);
  AnyParams empty;
  //Space<float>    *space = SpaceFactoryRegistry<float>::Instance().CreateSpace("cosinesimil", empty);
  SpaceCosineSimilarity<float> space;
  //SpaceLp<float> space(2);
  //SpaceJSMetric<float> space(SpaceJSBase<float>::kJSFastPrecompApprox);
  //SpaceJSDiv<float> space(SpaceJSBase<float>::kJSFastPrecomp);
  //SpaceAngularDistance<float> space;
  // each row of a matrix will be an entry
  if(seed!=-1) srand(seed);
  if(verbose) cout << "reading points ... " << flush;
  vector<vector<float> > data;
  ObjectVector    dataSet; 
  for(int i=0;i<m.nrow();i++) {
    NumericVector nv = m.row(i);
    for(int j=0;j<multiplex;j++) {
      NumericVector nv2=nv; nv2=nv2+ R::rnorm(0,0.01);
      std::vector<float> v=Rcpp::as<std::vector<float> >(nv2);
      //data.push_back(v);
      Object *o = space.CreateObjFromVect(i, NULL, v);
      dataSet.push_back(o);
    }
  }
  if(verbose) cout << "done ("<<dataSet.size()<< " points)" << endl;  
  if(verbose) cout << "converting ... " << flush;
  
  //  vector<LabelType> labels(data.size()); 
  //space->CreateDataset(dataSet,data,labels);
  if(verbose)   cout << " done" << endl;  
  
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
  //auto table = construct_table<Point>(dataset, params);
  Index<float> *index=MethodFactoryRegistry<float>::Instance().CreateMethod(verbose,"hnsw", "consinesimil",space, dataSet);
  index->CreateIndex(IndexParams);
  index->SetQueryTimeParams(QueryTimeParams);
  
  auto t2 = high_resolution_clock::now();
  auto elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  if(verbose) if(verbose) cout << endl<<"done (" << elapsed_time <<"s)"<< endl;
  
  //int nqueries=m.nrow();
  vector<vector<int32_t> > answers(nqueries);
  
  if(verbose) cout << "running "<<nqueries<<" queries with k="<<k<<" ... " << flush;
  t1 = high_resolution_clock::now();
  int nanswers=nqueries*k;
  vector<vector<int32_t> > ids(nqueries);
  vector<vector<float> > dists(nqueries);
  
  omp_set_num_threads(nThreads);
  unique_ptr<boost::progress_display> query_bar(verbose ? new boost::progress_display(nqueries) : NULL);
  //boost::progress_display query_bar(nqueries);
#pragma omp parallel for shared(index,ids,dists) schedule(auto)
  for(int i=0;i <nqueries;i++) {
    const Object* queryObj=dataSet[i];
    KNNQuery<float>   knnQ(space, queryObj, k);
    index->Search(&knnQ);
    
    KNNQueue<float>* res = knnQ.Result()->Clone();
    vector<int32_t> candidates;
    vector<float> candidateDists;
    while (!res->Empty()) {
      candidates.push_back(res->TopObject()->id());
      candidateDists.push_back(res->TopDistance());
      //candidateDists.push_back(space->IndexTimeDistance(dataSet[i],dataSet[res->TopObject()->id()]))
      //cout << res->TopObject()->id() << " : " << res->TopDistance() << endl;
      
      res->Pop();
    }
    
    //table.get_candidates_with_duplicates(dataset[i],&candidates);
    //Point p=dataset[i];
    //table->get_candidates_with_duplicates(p, &candidates);
    //table->find_k_nearest_neighbors(p,20,&candidates);
    ids[i]=candidates;
    dists[i]=candidateDists;
    if(query_bar) { ++(*query_bar); }
  }
  t2 = high_resolution_clock::now();
  elapsed_time = duration_cast<duration<double>>(t2 - t1).count();
  if(verbose) cout << endl << "done (" << elapsed_time << "s)"<< endl;
  
  // recode into an edge data frame ($s $e $d)
  if(verbose) cout << "creating report data frame ... " << flush;
  vector<int> startV,endV; vector<float> distV;
  for(auto i=0;i<nqueries;i++) {
    auto k=dists[i].begin();
    for(auto j=ids[i].begin();j!=ids[i].end();++j, ++k) { 
      startV.push_back(i); 
      endV.push_back(*j); 
      distV.push_back(*k);
    }
  }
  if(verbose) cout<<"done"<<endl;
  return DataFrame::create(_["s"] = startV,
                           _["e"] = endV,
                           _["d"] = distV);
}

