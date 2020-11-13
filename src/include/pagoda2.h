#ifndef __PAGODA2__
#define __PAGODA2__

#include <RcppArmadillo.h>
#include <queue>
#include <algorithm>
#include <iostream>
#include <chrono>
#include <vector>
#include <queue>
#include <list>

#ifdef _OPENMP
  #include <omp.h>
#endif


/*#include "space.h"
#include "space/space_scalar.h"
#include "space/space_js.h"
#include "lshkit.h"

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
using namespace std;
using namespace Rcpp;
//using namespace similarity;

using std::chrono::duration;
using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;

#endif
