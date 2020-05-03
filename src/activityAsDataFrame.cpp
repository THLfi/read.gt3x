#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

const int TIME_UNIT = 100; // same as in parseGT3X, times are presented as 100th of seconds.

// [[Rcpp::export]]
DataFrame activityAsDataFrame(NumericMatrix &m, IntegerVector &time_index, double start_time) {

  NumericVector timestamp = NumericVector(m.nrow());
  for(int i = 0; i < m.nrow(); i++)
    timestamp(i) = start_time + time_index(i)/(double)TIME_UNIT;

  DataFrame df = DataFrame::create(Named("X", m(_, 0)),
                                   Named("Y", m(_, 1)),
                                   Named("Z", m(_, 2)),
                                   Named("time", timestamp));

  return df;
}
