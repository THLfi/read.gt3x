#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
DataFrame activityAsDataFrame(NumericMatrix &m, NumericVector &time_index, double start_time, int divider) {

  NumericVector timestamp = NumericVector(m.nrow());
  for(int i = 0; i < m.nrow(); i++)
    timestamp(i) = start_time + time_index(i)/(double)divider;

  DataFrame df = DataFrame::create(
    Named("time", timestamp),
    Named("X", m(_, 0)),
    Named("Y", m(_, 1)),
    Named("Z", m(_, 2)));

  return df;
}
