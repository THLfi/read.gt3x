// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// activityAsDataFrame
DataFrame activityAsDataFrame(NumericMatrix& m, NumericVector& time_index, double start_time, int divider);
RcppExport SEXP _read_gt3x_activityAsDataFrame(SEXP mSEXP, SEXP time_indexSEXP, SEXP start_timeSEXP, SEXP dividerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type time_index(time_indexSEXP);
    Rcpp::traits::input_parameter< double >::type start_time(start_timeSEXP);
    Rcpp::traits::input_parameter< int >::type divider(dividerSEXP);
    rcpp_result_gen = Rcpp::wrap(activityAsDataFrame(m, time_index, start_time, divider));
    return rcpp_result_gen;
END_RCPP
}
// parseGT3X
NumericMatrix parseGT3X(const char* filename, const int max_samples, const double scale_factor, const int sample_rate, const uint32_t start_time, const uint32_t batch_begin, const uint32_t batch_end, const bool verbose, const bool debug, const bool impute_zeroes);
RcppExport SEXP _read_gt3x_parseGT3X(SEXP filenameSEXP, SEXP max_samplesSEXP, SEXP scale_factorSEXP, SEXP sample_rateSEXP, SEXP start_timeSEXP, SEXP batch_beginSEXP, SEXP batch_endSEXP, SEXP verboseSEXP, SEXP debugSEXP, SEXP impute_zeroesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char* >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const int >::type max_samples(max_samplesSEXP);
    Rcpp::traits::input_parameter< const double >::type scale_factor(scale_factorSEXP);
    Rcpp::traits::input_parameter< const int >::type sample_rate(sample_rateSEXP);
    Rcpp::traits::input_parameter< const uint32_t >::type start_time(start_timeSEXP);
    Rcpp::traits::input_parameter< const uint32_t >::type batch_begin(batch_beginSEXP);
    Rcpp::traits::input_parameter< const uint32_t >::type batch_end(batch_endSEXP);
    Rcpp::traits::input_parameter< const bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< const bool >::type debug(debugSEXP);
    Rcpp::traits::input_parameter< const bool >::type impute_zeroes(impute_zeroesSEXP);
    rcpp_result_gen = Rcpp::wrap(parseGT3X(filename, max_samples, scale_factor, sample_rate, start_time, batch_begin, batch_end, verbose, debug, impute_zeroes));
    return rcpp_result_gen;
END_RCPP
}
// parseActivityBin
NumericMatrix parseActivityBin(const char* filename, const int max_samples, const double scale_factor, const int sample_rate, const bool verbose, const bool debug);
RcppExport SEXP _read_gt3x_parseActivityBin(SEXP filenameSEXP, SEXP max_samplesSEXP, SEXP scale_factorSEXP, SEXP sample_rateSEXP, SEXP verboseSEXP, SEXP debugSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char* >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const int >::type max_samples(max_samplesSEXP);
    Rcpp::traits::input_parameter< const double >::type scale_factor(scale_factorSEXP);
    Rcpp::traits::input_parameter< const int >::type sample_rate(sample_rateSEXP);
    Rcpp::traits::input_parameter< const bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< const bool >::type debug(debugSEXP);
    rcpp_result_gen = Rcpp::wrap(parseActivityBin(filename, max_samples, scale_factor, sample_rate, verbose, debug));
    return rcpp_result_gen;
END_RCPP
}
// parseLuxBin
NumericVector parseLuxBin(const char* filename, const int max_samples, const double scale_factor, const double max_value, const bool verbose);
RcppExport SEXP _read_gt3x_parseLuxBin(SEXP filenameSEXP, SEXP max_samplesSEXP, SEXP scale_factorSEXP, SEXP max_valueSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char* >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const int >::type max_samples(max_samplesSEXP);
    Rcpp::traits::input_parameter< const double >::type scale_factor(scale_factorSEXP);
    Rcpp::traits::input_parameter< const double >::type max_value(max_valueSEXP);
    Rcpp::traits::input_parameter< const bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(parseLuxBin(filename, max_samples, scale_factor, max_value, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_read_gt3x_activityAsDataFrame", (DL_FUNC) &_read_gt3x_activityAsDataFrame, 4},
    {"_read_gt3x_parseGT3X", (DL_FUNC) &_read_gt3x_parseGT3X, 10},
    {"_read_gt3x_parseActivityBin", (DL_FUNC) &_read_gt3x_parseActivityBin, 6},
    {"_read_gt3x_parseLuxBin", (DL_FUNC) &_read_gt3x_parseLuxBin, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_read_gt3x(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
