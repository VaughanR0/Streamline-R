// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "hts2_types.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// cgm_c
MatrixXd cgm_c(SEXP As, SEXP bs);
RcppExport SEXP hts2_cgm_c(SEXP AsSEXP, SEXP bsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type As(AsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type bs(bsSEXP);
    rcpp_result_gen = Rcpp::wrap(cgm_c(As, bs));
    return rcpp_result_gen;
END_RCPP
}
