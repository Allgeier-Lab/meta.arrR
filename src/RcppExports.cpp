// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_get_table
Rcpp::IntegerVector rcpp_get_table(Rcpp::NumericVector x, int n);
RcppExport SEXP _meta_arrR_rcpp_get_table(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_table(x, n));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_list_to_matrix
Rcpp::NumericMatrix rcpp_list_to_matrix(Rcpp::List x, int n, int pop_n);
RcppExport SEXP _meta_arrR_rcpp_list_to_matrix(SEXP xSEXP, SEXP nSEXP, SEXP pop_nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type pop_n(pop_nSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_list_to_matrix(x, n, pop_n));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_matrix_to_list
Rcpp::List rcpp_matrix_to_list(Rcpp::NumericMatrix x, int n);
RcppExport SEXP _meta_arrR_rcpp_matrix_to_list(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_matrix_to_list(x, n));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_move_meta
Rcpp::List rcpp_move_meta(Rcpp::List fishpop_values, int n, int pop_n, double prob_move, Rcpp::NumericVector extent);
RcppExport SEXP _meta_arrR_rcpp_move_meta(SEXP fishpop_valuesSEXP, SEXP nSEXP, SEXP pop_nSEXP, SEXP prob_moveSEXP, SEXP extentSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type fishpop_values(fishpop_valuesSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type pop_n(pop_nSEXP);
    Rcpp::traits::input_parameter< double >::type prob_move(prob_moveSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_move_meta(fishpop_values, n, pop_n, prob_move, extent));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_meta_arrR_rcpp_get_table", (DL_FUNC) &_meta_arrR_rcpp_get_table, 2},
    {"_meta_arrR_rcpp_list_to_matrix", (DL_FUNC) &_meta_arrR_rcpp_list_to_matrix, 3},
    {"_meta_arrR_rcpp_matrix_to_list", (DL_FUNC) &_meta_arrR_rcpp_matrix_to_list, 2},
    {"_meta_arrR_rcpp_move_meta", (DL_FUNC) &_meta_arrR_rcpp_move_meta, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_meta_arrR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
