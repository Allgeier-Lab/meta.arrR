// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

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
Rcpp::NumericMatrix rcpp_list_to_matrix(Rcpp::List fishpop, int pop_n_sum, bool id);
RcppExport SEXP _meta_arrR_rcpp_list_to_matrix(SEXP fishpopSEXP, SEXP pop_n_sumSEXP, SEXP idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< int >::type pop_n_sum(pop_n_sumSEXP);
    Rcpp::traits::input_parameter< bool >::type id(idSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_list_to_matrix(fishpop, pop_n_sum, id));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_matrix_to_list
Rcpp::List rcpp_matrix_to_list(Rcpp::NumericMatrix fishpop, int n);
RcppExport SEXP _meta_arrR_rcpp_matrix_to_list(SEXP fishpopSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_matrix_to_list(fishpop, n));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_move_meta
Rcpp::List rcpp_move_meta(Rcpp::List fishpop, Rcpp::NumericMatrix seafloor_probs, Rcpp::NumericMatrix fishpop_attr, Rcpp::NumericVector extent);
RcppExport SEXP _meta_arrR_rcpp_move_meta(SEXP fishpopSEXP, SEXP seafloor_probsSEXP, SEXP fishpop_attrSEXP, SEXP extentSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor_probs(seafloor_probsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_attr(fishpop_attrSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_move_meta(fishpop, seafloor_probs, fishpop_attr, extent));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_simulate_meta
void rcpp_simulate_meta(Rcpp::List seafloor, Rcpp::List fishpop, Rcpp::List nutrients_input, Rcpp::NumericMatrix fishpop_attr, Rcpp::NumericMatrix seafloor_probs, Rcpp::List seafloor_track, Rcpp::List fishpop_track, Rcpp::List parameters, Rcpp::String movement, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions, int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in, bool verbose);
RcppExport SEXP _meta_arrR_rcpp_simulate_meta(SEXP seafloorSEXP, SEXP fishpopSEXP, SEXP nutrients_inputSEXP, SEXP fishpop_attrSEXP, SEXP seafloor_probsSEXP, SEXP seafloor_trackSEXP, SEXP fishpop_trackSEXP, SEXP parametersSEXP, SEXP movementSEXP, SEXP extentSEXP, SEXP dimensionsSEXP, SEXP max_iSEXP, SEXP min_per_iSEXP, SEXP save_eachSEXP, SEXP seagrass_eachSEXP, SEXP burn_inSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type nutrients_input(nutrients_inputSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_attr(fishpop_attrSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor_probs(seafloor_probsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type seafloor_track(seafloor_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type parameters(parametersSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type movement(movementSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< int >::type max_i(max_iSEXP);
    Rcpp::traits::input_parameter< int >::type min_per_i(min_per_iSEXP);
    Rcpp::traits::input_parameter< int >::type save_each(save_eachSEXP);
    Rcpp::traits::input_parameter< int >::type seagrass_each(seagrass_eachSEXP);
    Rcpp::traits::input_parameter< int >::type burn_in(burn_inSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_simulate_meta(seafloor, fishpop, nutrients_input, fishpop_attr, seafloor_probs, seafloor_track, fishpop_track, parameters, movement, extent, dimensions, max_i, min_per_i, save_each, seagrass_each, burn_in, verbose);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_meta_arrR_rcpp_get_table", (DL_FUNC) &_meta_arrR_rcpp_get_table, 2},
    {"_meta_arrR_rcpp_list_to_matrix", (DL_FUNC) &_meta_arrR_rcpp_list_to_matrix, 3},
    {"_meta_arrR_rcpp_matrix_to_list", (DL_FUNC) &_meta_arrR_rcpp_matrix_to_list, 2},
    {"_meta_arrR_rcpp_move_meta", (DL_FUNC) &_meta_arrR_rcpp_move_meta, 4},
    {"_meta_arrR_rcpp_simulate_meta", (DL_FUNC) &_meta_arrR_rcpp_simulate_meta, 17},
    {NULL, NULL, 0}
};

RcppExport void R_init_meta_arrR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
