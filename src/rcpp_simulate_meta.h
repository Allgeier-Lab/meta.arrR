//#ifndef RCPP_SIMULATE_META
//#define RCPP_SIMULATE_META

void rcpp_simulate_meta(Rcpp::List seafloor, Rcpp::List fishpop, Rcpp::NumericMatrix seafloor_probs,
                        Rcpp::List seafloor_track, Rcpp::List fishpop_track,
                        Rcpp::List parameters, Rcpp::String movement, double max_dist,
                        int n, Rcpp::NumericVector pop_n, Rcpp::NumericMatrix fishpop_attributes,
                        Rcpp::List nutrients_input, Rcpp::List coords_reef, Rcpp::NumericMatrix cell_adj,
                        Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                        int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                        bool verbose);

//#endif // RCPP_SIMULATE_META
