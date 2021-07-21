//#ifndef RCPP_SIM_PROCESSES
//#define RCPP_SIM_PROCESSES

void rcpp_sim_processes(Rcpp::List seafloor, Rcpp::List fishpop,
                        Rcpp::List seafloor_track, Rcpp::List fishpop_track,
                        Rcpp::List parameters, Rcpp::String movement, double max_dist,
                        int n, Rcpp::NumericVector pop_n, Rcpp::NumericMatrix fishpop_attributes,
                        Rcpp::List nutr_input,
                        Rcpp::List coords_reef, Rcpp::NumericMatrix cell_adj,
                        Rcpp::NumericVector extent,Rcpp::NumericVector dimensions,
                        int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                        bool verbose);

//#endif // RCPP_SIM_PROCESSES
