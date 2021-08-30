// [[Rcpp::depends(RcppProgress, arrR)]]

#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <arrR.h>
#include "rcpp_meta_processes.h"
#include "rcpp_list_to_matrix.h"
#include "rcpp_move_meta.h"
#include "rcpp_which.h"
#include "rcpp_subset_matrix.h"

using namespace Rcpp;

//' rcpp_meta_processes
//'
//' @description
//' Rcpp run simulation of metaecosystems processes.
//'
//' @param seafloor,fishpop List with seafloor and fishpop data of metaecosystems.
//' @param seafloor_track,fishpop_track List with entry for each saving timestep and metaecosystems.
//' @param parameters List with parameters.
//' @param movement String specifying movement algorithm. Either 'rand', 'attr' or 'behav'.
//' @param max_dist Double with maximum movement distance.
//' @param n Integer with number of metaecosystems.
//' @param pop_n Vector with number of individuals.
//' @param fishpop_attributes Matrix with residence and reserves_thres values for each individual
//' @param nutr_input List with amount of nutrient input each timestep.
//' @param coords_reef List with ID and coords of reef cells.
//' @param cell_adj Matrix with cell adjacencies.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param max_i Integer with maximum number of simulation timesteps.
//' @param min_per_i Integer to specify minutes per i.
//' @param save_each Numeric how often data should be saved to return.
//' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
//' @param burn_in Numeric with timesteps used to burn in.
//' @param verbose If TRUE, progress reports are printed.
//'
//' @details
//' Wrapper function including all sub-processes. The core of the function is a
//' nested loop through all i) time steps and ii) all local metaecosystems.
//'
//' The functions is a 'wrapper' around the following sub-processes: i) movement across
//' local metaecosystems (outter loop) (ii) nutrient input, (iii) seagrass growth,
//' (iv) detritus mineralization, (v) movement of individuals, (vi) respiration of
//' individuals, (vii) growth of individuals, (viii) mortality of individuals,
//' (ix) diffusion of nutrients/detritus, and (x) nutrient output (all inner loop).
//'
//' For a detailed description of all sub-processes (with exception of (i)), see the
//' \pkg{arrR} package.
//'
//' @references
//' For a detailed model description, see Esquivel, K., Hesselbarth, M.H.K., Allgeier, J.E.
//' In preparation. Mechanistic support for increased primary production around artificial reefs.
//'
//' Add references about meta approach.
//'
//' @return void
//'
//' @aliases rcpp_meta_processes
//' @rdname rcpp_meta_processes
//'
//' @export
// [[Rcpp::export]]
void rcpp_meta_processes(Rcpp::List seafloor, Rcpp::List fishpop,
                         Rcpp::List seafloor_track, Rcpp::List fishpop_track,
                         Rcpp::List parameters, Rcpp::String movement, double max_dist,
                         int n, Rcpp::NumericVector pop_n, Rcpp::NumericMatrix fishpop_attributes,
                         Rcpp::List nutr_input,
                         Rcpp::List coords_reef, Rcpp::NumericMatrix cell_adj,
                         Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                         int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                         bool verbose) {

  // setup progress bar
  Progress progress(max_i, verbose);

  // init all flags to run processes //

  // init flag for movement across metasystem
  bool flag_move = Rcpp::is_true(Rcpp::any(pop_n > 0));

  // set movement to false if parameter is 0.0
  if (as<double>(parameters["move_residence"]) <= 0.0) {

    flag_move = FALSE;

  }

  // flag if diffusion needs to be run
  bool flag_diffuse = (as<double>(parameters["nutrients_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_fish_diffusion"])) > 0.0;

  // flag if nutrient output needs to be run
  bool flag_output = as<double>(parameters["nutrients_output"]) > 0.0;

  // init fish population things //

  // init sum of individuals
  int pop_n_sum = Rcpp::sum(pop_n);

  // get id of attribute matrix
  Rcpp::NumericVector id_attr_num = fishpop_attributes(_, 0);

  // convert to IntegerVector
  // MH: This is not very elegant
  Rcpp::IntegerVector id_attr = as<Rcpp::IntegerVector>(id_attr_num);

  // get residence values
  Rcpp::NumericVector residence_values = fishpop_attributes(_, 1);

  // get pop_reserves_thres values
  Rcpp::NumericVector pop_reserves_thres = fishpop_attributes(_, 2);

  // get matrix with inital values
  Rcpp::NumericMatrix fishpop_start = rcpp_list_to_matrix(fishpop, pop_n_sum, FALSE);

  // init seafloor stuff //

  // init list for cells_reef
  Rcpp::List cells_reef(n);

  // init various stuff //

  // get unique metaecosystem id
  Rcpp::IntegerVector id_meta = Rcpp::seq(1, fishpop.length());

  // calc time_frac for rcpp_seagrass_growth
  double time_frac = (min_per_i / 60.0) * seagrass_each;

  // save original data //
  for(int i = 0; i < n; i++) {

    as<Rcpp::List>(seafloor_track[i])[0] = Rcpp::clone(as<Rcpp::NumericMatrix>(seafloor[i]));

    as<Rcpp::List>(fishpop_track[i])[0] = Rcpp::clone(as<Rcpp::NumericMatrix>(fishpop[i]));

    // get only ID of reefs as vector
    cells_reef[i] = as<Rcpp::NumericMatrix>(coords_reef[i])(_, 0);

  }

  // run simulation, loop through timesteps
  for (int i = 1; i <= max_i; i++) {

    // check abort of function
    if (Progress::check_abort()) {

      Rcpp::stop("Stopped by user.");

    }

    // check if individuals move between meta systems
    if (flag_move) {

      fishpop = rcpp_move_meta(fishpop, residence_values, n, pop_n_sum,
                               id_attr, id_meta, extent);

    }

    // loop through all metaecosystems
    for (int j = 0; j < n; j++) {

      // get current number of individuals
      int pop_n_temp = as<Rcpp::NumericMatrix>(fishpop[j]).nrow();

      // if pop_n_temp either one individual or NA possible
      if (pop_n_temp == 1) {

        if (Rcpp::any(Rcpp::is_na(as<Rcpp::NumericMatrix>(fishpop[j])(0, _)))) {

          pop_n_temp = 0;

        }
      }

      // simulate nutrient input if present
      if (as<Rcpp::NumericVector>(nutr_input[j])(i - 1) > 0.0) {

        // simulate nutrient input
        arrR::rcpp_nutr_input(seafloor[j], as<Rcpp::NumericVector>(nutr_input[j])(i - 1));

      }

      // simulate seagrass only each seagrass_each iterations
      if ((i * min_per_i) % (seagrass_each * min_per_i) == 0) {

        // simulate seagrass growth
        arrR::rcpp_seagrass_growth(seafloor[j], cells_reef[j],
                                   parameters["bg_v_max"], parameters["bg_k_m"], parameters["bg_gamma"],
                                   parameters["ag_v_max"], parameters["ag_k_m"], parameters["ag_gamma"],
                                   parameters["bg_biomass_max"], parameters["bg_biomass_min"],
                                   parameters["ag_biomass_max"], parameters["ag_biomass_min"],
                                   parameters["seagrass_thres"], parameters["seagrass_slope"],
                                   parameters["seagrass_slough"], time_frac);

        // simulate mineralization (detritus to nutrients pool)
        arrR::rcpp_mineralization(seafloor[j], parameters["detritus_mineralization"],
                                  parameters["detritus_fish_decomp"]);

      }

      // fish individuals are present and i above burn_in
      if ((i > burn_in) && (pop_n_temp > 0)) {

        // get present ids
        Rcpp::NumericVector id_temp_num = as<Rcpp::NumericMatrix>(fishpop[j])(_, 0);

        // convert to IntegerVector
        // MH: This is not very elegant
        Rcpp::IntegerVector id_temp = as<Rcpp::IntegerVector>(id_temp_num);

        // get row id of current individual
        Rcpp::IntegerVector id_attr_temp = rcpp_which(id_attr, id_temp);

        // get pop_reserves_thres values of current ids
        Rcpp::NumericVector pop_reserves_thres_temp = pop_reserves_thres[id_attr_temp];

        // calculate new coordinates and activity
        arrR::rcpp_move_wrap(fishpop[j], coords_reef[j], movement, pop_reserves_thres_temp,
                             parameters["move_mean"], parameters["move_var"],
                             parameters["move_reef"], parameters["move_border"],
                             parameters["move_return"], max_dist, extent, dimensions);

        // simulate fish respiration (26°C is mean water temperature in the Bahamas)
        arrR::rcpp_respiration(fishpop[j], parameters["resp_intercept"], parameters["resp_slope"],
                               parameters["resp_temp_low"], parameters["resp_temp_max"],
                               parameters["resp_temp_optm"], 26.0, min_per_i);

        // get submatrix with present individuals at start
        Rcpp::NumericMatrix fishpop_start_temp = rcpp_subset_matrix(fishpop_start, id_attr_temp);

        // simulate fishpop growth and including change of seafloor pools
        arrR::rcpp_fishpop_growth(fishpop[j], fishpop_start_temp, seafloor[j],
                                  parameters["pop_k"], parameters["pop_linf"],
                                  parameters["pop_a"], parameters["pop_b"],
                                  parameters["pop_n_body"], parameters["pop_reserves_max"],
                                  parameters["pop_reserves_consump"], extent, dimensions, min_per_i);

        // simulate mortality
        arrR::rcpp_mortality(fishpop[j], fishpop_start_temp, seafloor[j],
                             parameters["pop_linf"], parameters["pop_n_body"],
                             parameters["pop_reserves_max"], extent, dimensions);

      }

      // only diffuse if all parameters larger than zero
      if (flag_diffuse) {

        // diffuse values between neighbors
        arrR::rcpp_diffuse_values(seafloor[j], cell_adj,
                                  parameters["nutrients_diffusion"], parameters["detritus_diffusion"],
                                  parameters["detritus_fish_diffusion"]);

      }

      // remove nutrients from cells if output parameter > 0
      if (flag_output) {

        arrR::rcpp_nutr_output(seafloor[j], parameters["nutrients_output"]);

      }

      // update tracking list
      if (i % save_each == 0) {

        as<Rcpp::List>(seafloor_track[j])[i / save_each] = Rcpp::clone(as<Rcpp::NumericMatrix>(seafloor[j]));

        as<Rcpp::List>(fishpop_track[j])[i / save_each] = Rcpp::clone(as<Rcpp::NumericMatrix>(fishpop[j]));

      }
    }

    // update progress bar
    progress.increment();

  }
}

/*** R
rcpp_meta_processes(seafloor = seafloor, fishpop = fishpop,
                   seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                   parameters = parameters, movement = movement, max_dist = max_dist,
                   n = metasyst$n, pop_n = metasyst$starting_values$pop_n,
                   fishpop_attributes = metasyst$fishpop_attributes,
                   nutr_input = nutr_input, max_i = max_i, min_per_i = min_per_i,
                   coords_reef = coords_reef, cell_adj = cell_adj,
                   extent = extent, dimensions = dimensions,
                   save_each = save_each, seagrass_each = seagrass_each, burn_in = burn_in,
                   verbose = verbose)
*/
