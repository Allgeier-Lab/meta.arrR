// [[Rcpp::depends(RcppProgress, arrR)]]

#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <arrR.h>

#include "rcpp_simulate_meta.h"
#include "rcpp_list_to_matrix.h"
#include "rcpp_move_meta.h"

using namespace Rcpp;

//' rcpp_simulate_meta
//'
//' @description
//' Rcpp run simulation of metaecosystems processes.
//'
//' @param seafloor,fishpop List with seafloor and fishpop data of metaecosystems.
//' @param nutrients_input List with amount of nutrient input each timestep.
//' @param fishpop_attr NumericMatrix with residence and reserves_thres values for each individual
//' @param seafloor_probs NumericMatrix with local ecosystems probabilities.
//' @param seafloor_track,fishpop_track List with entry for each saving timestep and metaecosystems.
//' @param parameters List with parameters.
//' @param movement String specifying movement algorithm. Either 'rand', 'attr' or 'behav'.
//' @param extent NumericVector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions IntegerVector with dimensions (nrow, ncol).
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
//' Add references
//'
//' @return void
//'
//' @aliases rcpp_simulate_meta
//' @rdname rcpp_simulate_meta
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_simulate_meta(Rcpp::List seafloor, Rcpp::List fishpop, Rcpp::List nutrients_input,
                        Rcpp::NumericMatrix fishpop_attr, Rcpp::NumericMatrix seafloor_probs,
                        Rcpp::List seafloor_track, Rcpp::List fishpop_track,
                        Rcpp::List parameters, Rcpp::String movement,
                        Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                        int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                        bool verbose) {

  // init all flags to run processes //

  // init flag if fish are present
  bool flag_fish = fishpop_attr.nrow() > 0;

  // init flag for movement across metasystem
  bool flag_move = (as<double>(parameters["move_residence_mean"]) > 0.0) && flag_fish;

  // flag if diffusion needs to be run
  bool flag_diffuse = (as<double>(parameters["nutrients_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_fish_diffusion"])) > 0.0;

  // vector to store sum of nutrient inputs for flag
  Rcpp::NumericVector nutr_sum (nutrients_input.length());

  // loop and sum all nutrient inputs
  for (int i = 0; i < nutr_sum.length(); i++) {

    nutr_sum(i) = Rcpp::sum(as<Rcpp::NumericVector>(nutrients_input[i]));

  }

  // flag if nutrient input is present
  bool flag_input = Rcpp::sum(nutr_sum) > 0.0;

  // flag if nutrient output needs to be run
  bool flag_output = (as<double>(parameters["nutrients_loss"]) > 0.0) ||
    (as<double>(parameters["detritus_loss"]) > 0.0);

  // init seafloor related objects //

  // init list for coords_reef
  Rcpp::List coords_reef(seafloor.length());

  // get reef coords for each ecosystem
  for (int i = 0; i < seafloor.length(); i++) {

    // get reef coords matrix
    coords_reef[i] = arrR::rcpp_get_reef(seafloor[i]);

  }

  // get cell neighborhoods; identical for all ecosystems
  Rcpp::IntegerMatrix cell_adj = arrR::rcpp_get_adjacencies(dimensions);

  // init fishpop related objects //

  // init double for maximum movement distance
  double max_dist = 0.0;

  // calculate maximum movement distance
  if (flag_fish) {

    max_dist = arrR::rcpp_get_max_dist(movement, parameters, 1000000);

  }

  // get matrix with inital values
  Rcpp::NumericMatrix fishpop_start = rcpp_list_to_matrix(fishpop, fishpop_attr.nrow(),
                                                          false);

  // init various //

  // calc time_frac for rcpp_seagrass_growth
  double time_frac = (min_per_i / 60.0) * seagrass_each;

  // save original data //
  for(int i = 0; i < seafloor.length(); i++) {

    as<Rcpp::List>(seafloor_track[i])[0] = Rcpp::clone(as<Rcpp::NumericMatrix>(seafloor[i]));

    as<Rcpp::List>(fishpop_track[i])[0] = Rcpp::clone(as<Rcpp::NumericMatrix>(fishpop[i]));

  }

  // setup progress bar
  Progress progress(max_i, verbose);

  // run simulation, loop through timesteps
  for (int i = 1; i <= max_i; i++) {

    // check abort of function
    if (Progress::check_abort()) {

      Rcpp::stop("Stopped by user.");

    }

    // check if individuals move between meta systems
    if (flag_move && (i > burn_in)) {

      fishpop = rcpp_move_meta(fishpop, seafloor_probs, fishpop_attr, extent);

    }

    // loop through all metaecosystems
    for (int j = 0; j < seafloor.length(); j++) {

      // nutrient input present
      if (flag_input) {

        // get current nutrients input
        Rcpp::NumericVector nutrients_input_temp = nutrients_input[j];

        // adapt i index to C++
        int i_temp = i - 1;

        // simulate nutrient input if present at current timestep
        if (nutrients_input_temp(i_temp) > 0.0) {

          // simulate nutrient input
          arrR::rcpp_nutr_input(seafloor[j], nutrients_input_temp(i_temp));

        }
      }

      // simulate seagrass only each seagrass_each iterations
      if ((i % seagrass_each) == 0) {

        // simulate seagrass growth
        arrR::rcpp_seagrass_growth(seafloor[j], parameters["bg_v_max"], parameters["bg_k_m"], parameters["bg_gamma"],
                                   parameters["ag_v_max"], parameters["ag_k_m"], parameters["ag_gamma"],
                                   parameters["bg_biomass_max"], parameters["bg_biomass_min"],
                                   parameters["ag_biomass_max"], parameters["ag_biomass_min"],
                                   parameters["seagrass_thres"], parameters["seagrass_slope"],
                                   parameters["seagrass_slough"], time_frac);

        // simulate mineralization (detritus to nutrients pool)
        arrR::rcpp_mineralization(seafloor[j], parameters["detritus_mineralization"],
                                  parameters["detritus_fish_decomp"]);

      }

      // run if fish are present
      if (flag_fish) {

        // get current number of individuals
        int pop_n_temp = as<Rcpp::NumericMatrix>(fishpop[j]).nrow();

        // if set pop_n_temp to zero if row is actually NA
        if ((pop_n_temp == 1) && (NumericVector::is_na(as<Rcpp::NumericMatrix>(fishpop[j])(0,0)))) {

          pop_n_temp = 0;

        }

        // fish individuals are present and i above burn_in
        if ((i > burn_in) && (pop_n_temp > 0)) {

          // creating final matrix
          Rcpp::NumericMatrix fishpop_start_temp(pop_n_temp, as<Rcpp::NumericMatrix>(fishpop[j]).ncol());

          // loop through all individuals
          for (int k = 0; k < pop_n_temp; k++) {

            // get row id of current individual. Same id for fishpop_attr & fishpop_start
            int id_temp = arrR::rcpp_which(as<Rcpp::NumericMatrix>(fishpop[j])(k, 0),
                                           fishpop_attr(_, 0));

            // fill matrix with inital fishpop values for reincarnation
            fishpop_start_temp(k, _) = fishpop_start(id_temp, _);

          }

          // calculate new coordinates and activity
          arrR::rcpp_move_wrap(fishpop[j], fishpop_attr, movement,
                               parameters["move_mean"], parameters["move_sd"],
                               parameters["move_reef"], parameters["move_border"],
                               parameters["move_return"], max_dist, coords_reef[j],
                               extent, dimensions);

          // simulate fish respiration (26°C is mean water temperature in the Bahamas)
          arrR::rcpp_respiration(fishpop[j], parameters["resp_intercept"], parameters["resp_slope"],
                                 parameters["resp_temp_low"], parameters["resp_temp_max"],
                                 parameters["resp_temp_optm"], 26.0, min_per_i);

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

        arrR::rcpp_nutr_output(seafloor[j], parameters["nutrients_loss"], parameters["detritus_loss"]);

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
