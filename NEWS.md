# meta.arrR 0.8.0
* Sub-packages `arrR`

# meta.arrR 0.7.1
* No diffusion torus option `arrR`
* Updates to nutrient input simulations

# meta.arrR 0.7
* Use simplified cv calculations
* Rename `simulate_nutr_input` to `simulate_nutrient_sine`
* Adding `simulate_nutrient_noise`

# meta.arrR 0.6
* Remove `terra` dependency

# meta.arrR 0.5.1
* Changes to `simulate_nutr_input()`
* Changes to `rcpp_move_meta()`
* Rename all `*_var` parameters with `*_sd`
* Adding noise parameter to nutrients input

# meta.arrR 0.5
* Place local ecosystems in "empty space"
* Move some code to `Rcpp`

# meta.arrR 0.4
* Use `terra` instead of `raster`
* Rename `nutr_input` to `nutrients_input`
* Rename `nutrients_output` to `nutrients_loss`
* Rename `detritus_output` to `detritus_loss`

# meta.arrR 0.3
* Move only across neighboring ecosystems
* Add `calc_variability()` and `sample_variability()` function
* Add `simulate_nutr_input()` function
* Add `get_input_df()` function
* Add `filter_meta()` function
* Simplified code for `plot.meta_rn()`
* Add `get_meta_production()` and `plot_meta_production()`
* Remove `timestep` argument from many functions
* Print time-stamp during `run_meta()`
* Add `summarize_meta()` function and delete `get_meta_production`

# meta.arrR 0.2.1
* Adding `testthat` functions for all R functions
* Renamed `simulate_meta()` to `run_meta()`
* Adding plotting method for `summarize = FALSE`
* Adding `get_global_range()`
* Renamed `stationary` to `residence`

# meta.arrR 0.2.0
* First running framework
* Adding `Rcpp`

# meta.arrR 0.1.0
* Init package
* Added a `NEWS.md` file to track changes to the package.
