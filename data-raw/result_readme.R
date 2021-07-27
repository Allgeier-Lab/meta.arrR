## code to prepare `DATASET` dataset goes here
library(meta.arrR)
library(arrR)

# get starting values
starting_values <- meta.arrR::meta.arrR_starting_values

# get parameters
parameters <- meta.arrR::meta.arrR_parameters

parameters$nutrients_output <- 0.1

parameters$move_stationary <- max_i / (5 * 4)

# set number of metaecosystems
n <- 5

# change values
starting_values$pop_n <- rep(x = 6, times = n)

# create 5 reef cells in center of seafloor
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                ncol = 2, byrow = TRUE)

# setup extent and grain
extent <- c(100, 100)
grain <- 1

# set time per iterations
min_per_i <- 120

# run the model for n years
years <- 3
max_i <- (60 * 24 * 365 * years) / min_per_i

# save results only every m days
days <- 15
save_each <- max_i # (24 / (min_per_i / 60)) * days

max_i %% save_each

stable_vals <- arrR::get_stable_values(starting_values = starting_values,
                                       parameters = parameters)

starting_values$nutrients_pool <- stable_vals$nutrients_pool

starting_values$detritus_pool <- stable_vals$detritus_pool

metasyst <- setup_meta(n = n, extent = extent, grain = grain, reefs = reefs,
                       starting_values = starting_values, parameters = parameters)

nutr_input <- simulate_input_sine(n = n, max_i = max_i,
                                  freq_mn = 5, freq_sd = 0.1,
                                  input_max = starting_values$nutrients_pool *
                                    parameters$nutrients_output,
                                  input_sd = 0.1)

result <- simulate_meta(metasyst = metasyst, nutr_input = nutr_input,
                        parameters = parameters, movement = "attr",
                        max_i = max_i, save_each = save_each, min_per_i = min_per_i,
                        verbose = TRUE)

usethis::use_data(result, internal = TRUE, overwrite = TRUE)
