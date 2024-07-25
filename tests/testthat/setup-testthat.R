library(arrR)
library(dplyr)

# get parameters
parameters <- meta.arrR::default_parameters

# get starting values
starting_values <- meta.arrR::default_starting

# change starting values
starting_values$pop_n <- c(2, 4, 8)

# set number of metaecosystems
n <- 3

# create 5 reef cells in center of seafloor
reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
               ncol = 2, byrow = TRUE)

# setup extent and grain
dimensions <- c(100, 100)

grain <- 1

# set time per iterations
min_per_i <- 120

# set maximum i
max_i <- 100

min_per_i <- 120

seagrass_each <- 12

save_each <- 2

# simulate nutrient input
nutrients_input <- meta.arrR::simulate_nutrient_sine(n = n, max_i = max_i, input_mn = 1, frequency = 3,
                                                     noise = 0.5)

# setup metaecosystems
metasyst <- meta.arrR::setup_meta(n = n, dimensions = dimensions, grain = grain, reef = reef,
                                  starting_values = starting_values, parameters = parameters,
                                  verbose = FALSE)

# setup metaecosystems w/o reef
metasyst_nr <- meta.arrR::setup_meta(n = n, dimensions = dimensions, grain = grain, reef = NULL,
                                     starting_values = starting_values, parameters = parameters,
                                     verbose = FALSE)

# run model
result_behav <- meta.arrR::run_simulation_meta(metasyst = metasyst, parameters = parameters,
                                               movement = "behav",  max_i = max_i, save_each = save_each,
                                               min_per_i = min_per_i)
