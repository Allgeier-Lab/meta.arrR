library(arrR)
library(dplyr)
library(raster)

# get parameters
parameters <- meta.arrR::meta.arrR_parameters

# get starting values
starting_values <- meta.arrR::meta.arrR_starting_values

# change starting values
starting_values$pop_n <- c(2, 4, 8)

# set number of metaecosystems
n <- 3

# create 5 reef cells in center of seafloor
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
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
nutr_input <- meta.arrR::sim_nutr_input(n = n, max_i = max_i, input_mn = 1, freq_mn = 3,
                                        variability = 0.5)

# setup metaecosystems
metasyst <- meta.arrR::setup_meta(n = n, dimensions = dimensions, grain = grain, reefs = reefs,
                                  starting_values = starting_values, parameters = parameters,
                                  verbose = FALSE)

# setup metaecosystems
metasyst_nr <- meta.arrR::setup_meta(n = n, dimensions = dimensions, grain = grain, reefs = NULL,
                                     starting_values = starting_values, parameters = parameters,
                                     verbose = FALSE)

result_rand <- meta.arrR::run_meta(metasyst = metasyst, parameters = parameters,
                                   movement = "rand",  max_i = max_i, min_per_i = min_per_i,
                                   save_each = save_each, seagrass_each = seagrass_each,
                                   verbose = FALSE)

result_rand_nr <- meta.arrR::run_meta(metasyst = metasyst_nr, parameters = parameters,
                                      movement = "rand",  max_i = max_i, min_per_i = min_per_i,
                                      save_each = save_each, seagrass_each = seagrass_each,
                                      verbose = FALSE)

result_attr <- meta.arrR::run_meta(metasyst = metasyst, parameters = parameters,
                                   movement = "attr",  max_i = max_i, min_per_i = min_per_i,
                                   save_each = save_each, seagrass_each = seagrass_each,
                                   verbose = FALSE)
