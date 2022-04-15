## code to prepare `DATASET` dataset goes here
library(meta.arrR)
library(arrR)

# get starting values
starting_values <- meta.arrR::default_starting

# get parameters
parameters <- meta.arrR::default_parameters

# set number of metaecosystems
n <- 3

# change values
starting_values$pop_n <- rep(x = 5, n = n)

# create 5 reef cells in center of seafloor
reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0), ncol = 2, byrow = TRUE)

# setup extent and grain
dimensions <- c(50, 50)
grain <- c(1, 1)

# set time per iterations
min_per_i <- 120

# run the model for n years
years <- 3
max_i <- (60 * 24 * 365 * years) / min_per_i

# save results only every m days
days <- 15 # which(max_i %% ((24 / (min_per_i / 60)) * (1:365)) == 0)
save_each <- (24 / (min_per_i / 60)) * days

parameters$nutrients_loss <- 0.1

parameters$move_residence <- max_i / 10

stable_vals <- arrR::get_stable_values(bg_biomass = starting_values$bg_biomass,
                                       ag_biomass = starting_values$ag_biomass,
                                       parameters = parameters)

starting_values$nutrients_pool <- stable_vals$nutrients_pool

starting_values$detritus_pool <- stable_vals$detritus_pool

nutrients_input <- sim_nutr_input(n = n, max_i = max_i, input_mn = stable_vals$nutrients_input,
                                  freq_mn = 5, amplitude_sd = 0.5)

metasyst <- setup_meta(n = n, max_i = max_i, dimensions = dimensions, grain = grain,
                       reef = reef, starting_values = starting_values, parameters = parameters)

result_readme <- run_simulation_meta(metasyst = metasyst, nutrients_input = nutrients_input,
                                     parameters = parameters, movement = "attr",
                                     max_i = max_i, save_each = save_each, min_per_i = min_per_i)

# usethis::use_data(result_readme, internal = TRUE, overwrite = FALSE)
