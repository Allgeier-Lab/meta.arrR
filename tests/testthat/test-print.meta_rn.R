# get parameters
parameters <- meta.arrR::meta.arrR_parameters

starting_values <- meta.arrR::meta.arrR_starting_values

# set number of metaecosystems
n <- 3

# setup extent and grain
extent <- c(100, 100)
grain <- 1

# create 5 reef cells in center of seafloor
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                ncol = 2, byrow = TRUE)

# setup metaecosystems
metasyst <- setup_meta(n = n, extent = extent, grain = grain, reefs = reefs,
                       starting_values = starting_values, parameters = parameters)

metasyst_noreef <- setup_meta(n = n, extent = extent, grain = grain, reefs = NULL,
                              starting_values = starting_values, parameters = parameters)

# set max_i and save_each
max_i <- 10

save_each <- 2

result <- simulate_meta(metasyst = metasyst, parameters = parameters,
                        max_i = max_i, min_per_i = 120, save_each = save_each)

result_noreef <- simulate_meta(metasyst = metasyst_noreef, parameters = parameters,
                               max_i = max_i, min_per_i = 120, save_each = save_each)

test_that("print.meta_rn generates output", {

  expect_output(print(result))

  expect_output(print(result_noreef))

})
