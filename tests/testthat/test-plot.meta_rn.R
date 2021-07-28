# get parameters
parameters <- meta.arrR::meta.arrR_parameters

starting_values <- meta.arrR::meta.arrR_starting_values

# set number of metaecosystems
n <- 3

# setup extent and grain
extent <- c(100, 100)
grain <- 1

# set time per iterations
min_per_i <- 120

# setup metaecosystems
metasyst <- setup_meta(n = n, extent = extent, grain = grain, reefs = NULL,
                       starting_values = starting_values, parameters = parameters)

# set max_i and save_each
max_i <- 10

save_each <- 2

result <- run_meta(metasyst = metasyst, parameters = parameters,
                   max_i = max_i, min_per_i = 120, save_each = save_each)

result_gg <- plot(result)

test_that("plot.meta_rn returns ggplot", {

  expect_is(object = result_gg, class = "ggplot")

})
