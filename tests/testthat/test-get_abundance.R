# get parameters
parameters <- meta.arrR::meta.arrR_parameters

starting_values <- meta.arrR::meta.arrR_starting_values

starting_values$pop_n <- c(2, 4, 8)

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

result <- simulate_meta(metasyst = metasyst, parameters = parameters,
                        max_i = max_i, min_per_i = 120, save_each = save_each)

abundance <- get_abundance(result)

test_that("get_abundance returns data.frame", {

  expect_is(object = abundance, class = "data.frame")

})

test_that("get_abundance has correct dimeonsions", {

  expect_equal(object = nrow(abundance), expected = (max_i / save_each + 1) * 3)

  expect_equal(object = ncol(abundance), expected = 3)

  expect_equal(object = colnames(abundance),
               expected = c("timestep", "meta", "abundance"))

})

test_that("get_abundance has value for each meta", {

  expect_equal(object = unique(abundance$meta), expected = 1:3)

})

test_that("get_abundance returns correct abundances ", {

  abundance_temp <- abundance[abundance$timestep == 0, "abundance"]

  expect_equal(object = abundance_temp, expected = starting_values$pop_n)

})
