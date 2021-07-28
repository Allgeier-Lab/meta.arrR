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

# set max_i and save_each
max_i <- 10

save_each <- 2

result <- simulate_meta(metasyst = metasyst, parameters = parameters,
                        max_i = max_i, min_per_i = 120, save_each = save_each)

# create vector with names
present_names <- c("seafloor", "fishpop", "movement", "n", "fishpop_attributes",
                   "starting_values", "parameters", "nutr_input", "max_i" , "min_per_i",
                   "burn_in", "save_each", "seagrass_each", "extent", "grain", "coords_reef")

test_that("simulate_meta returns meta_rn", {

  expect_is(object = result, class = "meta_rn")

})

test_that("simulate_meta contains all information", {

  expect_equal(object = names(result), expected = present_names)

})

test_that("simulate_meta returns list with seafloor and fishpop", {

  expect_length(object = result$seafloor, n = n)

  expect_length(object = result$fishpop, n = n)


  expect_equal(object = nrow(result$seafloor[[1]]),
               expected = (max_i / save_each + 1) * prod(extent))

  expect_equal(object = nrow(result$fishpop[[3]]),
               expected = (max_i / save_each + 1) * starting_values$pop_n)
})

test_that("simulate_meta returns correct information", {

  expect_equal(object = result$n, expected = n)

  expect_equal(object = result$grain, expected = c(grain, grain))

  # expect_equal(object = as.vector(result$extent), expected = extent)

  expect_equal(object = result$max_i, expected = max_i)

  expect_equal(object = result$save_each, expected = save_each)

})
