# get parameters
parameters <- meta.arrR::meta.arrR_parameters

starting_values <- meta.arrR::meta.arrR_starting_values

# set number of metaecosystems
n <- 3

# setup extent and grain
dimensions <- c(100, 100)
grain <- 1

# create 5 reef cells in center of seafloor
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                ncol = 2, byrow = TRUE)

# setup metaecosystems
metasyst <- setup_meta(n = n, dimensions = dimensions, grain = grain, reefs = reefs,
                       starting_values = starting_values, parameters = parameters)

# setup metaecosystems
metasys_noreef <- setup_meta(n = n, dimensions = dimensions, grain = grain, reefs = NULL,
                             starting_values = starting_values, parameters = parameters)

# set max_i and save_each
max_i <- 10

save_each <- 2

result <- run_meta(metasyst = metasyst, parameters = parameters,
                   max_i = max_i, min_per_i = 120, save_each = save_each)

result_noreef <- run_meta(metasyst = metasys_noreef, parameters = parameters,
                          movement = "attr",
                          max_i = max_i, min_per_i = 120, save_each = save_each)

# create vector with names
present_names <- c("seafloor", "fishpop", "n", "fishpop_attributes", "movement",
                   "starting_values", "parameters", "nutr_input",
                   "coords_reef", "extent",  "grain", "dimensions",
                   "max_i" , "min_per_i", "burn_in", "seagrass_each", "save_each")

test_that("run_meta returns meta_rn", {

  expect_is(object = result, class = "meta_rn")

})

test_that("run_meta contains all information", {

  expect_equal(object = names(result), expected = present_names)

})

test_that("run_meta returns list with seafloor and fishpop", {

  expect_length(object = result$seafloor, n = n)

  expect_length(object = result$fishpop, n = n)


  expect_equal(object = nrow(result$seafloor[[1]]),
               expected = (max_i / save_each + 1) * prod(dimensions))

  expect_equal(object = nrow(result$fishpop[[3]]),
               expected = (max_i / save_each + 1) * starting_values$pop_n)
})

test_that("run_meta returns correct information", {

  expect_equal(object = result$n, expected = n)

  expect_equal(object = result$extent,
               expected = raster::extent(metasyst$seafloor[[1]]))

  expect_equal(object = result$grain, expected = c(grain, grain))

  expect_equal(object = result$dimensions, expected = dimensions)

  expect_equal(object = result$max_i, expected = max_i)

  expect_equal(object = result$save_each, expected = save_each)

})
