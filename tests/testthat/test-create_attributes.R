# get parameters
parameters <- meta.arrR::meta.arrR_parameters

starting_values <- meta.arrR::meta.arrR_starting_values

# set number of metaecosystems
n <- 3

# setup extent and grain
dimensions <- c(100, 100)
grain <- 1

# set time per iterations
min_per_i <- 120

# setup seafloor and fishpop
seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain, reefs = NULL,
                                 starting_values = starting_values)

fishpop <- lapply(1:n, function(i)
  arrR::setup_fishpop(seafloor = seafloor, starting_values = starting_values,
                      parameters = parameters))

attributes_matrix <- create_attributes(fishpop = fishpop, parameters = parameters)

test_that("create_attributes returns matrix", {

  expect_is(object = attributes_matrix, class = "matrix")

})

test_that("create_attributes has correct dimeonsions", {

  expect_equal(object = nrow(attributes_matrix), expected = n * starting_values$pop_n)

  expect_equal(object = ncol(attributes_matrix), expected = 3)

  expect_equal(object = colnames(attributes_matrix),
               expected = c("id", "residence", "reserves_thres"))

})

