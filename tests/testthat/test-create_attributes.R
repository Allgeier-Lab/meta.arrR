# setup seafloor and fishpop
seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain, reefs = NULL,
                                 starting_values = starting_values, verbose = FALSE)

starting_values$pop_n <- 8

# setupt fishpop
fishpop <- lapply(1:n, function(i)
  arrR::setup_fishpop(seafloor = seafloor, starting_values = starting_values,
                      parameters = parameters, verbose = FALSE))

# create attributes
attributes_matrix <- create_attributes(fishpop = fishpop, parameters = parameters)

test_that("create_attributes returns matrix", {

  expect_is(object = attributes_matrix, class = "matrix")

})

test_that("create_attributes has correct dimensions", {

  expect_equal(object = nrow(attributes_matrix), expected = n * starting_values$pop_n)

  expect_equal(object = ncol(attributes_matrix), expected = 3)

  expect_equal(object = colnames(attributes_matrix),
               expected = c("id", "residence", "reserves_thres"))

})

