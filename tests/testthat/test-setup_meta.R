# get parameters
parameters <- meta.arrR::meta.arrR_parameters

starting_values <- meta.arrR::meta.arrR_starting_values

# set number of metaecosystems
n <- 3

# setup dimensions and grain
dimensions <- c(100, 100)
grain <- 1

# create 5 reef cells in center of seafloor
reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                ncol = 2, byrow = TRUE)

# setup metaecosystems
metasyst <- setup_meta(n = n, dimensions = dimensions, grain = grain, reefs = reefs,
                       starting_values = starting_values, parameters = parameters)

metasyst_noreef <- setup_meta(n = n, dimensions = dimensions, grain = grain, reefs = NULL,
                              starting_values = starting_values, parameters = parameters)

# create vector with names
present_names <- c("seafloor", "fishpop", "n", "fishpop_attributes",
                   "starting_values", "parameters", "reefs", "extent",
                   "grain", "dimensions")

test_that("setup_meta returns meta_syst", {

  expect_is(object = metasyst, class = "meta_syst")

  expect_is(object = metasyst_noreef, class = "meta_syst")

})

test_that("setup_meta contains all information", {

  expect_equal(object = names(metasyst), expected = present_names)

  expect_equal(object = names(metasyst_noreef), expected = present_names)

})

test_that("setup_meta returns list with seafloor and fishpop", {

  expect_length(object = metasyst$seafloor, n = n)

  expect_length(object = metasyst$fishpop, n = n)


  expect_length(object = metasyst_noreef$seafloor, n = n)

  expect_length(object = metasyst_noreef$fishpop, n = n)

})

test_that("setup_meta returns correct information", {

  expect_equal(object = metasyst$n, expected = n)

  expect_equal(object = metasyst$extent,
               expected = raster::extent(dimensions / 2 * c(-1, 1, -1, 1)))

  expect_equal(object = metasyst$grain, expected = c(grain, grain))

  expect_equal(object = metasyst$dimensions, expected = dimensions)


  expect_equal(object = metasyst_noreef$n, expected = n)

  expect_equal(object = metasyst_noreef$extent,
               expected = raster::extent(dimensions / 2 * c(-1, 1, -1, 1)))

  expect_equal(object = metasyst_noreef$grain, expected = c(grain, grain))

  expect_equal(object = metasyst_noreef$dimensions, expected = dimensions)

})
