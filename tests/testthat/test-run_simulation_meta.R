# create vector with names
present_names <- c("seafloor", "fishpop", "nutrients_input", "n", "movement", "fishpop_attr",
                   "parameters", "starting_values", "extent", "grain", "dimensions",
                   "max_i" , "min_per_i", "burn_in", "seagrass_each", "save_each")

test_that("run_meta returns meta_rn", {

  expect_s3_class(object = result_rand, class = "meta_rn")

})

test_that("run_meta contains all information", {

  expect_equal(object = names(result_rand), expected = present_names)

})

test_that("run_meta returns list with seafloor and fishpop", {

  expect_length(object = result_rand$seafloor, n = n)

  expect_length(object = result_rand$fishpop, n = n)


  expect_equal(object = nrow(result_rand$seafloor[[1]]),
               expected = (max_i / save_each + 1) * prod(dimensions))

  expect_equal(object = nrow(result_rand$fishpop[[3]]),
               expected = (max_i / save_each + 1) * starting_values$pop_n[[3]])
})

test_that("run_meta returns correct information", {

  expect_equal(object = result_rand$n, expected = n)

  expect_equal(object = terra::ext(result_rand$extent),
               expected = terra::ext(metasyst$seafloor[[1]]))

  expect_equal(object = result_rand$grain, expected = c(grain, grain))

  expect_equal(object = result_rand$dimensions, expected = dimensions)

  expect_equal(object = result_rand$max_i, expected = max_i)

  expect_equal(object = result_rand$save_each, expected = save_each)

})
