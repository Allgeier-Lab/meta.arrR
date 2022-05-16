# create vector with names
present_names <- c("seafloor", "fishpop", "nutrients_input", "n", "movement", "fishpop_attr",
                   "parameters", "starting_values", "dimensions", "extent", "grain",
                   "max_i" , "min_per_i", "burn_in", "seagrass_each", "save_each")

test_that("run_meta returns meta_rn", {

  expect_s3_class(object = result_behav, class = "meta_rn")

})

test_that("run_meta contains all information", {

  expect_equal(object = names(result_behav), expected = present_names)

})

test_that("run_meta returns list with seafloor and fishpop", {

  expect_length(object = result_behav$seafloor, n = n)

  expect_length(object = result_behav$fishpop, n = n)


  expect_equal(object = nrow(result_behav$seafloor[[1]]),
               expected = (max_i / save_each + 1) * prod(dimensions))

  expect_equal(object = nrow(result_behav$fishpop[[3]]),
               expected = (max_i / save_each + 1) * starting_values$pop_n[[3]])
})

test_that("run_meta returns correct information", {

  expect_equal(object = result_behav$n, expected = n)

  seafloor_dim <- arrR::get_seafloor_dim(metasyst$seafloor[[1]])

  expect_equal(object = result_behav$extent,
               expected = seafloor_dim$extent)

  expect_equal(object = result_behav$grain, expected = grain)

  expect_equal(object = unname(result_behav$dimensions), expected = dimensions)

  expect_equal(object = result_behav$max_i, expected = max_i)

  expect_equal(object = result_behav$save_each, expected = save_each)

})
