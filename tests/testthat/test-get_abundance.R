# get abundance
abundance <- get_abundance(result_behav)

test_that("get_abundance returns data.frame", {

  expect_s3_class(object = abundance, class = "data.frame")

})

test_that("get_abundance has correct dimensions", {

  expect_equal(object = nrow(abundance), expected = (max_i / save_each + 1) * n)

  expect_equal(object = ncol(abundance), expected = 3)

  expect_equal(object = colnames(abundance),
               expected = c("timestep", "meta", "abundance"))

})

test_that("get_abundance has value for each meta", {

  expect_equal(object = unique(abundance$meta), expected = 1:n)

})

test_that("get_abundance returns correct abundances ", {

  abundance_temp <- abundance[abundance$timestep == 0, "abundance"]

  expect_equal(object = abundance_temp, expected = starting_values$pop_n)

})
