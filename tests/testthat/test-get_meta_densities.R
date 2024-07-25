densities <- get_meta_densities(result = result_behav, verbose = FALSE)

test_that("get_meta_densities returns data.frame", {

  expect_s3_class(object = densities, class = "data.frame")

  expect_equal(object = nrow(densities), expected = prod(metasyst$dimensions) * metasyst$n)

})
