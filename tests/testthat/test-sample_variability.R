sample_input <- sample_variability(x = nutrients_input, itr = 5)

sample_result <- sample_variability(x = result_behav, itr = 5)

test_that("sample_variability returns correct classes", {

  expect_s3_class(object = sample_input, class = "data.frame")

  expect_true(object = inherits(x = sample_result, what = "list"))

})

test_that("sample_variability returns mean and sd", {

  expect_false(object = anyNA(sample_input))

  expect_false(object = all(sapply(sample_result, anyNA)))

})
