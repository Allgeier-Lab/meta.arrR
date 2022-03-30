
nutr_filter <- filter_meta(x = nutrients_input, as.integer(c(50, 75)))

result_filter <- filter_meta(x = result_rand, as.integer(c(50, 75)))

test_that("filter_meta returns correct classes", {

  expect_s3_class(object = nutr_filter, class = "nutr_input")

  expect_s3_class(object = result_filter, class = "meta_rn")

})

test_that("filter_meta returns filters data", {

  expect_true(object = all(sapply(X = result_filter$seafloor, FUN = function(i)
    all(range(i$timestep) == c(50, 74))))) # because save_each = 2

  expect_true(object = all(sapply(X = nutr_filter$values, FUN = function(i)
    all(range(i$timestep) == c(50, 75)))))

})
