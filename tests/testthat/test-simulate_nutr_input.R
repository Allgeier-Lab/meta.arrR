# set number of metaecosystems
n <- 3

# set max_i and save_each
max_i <- 10

save_each <- 2

nutr <- simulate_nutr_input(n = n, max_i = max_i, freq_mn = 3, freq_sd = 0.1,
                            input_max = 0.1, input_sd = 0.5)

values <- vapply(nutr, FUN = length, FUN.VALUE = numeric(1))

test_that("simulate_nutr_input returns meta_rn", {

  expect_is(object = nutr, class = "nutr_input")

})

test_that("simulate_nutr_input returns input for each n", {

  expect_length(object = nutr, n = n)

})

test_that("simulate_nutr_input returns value for time step", {

  expect_equal(object = values, expected = rep(x = max_i, times = n))

})
