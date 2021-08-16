# set number of metaecosystems
n <- 3

# set max_i and save_each
max_i <- 10

# simulate nutrient input
nutr_noise <- simulate_nutr_input_noise(n = n, max_i = max_i, freq_mn = 3,
                                        input_max = 1, variability = 0.5)

# simulate nutrient input
nutr_jitter <- simulate_nutr_input_jitter(n = n, max_i = max_i, freq_mn = 3,
                                          input_max = 1, variability = 0.5)

# extract values only
values_noise <- vapply(nutr_noise$values, FUN = length, FUN.VALUE = numeric(1))

values_jitter <- vapply(nutr_jitter$values, FUN = length, FUN.VALUE = numeric(1))

test_that("simulate_nutr_input returns meta_rn", {

  expect_is(object = nutr_noise, class = "nutr_input")

  expect_is(object = nutr_jitter, class = "nutr_input")


})

test_that("simulate_nutr_input returns input for each n", {

  expect_length(object = nutr_noise$values, n = n)

  expect_length(object = nutr_jitter$values, n = n)


})

test_that("simulate_nutr_input returns value for time step", {

  expect_equal(object = values_noise, expected = rep(x = max_i, times = n))

  expect_equal(object = values_jitter, expected = rep(x = max_i, times = n))

})
