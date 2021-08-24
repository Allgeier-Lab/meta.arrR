# simulate nutrient input
nutr_noise <- meta.arrR::sim_nutr_input(n = n, max_i = max_i, input_mn = 1, freq_mn = 3,
                                        variability = 0.5, method = "noise")

# simulate nutrient input
nutr_sd <- meta.arrR::sim_nutr_input(n = n, max_i = max_i, input_mn = 1, freq_mn = 3,
                                     variability = 0.5, method = "sd", verbose = FALSE)

# extract values only
values_noise <- vapply(nutr_noise$values, FUN = length, FUN.VALUE = numeric(1))

values_sd <- vapply(nutr_sd$values, FUN = length, FUN.VALUE = numeric(1))

test_that("simulate_nutr_input returns meta_rn", {

  expect_is(object = nutr_noise, class = "nutr_input")

  expect_is(object = nutr_sd, class = "nutr_input")

})

test_that("simulate_nutr_input returns input for each n", {

  expect_length(object = nutr_noise$values, n = n)

  expect_length(object = nutr_sd$values, n = n)

})

test_that("simulate_nutr_input returns value for time step", {

  expect_equal(object = unname(values_noise), expected = rep(x = max_i, times = n))

  expect_equal(object = unname(values_sd), expected = rep(x = max_i, times = n))

})
