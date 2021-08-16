# set number of metaecosystems
n <- 3

# set max_i and save_each
max_i <- 10

# simulate nutrients
nutr <- sim_nutr_input_noise(n = n, max_i = max_i, freq_mn = 3,
                             input_max = 1, variability = 0.5)

test_that("print.nutr_input generates output", {

  expect_output(print(nutr))

})
